parseQueries <- function(query){
  query = gsub(' OR ', ' | ', query)
  query = gsub(' AND ', ' & ', query)
  query = gsub(' NOT ', ' &! ', query)

  ## also allow empty space as OR
  query = gsub('(?<=[*a-zA-Z0-9?)])[ ]+(?=[*a-zA-Z0-9?(])', ' | ', query, perl=T)

  ## make " * ", as a 'find all' solution, an immediate TRUE
  query = tolower(query) # safety first: for the odd possibility that someone uses T or F as a query term, which would be interpreted as TRUE or FALSE
  query = gsub('(?<= )\\*(?= )|(?<=^)\\*(?= )', 'T', query, perl=T)

  query_form = as.list(gsub('([*a-z0-9/~_-]+)', '%s', query)) # note that uppercase is not replaced, to keep the TRUE
  query_terms = regmatches(query, gregexpr('([*a-z0-9/~_-]+)', query))

  query_form[query_form == ''] = NA
  t(mapply(function(x,y) list(form=x, terms=y), query_form, query_terms))
}

fillQuery <- function(query_values, query_form){
  do.call(sprintf, as.list(c(query_form, query_values)))
}

evalQuery <- function(query_values, query_form){
  eval(parse(text=fillQuery(query_values, query_form)))
}

evalQueryMatrix <- function(qm, terms, form){
  apply(qm[,terms, drop=F], MARGIN = 1, evalQuery, query_form=form)
}

getTermRegex <- function(terms, default.window=50){
  terms = parseQueries(terms)
  terms = unlist(terms[,2])
  terms = data.frame(term = terms,
                     regex = gsub('~.*', '', terms),
                     window = as.numeric(ifelse(grepl('~', terms) == T, gsub('.*~', '', terms), default.window)))
  terms$regex = gsub('*', '.*', terms$regex, fixed=T)
  terms$regex = sprintf('\\b%s\\b', terms$regex)
  terms
}



#' Annotate a data frame of tokens with codes using Lucene-like search queries
#'
#' @param tokens a data frame of tokens containing columns for article id, term location and term string (column names can be specified in aid_var, term_i_var and term_var parameters, respectively).
#' @param queries a data frame containing the queries.
#' @param default.window
#' @param aid_var a character string giving the name of the article id column
#' @param term_i_var a character string giving the name of the term location column
#' @param term_var a character string giving the name of the term string column
#' @param condition_once logical. If TRUE, then if an indicator satisfies its conditions once in an article, all indicators within that article are coded.
#' @param indicator_filter
#' @param presorted The data has to be sorted on order(aid_var, term_i_var). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#'
#' @return the annotated tokens data frame
#' @export
codeTokens <- function(tokens, queries, default.window=25, indicator_filter=rep(T, nrow(tokens)), aid_var='aid', term_i_var='id', term_var='word', condition_once=FALSE, presorted=F){
  ## consider underscores as wordboundaries (important for some parsers that chunk words together, separated by underscores)
  if(!class(tokens[,term_var]) == 'factor') as.factor(tokens[,term_var])
  levels(tokens[,term_var]) = gsub('_', ' ', levels(tokens[,term_var]), fixed=T)

  ## make query forms for the indicators and conditions
  ind = parseQueries(queries$indicator)
  con = parseQueries(queries$condition)

  ## get regular expressions for each term in the queries
  indr = getTermRegex(queries$indicator, 1) # for indicators the window should always be 1 (the exact location of the indicator)
  conr = getTermRegex(queries$condition, default.window)
  query_regex = unique(rbind(indr, conr))

  ## speed up: filter the tokens list
  ## only look at articles in which at least one of the indicator terms occurs
  article_filter = unique(tokens[tokenGrepl(indr$regex, tokens[,term_var]), aid_var])
  article_filter = which(tokens[,aid_var] %in% article_filter)
  ## only look at tokens that match one of the regex (and only for the relevant articles)
  tokens_within_article_filter = tokenGrepl(query_regex$regex, tokens[article_filter,term_var])
  tokens_filter = article_filter[tokens_within_article_filter]

  ## filter tokens, and use an index column to keep the original row location in tokenlist (so it can be returned in this position without having to match values)
  tokens$i = 1:nrow(tokens)
  ftokens = tokens[tokens_filter, c('i', aid_var, term_i_var, term_var)]
  indicator_filter = indicator_filter[tokens_filter] ## the indicator filter needs to match the filtered tokens list

  ## create matrix where rows are tokens, columns are the query terms, and cells indicate whether the query terms occur (within the given word distance) at the place of each token.
  qm = getQueryMatrix(ftokens, query_regex, aid_var, term_i_var, term_var, presorted)

  ## evaluate queries
  nqueries = nrow(queries)
  ftokens$code = ''
  for(i in 1:nqueries){
    if(i %% 10 == 0) message(sprintf('\t%s / %s', i, nqueries))
    indicator_columns = ind[i,]$terms
    ind_hit = Matrix::rowSums(qm[,indicator_columns,drop=F]) > 0 ## indicator query can only contain OR statements, so this is a fast alternative to evalQueryMatrix
    ind_hit = ind_hit & indicator_filter ## indicator tokens have to pass the filter
    if(sum(ind_hit) == 0) next

    ## evaluate condition queries
    con_terms = con[i,]$terms
    con_form = con[i,]$form
    code = as.character(queries$code[i])
    if(is.na(con_form)){
      ## if no condition is given, indicator hits are all we need
      ftokens$code[ind_hit] = code
    } else {
      hit_and_condition = evalQueryMatrix(qm[ind_hit,,drop=F], con_terms, con_form)
      if(sum(hit_and_condition) == 0) next
      ftokens$code[ind_hit] = ifelse(hit_and_condition, code, ftokens$code[ind_hit])
    }

    ## if condition_once is TRUE, then all indicators hits are also coded if the indicator satisfies its condition at least once within the article
    if(condition_once){
      article_with_code = unique(ftokens[ftokens$code == code, aid_var]) # articles in which indicator satisfies condition at least once
      hit_and_code = ftokens[ind_hit, aid_var] %in% article_with_code # for all indicator hits, check whether they occur in one of these articles.
      ftokens$code[ind_hit] = ifelse(hit_and_code, code, ftokens$code[ind_hit])
    }
  }
  tokens$code = ''
  tokens$code[ftokens$i] = ftokens$code ## add code to original location in tokenlist
  tokens$code
}

#' Get keyword-in-context from a token list
#'
#' @param tokens a data frame of tokens containing columns for article id, term location and term string (column names can be specified in aid_var, term_i_var and term_var parameters, respectively).
#' @param nwords the number of words in front and after the keyword
#' @param aid_var a character string giving the name of the article id column
#' @param term_i_var a character string giving the name of the term location column
#' @param hits
#' @param prettypaste
#' @param term_var a character string giving the name of the term string column
#'
#' @return A data.frame with the keyword in context
#' @export
kwic <- function(tokens, hits, nwords=10, aid_var='aid', term_i_var='id', term_var='word', prettypaste=T){
  token_i = tokenLookup(tokens, hits[,aid_var], hits[,term_i_var], aid_var, term_i_var)

  kwicldply <- function(i, aids, terms, nwords){
    aid = aids[i]
    sent_i = (i-nwords):(i+nwords)
    sent = as.character(terms[sent_i])
    sent = gsub('\\[|\\]', '', sent)

    sent[nwords+1] = sprintf('[%s]', sent[nwords+1])
    sent = sent[aids[sent_i] == aid] # only show context words if they occur in the same article
    data.frame(aid=aid, kwic=paste(sent, collapse=' '))
  }
  o = ldply(token_i, kwicldply, aids=tokens[,aid_var], terms=tokens[,term_var], nwords=nwords)
  if(prettypaste) {
    o$kwic = gsub('_', ' ', o$kwic)
    o$kwic = gsub('  ', ' ', o$kwic)
    o$kwic = gsub(" ([.,?!:;>)])", '\\1', o$kwic)
    o$kwic = gsub('([(<]) ', '\\1', o$kwic)
    o$kwic = sprintf('...%s...', o$kwic)
  }
  o$kwic
}

tokenLookup <- function(tokens, article_id, term_i, aid_var='aid', term_i_var='id'){
  tokens$i = 1:nrow(tokens)
  tokens = tokens[tokens[,aid_var] %in% unique(article_id), c('i', aid_var, term_i_var)]
  which.sub = match(paste(article_id, term_i, sep='___'),
                    paste(tokens[,aid_var], tokens[,term_i_var], sep='___'))
  tokens$i[which.sub]
}

#' Search for tokens in a tokenlist using indicators with conditions
#'
#' Tokens need to be sorted on order(aid_var, term_i_var), or presorted needs to be set to FALSE. Note that if this function is used often, its faster to sort tokens first and use presorted = TRUE.
#'
#' @param tokens
#' @param indicator
#' @param condition
#' @param meta
#' @param aid_var
#' @param term_i_var
#' @param term_var
#' @param condition_once logical. If TRUE, then if an indicator satisfies its conditions once in an article, all indicators within that article are coded.
#' @param presorted The data has to be sorted on order(aid_var, term_i_var). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#' @param default.window
#' @param indicator_filter
#'
#' @return the tokens that match the query
#' @export
searchQuery <- function(tokens, indicator, condition='', default.window=25, condition_once=FALSE, presorted=T, indicator_filter=rep(T, nrow(tokens)), aid_var='aid', term_i_var='id', term_var='word', hitcount=T, tokenfreq=F, keywordIC=F, ...){
  tokens$code = NULL
  query = data.frame(code=1, indicator=indicator, condition=condition)
  code = codeTokens(tokens, query, indicator_filter=indicator_filter, aid_var=aid_var, term_i_var=term_i_var, term_var=term_var, presorted=presorted, default.window = default.window)
  hits = tokens[code == 1, c(aid_var, term_i_var, term_var)]

  reportSummary(tokens, hits, hitcount, tokenfreq, keywordIC, aid_var=aid_var, term_i_var=term_i_var, term_var=term_var, ...)
  hits
}

#' Title
#'
#' @param tokens
#' @param hits
#' @param tokenfreq
#' @param keywordIC
#' @param kwic_nwords
#' @param kwic_sample
#' @param random_sample
#' @param aid_var
#' @param term_i_var
#' @param term_var
#'
#' @export
reportSummary <- function(tokens, hits, hitcount=T, tokenfreq=T, keywordIC=T, kwic_nwords=10, kwic_sample=10, random_sample=T, aid_var='aid', term_i_var='id', term_var='word'){
  ## report number of hits and articles
  if(hitcount) reportHitcount(tokens, hits, aid_var)

  ## report token frequency
  if(tokenfreq & nrow(hits) > 0) {
    cat('\n')
    reportTokenFreq(hits, aid_var, term_var)
  }

  ## report keywords in context
  if(keywordIC & nrow(hits) > 0) {
    cat('\n')
    reportKWIC(tokens, hits, kwic_nwords, kwic_sample, random_sample, aid_var, term_i_var, term_var)
  }
}

#' Title
#'
#' @param tokens
#' @param hits
#' @param aid_var
#'
#' @export
reportHitcount <- function(tokens, hits, aid_var='aid'){
  nhits = nrow(hits)
  narts = length(unique(hits[,aid_var]))
  message(sprintf('%s hit%s in %s article%s (N = %s)', nhits, ifelse(nhits==1, '', 's'),
                  narts, ifelse(narts==1, '', 's'),
                  length(unique(tokens[,aid_var]))))
}

#' Title
#'
#' @param hits
#' @param aid_var
#' @param term_var
#'
#' @export
reportTokenFreq <- function(hits, aid_var='aid', term_var='word'){
  termfreq = aggregate(list(hits=hits[,aid_var]), by=list(token=as.character(hits[,term_var])), FUN='length')
  print(termfreq, row.names=F)
}

#' Title
#'
#' @param tokens
#' @param hits
#' @param nwords
#' @param nsample
#' @param random_sample
#' @param aid_var
#' @param term_i_var
#' @param term_var
#'
#' @export
reportKWIC <- function(tokens, hits, nwords=10, nsample=10, random_sample=T, aid_var='aid', term_i_var='id', term_var='word'){
  if(!is.null(nsample)) {
    if(random_sample) hits = hits[sample(1:nrow(hits), nrow(hits)),]
    hits = head(hits, nsample)
  }
  hits$kwic = kwic(tokens, hits, nwords = nwords)

  for(aid in unique(hits[,aid_var])){
    cat('###################')
    ahits = hits[hits[,aid_var] == aid,]
    metanames = colnames(ahits[!colnames(ahits) %in% c(term_i_var, term_var, 'kwic')])
    for(metaname in metanames){
      message(paste(metaname, paste(unique(ahits[,metaname]), collapse=' / '), sep=': '))
    }
    for(term_i in ahits[,term_i_var]){
      cat('\t',ahits[ahits$id == term_i,]$kwic)
      cat('\n')
    }
  }
}


compareHits <- function(tokens, hits.x, hits.y, aid_var='aid', term_i_var='id'){
  id.x = paste(hits.x[,aid_var], hits.x[,term_i_var], sep='---')
  id.y = paste(hits.y[,aid_var], hits.y[,term_i_var], sep='---')
  x = length(id.x)
  y = length(id.y)
  x_not_y = length(setdiff(id.x, id.y))
  y_not_x = length(setdiff(id.y, id.x))
  x_and_y = length(intersect(id.x, id.y))
}

#hits.x = searchQuery(tokens, 'sap')
#hits.y = searchQuery(tokens, 'sap', 'jolande fractievoorzit* groenlinks')


#hits = list(test1=data.frame(aid=c(1,2,3), hits=c(1,1,1)),test2=data.frame(aid=c(1,2,3), hits=c(1,1,1)), test3=data.frame(aid=c(1,2,3), hits=c(1,1,1)))
#xy = combn(names(hits), 2)
#compareHitsList <- function(..., sample_n=10, aid_var='aid', term_i_var='id'){
#  hits = list(...)
#  xy = combn(names(hits), 2)
#
#  o = NULL
#  for(comb in 1:ncol(xy)){
#
#  }
#}

#' a wrapper for grepl that takes multiple patterns. (and hopefully to optimize speed in the near future)
#'
#' @param patterns
#' @param x
#' @param ignore.case
#' @param perl
#'
#' @return a logical vector
#' @export
tokenGrepl <- function(patterns, x, ignore.case=T, perl=F){
  pattern = paste(patterns, collapse='|')
  grepl(pattern, x, ignore.case=ignore.case, perl=perl)
}

