parseQueries <- function(query){
  query = gsub(' OR ', ' | ', query)
  query = gsub(' AND ', ' & ', query)
  query = gsub(' NOT ', ' &! ', query)

  ## also allow empty space as OR
  query = gsub('(?<=[*?a-zA-Z0-9)])[ ]+(?=[*?a-zA-Z0-9(])', ' | ', query, perl=T)

  ## make " * ", as a 'find all' solution, an immediate TRUE
  query = tolower(query) # safety first: for the odd possibility that someone uses T or F as a query term, which would be interpreted as TRUE or FALSE
  query = gsub('(?<= )\\*(?= )|(?<=^)\\*(?= )', 'T', query, perl=T)

  query_form = as.list(gsub('([*?a-z0-9/~_-]+)', '%s', query)) # note that uppercase is not replaced, to keep the TRUE
  query_terms = regmatches(query, gregexpr('([*?a-z0-9/~_-]+)', query))

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
  terms$regex = gsub('*', '.*', terms$regex, fixed=T) # wildcard: none or any symbols
  terms$regex = gsub('?', '.{1}', terms$regex, fixed=T) # wildcard: one character that can be anything
  terms$regex = sprintf('\\b%s\\b', terms$regex)
  terms
}



#' Annotate a data frame of tokens with codes using Lucene-like search queries
#'
#' @param tokens a data frame of tokens containing columns for document id (doc_id), text position (position) and text string (column name can be specified in text_var, defaults to 'word').
#' @param queries a data frame containing the queries.
#' @param default.window
#' @param text_var a character string giving the name of the term string column
#' @param condition_once logical. If TRUE, then if an indicator satisfies its conditions once in an article, all indicators within that article are coded.
#' @param indicator_filter
#' @param presorted The data has to be sorted on order(doc_id, position). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#'
#' @return the annotated tokens data frame
#' @export
codeTokens <- function(tokens, queries, text_var='word', default.window=25, indicator_filter=rep(T, nrow(tokens)), condition_once=FALSE, presorted=F){
  ## consider underscores as wordboundaries (important for some parsers that chunk words together, separated by underscores)
  if(!class(tokens[,text_var]) == 'factor') as.factor(tokens[,text_var])
  levels(tokens[,text_var]) = gsub('_', ' ', levels(tokens[,text_var]), fixed=T)

  ## make query forms for the indicators and conditions
  ind = parseQueries(queries$indicator)
  con = parseQueries(queries$condition)

  ## get regular expressions for each term in the queries
  indr = getTermRegex(queries$indicator, 0) # for indicators the window should always be 1 (the exact location of the indicator)
  conr = getTermRegex(queries$condition, default.window)
  query_regex = unique(rbind(indr, conr))

  ## speed up: filter the tokens list
  ## only look at articles in which at least one of the indicator terms occurs
  article_filter = unique(tokens$doc_id[tokenGrepl(indr$regex, tokens[,text_var])])
  article_filter = which(tokens$doc_id %in% article_filter)
  ## only look at tokens that match one of the regex (and only for the relevant articles)
  tokens_within_article_filter = tokenGrepl(query_regex$regex, tokens[article_filter,text_var])
  tokens_filter = article_filter[tokens_within_article_filter]

  ## if no hits, return
  if(length(tokens_filter) == 0) return(rep('', nrow(tokens)))

  ## filter tokens, and use an index column to keep the original row location in tokenlist (so it can be returned in this position without having to match values)
  tokens$i = 1:nrow(tokens)
  ftokens = tokens[tokens_filter, c('i', 'doc_id', 'position', text_var),drop=F]
  indicator_filter = indicator_filter[tokens_filter] ## the indicator filter needs to match the filtered tokens list

  ## create matrix where rows are tokens, columns are the query terms, and cells indicate whether the query terms occur (within the given word distance) at the place of each token.
  qm = getQueryMatrix(ftokens, query_regex, text_var, presorted)

  ## evaluate queries
  nqueries = nrow(queries)
  ftokens$code = ''
  for(i in 1:nqueries){
    if(i %% 10 == 0) message(sprintf('\t%s / %s', i, nqueries))

    ## find tokens that match one of the indicator terms
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
      ## evaluate condition queries only for rows where the indicator term occurs
      hit_and_condition = evalQueryMatrix(qm[ind_hit,,drop=F], con_terms, con_form)
      if(sum(hit_and_condition) == 0) next
      ftokens$code[ind_hit] = ifelse(hit_and_condition, code, ftokens$code[ind_hit])
    }

    ## if condition_once is TRUE, then all indicators hits are also coded if the indicator satisfies its condition at least once within the article
    #### !!!!!!!!!!!!! voorrang aan WEL conditie
    if(condition_once){
      article_with_code = unique(ftokens$doc_id[ftokens$code == code]) # articles in which indicator satisfies condition at least once
      hit_and_articlecondition = ftokens$doc_id[ind_hit] %in% article_with_code # for all indicator hits, check whether they occur in one of these articles.
      ftokens$code[ind_hit] = ifelse(hit_and_articlecondition, code, ftokens$code[ind_hit])
    }
  }
  tokens$code = ''
  tokens$code[ftokens$i] = ftokens$code ## add code to original location in tokenlist
  tokens$code
}


#' Search for tokens in a tokenlist using indicators with conditions
#'
#' Tokens need to be sorted on order(doc_id, position), or presorted needs to be set to FALSE. Note that if this function is used often, its faster to sort tokens first and use presorted = TRUE.
#'
#' @param tokens
#' @param indicator
#' @param condition
#' @param text_var
#' @param condition_once logical. If TRUE, then if an indicator satisfies its conditions once in an article, all indicators within that article are coded.
#' @param presorted The data has to be sorted on order(aid_var, term_i_var). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#' @param default.window
#' @param indicator_filter
#' @param hitcount
#' @param tokenfreq
#' @param keywordIC
#' @param ...
#'
#' @return the tokens that match the query
#' @export
searchQuery <- function(tokens, indicator, condition='', text_var='word', default.window=25, condition_once=FALSE, presorted=T, indicator_filter=rep(T, nrow(tokens)), hitcount=T, tokenfreq=F, keywordIC=F, ...){
  tokens$code = NULL
  query = data.frame(code=1, indicator=indicator, condition=condition)
  code = codeTokens(tokens, query, text_var, indicator_filter=indicator_filter, condition_once=condition_once, presorted=presorted, default.window = default.window)
  hits = tokens[code == 1, c('doc_id', 'position', text_var)]

  reportSummary(tokens, hits, hitcount, tokenfreq, keywordIC, text_var=text_var, ...)
  hits
}

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


