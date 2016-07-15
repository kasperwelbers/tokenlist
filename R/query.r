#' Set which columns in the tokens data frame should be used
#'
#' @param doc.col The name of the column containing the unique id's for documents
#' @param position.col The name of the column contaning the positions of a token in a document
#' @param word.col The name of the column contaning the token text (e.g., word, lemma)
#'
#' @return Nothing. Sets the names globally (in options())
#' @export
setTokenlistColnames <- function(doc.col='doc_id', position.col='position', word.col='word'){
  options(doc.col = doc.col, position.col = position.col, word.col=word.col)
}

parseQueries <- function(query){
  query = iconv(query, to='ASCII//TRANSLIT') # if present, try to remove accented characters

  query = gsub(' OR ', ' | ', query)
  query = gsub(' AND ', ' & ', query)
  query = gsub(' NOT ', ' &! ', query)

  ## also allow empty space as OR
  query = gsub('(?<=[+*?.a-zA-Z0-9/~_)-])[ ]+(?=[+*?.a-zA-Z0-9/~_(-])', ' | ', query, perl=T)

  ## make " * ", as a 'find all' solution, an immediate TRUE
  query = tolower(query) # safety first: for the odd possibility that someone uses T or F as a query term, which would be interpreted as TRUE or FALSE
  query = gsub('(?<= )\\*(?= )|(?<=^)\\*(?= )', 'T', query, perl=T)

  query_form = as.list(gsub('([+*?.a-z0-9/~_-]+)', '%s', query)) # note that uppercase is not replaced, to keep the TRUE
  query_terms = regmatches(query, gregexpr('([+*?.a-z0-9/~_-]+)', query))

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
  ## only evaluate unique rows of the query matrix, and then match to return the results for each row
  rowid = apply(qm[,terms,drop=F], 1, paste, collapse='')

  isunique = !duplicated(rowid)
  urowid = rowid[isunique]
  uqm = qm[isunique,,drop=F]

  #apply(qm[,terms, drop=F], 1, evalQuery, query_form=form) ## old solution, without accounting for duplicates. Still have to check whether new solution is actually better
  res = apply(uqm[,terms, drop=F], 1, evalQuery, query_form=form)
  res[match(rowid, urowid)]
}

getTermRegex <- function(terms, default.window=NA){
  terms = parseQueries(terms)

  if(length(default.window) == nrow(terms)){
    reptimes = sapply(terms[,2], length)
    default.window = rep(default.window, reptimes)
  }
  terms = unlist(terms[,2])
  terms = data.frame(term = terms,
                     regex = gsub('~.*', '', terms),
                     window = ifelse(grepl('~', terms) == T, gsub('.*~', '', terms), default.window))
  terms$window[terms$window == 'd'] = NA
  terms$window = as.numeric(as.character(terms$window))

  terms$regex = gsub('([.+])', '\\\\\\1', terms$regex) ## escape special regex characters

  terms$regex = gsub('*', '.*', terms$regex, fixed=T) # wildcard: none or any symbols
  terms$regex = gsub('?', '.{1}', terms$regex, fixed=T) # wildcard: one character that can be anything
  terms$regex = sprintf('\\b%s\\b', terms$regex)
  unique(terms)
}

qualifyQueries <- function(queries){
  boo = c()
  for(i in 1:nrow(queries)){
    if(queries$indicator[i] == '') boo = c(boo, sprintf('Code "%s": no indicator', queries$code[i]))
    if(queries$indicator[i] == '*') boo = c(boo, sprintf('Code "%s": indicator cannot be *', queries$code[i]))
  }
  if(length(boo) > 0) stop(paste(boo, collapse='\n'))
}


#' A wrapper for grepl that takes multiple patterns. For efficiency, grepl is performed on unique words, and results for individual values are matched. Uses batches to go easy on memory (though sacrificing a bit of speed)
#'
#' @param patterns
#' @param x
#' @param ignore.case
#' @param perl
#'
#' @return a logical vector
#' @export
tokenGrepl <- function(patterns, x, ignore.case=T, perl=F, batchsize=25, useBytes=T){
  ## make batches of terms and turn each batch into a single regex
  patterns = split(patterns, ceiling(seq_along(patterns)/batchsize))
  patterns = sapply(patterns, paste, collapse='|')

  ## grepl in unique x
  ux = unique(x)
  out = rep(F, length(ux))
  for(pattern in patterns){
    out = out | grepl(pattern, ux, ignore.case=ignore.case, perl=perl, useBytes=useBytes)
  }

  ## return result for each x
  #out[match(x, ux)]
  x %in% ux[out]
}


#' Report number of unique terms that matches each query
#'
#' This function is usefull to test whether certain queries match a high number of unique terms, thus possibly causing memory issues.
#'
#' @param terms a character vector of (unique) terms
#' @param queries a dataframe with queries as used in the searchQueries en codeQueries functions.
#'
#' @export
queryTermMatches <- function(terms, queries){
  uterms = unique(terms)
  queries$nterms = NA
  for(i in 1:nrow(queries)){
    regterms = unique(c(getTermRegex(queries$indicator[i])$regex, getTermRegex(queries$condition[i])$regex))
    queries$nterms[i] = sum(tokenGrepl(regterms, uterms))
  }
  queries$nterms
}


#smartBatch <- function(queries, max_bsize=50){
#  qterms = parseQueries(queries$condition)[,2]
#  qterms = data.frame(code = rep(queries$code, sapply(qterms, length)),
#                       term = unlist(qterms))
#  ucodes = unique(queries$code)
#  uterms = unique(qterms$term)
#  qtm = spMatrix(length(uterms), length(ucodes), match(qterms$term, uterms), match(qterms$code, ucodes), rep(1, nrow(qterms)))
#  dimnames(qtm) = list(uterms, ucodes)
#
#  crossprod(qtm)

#' Find tokens using a Lucene-like search query
#'
#' @param tokens a data frame of tokens containing columns for document id (doc_id), text position (position) and text string (column name can be specified in word.col, defaults to 'word').
#' @param queries a data frame containing the queries.
#' @param doc.col a character string giving the name of the document id column in the tokens data.frame
#' @param position.col a character string giving the name of the word position column in the tokens data.frame
#' @param word.col a character string giving the name of the word column in the tokens data.frame
#' @param batchsize This function is faster if multiple queries are searched together, but too many queries (with too many tokens) at once can eat up memory or crash R. Try lowering batchsize in case of issues.
#' @param default.window
#' @param condition_once logical. If TRUE, then if an indicator satisfies its conditions once in an article, all indicators within that article are coded.
#' @param indicator_filter
#' @param presorted The data has to be sorted on order(doc_id, position). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#'
#' @return the annotated tokens data frame
#' @export
searchQuery <- function(tokens, indicator, condition='', code='', doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position'), word.col=getOption('word.col','word'), default.window=NA, condition_once=FALSE, indicator_filter=rep(T, nrow(tokens)), presorted=F){
  queries = data.frame(code=code, indicator=indicator, condition=condition)
  searchQueries(tokens, queries, doc.col, position.col, word.col, batchsize=NA, default.window, condition_once, indicator_filter, presorted)
}

#' Find tokens using Lucene-like search queries
#'
#' @param tokens a data frame of tokens containing columns for document id (doc_id), text position (position) and text string (column name can be specified in word.col, defaults to 'word').
#' @param queries a data frame containing the queries.
#' @param doc.col a character string giving the name of the document id column in the tokens data.frame
#' @param position.col a character string giving the name of the word position column in the tokens data.frame
#' @param word.col a character string giving the name of the word column in the tokens data.frame
#' @param batchsize This function is faster if multiple queries are searched together, but too many queries (with too many tokens) at once can eat up memory or crash R. Try lowering batchsize in case of issues.
#' @param default.window
#' @param condition_once logical. If TRUE, then if an indicator satisfies its conditions once in an article, all indicators within that article are coded.
#' @param indicator_filter
#' @param presorted The data has to be sorted on order(doc_id, position). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#'
#' @return the annotated tokens data frame
#' @export
searchQueries <- function(tokens, queries, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position'), word.col=getOption('word.col','word'), batchsize=5, default.window=NA, condition_once=FALSE, indicator_filter=rep(T, nrow(tokens)), presorted=F, verbose=T){
  queries = queries[!queries$indicator == '',]
  qualifyQueries(queries)

  if(!'condition_once' %in% colnames(queries)) queries$condition_once = condition_once
  if(!'default.window' %in% colnames(queries)) queries$default.window = default.window
  tokens$i = 1:nrow(tokens) # add row indices as vector to recall after filtering and sorting

  ## sort beforehand and set presorted to true (so that tokens will not be sorted for each batch)
  if(!presorted) {
    tokens = tokens[order(tokens[,doc.col], tokens[,position.col]),]
    presorted = T
  }

  ## consider underscores as wordboundaries (important for some parsers that chunk words together, separated by underscores)
  if(!class(tokens[,word.col]) == 'factor') as.factor(tokens[,word.col])
  levels(tokens[,word.col]) = gsub('_', ' ', levels(tokens[,word.col]), fixed=T)


  if(is.na(batchsize)){
    out = searchQueriesBatch(tokens, queries, doc.col, position.col, word.col, presorted, indicator_filter)
  } else {
    out = list()

    query_i = 1:nrow(queries)
    batches = split(query_i, ceiling(seq_along(query_i)/batchsize))

    for(i in 1:length(batches)){
      if(verbose) message('\t', min(batches[[i]]), ' / ', nrow(queries))
      batch = searchQueriesBatch(tokens, queries[batches[[i]],,drop=F], doc.col, position.col, word.col, presorted, indicator_filter)
      if(!is.null(batch)) out[['']] = batch
    }
  }
  rbind.fill(out)
}

emptyHitsDf <- function(doc.col, position.col, word.col){
  d = data.frame(i=numeric(0), doc_id=numeric(0), position.col=numeric(0), code=character(0), word=character(0))
  colnames(d) = c('i', doc.col, position.col, 'code', word.col)
  d
}

searchQueriesBatch <- function(tokens, queries, doc.col, position.col, word.col, presorted, indicator_filter){
  tokens$ind_filter = indicator_filter

  ind = parseQueries(queries$indicator)
  indr = getTermRegex(queries$indicator, queries$default.window)
  con = parseQueries(queries$condition)
  conr = getTermRegex(queries$condition, queries$default.window)

  #### first make a query matrix for all terms used in the queries
  ## only look at articles if one of the indicators (ind_hit) is found
  tokens$ind_hit = tokenGrepl(indr$regex, tokens[,word.col])
  tokens$ind_hit = tokens$ind_hit & tokens$ind_filter ## use the indicator filter
  article_filter = unique(tokens[tokens$ind_hit, doc.col])
  tokens = tokens[tokens[,doc.col] %in% article_filter,]
  ## and look only at tokens that are indicators or condition terms
  tokens$is_cond_term = tokenGrepl(conr$regex, tokens[,word.col])
  tokens_filter = tokens$ind_hit | tokens$is_cond_term
  tokens = tokens[tokens_filter,]

  if(nrow(tokens) == 0) return(emptyHitsDf(doc.col,position.col,word.col))

  ## create matrix where rows are tokens, columns are the query terms, and cells indicate whether the query terms occur (within the given word distance) at the place of each token.
  ## creating the query matrix can (and should) be skipped if no conditions are given
  if(nrow(conr) > 0){
    tokens$cond_term = ifelse(tokens$is_cond_term, tokens[,word.col], NA) ## when making the query matrix, its more memory efficient to use a single column for all indicator terms (that are not condition terms). Note that this indicator column is necessary because we need to know where the indicators are in the query matrix
    qm = getQueryMatrix(tokens, conr, doc.col, position.col, word.col, presorted, default.window, return_i=tokens$ind_hit)
    tokens = tokens[tokens$ind_hit,,drop=F]
  }

  #### then evaluate each query individually
  result_i = list()
  for(i in 1:nrow(queries)){
    tokens$code = queries$code[i]

    indr = getTermRegex(queries$indicator[i])
    tokens$hit = tokenGrepl(indr$regex, tokens[,word.col]) & tokens$ind_filter
    if(sum(tokens$hit) == 0) next

    ## if there is no condition, accept all indicator hits
    if(is.na(con[i,]$form)){
      result_i[['']] = tokens[tokens$hit, c('i', doc.col, position.col, 'code', word.col)]
      next
    }

    ## evaluate condition query
    tokens$hit_and_condition = F
    tokens$hit_and_condition[tokens$hit] = evalQueryMatrix(qm[tokens$hit,,drop=F], con[i,]$terms, con[i,]$form)

    ## if condition_once is TRUE, then all indicator hits are also coded if the indicator satisfies its condition at least once within the article
    #### !!!!!!!!!!!!! voorrang aan WEL conditie
    if(queries$condition_once[i]){
      article_with_code = unique(tokens[tokens$hit_and_condition, doc.col]) # articles in which indicator satisfies condition at least once
      hit_and_articlecondition = tokens[tokens$hit,doc.col] %in% article_with_code # for all indicator hits, check whether they occur in one of these articles.
      tokens$hit_and_condition[tokens$hit] = ifelse(hit_and_articlecondition, T, tokens$hit[tokens$hit])
    }
    if(sum(tokens$hit_and_condition) > 0) {
      result_i[['']] = tokens[tokens$hit_and_condition, c('i',doc.col,position.col,'code',word.col)]
    } else {
      result_i[['']] = emptyHitsDf(doc.col,position.col,word.col)
    }
  }
  unique(rbind.fill(result_i))
}

#' Annotate a data frame of tokens with codes using Lucene-like search queries
#'
#' @param tokens a data frame of tokens containing columns for document id (), text position (position) and text string (column name can be specified in word.col, defaults to 'word').
#' @param queries a data frame containing the queries.
#' @param word.col a character string giving the name of the term string column
#' @param batchsize This function is faster if multiple queries are searched together, but too many queries (with too many tokens) at once can eat up memory or crash R. Try lowering batchsize in case of issues.
#' @param default.window
#' @param condition_once logical. If TRUE, then if an indicator satisfies its conditions once in an article, all indicators within that article are coded.
#' @param indicator_filter
#' @param presorted The data has to be sorted on order(, position). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#'
#' @return the annotated tokens data frame
#' @export
codeQueries <- function(tokens, queries, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position'), word.col=getOption('word.col','word'), batchsize=5, default.window=NA, condition_once=FALSE, indicator_filter=rep(T, nrow(tokens)), presorted=F, verbose=T){
  hits = searchQueries(tokens, queries, doc.col, position.col, word.col, batchsize, default.window, condition_once, indicator_filter, presorted, verbose)

  tokens$code = ''
  if(nrow(hits) == 0) return(as.factor(tokens$code))

  ## if a token has multiple codes, only use the last one (this way, if queries has a top-down hierarchical structure, the most specific coding will be used)
  ## to do so, hits are first ordered according to the reversed order of the queries dataframe, and then duplicate rowindices (i) are deleted
  #hits = hits[rev(1:nrow(hits)),]
  hits$queryorder = match(hits$code, tokens$code)
  hits = hits[order(-hits$queryorder),]
  hits = hits[!duplicated(hits$i),]

  tokens$code[hits$i] = as.character(hits$code)
  as.factor(tokens$code)
}


