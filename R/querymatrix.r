## Contains the functions to create a sparse matrix giving the windows within which queries occur.
## Please note that simplicity is sacrificed for performance with little mercy

getWindowMatrix <- function(location, context, term, window.size, return_i_filter){
  location = globalLocation(location, context, window.size=window.size)
  shifts = -window.size:window.size

  if(!is.null(return_i_filter)) return_i_filter = location[return_i_filter] ## make sure the return_i_filter uses the transformed indices

  terms = unique(term)
  term_index = match(term, terms)
  m = locationMatrix(i=location, j=term_index, shifts, distance.as.value=T, return_i_filter=return_i_filter)
  colnames(m) = terms
  m
}

localLocation <- function(location, context){
  newcontext = which(!duplicated(context))
  repeat_multiplier = c(newcontext[-1], length(context)+1) - newcontext
  context_start = rep(location[newcontext], repeat_multiplier)
  (location - context_start) + 1
}

globalLocation <- function(location, context, window.size=NA){
  ## makes the word location counter global with dummy locations between contexts to prevent overlapping windows (so it can be used as an index).
  ## this way, overlapping word windows can be calculated for multiple documents within a single matrix.
  ## location and context need to be sorted on order(context,location)!! (hence the presorted argument in getQueryMatrix)

  ## first, make sure location is local and starts at 1 for each context (otherwise things get very slow)
  location = localLocation(location, context)

  if(min(location) == 0) location = location + 1 ## location will be treated as an index, so it cannot be zero in r where an index starts at 1 (and some parsers start indexing at zero)

  if(length(unique(context)) == 1){
    return(location)
  } else {
    newcontext = which(!duplicated(context)) # where does a new context start

    context.max = location[newcontext-1] # the highest value of each context
    if(!is.na(window.size)) context.max = context.max + window.size # increase the highest value of each context with window.size to make sure windows of different contexts do not overlap.
    multiplier_scores = cumsum(c(0,context.max)) # the amount that should be added to the location at the start of each context

    repeat_multiplier = c(newcontext[-1], length(location)+1) - newcontext # the number of times the multiplier scores need to be repeated to match the location vector
    multiplier_vector = rep(multiplier_scores, repeat_multiplier)
    return(location + multiplier_vector)
  }
}

locationMatrix <- function(i, j, shifts=0, count.once=T, distance.as.value=F, return_i_filter=NULL){
  mat = spMatrix(max(i), max(j))

  shifts = shifts[order(abs(shifts))] # order from 0 to higher (required if distance.as.value = T)
  for(shift in shifts){
    i_shift = i + shift

    if(!is.null(return_i_filter)) {
      select = i_shift %in% return_i_filter
    } else {
      select = i_shift > 0 & i_shift <= max(i)
    }

    if(sum(select) == 0) next

    if(distance.as.value){
      mat = mat + spMatrix(nrow=max(i), ncol=max(j), i=i_shift[select], j=j[select], rep(abs(shift)+1, sum(select)))
    } else{
      mat = mat + spMatrix(nrow=max(i), ncol=max(j), i=i_shift[select], j=j[select], rep(1, sum(select)))
    }
  }


  if(distance.as.value){
    ## remove duplicates. since the stacked triples are ordered by shifts, this leaves the shortest distance to a term in case of duplicate cells
    count.once = F
    select = !duplicated(data.frame(mat@i, mat@j))
    mat = spMatrix(nrow(mat), ncol(mat), mat@i[select]+1, mat@j[select]+1, mat@x[select])
  }
  mat = mat[i,,drop=F]
  mat = as(mat, 'dgCMatrix')
  if(count.once) mat@x[mat@x>0] = 1
  mat
}




#' Creates a matrix where rows match the tokens, columns represent the queries in query_regex, and values represent the word distance to each query hit
#'
#' @param tokens
#' @param query_regex
#' @param word.col
#' @param presorted
#' @param default.window
#'
#' @return a sparse matrix
#' @export
getQueryMatrix <- function(tokens, query_regex, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position'), word.col=getOption('word.col','word'), presorted=F, default.window=NA, return_i=NULL){
  if(!'window' %in% colnames(query_regex)) query_regex$window = default.window

  ## replace terms that do not occur in the queries with a dummy (NA). Skip if there are already NA's in the data
  if(sum(is.na(tokens[,word.col])) == 0) {
    tokens[,word.col] = ifelse(tokenGrepl(query_regex$regex, tokens[,word.col]), as.character(tokens[,word.col]), NA)
  }

  if(!presorted){
    ## unless explicitly noted that the token list is sorted, first sort tokens by document id and word id. (keeps order to restore original order)
    ord = order(tokens[,doc.col], tokens[,position.col])
    tokens = tokens[ord,]
  }

  ## get query matrix; separately for query terms with a given word distance and query terms at the article level
  document_level = is.na(query_regex$window) | query_regex$window == 'd'
  if(mean(document_level) == 1) qm = documentOccurenceQueryMatrix(tokens, query_regex[document_level,], doc.col, position.col, word.col)
  if(mean(document_level) == 0) qm = wordDistanceQueryMatrix(tokens, query_regex[!document_level,], doc.col, position.col, word.col, return_i)
  if(!mean(document_level) %in% c(0,1)) qm = cbind(wordDistanceQueryMatrix(tokens, query_regex[!document_level,], doc.col, position.col, word.col, return_i),
                                                   documentOccurenceQueryMatrix(tokens, query_regex[document_level,], doc.col, position.col, word.col))

  if(!presorted) qm = qm[match(1:nrow(qm), ord),,drop=F] # return matrix in order of input tokens
  if(!is.null(return_i)) qm = qm[return_i,,drop=F]
  qm
}

wordDistanceQueryMatrix <- function(tokens, query_regex, doc.col, position.col, word.col, return_i_filter=NULL){
  if(nrow(query_regex) == 0) return(NULL)
  query_regex$window = as.numeric(query_regex$window) + 1 # Plus 1, because in the window matrix 1 indicates no distance (because zero already indicates no presence [because this keeps the matrix sparse])

  m = getWindowMatrix(location=tokens[,position.col],
                      context=tokens[,doc.col],
                      term = tokens[,word.col],
                      window.size = max(query_regex$window),
                      return_i_filter = return_i_filter)

  ## create the rows and columns for the query matrix by looking in which rows one of the terms that matches the regex is TRUE.
  getWindowQueryHits <- function(j, query_regex, m){
    query_m = m[,tokenGrepl(query_regex$regex[j], colnames(m)),drop=F]
    hits = Matrix::rowSums(query_m > 0 & query_m <= query_regex$window[j]) > 0 # at least one column should have a value between 1 and its window size
    if(sum(hits) == 0) return(NULL)
    data.frame(i = which(hits), j = j)
  }


  qm = ldply(1:nrow(query_regex), getWindowQueryHits, query_regex=query_regex, m=m)
  qm = spMatrix(nrow(tokens), nrow(query_regex), qm$i, qm$j, rep(T, nrow(qm)))
  colnames(qm) = query_regex$term
  qm
}

documentOccurenceQueryMatrix <- function(tokens, query_regex, doc.col, position.col, word.col){
  if(nrow(query_regex) == 0) return(NULL)

  udoc = unique(tokens[,doc.col])
  uterm = unique(tokens[,word.col])
  doc_i = match(tokens[,doc.col], udoc)
  term_i = match(tokens[,word.col], uterm)
  m = spMatrix(length(udoc), length(uterm), doc_i, term_i, rep(1,nrow(tokens)))
  colnames(m) = uterm

  ## create the rows and columns for the query matrix by looking in which rows one of the terms that matches the regex is TRUE.
  getQueryHits <- function(j, query_regex, m){
    query_m = m[,tokenGrepl(query_regex$regex[j], colnames(m)),drop=F]
    hits = Matrix::rowSums(query_m) > 0 # matrix value has to be higher than 0, but not higher than the window size
    if(sum(hits) == 0) return(NULL)
    data.frame(i = which(hits), j = j)
  }

  qm = ldply(1:nrow(query_regex), getQueryHits, query_regex=query_regex, m=m)

  qm = spMatrix(nrow(tokens), nrow(query_regex), qm$i, qm$j, rep(T, nrow(qm)))
  colnames(qm) = query_regex$term

  qm[doc_i,,drop=F] ## repeat rows (i.e. documents) to match the token list input
}
