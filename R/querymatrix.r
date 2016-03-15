## Contains the functions to create a sparse matrix giving the windows within which queries occur.
## Please note that simplicity is sacrificed for performance without mercy



getWindowMatrix <- function(location, context, term, window.size){
  location = stretchLocation(location, context, window.size=window.size)
  length(location)

  shifts = -window.size:window.size

  terms = unique(term)
  term_index = match(term, terms)
  m = locationMatrix(i=location, j=term_index, shifts, distance.as.value=T)
  colnames(m) = terms
  m
}

stretchLocation <- function(location, context, window.size){
  ## makes the word location counter global, and adds dummy locations between contexts to prevent overlapping windows (so it can be used as an index).
  ## this way, overlapping word windows can be calculated for multiple documents within a single matrix.
  ## location and context need to be sorted on order(context,location)!! (hence the presorted argument in getQueryMatrix)

  if(min(location) == 0) location = location + 1 ## location will be treated as an index, so it cannot be zero in r where an index starts at 1 (and some parsers start indexing at zero)

  if(length(unique(context)) == 1){
    return(location)
  } else {
    newcontext = which(!duplicated(context)) # where does a new context start
    context.max = location[newcontext-1] + (window.size*2)
    multiplier_scores = cumsum(c(0,context.max)) # the amount that should be added to the location at the start of each context
    repeat_multiplier = c(newcontext[2:length(newcontext)], length(location)+1) - newcontext # the number of times the multiplier scores need to be repeated to match the location vector
    multiplier_vector = rep(multiplier_scores, repeat_multiplier)
    return(location + multiplier_vector)
  }
}

locationMatrix <- function(i, j, shifts=0, count.once=T, distance.as.value=F){
  mat = spMatrix(max(i), max(j))

  shifts = shifts[order(abs(shifts))] # order from 0 to higher (required if distance.as.value = T)
  for(shift in shifts){
    i_shift = i + shift
    select = i_shift > 0 & i_shift <= max(i)
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
  mat
  length(i)
  nrow(mat)
  mat = mat[i,,drop=F]
  mat = as(mat, 'dgCMatrix')
  if(count.once) mat@x[mat@x>0] = 1
  mat
}




#' Creates a matrix where rows match the tokens, columns represent the queries in query_regex, and values represent the word distance to each query hit
#'
#' @param tokens
#' @param query_regex
#' @param text_var
#' @param presorted
#' @param default.window
#'
#' @return a sparse matrix
#' @export
getQueryMatrix <- function(tokens, query_regex, text_var='word', presorted=F, default.window=NA){
  if(!'window' %in% colnames(query_regex)) query_regex$window = default.window
  print(query_regex)
  if(!presorted){
    ## unless explicitly noted that the token list is sorted, first sort tokens by document id and word id. (keeps order to restore original order)
    ord = order(tokens$doc_id, tokens$position)
    tokens = tokens[ord,]
  }

  ## get query matrix; separately for query terms with a given word distance and query terms at the article level
  document_level = is.na(query_regex$window) | query_regex$window == 'd'
  qm = cbind(wordDistanceQueryMatrix(tokens, query_regex[!document_level,], text_var),
             documentOccurenceQueryMatrix(tokens, query_regex[document_level,], text_var))

  if(!presorted) qm = qm[match(1:nrow(qm), ord),,drop=F] # return matrix in order of input tokens
  qm
}

wordDistanceQueryMatrix <- function(tokens, query_regex, text_var){
  if(nrow(query_regex) == 0) return(NULL)
  query_regex$window = as.numeric(query_regex$window) + 1 # Plus 1, because in the window matrix 1 indicates no distance (because zero already indicates no presence [because this keeps the matrix sparse])

  m = getWindowMatrix(location=tokens$position,
                      context=tokens$doc_id,
                      term = tokens[, text_var],
                      window.size = max(query_regex$window))

  ## create the rows and columns for the query matrix by looking in which rows one of the terms that matches the regex is TRUE.
  getWindowQueryHits <- function(j, query_regex, m){
    query_m = m[,tokenGrepl(query_regex$regex[j], colnames(m)),drop=F]
    hits = Matrix::rowSums(query_m > 0 & query_m <= query_regex$window[j]) > 0 # matrix value has to be higher than 0, but not higher than the window size
    if(sum(hits) == 0) return(NULL)
    data.frame(i = which(hits), j = j)
  }
  qm = ldply(1:nrow(query_regex), getWindowQueryHits, query_regex=query_regex, m=m)

  qm = spMatrix(nrow(tokens), nrow(query_regex), qm$i, qm$j, rep(T, nrow(qm)))
  colnames(qm) = query_regex$term
  qm
}

documentOccurenceQueryMatrix <- function(tokens, query_regex, text_var){
  if(nrow(query_regex) == 0) return(NULL)

  udoc = unique(tokens$doc_id)
  uterm = unique(tokens[,text_var])
  doc_i = match(tokens$doc_id, udoc)
  term_i = match(tokens[,text_var], uterm)
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
