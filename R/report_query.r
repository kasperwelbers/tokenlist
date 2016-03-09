
#' Title
#'
#' @param tokens
#' @param hits
#' @param tokenfreq
#' @param keywordIC
#' @param kwic_nwords
#' @param kwic_sample
#' @param random_sample
#' @param text_var
#' @param hitcount
#'
#' @export
reportSummary <- function(tokens, hits, hitcount=T, tokenfreq=T, keywordIC=T, kwic_nwords=10, kwic_sample=10, random_sample=T, text_var='word'){
  ## report number of hits and articles
  if(hitcount) reportHitcount(tokens, hits)

  ## report token frequency
  if(tokenfreq & nrow(hits) > 0) {
    cat('\n')
    reportTokenFreq(hits, text_var)
  }

  ## report keywords in context
  if(keywordIC & nrow(hits) > 0) {
    cat('\n')
    reportKWIC(tokens, hits, kwic_nwords, kwic_sample, random_sample, text_var)
  }
}

#' Title
#'
#' @param tokens
#' @param hits
#'
#' @export
reportHitcount <- function(tokens, hits){
  nhits = nrow(hits)
  narts = length(unique(hits$doc_id))
  message(sprintf('%s hit%s in %s article%s (N = %s)', nhits, ifelse(nhits==1, '', 's'),
                  narts, ifelse(narts==1, '', 's'),
                  length(unique(tokens$doc_id))))
}

#' Title
#'
#' @param hits
#' @param text_var
#'
#' @export
reportTokenFreq <- function(hits, text_var='word'){
  termfreq = aggregate(list(hits=hits$doc_id), by=list(token=as.character(hits[,text_var])), FUN='length')
  print(termfreq, row.names=F)
}

#' Title
#'
#' @param tokens
#' @param hits
#' @param nwords
#' @param nsample
#' @param random_sample
#' @param text_var
#'
#' @export
reportKWIC <- function(tokens, hits, nwords=10, nsample=10, random_sample=T, text_var='word'){
  if(!is.null(nsample)) {
    if(random_sample) hits = hits[sample(1:nrow(hits), nrow(hits)),]
    hits = head(hits, nsample)
  }
  print(nsample)
  hits$kwic = kwic(tokens, hits, nwords = nwords)

  for(doc_id in unique(hits$doc_id)){
    #cat('###################')
    ahits = hits[hits$doc_id == doc_id,]
    metanames = colnames(ahits[!colnames(ahits) %in% c('position', text_var, 'kwic')])
    for(metaname in metanames){
      message(paste(metaname, paste(unique(ahits[,metaname]), collapse=' / '), sep=': '))
    }
    for(position in ahits$position){
      cat('\t', ahits[ahits$position == position,]$kwic)
      cat('\n')
    }
  }
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#'
#' @return
#' @export
#'
#' @examples
compareHits <- function(tokens, hits.x, hits.y){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
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


#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
reportXandY <- function(tokens, hits.x, hits.y, ...){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
  reportSummary(tokens, hits.x[id.x %in% id.y,], ...)
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
reportXnotY <- function(tokens, hits.x, hits.y, ...){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
  reportSummary(tokens, hits.x[!id.x %in% id.y,], ...)
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
reportYnotX <- function(tokens, hits.x, hits.y, ...){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
  reportSummary(tokens, hits.y[!id.y %in% id.x,], ...)
}
