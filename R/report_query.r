
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
  if(hitcount) message(reportHitcount(tokens, hits))

  ## report token frequency
  if(tokenfreq & nrow(hits) > 0) {
    cat('\n')
    tokenfreq = reportTokenFreq(hits, text_var)
    print(tokenfreq, row.names=F)
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
  sprintf('%s hit%s in %s article%s (N = %s)', nhits, ifelse(nhits==1, '', 's'),
                  narts, ifelse(narts==1, '', 's'),
                  length(unique(tokens$doc_id)))
}

#' Title
#'
#' @param hits
#' @param text_var
#'
#' @export
reportTokenFreq <- function(hits, text_var='word'){
  termfreq = aggregate(list(hits=hits$doc_id), by=list(token=as.character(hits[,text_var])), FUN='length')
  termfreq
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
  hits$kwic = getKwic(tokens, hits, nwords = nwords)

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
XandY <- function(hits.x, hits.y){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
  hits.x[id.x %in% id.y,]
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
XnotY <- function(hits.x, hits.y){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
  hits.x[!id.x %in% id.y,]
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
YnotX <- function(hits.x, hits.y){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
  hits.y[!id.y %in% id.x,]
}


#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
reportXandY <- function(tokens, hits.x, hits.y, ...){
  reportSummary(tokens, XandY(hits.x, hits.y), ...)
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
  reportSummary(tokens, XnotY(hits.x, hits.y), ...)
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
  reportSummary(tokens, YnotX(hits.x, hits.y), ...)
}


#' Get keyword-in-context from a token list
#'
#' @param tokens a data frame of tokens containing columns for document id (doc_id), text position (position) and text string (column name can be specified in text_var, defaults to 'word').
#' @param nwords the number of words in front and after the keyword
#' @param hits
#' @param prettypaste
#' @param text_var a character string giving the name of the term string column
#'
#' @return A data.frame with the keyword in context
#' @export
getKwic <- function(tokens, hits, nwords=10, text_var='word', prettypaste=T){
  ## first filter tokens on document id (to speed up computation)
  tokens = tokens[tokens$doc_id %in% unique(hits$doc_id),]

  token_i = tokenLookup(tokens, hits$doc_id, hits$position)

  kwicldply <- function(i, doc_ids, words, nwords){
    doc_id = doc_ids[i]

    sent_i = (i-nwords):(i+nwords)
    keyword_i = if(min(sent_i) < 0) min(sent_i) + nwords else nwords + 1

    sent_i = sent_i[sent_i >= 0 & sent_i <= length(words)]
    sent = as.character(words[sent_i])

    sent = gsub('\\[|\\]', '', sent)
    sent[keyword_i] = sprintf('[%s]', sent[keyword_i])
    sent = sent[doc_ids[sent_i] == doc_id] # only show context words if they occur in the same article
    data.frame(doc_id=doc_id, kwic=paste(sent, collapse=' '))
  }
  o = ldply(token_i, kwicldply, doc_ids=tokens$doc_id, words=tokens[,text_var], nwords=nwords)

  if(prettypaste) o$kwic = prettyKWIC(o$kwic)
  o$kwic
}

prettyKWIC <- function(x){
  x = gsub('_', ' ', x)
  x = gsub('  ', ' ', x)
  x = gsub(" ([.,?!:;>)])", '\\1', x)
  x = gsub('([(<]) ', '\\1', x)
  x = sprintf('...%s...', x)
}

tokenLookup <- function(tokens, doc_id, position){
  tokens$i = 1:nrow(tokens)
  tokens = tokens[tokens$doc_id %in% unique(doc_id), c('i', 'doc_id', 'position')]
  which.sub = match(paste(doc_id, position, sep='___'),
                    paste(tokens$doc_id, tokens$position, sep='___'))
  tokens$i[which.sub]
}
