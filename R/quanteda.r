#' Create a tokenlist data.frame
#'
#' A tokenlist is a data.frame in which rows represent the tokens of a text (e.g., words, lemma, ngrams). This function creates a tokenlist that is ordered by document ('doc_id' column) and the position of the token in the text ('position' column).
#'
#' The tokenization is taken care of by the tokenize function of the quanteda package. Additional arguments (...) are passed to the tokenize function.
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
getTokenlist <- function(x, doc_id=1:length(x), removePunct=T, ...){
  if(is(x, 'factor')) x = as.character(x)
  if(is(x, 'character')) x = tokenize(x, removePunct=removePunct, ...)

  if(is(x, 'tokenizedTexts')){
    doclen = sapply(x, length)

    ##
    filter = doclen > 0 ## ignore document with length 0
    x = data.frame(doc_id = rep(doc_id[filter], doclen[filter]),
                   position = unlist(sapply(doclen[filter], function(x) 1:x)),
                   word = unlist(x[filter]))
    colnames(x) = c(getOption('doc.col','doc_id'),
                    getOption('position.col','position'),
                    getOption('word.col','word'))
  }

  x
}
