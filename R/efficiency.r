
testdit <- function(){
  library(tokenlist)
  source('~/Dropbox/syntax/R/lib.r')
  tokens = readRDS('~/Dropbox/syntax/data/tokens_oekraine.rds')
  cb = read.csv('~/Dropbox/syntax/data/VD_04-29_ jk OekOntologie.csv')
  cb = prepareCodebook(cb)
  pol = read.csv('~/Dropbox/syntax/data/politicians_queries.csv')


  setTokenlistColnames(doc.col = 'aid', position.col='id', word.col='word')

  test = codeQueries(tokens, cb, batchsize = 5)
}

