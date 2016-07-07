test_that("extracting clauses works", {
  tokens = data.frame(doc_id = c(1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3),
                      position = c(1,2,3,4,5,6,7,8,1,2,3,4,5,1,2,3,4),
                      word = c('Renewable','fuel','is','better','than','fossil', 'fuels', '!',
                               'A', 'fueled', 'debate', 'about', 'fuel',
                               'Mark_Rutte', 'is', 'simply', 'Rutte'))

  ## simple indicator only
  hits = searchQuery(tokens, indicator = 'fuel')
  expect_equal(as.character(hits$word), c('fuel','fuel'))

  ## two indicators
  hits = searchQuery(tokens, indicator = 'fuel fuels')
  expect_equal(as.character(hits$word), c('fuel','fuels','fuel'))

  ## indicator with wildcard
  hits = searchQuery(tokens, indicator = 'fuel*')
  expect_equal(as.character(hits$word), c('fuel','fuels','fueled','fuel'))

  ## indicator and condition
  hits = searchQuery(tokens, indicator = 'fuel*', condition = 'renewable green clean')
  expect_equal(as.character(hits$word), c('fuel','fuels'))

  ## condition with default.window
  hits = searchQuery(tokens, indicator = 'fuel*', condition = 'renewable green clean', default.window = 2)
  expect_equal(as.character(hits$word), c('fuel'))

  ## condition once parameter
  hits_f = searchQuery(tokens, indicator = 'rutte', condition = 'mark~2')
  hits_t = searchQuery(tokens, indicator = 'rutte', condition = 'mark~2', condition_once = T)
  expect_equal(as.character(hits_f$word), c('Mark Rutte'))
  expect_equal(as.character(hits_t$word), c('Mark Rutte','Rutte'))

  ## multiple queries
  queries = data.frame(code=c('Renewable fuel', 'Mark Rutte', 'debate'),
                       indicator=c('fuel*', 'rutte', 'debate'),
                       condition = c('renewable green clean', 'mark~2', ''))
  hits = searchQueries(tokens, queries, condition_once=c(F,T,F))
  expect_equal(as.character(hits$word), c('fuel','fuels','Mark Rutte', 'Rutte', 'debate'))
})
