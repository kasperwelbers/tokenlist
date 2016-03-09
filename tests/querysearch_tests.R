tok = data.frame(doc_id=rep(1, 9), position = 1:9,
                 word=c('jolande','sap','word','heel','soms','ook','gewoon','sap','genoemd'))
searchQuery(tok, indicator = 'sap', condition = 'jolande~2')
searchQuery(tok, indicator = 'sap', condition = 'jolande~2', condition_once = T)
