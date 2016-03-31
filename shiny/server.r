#install.packages('devtools')
#install_github("amcat/amcat-r")


#conn = amcat.connect('http://preview.amcat.nl') # connect to amcat
options(shiny.trace=F)

## plan: optie om csv of codebook te importeren en exporteren. Een veld waarin je de code selecteert. Dan kun je het aanpassen

shinyServer(function(input, output, session) {
  getTokens <- reactive({
    #amcat.gettokens(conn, project = 1, articleset = input$aset_id, module = 'elastic', npages = 1, page_size = 100) # get tokens from articleset 24812 in project 1006
    load(file='amcat_placeholder.rdata')
    tokens = tokens[,c('aid','position','term')]
    colnames(tokens) = c('doc_id','position','word')
    tokens
  })

  observeEvent(input$codebookfile,
               {
                 v$cb_orig = read.csv(input$codebookfile$name, stringsAsFactors = F)
                 if(!'indicator' %in% colnames(v$cb_orig)) v$cb_orig$indicator =  ''
                 if(!'condition' %in% colnames(v$cb_orig)) v$cb_orig$condition =  ''
                 if(!'code_id' %in% colnames(v$cb_orig)) v$cb_orig = cbind(code_id=1:nrow(v$cb_orig), v$cb_orig)
                 updateSelectInput(session, 'code_columns',
                                   choices = colnames(v$cb_orig),
                                   selected= colnames(v$cb_orig)[grep('[0-9]+', colnames(v$cb_orig))])
               })

  observe({
    if(!is.null(v$cb_orig) & !is.null(input$code_columns)){
      v$cb = interpretCodebook(v$cb_orig, input$code_columns)
    }
  })

  observe({
    #query = v$cb[as.numeric(input$codebooktable_row_last_clicked),]
    query = v$cb[v$cb$code_id == input$codebooktable_row_last_clicked,]
    updateTextInput(session, 'indicator', value = query$indicator)
    updateTextInput(session, 'condition', value = query$condition)
    updateTextInput(session, 'indicatorX', value = query$indicator)
    updateTextInput(session, 'conditionX', value = query$condition)
    updateTextInput(session, 'indicatorY', value = query$indicator)
    updateTextInput(session, 'conditionY', value = query$condition)
  })


  observeEvent(input$saveButton,
               {
                 i = which(input$codebooktable_row_last_clicked == v$cb_orig$code_id)
                 v$cb_orig$indicator[i] = input$indicator
                 v$cb_orig$condition[i] = input$condition
                 write.csv(v$cb_orig, input$codebookfile$name, row.names=F)
               })
  observeEvent(input$saveButtonX,
               {
                 i = which(input$codebooktable_row_last_clicked == v$cb_orig$code_id)
                 v$cb_orig$indicator[i] = input$indicatorX
                 v$cb_orig$condition[i] = input$conditionX
                 write.csv(v$cb_orig, input$codebookfile$name, row.names=F)
               })
  observeEvent(input$saveButtonY,
               {
                 i = which(input$codebooktable_row_last_clicked == v$cb_orig$code_id)
                 v$cb_orig$indicator[i] = input$indicatorY
                 v$cb_orig$condition[i] = input$conditionY
                 write.csv(v$cb_orig, input$codebookfile$name, row.names=F)
               })


  hits <- reactive(getHits(getTokens(), session, input, input$indicator, input$condition))
  hitsX <- reactive(getHits(getTokens(), session, input, input$indicatorX, input$conditionX))
  hitsY <- reactive(getHits(getTokens(), session, input, input$indicatorY, input$conditionY))

  observeEvent(input$searchButton, {v$result = prepareResults(getTokens(), input, hits())})
  observeEvent(input$searchButtonX, {v$result = prepareResults(getTokens(), input, hitsX())})
  observeEvent(input$searchButtonY, {v$result = prepareResults(getTokens(), input, hitsY())})
  observeEvent(input$searchXnotY, {v$result = prepareResults(getTokens(), input, XnotY(hitsX(), hitsY()))})
  observeEvent(input$searchYnotX, {v$result = prepareResults(getTokens(), input, YnotX(hitsX(), hitsY()))})

  output$hitsummary = renderText({
    d = v$result
    d$hitcount = sprintf('<p><b><i>%s</i></b></p><br>', d$hitcount)
    if(length(d$kwic) == 0) return(d$hitcount)

    d$kwic = sprintf('<p>%s</p>', d$kwic)
    kwic = aggregate(list(text=d$kwic), by=list(doc_id = d$hits$doc_id), paste, collapse = '\n\n')
    kwic$article_url = sprintf("https://amcat.nl/navigator/projects/1/articlesets/1/%s/", kwic$doc_id)
    kwic$head =  sprintf('<h4>Article <a href="%s">%s</a></h4>', kwic$article_url, kwic$doc_id)
    kwic$text = gsub('\\[(.*)\\]', '<mark>\\1</mark>', as.character(kwic$text), perl=T)

    c(d$hitcount, paste(kwic$head, kwic$text, sep='\n'))
  })

  output$tokenfreq <- renderDataTable({
    d = v$result
    d$tokenfreq$pct = round((d$tokenfreq$hits / sum(d$tokenfreq$hits)) * 100,1)
    d$tokenfreq
  })

  output$code <- renderText({
    code_id = input$codebooktable_row_last_clicked
    code = v$cb$code[v$cb$code_id == code_id]
    sprintf('%s: %s', code_id, code)
  })

  output$codebooktable = DT::renderDataTable({
    codebookDF(v$cb)
  }, escape=FALSE, selection='single', rownames=TRUE,
  options = list(autoWidth = FALSE, paging=FALSE, scrollX=T,
                 ordering=F, autofill='_all',
                 columnDefs = list(list(width = '50px', targets = "_all", align='left'))))

})


