sidebar <- dashboardSidebar(
  sidebarMenu(
    h3('Open Codebook'),
    fileInput('codebookfile', label = "Select codebook"),
    #textInput('codebookname', label = "Save codebook as", "new_codebook.csv"),
    selectInput('code_columns', label = "Code tree columns", choices=c(), multiple = T),

    hr(),
    h3("Select articles"),
    numericInput('aset_id', 'Articleset id', 10271),

    hr(),
    h3("Search parameters"),
    checkboxInput('condition_once', 'Condition only has to be TRUE once per article', F),
    #radioButtons('use_window', 'Conditions have to occur within:', list('Same document'='document', 'Word distance'='window'), 'document'),
    #conditionalPanel(
    #  condition = "input.use_window == 'window'",
    #  sliderInput('nwords', 'Window size', 0, 100, 25, post = ' words')
    #),

    #hr(),
    #h3("Output parameters"),
    sliderInput('sample_n', 'Summary sample', 1, 100, 15),
    sliderInput('kwic_words', 'Keyword in context window', 1, 100, 10)
  )
)

## add codebook box
body <- dashboardBody(
  fluidRow(
    tabBox(
      tabPanel(title = 'Search',
               h3(htmlOutput('code'), width=10),
               HTML(
                 '<h5>indicator</h5>
                 <textarea id="indicator" style=color:#000000 rows="2" cols="75">war*</textarea>
                 <h5>condition</h5>
                 <textarea id="condition" style=color:#000000 rows="4" cols="75"></textarea>
                 <br>
                 '),
               actionButton("searchButton", "Search", icon = icon('search'), width = 130, style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               actionButton("saveButton", "Save", icon = icon('save'), width = 70, style="color: #fff; background-color: #00FF00; border-color: #2e6da4"),
               hr(),


               DT::dataTableOutput('codebooktable')
               ## codebook within radiobutton div (deprecated, since DT also allows row selection)
               #HTML(sprintf('
               #  <div id="code_id", class = "form-group shiny-input-radiogroup shiny-bound-input">
               #    <div class="shiny-options-group">
               #       %s
               #    </div>
               #  </div>
               #     ', dataTableOutput('codebooktable')))
               ),

      tabPanel(title = 'Compare queries',
               fluidRow(
                 column(width=6,
                        h3('Query X'),
                        HTML(
                          '<h5>indicator</h5>
                          <textarea id="indicatorX" style=color:#000000 rows="3" cols="40">war*</textarea>
                          <h5>condition</h5>
                          <textarea id="conditionX" style=color:#000000 rows="6" cols="40"></textarea>
                          <br>
                          '),
                        actionButton('searchButtonX', label = 'Search', icon = icon('search'), width = 80, style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton('searchXnotY', label = 'X NOT Y', icon = icon('search'), width = 100, style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("saveButtonX", "Save", icon = icon('save'), width = 80, style="color: #fff; background-color: #00FF00; border-color: #2e6da4")
                        ),
                 column(width=6,
                        h3('Query Y'),
                        HTML(
                          '<h5>indicator</h5>
                          <textarea id="indicatorY" style=color:#000000 rows="3" cols="40">war*</textarea>
                          <h5>condition</h5>
                          <textarea id="conditionY" style=color:#000000 rows="6" cols="40"></textarea>
                          <br>
                          '),
              actionButton('searchButtonY', label = 'Search', icon = icon('search'), width = 80, style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              actionButton('searchYnotX', label = 'Y NOT X', icon = icon('search'), width = 100, style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              actionButton("saveButtonY", "Save", icon = icon('save'), width = 80, style="color: #fff; background-color: #00FF00; border-color: #2e6da4")
            )
          )
      )
    ),
    tabBox(
      tabPanel(title = "Summary", solidHeader = TRUE,
               htmlOutput('hitsummary')),
      tabPanel(title = "Indicator frequencies", solidHeader = TRUE,
               dataTableOutput("tokenfreq")),
      tabPanel(title = "Help", colidHeader=TRUE,
               h3('Queries'),
               queryhelp())
    )
  )
)

dashboardPage(
  dashboardHeader(title = ''),
  sidebar,
  body
)
