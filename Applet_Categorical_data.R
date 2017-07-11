if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('splitstackshape')) install.packages('splitstackshape'); library('splitstackshape')
if (!require('ggparallel')) install.packages('ggparallel'); library('ggparallel')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
if (!require('shinyBS')) install.packages('shinyBS'); library('shinyBS')
if (!require('shinyjs')) install.packages('shinyjs'); library('shinyjs')
if (!require('vcd')) install.packages('vcd'); library('vcd')
if (!require('pander')) install.packages('pander'); library('pander')
if (!require('combinat')) install.packages('combinat'); library('combinat')


###############################################
ui <- dashboardPage(
  
  dashboardHeader(title="Categorical data"),
  dashboardSidebar(radioButtons("method","Select a display method", c("Graph","Table")),
   conditionalPanel(condition = "input.method=='Table'", selectInput("tab", "Select type of table",
                                  c("Frequency", "Relative frequency", "Conditional relative frequency"))),
   conditionalPanel(condition = "input.tab=='Conditional relative frequency'&&input.method=='Table'",
                     radioButtons(inputId = "margin", "Select condition", c("Rows", "Columns"))),
   conditionalPanel(condition = "input.method=='Graph'",
                     selectInput("graphic", "Select type of graph", c("Bar chart","Mosaic plot","Spine plot", "Parallel coordinates"))),
   conditionalPanel(condition ="input.method=='Graph'&&input.graphic=='Bar chart'",
                     radioButtons("stack", "Select kind of bar chart", c("Grouped", "Stacked"))),
   conditionalPanel(condition = "input.method=='Graph'&&input.graphic=='Parallel coordinates'",
                     radioButtons(inputId="sets", label="Select method of display", choices=c("Parallel-sets"='parset', "Common-angle plot"='angle', "Hammock display"='hammock'))),
   checkboxInput(inputId = "coeff", "Show association statistics", value=T),
   sidebarMenu(menuItem("Data", icon = icon("th"),
    selectInput("dataset","Select a dataset", c("Titanic" ,"Hair/Eye/Color")),
    uiOutput("varselect"),
    bsTooltip("varselect", "Select and press delete to remove variable","right", options = list(container = "body")),
    div(style="text-align: center;",actionButton("button1", label = "Cycle variables", icon = icon("refresh"))),
    uiOutput("slider"),
    div(style="text-align: center;",actionButton("button", label = "Sample again", icon = icon("refresh")))))
    ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(" .shiny-output-error-validation {color: red; } "))
      ),
    fluidRow(
        box(width=12,
   conditionalPanel(condition = "input.method=='Table'", verbatimTextOutput("viz")),
   conditionalPanel(condition = "input.method=='Graph'",plotOutput("viz1")))),
   fluidRow(
   box(width=12, title="Association statistics",
   conditionalPanel(condition = "input.coeff", verbatimTextOutput("coeffOut"))))))

###############################################
shinyServer <- function(input, output, session) {
 output$varselect <- renderUI({})
 outputOptions(output, "varselect", suspendWhenHidden = FALSE)
 output$slider <- renderUI({})
 outputOptions(output, "slider", suspendWhenHidden = FALSE)
  
 datasetInput <- reactive({switch(input$dataset,
                                   "Titanic" = expandRows(as.data.frame(Titanic), "Freq"),
                                   "Hair/Eye/Color" = expandRows(as.data.frame(HairEyeColor), "Freq"))})
 output$varselect <- renderUI({
    if (input$method=='Graph' && input$graphic=='Parallel coordinates')
    {selectInput("invarselect", "Select variables", choices=names(datasetInput()),selected=names(datasetInput())[1:4],multiple=TRUE)}
    else {selectInput("invarselect", "Select variables", choices=names(datasetInput()),selected=names(datasetInput())[1:2],multiple=TRUE)}
    })
  
  c <- reactiveValues(count=1)
  observeEvent(input$button1, c$count <- c$count+1)
  observe({ if (c$count>factorial(length(input$invarselect))) c$count<-1})
  
 output$slider <- renderUI({
  sliderInput("inslider","Select sample size",step=1, min=1 , max=nrow(datasetInput()),value = nrow(datasetInput()),ticks=F) })
  
 y <- reactive({input$button
  sort(sample(nrow(datasetInput()),input$inslider))})
  
 output$viz <- renderPrint({
  z <- input$invarselect
  x <- datasetInput()
  perm <- permn(input$invarselect)
    
    if (input$method=="Table"&& input$tab=="Frequency")
    {validate(need(length(z)>0, "Select at least 1 variable."))
      pandoc.table(table(x[y(),unlist(perm[c$count])]), style="rmarkdown",plain.ascii = TRUE)  }
    else if (input$method=="Table"&&input$tab=="Relative frequency")
    {validate(need(length(z)>0, "Select at least 1 variable."))
      pandoc.table(round(prop.table(table(x[y(),unlist(perm[c$count])])),2), style="rmarkdown",plain.ascii = TRUE)}
    else if (input$method=="Table"&&input$tab=="Conditional relative frequency"&&input$margin=="Rows")
    {validate(need(length(z)>1, "Select at least 2 variables."))
      pandoc.table(round(prop.table(table(x[y(),unlist(perm[c$count])]), margin=1),2), style="rmarkdown",plain.ascii = TRUE)}
    else if (input$method=="Table"&&input$tab=="Conditional relative frequency"&&input$margin=="Columns")
    {validate(need(length(z)>1, "Select at least 2 variables."))
      pandoc.table(round(prop.table(table(x[y(),unlist(perm[c$count])]), margin=2),2), style="rmarkdown",plain.ascii = TRUE)}
    else{}
    })
  
 output$viz1 <- renderPlot({
    z <- input$invarselect
    x <- datasetInput()
    perm <- permn(input$invarselect)
    if (input$method=="Graph"&& input$graphic=="Bar chart"&&input$stack=="Grouped")
    {validate(need(length(z)==2, "Select 2 variables."))
      barplot(table(x[y(),unlist(perm[c$count])]),legend.text = TRUE, beside=TRUE, ylab="Frequency") }
    else if (input$method=="Graph"&& input$graphic=="Bar chart"&&input$stack=="Stacked")
    {validate(need(length(z)==2, "Select 2 variables."))
      barplot(table(x[y(),unlist(perm[c$count])]),legend.text = TRUE, beside=FALSE,ylab= "Frequency")}
    else if (input$method=="Graph"&& input$graphic=="Mosaic plot")
    {validate(need(length(z)>0, "Select at least 1 variable."))
      mosaicplot(table(x[y(),unlist(perm[c$count])]), main=NULL, cex.axis = 0.9)}
    else if (input$method=="Graph"&& input$graphic=="Spine plot")
    {validate(need(length(z)==2, "Select 2 variables."))
      spineplot(table(x[y(),unlist(perm[c$count])]))}
    else if (input$method=="Graph"&& input$graphic=="Parallel coordinates")
    {withProgress(message = 'Calculation in progress',
                  value = 0, {
                    for (i in 1:7) {
                      incProgress(1/7)
                      Sys.sleep(0.2)
                    }
                  })
      validate(need(length(z)>1, "Select at least 2 variables."))
      ggparallel(unlist(perm[c$count]), x[y(),], ratio = 0.25, text.angle = 0, width=0.2, method=input$sets)}
  })

 output$coeffOut <- renderPrint({
    z <- input$invarselect
    x <- datasetInput()
    perm <- permn(input$invarselect)
    validate(need(length(z)>1, "Select at least 2 variables."))
    assocstats(table(x[y(),unlist(perm[c$count])]))
  })
  
}

shinyApp(ui = ui, server = shinyServer)