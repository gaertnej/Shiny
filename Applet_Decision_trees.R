if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
if (!require('shinyBS')) install.packages('shinyBS'); library('shinyBS')
if (!require('mosaicData')) install.packages('mosaicData'); library('mosaicData')
if (!require('rpart')) install.packages('rpart'); library('rpart')
if (!require('rpart.plot')) install.packages('rpart.plot'); library('rpart.plot')
if (!require('splitstackshape')) install.packages('splitstackshape'); library('splitstackshape')



###############################################
ui <- dashboardPage(
  dashboardHeader(title="Decision trees"),
  dashboardSidebar(
   selectInput('method','Select a method', choices=c(`Regression tree`="M", `Classification tree`="N")),
   conditionalPanel(condition="input.dataset1!='Iris flower'",uiOutput("varselect1")),
   checkboxInput("pru", "Prune", value=FALSE),
   
   conditionalPanel(condition="(input.method=='M'||input.method=='N')&& !input.pru", sliderInput("depth", "Maximal depth", min=1,step=1, max=15, value = 1, ticks=F)),
   bsTooltip("depth", "The root node has depth 0.","right", options = list(container = "body")),
   conditionalPanel(condition="input.pru", uiOutput("pr")),
   sliderInput("min", "Select min. number in node for split", min=5, max=30,step=1, value=20, ticks=F),
   conditionalPanel(condition="input.method=='N'", radioButtons("parm", "Choose criterion for classification", choices=c("Gini index"="gini", "Information gain"="information"))),
   
   sidebarMenu(menuItem("Data", icon = icon("th"),
   uiOutput("dataset"),
   uiOutput("slider"),
   div(style="text-align: center;",actionButton("button", label = "Sample again", icon = icon("refresh"))) ))),
   
   dashboardBody(tags$head(
     tags$style(HTML(" .shiny-output-error-validation {color: red; } "))
   ),
   uiOutput("mytabs")))

shinyServer <- function(input, output, session) {
  output$varselect1 <- renderUI({})
  outputOptions(output, "varselect1", suspendWhenHidden = FALSE)
  output$slider <- renderUI({})
  outputOptions(output, "slider", suspendWhenHidden = FALSE)
  output$dataset <- renderUI({})
  outputOptions(output, "dataset", suspendWhenHidden = FALSE)
  
  
  output$dataset <- renderUI(
  {if (input$method=='N') {selectInput("dataset1","Select a dataset",
                           c("Iris flower","Titanic"), selected="Titanic")}
  else if(input$method=='M') {selectInput("dataset1","Select a dataset",
                    c("CPS85" ,"Cars" ,"Swiss"), selected = "CPS85")}
  })
  
  datasetInput <- reactive({
  if (input$method=='N') {switch(input$dataset1, "Iris flower"=iris[,c(5,1,2,3,4)],
                                                 "Titanic" = expandRows(as.data.frame(Titanic), "Freq")[,c(4,1,2,3)])}
  else {switch(input$dataset1,"CPS85"= CPS85[,c("wage", "educ", "exper")], "Cars" = mtcars[,-2], "Swiss" = swiss)} })

  output$mytabs = renderUI({
   if(input$method=='M'||input$method=='N')
   myTabs = tabsetPanel(
       tabPanel("Tree", plotOutput("viz8")),
       tabPanel("Info", verbatimTextOutput("viz9")))
   mainPanel(myTabs)
  })
  
  output$varselect1 <- renderUI({
    selectInput("invarselect1", "Select target variable (y)", choices=names(datasetInput()),selected=names(datasetInput())[1],multiple=FALSE)})
  
  output$slider <- renderUI({
    sliderInput("inslider","Select sample size",step=1, min = 20 , 
                max   = nrow(datasetInput()),
                value = nrow(datasetInput()), ticks=F) })
  
  y <- reactive({input$button
    sort(sample(nrow(datasetInput()),input$inslider))})
  
  regForm <- reactive( {as.formula(paste(input$invarselect1, "~","."))})

  model <- reactive({
    if (input$method=='M'&&!input$pru)
    {rpart(regForm(), data=datasetInput()[y(),], control=rpart.control(maxdepth=input$depth, cp=0, minsplit=input$min))}
    else if(input$method=='M'&&input$pru)
    {rpart(regForm(), data=datasetInput()[y(),],cp=0, minsplit=input$min)}
    else if (input$method=='N'&&!input$pru)
    {rpart(regForm(), data=datasetInput()[y(),],control=rpart.control(maxdepth=input$depth, minsplit=input$min, cp=0), method="class", parms=list(split=input$parm)) }
    else {rpart(regForm(), data=datasetInput()[y(),],cp=0,minsplit=input$min, method="class", parms=list(split=input$parm))}})

  output$pr <- renderUI({sliderInput("cp", "Select complexity parameter", min=0, max=round(max(model()$cptable[,1]),2),value=0.00, ticks=F)})
  
  output$viz8 <- renderPlot({
  if (input$pru) {model <- prune(model(), input$cp)
  prp(model, type=0, extra=101, yesno = 2, digits = 3, round=0)}
  else {prp(model(), type=0, extra=101, yesno = 2, digits = 3, round=0)}})

  output$viz9 <- renderPrint({
  if (input$pru) {model<- prune(model(), input$cp)
  summary(model)}
  else {summary(model())}})
  
}

shinyApp(ui = ui, server = shinyServer)
