if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('andrews')) install.packages('andrews'); library('andrews')
if (!require('aplpack')) install.packages('aplpack'); library('aplpack')
if (!require('MASS')) install.packages('MASS'); library('MASS')
if (!require('graphics')) install.packages('graphics'); library('graphics')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
if (!require('shinyBS')) install.packages('shinyBS'); library('shinyBS')
if (!require('combinat')) install.packages('combinat'); library('combinat')


###############################################
ui <- dashboardPage(
  dashboardHeader(title="Multivariate Graphics"),
  dashboardSidebar(
    selectInput("method","Select a method", c("Andrews curves", "Parallel coordinates", "Star plots", "Scatter plot", "Chernoff faces")),
    conditionalPanel(condition = "input.method=='Andrews curves'",sliderInput("ymax","ymax", value = 5, min = 0, max = 10, ticks=F)),
    bsTooltip("ymax", "Choose maximum for y-axis","right", options = list(container = "body")),
    conditionalPanel(condition = "input.method=='Andrews curves'",selectInput("curve", "Type of curve", c("1. sin() + cos()"="f(t)=x1/(2^0.5)+x2*sin(t)+x3*cos(t)+x4*sin(2*t)+x5*cos(2*t)+...", "2. sin() + cos()"="f(t)=x1*sin(t)+x2*cos(t)+x3*sin(2*t)+x4*cos(2*t)+...", "3. cos()"="f(t)=x1*cos(t)+x2*cos((2*t)^0.5)+x3*cos((3*t)^0.5)+...", "4. (sin()+cos())+(sin()-cos())"="f(t)=1/(2^0.5)*(x1+x2*(sin(t)+cos(t))+x3*(sin(t)-cos(t))+x4*(sin(2*t)+cos(2*t))+..."))),
    bsTooltip("curve", "Complete formular in plot title.","right", options = list(container = "body")),
    conditionalPanel(condition = "input.method=='Chernoff faces'",radioButtons("type", "Type of face", c("Color" = 1,"Black and White"= 0))),
    conditionalPanel(condition = "input.method=='Star plots'",radioButtons(inputId= "radius","Radius",c("True"=T, "False"=F))),
                     
 sidebarMenu(
    menuItem("Data", icon = icon("th"),
    conditionalPanel(condition="input.method=='Chernoff faces'||input.method=='Star plot'",
    uiOutput("trans")),
    selectInput("dataset","Select a dataset", c("Swiss" ,"U.S. Arrests" , "Iris flower data")),
    uiOutput("varselect"),
    bsTooltip("varselect", "Select and press delete to remove variable","right", options = list(container = "body")),
    div(style="text-align: center;",actionButton("button1", label = "Cycle variables", icon = icon("refresh"))),
    uiOutput("slider"),
    div(style="text-align: center;",actionButton("button", label = "Sample again", icon = icon("refresh")))))),
 dashboardBody(
   tags$head(
     tags$style(HTML("
      .shiny-output-error-validation {
        color: red;}"))),
   fluidRow(
     box(width=9,
   plotOutput("viz" , height=580)),
   
   conditionalPanel(condition="input.method=='Andrews curves' || input.method=='Star plots' || input.method=='Chernoff faces'",
   box(width=3, conditionalPanel(condition= "input.method=='Andrews curves' || input.method=='Star plots'", tableOutput("order")),
    conditionalPanel(condition= "input.method=='Chernoff faces'",tableOutput("text")))
   )
)))


###############################################
shinyServer <- function(input, output, session) {
  
  output$varselect <- renderUI({})
  outputOptions(output, "varselect", suspendWhenHidden = FALSE)
  output$slider <- renderUI({})
  outputOptions(output, "slider", suspendWhenHidden = FALSE)
  output$trans <- renderUI({})
  outputOptions(output, "trans", suspendWhenHidden = FALSE)
  
  output$trans <- renderUI ({
    if (input$method=='Star plots')
    {radioButtons("trans1","Select transformation of variables", c("Raw data", "Z-Score", "Scale [0,1]"), selected = "Scale [0,1]")}
    else if (input$method=='Scatter plot')
    {radioButtons("trans1","Select transformation of variables", c("Raw data", "Z-Score", "Scale [0,1]"), selected = "Raw data")}
    else
    {radioButtons("trans1","Select transformation of variables", c("Raw data", "Z-Score", "Scale [0,1]"), selected = "Z-Score")}
  })
  
  andrewstitle <- reactive({switch(input$curve,
                                    "f(t)=x1/(2^0.5)+x2*sin(t)+x3*cos(t)+x4*sin(2*t)+x5*cos(2*t)+..." = 1,
                                   "f(t)=x1*sin(t)+x2*cos(t)+x3*sin(2*t)+x4*cos(2*t)+..." = 2,
                                   "f(t)=x1*cos(t)+x2*cos((2*t)^0.5)+x3*cos((3*t)^0.5)+..." =3,
                                   "f(t)=1/(2^0.5)*(x1+x2*(sin(t)+cos(t))+x3*(sin(t)-cos(t))+x4*(sin(2*t)+cos(2*t))+..." =4)})
  
  datasetInput <- reactive({switch(input$dataset,
                                   "U.S. Arrests" = USArrests,
                                   "Iris flower data" = iris[,1:4],
                                   "Swiss" = swiss)})
  
  methodInput <- reactive({switch(input$method,
                                  "Andrews curves"= andrews,
                                  "Chernoff faces" = faces,
                                  "Parallel coordinates" = parcoord,
                                  "Star plots" = stars)})
  
 
  output$varselect <- renderUI({
    selectInput("invarselect", "Select variables", choices=names(datasetInput()),selected=names(datasetInput()),multiple=TRUE)})
  
  c <- reactiveValues(count=1)
    observeEvent(input$button1, c$count <- c$count+1)
    observe({ if (c$count>factorial(length(input$invarselect))) c$count<-1})
  
  output$slider <- renderUI({
    sliderInput("inslider","Select sample size",step=1, min=1 , max=nrow(datasetInput()), value = nrow(datasetInput()), ticks=F) })
  
  y <- reactive({input$button
    sort(sample(nrow(datasetInput()),input$inslider))})
  
  output$viz <- renderPlot({
    perm <- permn(input$invarselect)
    
  z <- input$invarselect
    if (input$trans1=="Raw data") {x <- as.matrix(datasetInput())}
    else if (input$trans1=="Z-Score") {x <- as.matrix(scale(datasetInput()))}
    else {doit <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))}
     x <- as.matrix(as.data.frame((lapply(datasetInput(), doit))))
     row.names(x) <- row.names(datasetInput())}
      
    if       (input$method=="Andrews curves")
    {validate(need(length(z)>1, "Select at least 2 variables."))
      andrews(x[y(),unlist(perm[c$count])],ymax=input$ymax, type=andrewstitle(), main=input$curve)}
    else if (input$method=="Chernoff faces") 
    {withProgress(message = 'Calculation in progress',
                  value = 0, {
                    for (i in 1:7) {
                      incProgress(1/7)
                      Sys.sleep(0.2)
                    }
                  })
      validate(need(length(z)>1, "Select at least 2 variables."))
      faces(x[y(),unlist(perm[c$count])],face.type=input$type, scale=F)}
    else if (input$method=="Parallel coordinates")
    {validate(need(length(z)>1, "Select at least 2 variables."))
      parcoord(x[y(),unlist(perm[c$count])])}
    else if (input$method=="Scatter plot") {
      validate(need(length(z)>0, "Select at least 1 variable."))
     if (length(z)==1) {plot(x[y(),z],ylab=z,   pch=19)}
     else {plot(as.data.frame(x[y(),unlist(perm[c$count])]), pch=19)}}
    else {validate(need(length(z)>1, "Select at least 2 variables."))
      stars(x[y(),unlist(perm[c$count])], scale=F, radius =input$radius)}
    })
  
  output$text <- renderTable({
    validate(need(length(input$invarselect)>1, "Select at least 2 variables."))
    perm <- permn(input$invarselect)
    faces(datasetInput()[,unlist(perm[c$count])])$info
    })
  
  output$order <- renderTable({
    validate(need(length(input$invarselect)>0, "Select at least 1 variable."))
    perm <- permn(input$invarselect)
    tab <- matrix(NA,length(input$invarselect),2)
    tab[,2] <- unlist(perm[c$count])
    tab[,1] <- seq(1,length(input$invarselect), 1)
    colnames(tab) <- c("Order", "Variables")
    tab
    })
  
}

shinyApp(ui = ui, server = shinyServer)