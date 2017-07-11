if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('MASS')) install.packages('MASS'); library('MASS')
if (!require('stats')) install.packages('stats'); library('stats')
if (!require('FNN')) install.packages('FNN'); library('FNN')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
if (!require('shinyBS')) install.packages('shinyBS'); library('shinyBS')
if (!require('KernSmooth')) install.packages('KernSmooth'); library('KernSmooth')

###############################################
ui <- dashboardPage(
  dashboardHeader(title="Nonparametric Regression",
                  titleWidth =270),
  dashboardSidebar(
    checkboxGroupInput('method','Select a method', choices=c(`K-nearest-neighbor (KNN)`="L", `Nadaraya-Watson (NW)`="I",
                    `Local polynomial (LP)`="J", `Smoothing splines (S)`="K"), selected = "L"),
    conditionalPanel(condition="input.method.indexOf('L') > -1", uiOutput("selectk")),
    bsTooltip("selectk", "K-nearest-neighbor","right", options = list(container = "body")),
    conditionalPanel(condition="input.method.indexOf('I') > -1", radioButtons("kernel", "Choose Kernel (NW)", choices=c("normal", "box"), selected="normal")),
    bsTooltip("kernel", "Nadaraya-Watson","right", options = list(container = "body")),
    conditionalPanel(condition="input.method.indexOf('I') > -1", uiOutput("bandwidth")),
    bsTooltip("bandwidth", "Nadaraya-Watson","right", options = list(container = "body")),
    conditionalPanel(condition="input.method.indexOf('J') > -1", sliderInput("smoother", "Select smoother span (LP)", min=0.1, max=1, value=2/3, ticks=F)),
    bsTooltip("smoother", "Local polynomial","right", options = list(container = "body")),
    conditionalPanel(condition="input.method.indexOf('K') > -1", sliderInput("spar", "Select smoother parameter (S)", min=0.1, max=1, value=0.9, ticks=F)),
    bsTooltip("spar", "Smoothing splines","right", options = list(container = "body")),
    
  sidebarMenu(menuItem("Data", icon = icon("th"),
    uiOutput("varselect1"),
    uiOutput("varselect"),
    uiOutput("dataset"),
    uiOutput("slider"),
    div(style="text-align: center;",actionButton("button", label = "Sample again", icon = icon("refresh")))))),
  dashboardBody(plotOutput("viz2" ,height=500, width=800)))

###############################################
 shinyServer <- function(input, output, session) {
  output$varselect <- renderUI({})
  outputOptions(output, "varselect", suspendWhenHidden = FALSE)
  output$varselect1 <- renderUI({})
  outputOptions(output, "varselect1", suspendWhenHidden = FALSE)
  output$slider <- renderUI({})
  outputOptions(output, "slider", suspendWhenHidden = FALSE)
  output$dataset <- renderUI({})
  outputOptions(output, "dataset", suspendWhenHidden = FALSE)
  
 output$dataset <- renderUI({
  selectInput("dataset1","Select a dataset", c("Cars" ,"Swiss"), selected = "Cars")})

 data <- reactive({
   switch(input$dataset1,"Cars" = mtcars[,-2], "Swiss" = swiss)})

 output$varselect <- renderUI({
  selectInput("invarselect", "Select independent variable (x)", choices=names(data()),selected=names(data())[2],multiple=FALSE)})
 
 output$varselect1 <- renderUI({
  selectInput("invarselect1", "Select dependent variable (y)", choices=names(data()),selected=names(data())[1],multiple=FALSE)})
 
 output$slider <- renderUI({
  sliderInput("inslider","Select sample size",step=1, min=1 , max=nrow(data()),value = nrow(data()),  ticks=F) })
 
 y <- reactive({input$button
    sort(sample(nrow(data()),input$inslider))})
  
 output$bandwidth <- renderUI({
  a<-which(colnames(data())==input$invarselect)
    sliderInput("bw", "Select bandwidth (NW)", min=1, max= max(data()[y(),a]), value=mean(data()[y(),a]), ticks=F)})
 
 output$selectk <- renderUI({
    sliderInput("k", "Select K (KNN)", min=1, max=max(row(data()[,]))-1, value=mean(row(data()[,])), step=1, ticks=F)})
  
 output$viz2 <- renderPlot({
   validate(need(length(input$method)!=0, "Select at least one method."))
   legend <- NULL
    kI <-ksmooth(data()[y(),input$invarselect], data()[y(),input$invarselect1],kernel=(input$kernel), bandwidth=input$bw, x.points=data()[y(),input$invarselect])
    kJ <- lowess(data()[y(),input$invarselect], data()[y(),input$invarselect1], f=input$smoother)
    kK <- smooth.spline(data()[y(),c(input$invarselect,input$invarselect1)], spar=input$spar)
    kL <- knn.reg(data()[y(),input$invarselect], y=data()[y(),input$invarselect1],k=input$k, algorithm = input$algo)
      
      plot(data()[y(),input$invarselect], data()[y(),input$invarselect1], xlab = input$invarselect, ylab=input$invarselect1, main="Nonparametric Regression",pch=16)
      if (is.element('L', input$method))
      {lines(data()[y(),input$invarselect][order(data()[y(),input$invarselect])], kL$pred[order(data()[y(),input$invarselect])],col=5)
        legend[1] <-"K-nearest-neighbour"} else {}
      
      if (is.element('I', input$method)) {lines(kI$x, kI$y,col=2)
        legend[2] <- "Nadaraya-Watson"} else {}
     
      if (is.element('J', input$method)) {lines(kJ$x, kJ$y,col=3)
        legend[3] <- "Local polynomial"} else{}
    
      if (is.element('K', input$method)) {lines(kK$x, kK$y,col=4)
       legend[4] <- "Smoothing splines"} else{}
       
      legend("topright",legend, lty=c(1,1), lwd=c(2.5,2.5),col=c(5,2,3,4))
      })
 }

shinyApp(ui = ui, server = shinyServer)
