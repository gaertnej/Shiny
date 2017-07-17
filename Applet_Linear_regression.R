if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('MASS')) install.packages('MASS'); library('MASS')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
if (!require('shinyBS')) install.packages('shinyBS'); library('shinyBS')
if (!require('rgl')) install.packages('rgl'); library('rgl')
if (!require('shinyRGL')) install.packages('shinyRGL'); library('shinyRGL')
if (!require('data.table')) install.packages('data.table'); library('data.table')


###############################################
ui <- dashboardPage(
  dashboardHeader(title="Linear Regression"),
  dashboardSidebar(
   selectInput('method','Select a model', choices=list("Binary Linear Regression"=c(`y=ß0+ß1x1`="A", `y=ß1x1`="B"),
                                                        "Multiple Linear Regression"=c(`y=ß0+ß1x1+ß2x1²`="C", `y=ß0+ß1x1+ß2x1²+ßx1³`="D",`y=ß0+ß1x1+ß2x2`="E",`y=ß0+ß1x1+ß2x2+ß3x1x2`="F"))),
   uiOutput("varselect"),
   bsTooltip("varselect", "The first selected variable is your dependent variable (y) ****** Select and press delete to remove variable","right", options = list(container = "body")),
   conditionalPanel(condition = "input.method=='E'||input.method=='F'", radioButtons("dis", "Select display", c("2D", "3D"))),
   bsTooltip("dis", "3D graphic is interactive.","right", options = list(container = "body")),
   
   sidebarMenu(menuItem("Data", icon = icon("th"),
   conditionalPanel(condition = "input.method=='B'", radioButtons("trans","Select transformation of variables", c("Raw data", "Z-Score"), selected = "Raw data")),         
   uiOutput("dataset"),
   uiOutput("slider"),
   div(style="text-align: center;",actionButton("button", label = "Sample again", icon = icon("refresh"))) ))),
   
   dashboardBody(tags$head(
     tags$style(HTML("
                     .shiny-output-error-validation {
                     color: red;}"))),
    uiOutput("mytabs")
  ))


###############################################
shinyServer <- function(input, output, session) {
  output$varselect <- renderUI({})
  outputOptions(output, "varselect", suspendWhenHidden = FALSE)
  output$slider <- renderUI({})
  outputOptions(output, "slider", suspendWhenHidden = FALSE)
  output$dataset <- renderUI({})
  outputOptions(output, "dataset", suspendWhenHidden = FALSE)
  
  output$dataset <- renderUI({
  selectInput("dataset1","Select a dataset", c("Cars" ,"Iris flower data","Swiss"))})
  
  datasetInput <- reactive({switch(input$dataset1,"Cars" = mtcars[,-2], "Swiss" = swiss, "Iris flower data" = iris[,-5])})
  output$mytabs = renderUI({
  if(input$method=='A'|| input$method=='B'|| input$method=='C'|| input$method=='D')
      myTabs = tabsetPanel(tabPanel("Scatter plot", plotOutput("viz2",height=500, width=800)),
                           tabPanel('Info', verbatimTextOutput("viz")),
                           tabPanel("Residuals", plotOutput("viz3",height=500, width=800)))
  if(input$method=='E'||input$method=='F')
      myTabs = tabsetPanel(tabPanel("2D Scatter plot", plotOutput("viz9", height=500, width=800)),
                           tabPanel("Info", verbatimTextOutput("viz")),
                           tabPanel("Residuals", plotOutput("viz3",height=500, width=800)))
  if((input$method=='E'||input$method=='F')&input$dis=='3D')
      myTabs = tabsetPanel(tabPanel("3D Scatter plot", rglwidgetOutput("viz8", height=500, width=800)),
                           tabPanel("Info", verbatimTextOutput("viz")),
                           tabPanel("Residuals", plotOutput("viz3",height=500, width=800)),
                           bsPopover("viz8", "Info", "Graphic is interactive", placement = "top"))
  mainPanel(myTabs)
  })
  
    
  data <- reactive({if (input$trans=="Raw data") {as.matrix(datasetInput())}
    else  {as.matrix(scale(datasetInput()))}
    })
  
  output$varselect <- renderUI({
    if(input$method=='E'||input$method=='F'){
    selectInput("invarselect", "Select variables", choices=names(datasetInput()),selected=names(datasetInput())[1:3],multiple=TRUE)}
    else {selectInput("invarselect", "Select variables", choices=names(datasetInput()),selected=names(datasetInput())[1:2],multiple=TRUE)}})
   
  output$slider <- renderUI({
    sliderInput("inslider","Select sample size",step=1, min   = 1 , 
                max   = nrow(datasetInput()),
                value = nrow(datasetInput()), ticks=F) })
  
    y <- reactive({input$button
    sort(sample(nrow(datasetInput()),input$inslider))})
  
    regForm <- reactive(if (input$method=='A'||input$method=='I')
    {as.formula(paste(input$invarselect[1],"~", input$invarselect[2]))}
    else if (input$method=='B') {as.formula(paste(input$invarselect[1],"~",0,"+", input$invarselect[2]))}
    else if (input$method=='C') {as.formula(paste(input$invarselect[1],"~", "poly(",input$invarselect[2],",",2,")"))}
    else if (input$method=='D') {as.formula(paste(input$invarselect[1],"~", "poly(",input$invarselect[2],",",3,")"))}  
    else if (input$method=='E') {as.formula(paste(input$invarselect[1], "~", input$invarselect[2], "+", input$invarselect[3]))}
    else if (input$method=='F') {as.formula(paste(input$invarselect[1], "~", input$invarselect[2], "*", input$invarselect[3]))})

  output$viz <- renderPrint({
    if (input$method=='E' || input$method=='F') {validate(need(length(input$invarselect)==3, "Select 3 variables."))}
    else {validate(need(length(input$invarselect)==2, "Select 2 variables."))}
    summary(lm(regForm(), as.data.frame(data()[y(),])))})
  
  output$viz2 <- renderPlot({
   validate(need(length(input$invarselect)==2, "Select 2 variables."))
   plot(data()[y(),input$invarselect[2]], data()[y(),input$invarselect[1]], xlab = input$invarselect[2], ylab = input$invarselect[1],  main = "Scatter plot with regression line", pch = 16, sub="", col = "black", cex = 1) 
   newdf <- data.frame(seq(min(data()[y(),input$invarselect[2]]), max(data()[y(),input$invarselect[2]]), length.out=100))
   names(newdf) <- input$invarselect[2]
   lines(newdf[,1], predict(lm(regForm(), as.data.frame(data()[y(),])), newdf), col=2)})
  
  output$viz3 <- renderPlot({
    if (input$method=='E' || input$method=='F') {validate(need(length(input$invarselect)==3, "Select 3 variables."))}
    else {validate(need(length(input$invarselect)==2, "Select 2 variables."))}
    par(mfrow=c(2,2))
    plot(lm(regForm(), as.data.frame(data()[y(),])), which=1, sub="")
    plot(lm(regForm(), as.data.frame(data()[y(),])), which=2, sub="")
    plot(lm(regForm(), as.data.frame(data()[y(),])), which=5, sub="")
    plot(lm(regForm(), as.data.frame(data()[y(),])), which=3, sub="")})

  output$viz8 <- renderRglwidget({ 
    withProgress(message = 'Calculation in progress',
                  value = 0, {
                   for (i in 1:7) {
                     incProgress(1/7)
                     Sys.sleep(0.2)
                   }
                 })
  validate(need(length(input$invarselect)==3, "Select 3 variables."))
if (input$method=='E') {
  plot3d(data()[y(),input$invarselect[2]], data()[y(),input$invarselect[3]], data()[y(),input$invarselect[1]], col = "black", type='p', xlab=input$invarselect[2], ylab=input$invarselect[3], zlab=input$invarselect[1])
  a <- lm(regForm(), data=as.data.frame(data()[y(),]))
  coef <- coef(a)
  rgl.planes(coef[input$invarselect[2]], coef[input$invarselect[3]], -1, coef["(Intercept)"], color="red", alpha=0.8)
  scene1 <- scene3d()
  rglwidget(scene1)}
  
else {
  a <- as.data.frame(data()[y(),])
  colnames(a)[colnames(a)==input$invarselect[2]] <- "x"
  colnames(a)[colnames(a)==input$invarselect[3]] <- "y"
  colnames(a)[colnames(a)==input$invarselect[1]] <- "z"
  plot3d(a$x,a$y,a$z, type="p", xlab=input$invarselect[2], ylab=input$invarselect[3], zlab=input$invarselect[1])

  mod2 <- lm(z~y*x, data=a)
  y=seq(range(a$y)[1],range(a$y)[2],len=30)
  x=seq(range(a$x)[1],range(a$x)[2],len=30)
  grd3 <- expand.grid(x=x, y=y)
  grd3$pred <- predict(mod2, newdata=grd3)
  table <- dcast(grd3,x~y,value.var="pred")[-1]
  surface3d(x,y,as.matrix(table), color="red", alpha=0.8)
  scene1 <- scene3d()
  rglwidget(scene1)}})

 output$viz9 <- renderPlot({
  validate(need(length(input$invarselect)==3, "Select 3 variables."))
  reg <- lm(regForm(), data=as.data.frame(data()[y(),]))
  X <- as.data.frame(data()[y(),c(input$invarselect[2], input$invarselect[3])])
  names(X) <- c(input$invarselect[2], input$invarselect[3])
  if (input$method=="F") {
  X$int <- X[,2]*X[,1]}
  X <- cbind(1,X)
  X <- as.matrix(X)
  plot(X%*%reg$coefficients, data()[y(),input$invarselect[1]], ylab=input$invarselect[1], xlab="Xb", pch=16)
  lines(sort(X%*%reg$coefficients), predict(reg, as.data.frame(X))[order(X%*%reg$coefficients)], col=2)})
}

shinyApp(ui = ui, server = shinyServer)
