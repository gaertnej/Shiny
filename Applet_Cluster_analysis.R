if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('cluster')) install.packages('cluster'); library('cluster')
if (!require('sparcl')) install.packages('sparcl'); library('sparcl')
if (!require('LICORS')) install.packages('LICORS'); library('LICORS')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
if (!require('shinyBS')) install.packages('shinyBS'); library('shinyBS')
if (!require('e1071')) install.packages('e1071'); library('e1071')
if (!require('dbscan')) install.packages('dbscan'); library('dbscan')
if (!require('mclust')) install.packages('mclust'); library('mclust')


###############################################
ui <- dashboardPage(
  dashboardHeader(title="Cluster analysis"),
  dashboardSidebar(
   selectInput('method','Select clustering method',choices= list(
                                "Hierarchical" = c(`Divisive` = 'D', `Agglomerative` = 'A'),
                                "Partitioning" = c(`K-means` = 'k',`K-means++`='kpp', `K-medoids` = 'km', `Fuzzy` = 'fuzz'),
                                "Model-based" = c(`Expectation max. (EM)` = 'EM'),
                                "Density-based" = c(`DBSCAN` = 'DB')), selectize=FALSE),
  conditionalPanel(condition="input.method=='A'",
  selectInput("AggMethod", "Agglomerative method", c("complete", "ward"="ward.D", "single", "average", "mcquitty", "median", "centroid"))),
  conditionalPanel(condition="input.method=='A'", selectInput("DistMethod", "Distance method", c("euclidian", "maximum", "manhattan", "canberra", "binary", "minkowski"))),
  conditionalPanel(condition ="input.method=='k'", selectInput("algorithm","Select algorithm", c("Hartigan-Wong", "Lloyd", "MacQueen"))),
  conditionalPanel(condition ="input.method=='km'|| input.method=='D'", selectInput("metric", "Distance method", c("euclidian", "manhattan"))),
  conditionalPanel(condition ="input.method=='A'||input.method=='D'", sliderInput("hang","Select hang", min=-0.1, max=0.2, step=0.1, value=0.1, ticks=F)),
  conditionalPanel(condition ="input.method=='fuzz'", selectInput("metric1", "Distance method", c("euclidean", "manhattan"))),
  conditionalPanel(condition ="input.method=='fuzz'", sliderInput("fuzzification","Degree of fuzzification",min=1.1, max=3, step=0.1, value=2, ticks=F)),
  conditionalPanel(condition="input.method=='DB'", uiOutput("ep")),
  bsTooltip("ep", "Determine the knee, which corresponds to  the optimal Epsilon parameter","right", options = list(container = "body")),
  conditionalPanel(condition="input.method=='DB'",sliderInput("minp",label="Minimum of points",min=1, max=10, step=1, value=5, ticks=F)),
  conditionalPanel("input.method=='km'|| input.method=='k'||input.method=='D'||input.method=='A'||input.method=='kpp'||input.method=='fuzz'", 
                  sliderInput("NOclust", "Select number of cluster (K)", min = 1, max=10, step=1, value=1, ticks=F)),
  bsTooltip("NOclust", "Number of clusters has to be defined for Partitioning methods","right", options = list(container = "body")),
  
  conditionalPanel(condition = "input.method=='k'||input.method=='fuzz'||input.method=='kpp'",
                  div(style="text-align: center;",actionButton("button1", label = "Recalculate", icon = icon("refresh")))),
  bsTooltip("button1", "Recalculating leads to different start values and can change the result.","right", options = list(container = "body")),
  
  sidebarMenu(
   menuItem("Data", icon = icon("th"),
   radioButtons("trans","Select transformation of variables", c("Raw data", "Z-Score", "Normalization [0,1]"), selected = "Z-Score"),         
   bsTooltip("trans", "Z-score is recommended for cluster analysis.","right", options = list(container = "body")),
   uiOutput("dataset1"),
   uiOutput("varselect"),
   bsTooltip("varselect", "Select and press delete to remove variable","right", options = list(container = "body")),
   uiOutput("slider"),
   div(style="text-align: center;",actionButton("button", label = "Sample again", icon = icon("refresh")))))),
dashboardBody(
  tags$head(
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
  output$dataset1 <- renderUI({})
  outputOptions(output, "dataset1", suspendWhenHidden = FALSE)
  
  output$mytabs = renderUI({
    if(input$method=='D'|| input$method=='A')
      myTabs = tabsetPanel(tabPanel('Dendrogram', plotOutput("viz",height=500, width=800)),
                           tabPanel("Scatter plot", plotOutput("viz2",height=500, width=800)),
                           tabPanel("Silhouette plot", plotOutput("viz3", height=500, width=800)))
    if(input$method=='k'||input$method=='km'||input$method=='kpp')
      myTabs = tabsetPanel(tabPanel('Scatter plot',  plotOutput("viz",height=500, width=800)),
                           tabPanel('Silhouette plot', plotOutput("viz3", height=500, width=800)))
    if(input$method=='fuzz')
      myTabs = tabsetPanel(tabPanel('Scatter plot',  plotOutput("viz",height=500, width=800)),
                           tabPanel('Silhouette plot', plotOutput("viz3", height=500, width=800)),
                           tabPanel('Memberships', dataTableOutput("memb")))
    if(input$method=='EM')
      myTabs = tabsetPanel(tabPanel('BIC', plotOutput("viz4",height=500, width=800)),
                           tabPanel('Scatter plot',  plotOutput("viz",height=500, width=800)),
                           tabPanel('Silhouette plot', plotOutput("viz3", height=500, width=800)))
    if(input$method=='DB')
      myTabs = tabsetPanel(tabPanel('Epsilon', plotOutput("viz5",height=500, width=800)),
                           tabPanel('Scatter plot',  plotOutput("viz7",height=500, width=800)),
                           tabPanel('Silhouette plot', plotOutput("viz3", height=500, width=800)),
                           bsPopover("viz7", "Info", "Black points are outliers", placement = "top"),
                           bsPopover("viz3", "Info", "Possibly to many observations for Silhouette Plot", placement = "top"))                      
    mainPanel(myTabs)
    })
  
  
 output$dataset1 <- renderUI({if (input$method=='DB') {selectInput("dataset","Select a dataset", c("U.S. Crime" ,"Swiss" , "Iris flower data", "Multishapes"), selected = "Multishapes")}
    else {selectInput("dataset","Select a dataset", c("U.S. Crime" ,"Swiss" , "Iris flower data", "Multishapes"))}
    })
  
  
  
 datasetInput <- reactive({switch(input$dataset,
                                   "U.S. Crime" = USArrests,
                                   "Iris flower data" = iris[,1:4],
                                   "Swiss" = swiss,
                                   "Multishapes" = multishapes[,1:2])})
  

  
 data <- reactive({if (input$trans=="Raw data") {as.matrix(datasetInput())}
    else if (input$trans=="Z-Score") {as.matrix(scale(datasetInput()))}
    else {doit <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -min(x, na.rm=TRUE))}
    as.matrix(as.data.frame((lapply(datasetInput(), doit))))}
    })
  
 output$varselect <- renderUI({
    selectInput("invarselect", "Select variables", choices=names(datasetInput()),selected=names(datasetInput()),multiple=TRUE)})
  
 output$slider <- renderUI({
    sliderInput("inslider","Select sample size",step=1, min=1 ,max=nrow(datasetInput()),value = nrow(datasetInput()), ticks=F) })
  
 output$ep <- renderUI({
   sliderInput("epin", "Epsilon", min=round(min(kNN(data(), input$minp)$dist),2), max=round(max(kNN(data(), input$minp)$dist),2), value=round(min(kNN(data(), input$minp)$dist),2), ticks=F)})
  
 y <- reactive({input$button
   sample(nrow(datasetInput()),input$inslider)})

 fitmeans <- reactive({input$button1
  kmeans(data()[y(),input$invarselect], input$NOclust, algorithm = input$algorithm)})

 fitkpp <- reactive({input$button1
  kmeanspp(data()[y(),input$invarselect], input$NOclust)})

 fitfuzz <- reactive({input$button1
  cmeans(data()[y(),input$invarselect], input$NOclust, dist=input$metric1, m=input$fuzzification)})

 fitEM <- reactive({input$button1
  Mclust(data()[y(),input$invarselect])})

 output$viz <- renderPlot({
    z <- input$invarselect
    
    if (input$method=="D") {
      validate(need(length(z)>0, "Select at least 1 variable."))
      hc <- diana(data()[y(),z], metric=input$metric)
      hc <- as.hclust(hc)
      plot(hc, main = "Divisive Hierarchical Clustering",xlab="", sub="", hang=input$hang)
      if (input$NOclust > 1)
      {rect.hclust(hc, k = input$NOclust, border = "red")} }
    else if (input$method=="A")  {
      validate(need(length(z)>0, "Select at least 1 variable."))
      hc <- hclust(dist(data()[y(),z],input$DistMethod),input$AggMethod)
      plot(hc, main = "Agglomerative Hierarchical Clustering", xlab="",sub="", hang=input$hang)
      if (input$NOclust > 1)
      {rect.hclust(hc, k = input$NOclust, border = "red")}}
    else if (input$method=="k"){
      validate(need(input$NOclust>1, "Select at least 2 cluster."))
      validate(need(length(z)>1, "Select at least 2 variables."))
      plot(as.data.frame(data()[y(),z]), col=fitmeans()$cluster)}
    else if (input$method=="kpp") {
      validate(need(input$NOclust>1, "Select at least 2 cluster."))
      validate(need(length(z)>1, "Select at least 2 variables."))
      plot(as.data.frame(data()[y(),z]), col=fitkpp()$cluster)}
    else if (input$method=="km") {
      validate(need(input$NOclust>1, "Select at least 2 cluster."))
      validate(need(length(z)>1, "Select at least 2 variables."))
      fit <- pam(data()[y(),z],input$NOclust, metric=input$metric)
      plot(as.data.frame(data()[y(),z]), col=fit$cluster)}
    else if (input$method=="fuzz") {
      validate(need(input$NOclust>1, "Select at least 2 cluster."))
      validate(need(length(z)>1, "Select at least 2 variables."))
      plot(as.data.frame(data()[y(),z]), col=fitfuzz()$cluster)}
    else if (input$method=="EM") {
      validate(need(length(z)>1, "Select at least 2 variables."))
      fit <- Mclust(data()[y(),z])
      plot(as.data.frame(data()[y(),z]),col=fitEM()$classification)}
    })
 
 output$viz2 <- renderPlot({
   y <- input$inslider
   z <- input$invarselect
   validate(need(input$NOclust>1, "Select at least 2 cluster."))
   validate(need(length(z)>1, "Select at least 2 variables."))
   if (input$method=='D')  {
   hc <- diana(data()[y(),z], metric=input$metric)
   hc <- as.hclust(hc)
   clust <- cutree(hc, input$NOclust)
   plot(as.data.frame(data()[y(),z]), col=clust)}
   else {hc <- hclust(dist(data()[y(),z],input$DistMethod),input$AggMethod)
   clust <- cutree(hc, input$NOclust)
   plot(as.data.frame(data()[y(),z]), col=clust)}
   })
  
 output$viz3 <- renderPlot({
    input$button
    y <- input$inslider
    z <- input$invarselect
    validate(need(length(z)>1, "Select at least 2 variables."))
    
    if (input$method=="km") {
    validate(need(input$NOclust>1, "Select at least 2 cluster."))
    fit <- pam(data()[y(),z],input$NOclust, metric=input$metric)
    dissE <- daisy(data()[y(), z])
    sk <- silhouette(fit$cluster, dissE)
    plot(sk, main="Silhouette plot", col=sort(fit$cluster))}
    
    else if (input$method=="k") {
    validate(need(input$NOclust>1, "Select at least 2 cluster."))
    dissE <- daisy(data()[y(),z])
    sk <- silhouette(fitmeans()$cl, dissE)
    plot(sk, main="Silhouette plot", col=sort(fitmeans()$cl))}
    
    else if (input$method=="fuzz") {
    validate(need(input$NOclust>1, "Select at least 2 cluster."))
    dissE <- daisy(data()[y(),z])
    sk <- silhouette(fitfuzz()$cl, dissE)
    plot(sk, main="Silhouette plot", col=sort(fitfuzz()$cl))}
    
    else if(input$method=="kpp") {
    validate(need(input$NOclust>1, "Select at least 2 cluster."))
    dissE <- daisy(data()[y(),z])
    sk <- silhouette(fitkpp()$cl, dissE)
    plot(sk, main="Silhouette plot", col=sort(fitkpp()$cl))}
    
    else if(input$method=="D") {
    validate(need(input$NOclust>1, "Select at least 2 cluster."))
    hc <- diana(data()[y(),z], metric=input$metric)
    hc <- as.hclust(hc)
    clust <- cutree(hc, input$NOclust)
    dissE <- daisy(data()[y(),z])
    sk <- silhouette(clust, dissE)
    plot(sk, main="Silhouette plot", col=sort(clust))}
    
    else if(input$method=="EM") {dissE <- daisy(data()[y(),z])
    sk <- silhouette(fitEM()$classification, dissE)
    plot(sk, main="Silhouette plot", col=sort(fitEM()$classification))}
    
    else if(input$method=="DB") {
    validate(need(input$epin != round(min(kNN(data(), input$minp)$dist),2), "Change Epsilon parameter."))
    fit <-dbscan(data()[y(),input$invarselect], eps = input$epin, minPts = input$minp)
    dissE <- daisy(data()[y(),z])
    sk <- silhouette(fit$cluster, dissE)
    plot(sk, main="Silhouette plot", col=sort(fit$cluster+1))}
    
    else {
    validate(need(input$NOclust>1, "Select at least 2 cluster."))
    hc <- hclust(dist(data()[y(),z],input$DistMethod),input$AggMethod)
    clust <- cutree(hc, input$NOclust)
    dissE <- daisy(data()[y(),z])
    sk <- silhouette(clust, dissE)
    plot(sk,main="Silhouette plot", col=sort(clust))} })
  
 output$memb <- renderDataTable({
   validate(need(length(input$invarselect)>1, "Select at least 2 variables."))
   validate(need(input$NOclust>1, "Select at least 2 cluster."))
   fitfuzz()$membership[1:length(y()),]
   }, options=list(lengthMenu=c(10), pageLength=10))

 output$viz4 <- renderPlot({
  withProgress(message = 'Calculation in progress',
               value = 0, {
                 for (i in 1:7) {
                   incProgress(1/7)
                   Sys.sleep(0.2)
                 }
               })
  validate(need(length(input$invarselect)>1, "Select at least 2 variables."))
  plot(fitEM()$BIC)})

 output$viz5 <- renderPlot({
  validate(need(length(input$invarselect)>1, "Select at least 2 variables."))
  kNNdistplot(data()[y(), input$invarselect], k=input$minp)
  abline(h = input$epin, lty = 2)})

 output$viz7 <- renderPlot ({
  z <- input$invarselect
  validate(need(length(z)>1, "Select at least 2 variables."))
  validate(need(input$epin != round(min(kNN(data(), input$minp)$dist),2), "Change Epsilon parameter."))
  fit <-dbscan(data()[y(),input$invarselect], eps = input$epin, minPts = input$minp)
  plot(as.data.frame(data()[y(),z]),col=(fit$cluster+1))
  })
 
}

shinyApp(ui = ui, server = shinyServer)