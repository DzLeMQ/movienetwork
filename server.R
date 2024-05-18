#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/


# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {

  movieTriples <- data.frame()
 
  output$transfStatus <- renderUI({
    
    movieTriples <- MoviesData %>% preprocessCountry
    updateFilters(movieTriples)
     movieTriples <<- movieTriples
    #print(format(Sys.time(),'%H:%M:%S'))
    HTML("Transformation Completed.")
  })

  updateFilters <-function(triples){
    country <- data.frame(label = unique(triples$Country))
    year <- data.frame(label = unique(triples$Year))
    actor <- data.frame(label = unique(triples$Actor))
    director <- data.frame(label = unique(triples$Director))
    
    updateSelectizeInput(session, "countryChoice", choices = country$label, selected = NULL, server=T)
    updateSelectizeInput(session, "yearChoice", choices = year$label, selected = NULL, server=T)
    updateSelectizeInput(session, "actorChoice", choices = actor$label, selected = NULL, server=T)
    updateSelectizeInput(session, "directorChoice", choices = director$label, selected = NULL, server=T)

  }
 #  
  networkGraphPreprocess <- reactive({
    networkData <- data.frame(NULL)

    if (is.null(input$yearChoice) & is.null(input$countryChoice)&is.null(input$actorChoice) & is.null(input$directorChoice)){return (NULL)}

    ifelse (!is.null(input$countryChoice),networkData <- movieTriples %>%
                               filter(movieTriples$Country %in% c(input$countryChoice)), networkData <- movieTriples)

    ifelse (!is.null(input$yearChoice),networkData <- networkData %>%
              filter(networkData$Year %in% c(input$yearChoice)), networkData)

    ifelse (!is.null(input$actorChoice),networkData <- networkData %>%
              filter(networkData$Actor %in% c(input$actorChoice)), networkData)
    #
    ifelse (!is.null(input$directorChoice),networkData <- networkData %>%
              filter(networkData$Director %in% c(input$directorChoice)), networkData)
    
    return(networkData)
  })




  output$forceGraph = renderVisNetwork({

    g2 <- networkGraphPreprocess()
   
  
    if (is.null(g2) || (nrow(g2) == 0)) return (NULL)

      
    colorDir <- unique(data.frame(dgroup = g2$Director))
    colorMovie <- unique(data.frame(mgroup = g2$Movie))
    colorActor <- unique(data.frame(agroup = g2$Actor))
    colorYear <- unique(data.frame(agroup = g2$Year))
    
    df <- unique(subset(g2, select = c(Movie, Country)))
    df$label = "released_in"
    
    df1 <- unique(subset(g2, select = c(Movie, Actor)))
    df1$label <- "act_in"
    
    df2 <- unique(subset(g2, select = c(Movie, Director)))
    df2$label <- "direct_by"
    
    df3 <- unique(subset(g2, select = c(Movie, Year)))
    df3$label <- "release_by"
    
   
    
    networkData <- rbind(df1, setnames(df2, names(df1)))
    networkData <- rbind(networkData, setnames(df, names(networkData)))
    networkData <- rbind(networkData, setnames(df3, names(networkData)))
    # networkData <- rbind(networkData, setnames(df4, names(networkData)))
    
    src <- networkData$Movie
    target <- networkData$Actor
    graphData <- data.frame(src, target, stringsAsFactors = FALSE)
    sdf <- data.frame(name = graphData$src)
    tdf <- data.frame(name = graphData$target)
    nodes <- unique(data.frame(rbind(sdf, tdf)))
    
    
    
 
    
    nodes$group <-with(nodes, ifelse(name %in% colorDir$dgroup, "Directors", 
                                     ifelse(name %in% colorMovie$mgroup, "Movies",
                                            ifelse(name %in% colorActor$agroup, "Actors", 
                                                   ifelse(name %in% colorYear$agroup, "Years", "Countries")))))
    
    
    # make a links data frame using the indexes (0-based) of nodes in 'nodes'
    links <- data.frame(source = match(graphData$src, nodes$name) - 1,
                        target = match(graphData$target, nodes$name) - 1)
    
    nodes$id = 1:nrow(nodes)
    nodes <- data.frame(nodes, shadow = TRUE) #, shape =c('square'))
    edges <- data.frame(from = match(graphData$src, nodes$name) ,
                        to = match(graphData$target, nodes$name),
                        label = networkData$label )
    
    edges$length <- input$n
    nodes$url <- with(nodes, ifelse(name %in% g2$Movie, g2$movieLink,NA))
    nodes$label <- nodes$name
    nodes$title = nodes$url
    
    # if (nrow(networkData) > 1000) { networkData <- head(networkData, 500)}
    
    visNetwork(nodes, edges, width = '100%', heigh = '80%') %>%
      #visEdges(arrows = "to") %>%
      visOptions(collapse = list(enabled = TRUE,
                                 clusterOptions = list(shape = "circle"))) %>% #end turn on the arrows
      visNodes (color = list(highlight = "pink"))%>%
      visOptions(highlightNearest = list(enabled = F, degree = 3, hover = T),
                 nodesIdSelection = F)%>%
      visEvents(doubleClick =
                  "function(params) {
                  var nodeID = params.nodes[0];
                  var url = this.body.nodes[nodeID].options.url;
                  if (url != null) {window.open(url)};
        }") %>%
      visEvents(select = "function(nodes) {
                  Shiny.onInputChange('current_node_id', nodes.nodes);
                  ;}") %>%
      visInteraction(multiselect = T) %>%
      
      visLegend(width = 0.1, stepX = 50, stepY=50, position = "left") %>%
      visInteraction(navigationButtons = TRUE,
                     tooltipDelay = 0) %>%
      
      
      visLegend(width = 0.1, stepX = 50, stepY=50, position = "left") %>%
      visIgraphLayout(layout = "layout_nicely",
                      smooth = TRUE,
                      physic = TRUE,
                      type = 'full')

  })

  output$Movies <- renderValueBox({
    
    m <- getMovies()
    Movies = nrow(distinct(m, Movie))
    if (Movies < 1){
      return(NULL)
    }
    
    valueBox(
      value = tags$b(Movies),
      subtitle = tags$b("Movies"),
      #width = 3,
      icon = icon("film"),
      color = "yellow")
  })
  
  output$Directors <- renderValueBox({
    d <- getDirectors()
    Directors = nrow(d)
    if (Directors < 1){
      return(NULL)
    }
    valueBox(
      value = Directors,
      subtitle = "Directors",
      width = 3,
      icon = icon("users"),
      color = "aqua")
  })
  
  output$Actors <- renderValueBox({
    a <- getActors()
    Actors = nrow(a)
    if (Actors < 1){
      return(NULL)
    }
    valueBox(
      value = Actors,
      subtitle = "Actors",
      width = "10%",
      icon = icon("users"),
      color = "blue")
  })
  
  output$Countries <- renderValueBox({
    c <- getCountries() #unique(subset(movieTriples, select=c(Country)))
    Countries = nrow(c)
    if (Countries < 1){
      return(NULL)
    }
    valueBox(
      value = Countries,
      subtitle = "Countries",
      width = 3,
      icon = icon("globe"),
      color = "green")
  })

  getLatestMovies <- eventReactive(input$reloadLatestMovies, {
 
      rMovies <- MoviesData
      rMovies$YYYY <- substring(rMovies$Year,nchar(rMovies$Year) - 3)
      rMovies <- arrange(rMovies, desc(as.numeric(YYYY)))
      names( rMovies)[names( rMovies) == "Year"] <- "Released Date"
      rMovies <- subset(rMovies, select = -c(YYYY))

   }, ignoreNULL = FALSE)

  getMovies <- eventReactive(input$reloadLatestMovies, {
    mm <- unique(subset(movieTriples, select = c(Movie)))
    return(mm)
  }, ignoreNULL = FALSE)

  getDirectors <- eventReactive(input$reloadLatestMovies, {
    dd <- unique(subset(movieTriples, select = c(Director)))
    return(dd)
  }, ignoreNULL = FALSE)

  getActors <- eventReactive(input$reloadLatestMovies, {
    aa <- unique(subset(movieTriples, select = c(Actor)))
    return(aa)
  }, ignoreNULL = FALSE)

  getCountries <- eventReactive(input$reloadLatestMovies, {
    cc <- unique(subset(movieTriples, select = c(Country)))
    return(cc)
  }, ignoreNULL = FALSE)


  NMovies <- reactive({
    k <- getLatestMovies()
    k$Movie <- paste0("<a href='",k$movieLink,"'>",k$Movie,"</a>")
    nReleases <- head(k, n = input$latestN)
    return(nReleases)
  })

  output$analysisDetails  <- DT::renderDataTable({
    
    DT::datatable(select(NMovies(),-movieLink), escape=FALSE)
   
  })



  output$latestNTitle <- renderText({
    paste('Latest ', input$latestN, ' Movies Released/to be Released')
  })

 getMovies <- function(){
    return(MoviesData)
 }
  
 
 output$FullRawData <- DT::renderDataTable({
 
   #add html link tags
   MoviesData$Movie <- paste0("<a href='",MoviesData$movieLink,"'>",MoviesData$Movie,"</a>")
   #render datatable
   DT::datatable(select(MoviesData, -movieLink),escape=FALSE)
 
 })

})
