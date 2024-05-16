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
  #print(format(Sys.time(),'%H:%M:%S'))
  output$transfStatus <- renderUI({
    
    movieTriples <- MoviesData %>% preprocessCountry
    updateFilters(movieTriples)
    # country <- data.frame(label = unique(movieTriples$Country))
    # year <- data.frame(label = unique(movieTriples$Year))
    # updateSelectInput(session, "countryChoice", choices = country$label, selected = NULL)
    # updateSelectInput(session, "yearChoice", choices = year$label, selected = NULL)
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


    # if (nrow(g2) > 0){
      
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
    
    # df4 <- unique(subset(g2, select = c(Director, Actor)))
    # df4$label <- "Work_with"
    
    
    networkData <- rbind(df1, setnames(df2, names(df1)))
    networkData <- rbind(networkData, setnames(df, names(networkData)))
    networkData <- rbind(networkData, setnames(df3, names(networkData)))
    # networkData <- rbind(networkData, setnames(df4, names(networkData)))
    
    # #making two columns data frame to establish relashionship
    # networkData <- rbind(df1, setnames(df2, names(df1)))
    # networkData <- rbind(networkData, setnames(df, names(networkData)))
    src <- networkData$Movie
    target <- networkData$Actor
    graphData <- data.frame(src, target, stringsAsFactors = FALSE)
    sdf <- data.frame(name = graphData$src)
    tdf <- data.frame(name = graphData$target)
    nodes <- unique(data.frame(rbind(sdf, tdf)))
    
    
    
    #nodes$group <- with(nodes, ifelse(name %in% colorgroup1$agroup, "au", "no"))
    
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



    # }

  })
 #
 #
  #if (nrow(movieTriples) > 0) {
      #N latest movie released cross the globe
      output$Movies <- renderValueBox({

        m <- getMovies()
        Movies = nrow(m)

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
  #}
 #
  getLatestMovies <- eventReactive(input$reloadLatestMovies, {
      mTriples = paste(PREFIXES, Q6)
      rMovies <- SPARQL::SPARQL(url = endpoint, query = mTriples)$results
      rMovies <- rMovies %>% preprocessCountry
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



  output$analysisDetails  <- renderTable({
    k <- getLatestMovies()
    analyse <- reactive(input$latestN, {
      nReleases <- head(k, n = input$latestN)
      return(nReleases)
    })
    return(analyses())
  })
 #
 #
  info2cloud <- reactive({

    if (input$freqGroup == "1") {

          triples = sqldf('select Director as word, count(*) as "freq"
                            from movieTriples group by Director')
    }else{triples = sqldf('select Actor as word, count(*) as "freq"
                            from movieTriples group by Actor')
    }

    infoFreq <- data.frame(triples)
    infoFreq <- infoFreq[order(-infoFreq$freq),]
    infoFreq <- head(infoFreq, n = input$cloudTopFreq)
  })

  output$info_cloud = renderWordcloud2({

    datafreq <- info2cloud()
    #wcloud <- data.frame(datafreq$keywords, datafreq$freq)
    wcloud <- data.frame(datafreq,row.names = datafreq$ID)

    #print("am I here")

    wordcloud2(wcloud, size = (sqrt(input$cloudTopFreq))/20,fontFamily = 'Segoe UI', fontWeight = 'bold',
               color = 'random-dark', backgroundColor = "white",
               shape = "circle")
  })
 #
 #
 #
 #  ##############Spatial View Dashboard###################################
 #  coords2continent = function(points)
 #  {
 #    countriesSP <- getMap(resolution = 'low')
 #
 #    # converting points to a SpatialPoints object
 #    pointsSP = SpatialPoints(points, proj4string = CRS(proj4string(countriesSP)))
 #
 #    # use 'over' to get indices of the Polygons object containing each point
 #    indices = over(pointsSP, countriesSP)
 #
 #    indices$ADMIN  #returns country name
 #
 #  }
 #
 #  getMapDetails <- eventReactive(input$reloadMap, {
 #
 #    networkData <- movieTriples
 #
 #    TARGET <-  networkData
 #
 #    networkData <-  data.frame(unique(trimws(networkData$Country)),stringsAsFactors = F)
 #
 #    names(networkData) <- c("place")
 #
 #
 #    country_points <- mutate_geocode(networkData, place)
 #    #geocode.country_points <- geocode(networkData$unique.trimws.networkData.Country..)
 #
 #    geocode.country_points <- country_points[,-1]
 #
 #
 #    geocode.country_points <- na.omit(geocode.country_points)
 #    geocode.country_points <- subset(geocode.country_points)
 #    output$map <- renderLeaflet({
 #      leaflet() %>%
 #        addProviderTiles(providers$Stamen.TonerLite,
 #                         options = providerTileOptions(noWrap = TRUE)
 #        ) %>%
 #        addMarkers(data = geocode.country_points)
 #    })
 #
 #  }, ignoreNULL = FALSE)
 #
 #
 #  output$map <- renderLeaflet({
 #
 #    networkData <- movieTriples
 #
 #    TARGET <- networkData
 #
 #    networkData <-  data.frame(unique(trimws(networkData$Country)),stringsAsFactors = F)
 #
 #    names(networkData) <- c("place")
 #
 #
 #    country_points <- mutate_geocode(networkData, place)
 #    #geocode.country_points <- geocode(networkData$unique.trimws.networkData.Country..)
 #
 #
 #    geocode.country_points <- country_points[,-1]
 #
 #    #geocode.country_points <- geocode(networkData$unique.trimws.networkData.Country..)
 #
 #
 #    geocode.country_points <- na.omit(geocode.country_points)
 #    geocode.country_points <- subset(geocode.country_points)
 #
 #    #geocode.country_points <- getMapDetails()
 #
 #    output$map <- renderLeaflet({
 #      leaflet() %>%
 #        addProviderTiles(providers$Stamen.TonerLite,
 #                         options = providerTileOptions(noWrap = TRUE)
 #        ) %>%
 #        addMarkers(data = geocode.country_points)
 #    })
 #    #
 #    #pop up windows of customers when a map location is clicked
 #    observe({
 #      updateSliderInput(session, "latestN",label = paste("Latest ", input$latestN," Movies Selected"))
 #      updateSliderInput(session, "cloudTopFreq",label = paste("Top ", input$cloudTopFreq, " Selected"))
 #      click <- input$map_marker_click
 #      if (is.null(click))
 #        return()
 #      else if (!is.null(click))
 #      {
 #
 #        points = data.frame(lon = c(click$lng), lat = c(click$lat))
 #
 #        name <- coords2continent(points)
 #        cName <- (as.data.frame(name))
 #
 #        if (!is.na.data.frame(cName$name)) {
 #          movies <- data.frame(TARGET[TARGET$Country == cName[1,1],])
 #          movies <- unique(subset(movies, select = c(Movie)))
 #          nMovies <- nrow(movies)
 #
 #          text <- paste("Movies: ", nMovies)
 #          #paste("Movies: ", paste("<font color=\"#1569C7\">",as.character(movies$Movie),  collapse="; ","</font>"))
 #          context <-
 #            leafletProxy(mapId = "map") %>%
 #            clearPopups() %>%
 #            addPopups(data = click, lat = ~lat, lng = ~lng, popup = text)
 #        }
 #
 #      }
 #
 #    })
 #
 #  })
 #
 #
  output$genres <- renderGvis({

    gm = paste(PREFIXES, Q2)
    mGenres <- SPARQL::SPARQL(url = endpoint, query = gm)$results

    mGenres$genres[mGenres$genres == "Drama films"] <- "Drama film"
    #mGenres$genres[mGenres$genres == "Drama"] <- "General Drama"
    mGenres$genres[mGenres$genres == "List Of Historical Drama Films"] <- "Historical Drama"
    mGenres$genres[mGenres$genres == "Action (genre)"] <- "Action"
    mGenres$genres[mGenres$genres == "Action fiction"] <- "Action"
    mGenres$genres[mGenres$genres == "Action (fiction)"] <- "Action"
    mGenres$genres[mGenres$genres == "Action film"] <- "Action"
    mGenres$genres[mGenres$genres == "Drama (genre)"] <- "General Drama"
    mGenres$genres[mGenres$genres == "Drama"] <- "General Drama"
    genresType <- data.frame(mGenres)
    genresMovies <- aggregate(list(totalMovies = genresType$movies),
                            list(genres  =  genresType$genres), sum)

    gmAll <- data.frame(genresid = c("All",
                                   as.character(genresMovies$genres),
                                   as.character(genresType$movie)),
                        parentid = c(NA, rep("All", 151),
                                   as.character(genresType$genres)),
                        mg = c(sum(genresType$movies),
                             genresMovies$totalMovies, genresType$movies))

    df = data.frame(gmAll$mg)
    gmAll$mg.log <-  as.numeric(unlist(log(df)))
    ProductLineTree <- gvisTreeMap(gmAll, "genresid", "parentid", "mg", "mg.log")

  })



  segmentTriples = reactive({#
    #movieActors <- SPARQL::SPARQL(url=endpoint, query=Q9)$results
    MAS <-  sprintf("select count(distinct Movie) as Movies, count(distinct Actor) as Actors, Year from movieTriples group by Year")
    mActors <-  sqldf(MAS)
    atriples <-  data.frame(mActors)

  })
 #
  bublesTriples = reactive({#

    #bTriples <- SPARQL::SPARQL(url=endpoint, query=Q11)$results
    DA <- sprintf("SELECT distinct Year, count(distinct Movie) as Movies, count(distinct Director) as Directors, count(distinct Actor) as Actors from movieTriples group by Year")
    bTriples <-  sqldf(DA)
    bTriples <-  data.frame(bTriples)

  })

  mTriples = reactive({#

    #mTriples <- SPARQL::SPARQL(url = endpoint, query = Q12)$results



    MA <-  sprintf("SELECT distinct Year, count(distinct Movie) as Movies from movieTriples group by Year")
    mTriples <- sqldf(MA)
    mTriples <- data.frame(mTriples)

  })


  output$segmentDetails <- renderGvis({
    dataSegments = segmentTriples()
    bublesData  = bublesTriples()
    moviesData = mTriples()


    Line2axis <- gvisLineChart(dataSegments, "Year", c("Movies","Actors"),
                               options = list(
                                 series = "[{targetAxisIndex: 0},
                                 {targetAxisIndex:1}]",
                                 vAxes = "[{title:'Movies'}, {title:'Actors'}]",
                                 title = "Movies & Actors by Year",
                                 width = '550', height = 360
                               ))


    Bubbles <- gvisBubbleChart(bublesData, idvar = "Year",
                               xvar = "Directors", yvar = "Actors",
                               colorvar = "", sizevar = "Movies",
                               options = list(
                                 title = "(Movies = Size of the Bubbles)",
                                 vAxis = "{title: 'Actors'}",
                                 hAxis = "{title: 'Directors'}",
                                 width = '550', height = 360,
                                 legend = 'none'))
    #
    # #Merge first two charts horizontally
    D12 <- gvisMerge(Line2axis,Bubbles, horizontal = TRUE)


    chartMovie <- gvisColumnChart(moviesData, xvar = "Year", yvar = "Movies",
                                 options  =  list(title = "Movies Trend by Released Year",
                                                vAxis = "{title: 'Movies'}",
                                                hAxis = "{title: ''}",
                                                width = '100%', height = 450,
                                                legend = 'none',bar = "{groupWidth:'100%'}"))
    #D12 <- gvisMerge(chartMovie,Bubbles, horizontal=TRUE)

    Line2axis_2 <- gvisLineChart(dataSegments, "Year", c("Actors","Movies"),
                                 options = list(
                                   series = "[{targetAxisIndex: 0},
                                   {targetAxisIndex:1}]",
                                   vAxes = "[{title:'Actors'}, {title:'Movies'}]",
                                   title = "Movies & Actors by Year",
                                   width = 500, height = 400
                                 ))


    #Merge the other two charts horizontally
    D34 <- gvisMerge(chartMovie,Line2axis, horizontal = TRUE)
    #Merge two pairs of charts vertically
    D1234 <- gvisMerge(D12,chartMovie, horizontal = FALSE)

  })
 #  output$downloadCsv <- downloadHandler(
 #    filename = "movies.csv",
 #    content = function(file) {
 #      write.csv(movieTriples, file)
 #    },
 #    contentType = "text/csv"
 #  )
 #
 #  output$rawtable <- renderPrint({
 #    orig <- options(width = 1000)
 #    validate(need(input$maxrows, 'Please provide a value for rows at least 1'))
 #    print(tail(movieTriples, input$maxrows), row.names = FALSE)
 #    options(orig)
 #  })
 #
 #  output$TopN <- renderText({
 #    paste('Top ', input$TopN, ' Movies by Most Actors')
 #  })
 #
 #  # output$transfStatus <- renderText({
 #  #   paste("Transformation in progress....")
 #  # })
 #  #
 #  output$latestN <- renderText({
 #    paste('Latest ', input$latestN, ' Movies Released/to be Released')
 #  })
 #
 #  stat <- paste(PREFIXES, Q7)
 #  sDetails <- SPARQL::SPARQL(url = endpoint, query = stat)$results
 #
 #
 #  data_model <- function()
 #  {
 #    #res <- MoviesStats()
 #    sDetails <- data.frame(sDetails$Year,sDetails$movies)
 #    names(sDetails) <- c("Year", "Movies")
 #    return(sDetails)
 #  }
 #
 #
 #  output$genericPlot <- renderPlot({
 #    df <- data_model()
 #    ggplot(df) + geom_line(aes(y = df$Movies, x = df$Year), data = df)+
 #    labs(title = "Actual Released Movies", x = "Year", y = "Movies",
 #           subtitle = "The most active time to release movies is Between 1915 - 1935 and 1990 - 2017")
 #  })
 #
 # output$summary <- renderPrint({
 #    m <- data_model()
 #    summary(m)
 #
 #  })
 # # #################Prediction############

 getMovies <- function(){
   stat <- paste(PREFIXES, predQ)
   sDetails <- SPARQL::SPARQL(url = endpoint, query = stat)$results
   return(sDetails)
 }
 output$monthlyActuals <- renderPlot({

   sDetails <- getMovies()
   moviesbyMonth <- sDetails %>%
     group_by(month, year) %>%
     summarise(movies = sum(movies),.groups = 'drop')

   moviesbyMonth$date <- as.Date(paste0("01","/", moviesbyMonth$month,"/",moviesbyMonth$year),format = "%d/%m/%Y")


   ggplot(moviesbyMonth) + geom_line(aes(y = moviesbyMonth$movies, x = moviesbyMonth$date), data = moviesbyMonth) +
     labs(title = "Actual Released Movies", x = "Date", y = "Movies",
          subtitle = "The most active time to release movies is Between 1915 - 1935 and 1990 - 2017")
 })
 #
 output$monthlySummary <- renderPrint({
   sDetails <- getMovies()
   moviesbyMonth <- sDetails %>%
     group_by(month, year) %>%
     summarise(movies = sum(movies),.groups = 'drop')
   moviesbyMonth$date <- as.Date(paste0("01","/", moviesbyMonth$month,"/",moviesbyMonth$year),format = "%d/%m/%Y")
   Movies <- moviesbyMonth[,c("date", "movies")]
   names(Movies) <- c("Date", "Movies")
   summary(Movies)
 })
 #
 #m_aa <- auto.arima(ts_movies,D = 1)
 #
 output$MonthlyARIMA <- renderPlot({
   #finalForecast <- forecast(auto.arima(ts(moviesbyMonth$movies,start = 1910, end = 2020, frequency = 4),D = 1),h = 36)
   #finalForecast <- forecast(m_aa,h = 12)
   #stat <- paste(PREFIXES, predQ)
   sDetails<- getMovies()
   #sDetails <- SPARQL::SPARQL(url = endpoint, query = stat)$results
   moviesbyMonth <- sDetails %>%
     group_by(month, year) %>%
     summarise(movies = sum(movies),.groups = 'drop')

   ts_movies <- ts(moviesbyMonth$movies,start = 1910, end = 2025, frequency = 12)
   m_aa <- auto.arima(ts_movies,D = 1)
   finalForecast <- forecast(m_aa,h = 12)

   g <- plot(finalForecast, font.main = 3, main = "Forecast 4 Years into the Future Apply Seasonal Model -  ARIMA ",  ylab = "Movies", xlab = "Date")
   g + theme_classic()

 })
 # m_ets <- ets(ts_movies)
 # output$expSmoothState <- renderPlot({
 #   forecastMovies <- forecast(m_ets, h = 36) # forecast 24 months into the future
 #   #finalForecast <- forecast(auto.arima(ts_movies,D = 1),h = 36)
 #   g <- plot(forecastMovies, font.main = 3, main = "Forecast 4 Year into the Future Apply Exponential State Smoothing - ETS",  ylab = "Movies", xlab = "Date")
 #   g + theme_classic()
 #
 # })
 #
 #
 # m_tbats <- tbats(ts_movies, seasonal.periods = 365)
 # output$tbats <- renderPlot({
 #   forecastMovies <- forecast(m_tbats, h = 36) # forecast 24 months into the future
 #
 #   g <- plot(forecastMovies, font.main = 3, main = "Forecast 4 into the Future Apply Exponential State Smoothing - TBAT",  ylab = "Movies", xlab = "Date")
 #   g + theme_classic()
 # })
 # output$modelsCompare <- renderPlot({
 #
 #   barplot(c(ETS = m_ets$aic, ARIMA = m_aa$aic, TBATS = m_tbats$AIC),
 #           col = "light blue",
 #           ylab = "AIC")
 # })
 
 
 output$FullRawData <- DT::renderDataTable({
 
   #add html link tags
   MoviesData$Movie <- paste0("<a href='",MoviesData$movieLink,"'>",MoviesData$Movie,"</a>")
   #render datatable
   DT::datatable(select(MoviesData, -movieLink),escape=FALSE)
 
 })

})
