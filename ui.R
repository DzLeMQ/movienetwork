

#https://github.com/rstudio/shiny/issues/828
#Add logo and contact icon in the banner area
dbHeader <- dashboardHeader(title = "Movies Network",
                            tags$li(a(href = 'http://',
                                      icon("envelope"),
                                      title = "Contact Us"),
                                    class = "dropdown"))


# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    skin = "green",
    dbHeader,
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction", tabName = "intro", icon = icon("sticky-note")),
        menuItem("Relationship Discovery", tabName = "explore", icon = icon("binoculars")),
        menuItem("Released Movies Summary", icon = icon("users"), tabName="analysis"),
        menuItem("Ask Movy", icon = icon("question-circle"), tabName = "QA"),
        menuItem("Triples in Tabular Data Format", icon = icon("table"), tabName = "rawData")

      )
    ),
    dashboardBody(
      tabItems(
        tabItem("explore",
                column(9,
                       h3("Relationship Discovery",style = "text-align:center;color:#cc0000;font-size:130%"),
                       p("Click and hold down the graph then drag to move it or drag a node to partially move it", style = "text-align:center;color:purple;font-size:70%"),
                       p("Double-click on a movie node to view movie's details.", style = "text-align:center;color:blue;font-size:70%") ,
                       withSpinner(visNetworkOutput("forceGraph", width = "100%", height = 800))    
                ),
                column(3, wellPanel(
                  sliderInput("n", "Distance:",
                              min = 20, max = 200, value = 120, step = 20),
                 
                  selectInput(inputId = "countryChoice", label = strong("Movie Released Country"), multiple = TRUE,
                              choices = NULL,
                              selected = NULL), 
                  selectInput(inputId = "yearChoice", label = strong("Movie Released Year"), multiple = TRUE,
                              choices = NULL,
                              selected = NULL),
                  selectInput(inputId = "actorChoice", label = strong("Actor"), multiple = TRUE,
                              choices = NULL,
                              selected = NULL),
                  selectInput(inputId = "directorChoice", label = strong("Director"), multiple = TRUE,
                              choices = NULL,
                              selected = NULL),

                  p("Use Backspace to remove items in dropdown list boxes Movie Released Country, Movie Released Year, Actor, etc."),
                  p("Muliple items allowed in a dropdown list box")
                  
                )
                )
        ),
        tabItem("rawData",
          fluidRow(
                    box(width = 12, title=h1("Movies Raw Data", style="text-align:center;color:purple;font-size:100%"), status = "info",  height = "870",
                        column(width = 12, style = "margin-top:10px", DT::dataTableOutput("FullRawData")))
                      
                    ),
                    tags$head(tags$style(".row1{height:280px;background-color: white;align:center}")),
                    tags$style(type = 'text/css', "#box_align { width:100%; margin-top: 150px;}")
                  
        ),
        tabItem("QA",
                h2("Comming soon...")
        ),
        tabItem("analysis", help="Released Movie Summary",
                fluidRow(
                  withSpinner(valueBoxOutput("Movies")),
                  withSpinner(valueBoxOutput("Directors")),
                  withSpinner(valueBoxOutput("Actors")),
                  withSpinner(valueBoxOutput("Countries")),
                  tags$style("#Movies, #Directors, #Actors, #Countries {width:25%;}")
                ),
           
                fluidRow(class = "row1",
                         column(width = 1),      
                         column(width = 10, align = "center", sliderInput("latestN", "", min = 5, max =nrow(MoviesData) , value = 10, step = 5)),
                         column(width = 1, align = "left",actionButton("reloadLatestMovies", "Refresh Data",icon = icon("sync")))
                         
                ),   
                fluidRow(
                  box(width="850%",
                      status="info",
                      DT::dataTableOutput("analysisDetails"),
                      #title = h3("N Latest Movies Released across the Globe",style="text-align:center;color:#cc0000;font-size:130%")
                      title = h3(textOutput("latestNTitle"), style = "text-align:center;color:#cc0000;font-size:130%")
                  )
                ),
                tags$head(tags$style("
                    .row1{height:90px;background-color: white;align:center}")
                ),
            
                tags$style(type='text/css', "#reloadLatestMovies {width:100%; margin-top: 25px;}"),
                tags$style(type='text/css', "#latestNTitle { max-width:100%; margin-top: 25px;}"),
                tags$style(type='text/css', "#latestN {width:2000px; background-color: #cc0000; margin-top: 25px;}")
        ),
        tabItem("intro",

                fluidRow(
                  h2("Introduction"),
                  p(),
                  span("This visualisation showcases the use of ", a("Open Linked Data ", href= "http://dbpedia.org"),". On this open linked data page,
                  one can find more infornation about how structured data was extracted from Wikipedia to construct the open knowledge graph
                  ",a("DBPEDIA", href= "http://dbpedia.org"), ". The open linked data, represented by triples, is part of open knowledge graph. 
                  We use SQL-like query or semantic query (SPARQL) to access the movies triples.
                  This visualisaiton currently points to testbed open knowledge graph DBpedia. Available triples can be very preliminary."),
                  p(),
                  p("The spining Spinner below indicates that the data is currently transformed on the first run. Please wait until you 
                    see a message of ' Transformation Completed ' appear and the spinner is disappeared. "), style ="margin-left: 10px;"),
                fluidRow( align="center", withSpinner(htmlOutput("transfStatus")), style = "text-align:center;color:#cc0000;font-size:130%"),
                p(),
                br(),
                p("This project is an on-going effort.  Please stop by sometime for new updates."),
                br(),
                p("Copyright @ 2017 Dung (Dzung) Le"),
                p(),
                p("Last updated: July 2023"),
                br("")

        )
       
      )
    )
  )
)