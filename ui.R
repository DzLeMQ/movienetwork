# #
# # This is the user-interface definition of a Shiny web application. You can
# # run the application by clicking 'Run App' above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
# 
# library(shiny)
# 
# # Define UI for application that draws a histogram
# fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# )

#https://github.com/rstudio/shiny/issues/828
#Add logo and contact icon in the banner area
dbHeader <- dashboardHeader(title = "Movies Network",
                            tags$li(a(href = 'http://',
                                      icon("envelope"),
                                      title = "Contact Us"),
                                    class = "dropdown"))
# tags$li(a(href = 'http://www.semanticsoftware.com',
#           img(src = 'http://semanticsoftware.com.au/images/logo-black.png',height='30',width='150',
#               title = "Company Home", height = "30px"),
#           style = "padding-top:10px; padding-bottom:10px;"),
#         class = "dropdown"))

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    skin = "green",
    dbHeader,
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction", tabName = "intro", icon = icon("sticky-note")),
        menuItem("Relationship Discovery", tabName = "explore", icon = icon("binoculars")),
        # menuItem("Released Movies Summary", icon = icon("users"), tabName="analysis"),
        # #menuItem("Geospatial Analysis", icon = icon("map"), tabName="map"),
        # menuItem("Actors/Directors Frequency", icon=icon("spinner"), tabName = "Cloud"),
        # menuItem("Analysis by Segments", icon = icon("line-chart"), tabName="TA"),
        # menuItem("Movies Projection", icon  =  icon("openid"), tabName  =  "prediction"),
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
                  #If 'All' selected for Year, please ensure to remove all years preceed it and wait patiently until the graph process is done before making the next selection, this may take a while."),
                  #p("If not all data is focussed then click one of the focussed data points and then move away from it to get all data in focus.")
                  
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
        # # tabItem("nactors",
        # #     fluidRow(class="row2",
        # #         status = "info", align="center",
        # #         title = h3(textOutput("TopN"), style = "text-align:center;color:#cc0000;font-size:130%"),
        # #         #h3("Movies Analysis by Genres",style="text-align:center;color:#cc0000;font-size:130%"),
        # #         p("Left mouse click to drill down. Right mouse click to roll up.", style="text-align:center;color:purple;font-size:70%"),
        # #         br(),
        # #         br()
        # #         #withSpinner(htmlOutput("genres"))
        # #      ),
        # #      p("Upgrading is in progress. Visit again soon.", style="text-align:center;color:purple;font-size:170%"),
        # #         tags$head(tags$style("
        # #                            .row1{height:90px;background-color: white;align:center}"
        # #     ))
        # # ),
        # tabItem("Cloud",
        #         fluidRow(
        #           box(width = 12, collapsible = TRUE,
        #               column(width = 4,height = 90, radioButtons("freqGroup", inline = T, label = h5(label = "Select a State"),
        #                                                          choices = list("Directors" = 1, "Actors" = 2),
        #                                                          selected = 1),inline = T, align = "right"),
        #               column(width = 4, align = "left", sliderInput("cloudTopFreq", "Select a Top N",  min = 10, max = 100, value = 50, step = 5))
        #               
        #               
        #           )
        #         ),
        #         #fluidRow(box(width = 12, collapsible = TRUE,title = h3(textOutput("freqGroup"), style = "text-align:center;color:#ffffff; font-weight: bold; font-size:100%"), background = "yellow" )),
        #         fluidRow(
        #           box(width = 12, title=h3("Actors/Directors by Frequency", style="text-align:center;color:purple;font-size:100%"), status = "info",  height = "870",
        #               column(width = 12, style = "margin-top:10px", withSpinner(wordcloud2Output("info_cloud",height ="680px")))
        #               #column(width = 6, withSpinner(dataTableOutput("word_cloud_tab")))       
        #           ),
        #           tags$head(tags$style(".row1{height:280px;background-color: white;align:center}")),
        #           tags$style(type = 'text/css', "#box_align { width:100%; margin-top: 150px;}")
        #         )
        # ),
        # tabItem("analysis", help="Released Movie Summary",
        #         fluidRow(
        #           withSpinner(valueBoxOutput("Movies")),
        #           withSpinner(valueBoxOutput("Directors")),
        #           withSpinner(valueBoxOutput("Actors")),
        #           withSpinner(valueBoxOutput("Countries")),
        #           tags$style("#Movies, #Directors, #Actors, #Countries {width:25%;}")
        #         ),
        #         #valueBoxOutput("Countries")),
        #         fluidRow(class = "row1",
        #                  column(width = 4),
        #                  column(width = 4, align = "center", sliderInput("latestN", "", label = strong("Show Latest N Movies Presented in Movies Knowledge Base:"), min = 5, max = 20, value = 10, step = 5)),
        #                  column(width = 4, align = "right",actionButton("reloadLatestMovies", "Refresh Data",icon = icon("sync")))
        #         ),
        #         fluidRow(
        #           box(width="850%",
        #               status="info",
        #               #withSpinner(tableOutput("analysisDetails")),
        #               tableOutput("analysisDetails"),
        #               #title = h3("N Latest Movies Released across the Globe",style="text-align:center;color:#cc0000;font-size:130%")
        #               title = h3(textOutput("latestN"), style = "text-align:center;color:#cc0000;font-size:130%")
        #           )
        #         ),
        #         tags$head(tags$style("
        #             .row1{height:90px;background-color: white;align:center}")
        #         ),
        # 
        #         tags$style(type='text/css', "#reloadLatestMovies { width:30%; margin-top: 25px;}"),
        #         tags$style(type='text/css', "#latestN { width:100%; margin-top: 25px;}")
        # ),
        # tabItem("map",
        #         
        #         h3("Released Location of Movies",style="text-align:center;color:#cc0000;font-size:130%"),
        #         p("Click on a country marker for more information.", style="text-align:center;color:purple;font-size:100%"),
        #         p("If you loose the map focus use -/+ on top left corner of the map.", style="text-align:center;color:red;font-size:80%"),
        #         #fluidRow(box(width = "12",  align = "center", actionButton("reloadMap", "Refresh Data",icon = icon("sync")))),
        #         #withSpinner(leafletOutput("map", width="100%",height=850)),
        #         #p()
        #         
        #         fluidRow(column(6, align="center", offset = 3,
        #                         actionButton("reloadMap", "Refresh Data",icon = icon("sync")),
        #                         tags$style(type='text/css', "#reloadMap { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
        #         )),
        #         # withSpinner(leafletOutput("map", width="100%",height=850))
        #         
        # ),
        # tabItem("QA",
        #         h2("Comming soon...")
        # ),
        # tabItem("TA", help="Analysis by Segments",
        #         fluidRow(
        #           box(width="100%", align='center',
        #               h1("Analysis by Segments",style="text-align:center;color:#cc0000;font-size:130%"),
        #               status="info",
        #               withSpinner(htmlOutput("segmentDetails"))
        #           )
        #         )
        # ),
        tabItem("intro",

                fluidRow(
                  h2("Introduction"),
                  p(),
                  span("This visualisation uses ", a("Open Linked Movie Data ", href= "http://dbpedia.org"),".
                  The movie open linked data, known as triples, is part of open knowledge graph ",a("DBpedia", href= "http://dbpedia.org"), ". 
                  We use semantic queries (SPARQL) to access the movies triples.
                  This visualisaiton currently points to testbed knowledge graph as the live knowledge graph has gone under maintenance.
                  You may find the Movies are incompleted in someways."),
                  p(),
                  p("The Spinner below indicates that the data in the process of being transformed, please wait until you 
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

                # fluidRow(status = "info", align="center",
                #            withSpinner(htmlOutput("transfStatus")), style = "text-align:center;color:#cc0000;font-size:130%")
                #
        )
        # tabItem("prediction",
        #         tabsetPanel(type = "tabs",
        #                     tabPanel("Actual Released Movies & ARIMA Forecast", (fluidRow(width = 12,
        #                                                                                   box(width = 9, status = "success", solidHeader = FALSE, withSpinner(plotOutput("monthlyActuals"))),
        #                                                                                   box(width = 3, p("Summary of Variables",style  =  "text-align:left;font-size:120%"), status = "success",
        #                                                                                       solidHeader = FALSE, withSpinner(verbatimTextOutput("monthlySummary"))),
        #                                                                                   box(width = 12, title = "", status = "primary", withSpinner(plotOutput("MonthlyARIMA")))
        #                     )) #close fluidRow
        #                     )
        #                     # tabPanel("Exponential Smooth State",
        #                     #          (fluidRow(width = 12,box(width = 12, status = "success", solidHeader = FALSE, withSpinner(plotOutput("expSmoothState"))),
        #                     #                    box(width = 12, title = "", status = "primary", plotOutput("tbats"))
        #                     #                    # box(width = 3, p("Summary of Variables",style  =  "text-align:left;font-size:120%"), status = "success",
        #                     #                    #     solidHeader = FALSE, verbatimTextOutput("summary")),
        #                     #                    # box(width = 12, title = "", status = "primary", plotOutput("forecasProphettPlot"))
        #                     #          ))
        #                     # ),
        #                     # tabPanel("Compared Models",
        #                     #          (fluidRow(width = 12,
        #                     #                    box(width = 8, status = "success", solidHeader = FALSE, withSpinner(plotOutput("modelsCompare")))
        #                     # 
        #                     #          )),
        #                     #          fluidRow(width = 12,
        #                     #                   box(width = 8, status = "success", solidHeader = FALSE,
        #                     #                       span("We use AIC method to determine how well these time series models fit the data.
        #                     #                            The model with the smallest AIC is the best fitting mode. As the results, We see that the ARIMA model performs the best.")
        #                     # 
        #                     #                       ))
        #                     # )
        #                     
        #         )
        # )
        
      )
    )
  )
)