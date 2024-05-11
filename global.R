# install <- function(packages){
#   new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
#   if (length(new.packages)) 
#     install.packages(new.packages, dependencies = TRUE)
#   sapply(packages, require, character.only = TRUE)
# }
# 
# # # usage
# required.packages <- c("maps","googleVis","lubridate","sqldf", "maptools","ggplot2", "devtools", "shiny", "shinydashboard","stringi", "stringr",
#                          "tidyr","rlist","ggmap","SPARQL","networkD3","dplyr","shinycssloaders","leaflet","treemap","plotly","gridBase","sp","scales",
#                          "forecast", "rworldmap","googleVis","gdata","wordcloud2","prophet","rlist","lubridate","data.table")
# install(required.packages)

# if(!require(installr)) { 
#   install.packages("installr"); 
#   require(installr)
# } #load / install+load installr
# updateR(F, T, F, F, F, F, T) 
library(plyr)
library(maps)
library(maptools)
#library(shinyMobile)
library(ggmap)
library(shiny)
library(shinydashboard)
library(SPARQL)
library(dplyr)
library(stringr)
#library(networkD3)
library(visNetwork)
library(shinycssloaders)
library(leaflet)
library(treemap)
library(plotly)
library(gridBase)
library(sp)
library(tidyr)
library(scales)
library(rworldmap)
library(rlist)
library(googleVis)
library(stringi)
library(sqldf)
library(lubridate)
library(data.table)
library(gdata)
library(wordcloud2)
#library(prophet)
library(forecast)
# 
# httr::set_config(httr::config(ssl_verifypeer=0L))
# options(rsconnect.http = "rcurl")
# 
# endpoint <- "https://live.dbpedia.org/sparql"
# PREFIXES = "PREFIX dbp: <http://dbpedia.org/property/>
# PREFIX dc: <http://purl.org/dc/terms/>
# PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
# PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
# 
# ctry = '
# SELECT DISTINCT (str(?Country) as ?C) (str(?Country) as ?Country)
# WHERE{
# ?m dc:subject <http://dbpedia.org/resource/Category:American_films> .
# ?m rdfs:label ?movie .
# ?m dbp:country ?Country
# FILTER(LANG(?movie) = "en")
# 
# }'
# 
# year ='SELECT DISTINCT (year(?movie_date) as ?y) (year(?movie_date) as ?Year)
# WHERE{
# ?m dc:subject <http://dbpedia.org/resource/Category:American_films> .
# ?m rdfs:label ?movie .
# ?m <http://dbpedia.org/ontology/releaseDate> ?movie_date .
# FILTER(DATATYPE(?movie_date) = xsd:date)
# }'
# 
# Q1 = '
# SELECT Distinct (str(?movie)  as ?Movie)  (str(?director) as ?Director)  (YEAR(?movie_date) as ?Year) (str(?Country) as ?Country) (str(?actor) as ?Actor) (?m as ?movieLink)
#  WHERE{ 
# 
# ?m dc:subject <http://dbpedia.org/resource/Category:American_films> . 
# ?m rdfs:label ?movie .
# FILTER(LANG(?movie) = "en") 
# ?m <http://dbpedia.org/ontology/releaseDate> ?movie_date .
# FILTER(DATATYPE(?movie_date) = xsd:date)
# ?m dbp:country ?Country .
# ?m dbp:starring ?a .
# ?a rdfs:label ?actor .
# FILTER(LANG(?actor) = "en")
# ?m dbp:director ?d .
# ?d rdfs:label ?director .
# FILTER(LANG(?director) = "en")
# }order by desc(?movie_date) 
# 
# '
# 
# Q2 = 'SELECT (str(?movie) as ?movie) (str(?genres) as ?genres) (count(?movie) as ?movies)
# WHERE{
# ?m dc:subject <http://dbpedia.org/resource/Category:American_films> .
# ?m rdfs:label ?movie .
# FILTER(LANG(?movie) = "en")
# ?m <http://dbpedia.org/ontology/genre> ?g .
# ?g rdfs:label ?genres
# }group by ?genres ?movie'
# 
# Q6 = 'SELECT Distinct str(?movie)  as ?Movie (GROUP_CONCAT(distinct ?director ; separator=",") as ?Director) (CONCAT(STR(MONTH(?movie_date)), 
#                                                                             "/", 
#                                                                             STR(DAY(?movie_date)), 
#                                                                             "/", 
#                                                                             STR(YEAR(?movie_date))) as ?Year)
#  str(?Country) as ?Country (GROUP_CONCAT(distinct ?actor ; separator=",") as ?Actor)
#  WHERE{ 
#   
#   ?m dc:subject <http://dbpedia.org/resource/Category:American_films> . 
#   ?m rdfs:label ?movie .
#   FILTER(LANG(?movie) = "en") 
#   ?m <http://dbpedia.org/ontology/releaseDate> ?movie_date .
#   FILTER(DATATYPE(?movie_date) = xsd:date)
#   ?m dbp:country ?Country .
#   ?m dbp:starring ?a .
#   ?a rdfs:label ?actor .
#   FILTER(LANG(?actor) = "en")
#   ?m dbp:director ?d .
#   ?d rdfs:label ?director .
#   FILTER(LANG(?director) = "en")
# }order by desc(?movie_date) 
# '
# 
# 
# Q8='
# select (sum(?movies) as ?movies) (year(?movie_date) as ?year) (month(?movie_date) as ?month)
# {
#   SELECT (count(?movie) as ?movies) ?movie_date
#   {
#     ?m dc:subject <http://dbpedia.org/resource/Category:American_films> .
#     ?m rdfs:label ?movie .
#     FILTER(LANG(?movie) = "en")
#     ?m dbp:released ?movie_date .
#     FILTER(DATATYPE(?movie_date) = xsd:date)
#     filter (?movie_date > "1910-01-01"^^xsd:date)
#     filter (?movie_date < "2017-12-30"^^xsd:date)
#   }group by ?movie_date order by ?movie_date
# }
# '
# 
# Q7 = '
#  select (sum(?movies) as ?movies) (year(?movie_date) as ?year){
#     SELECT (count(?movie) as ?movies) ?movie_date
#     {
#       ?m dc:subject <http://dbpedia.org/resource/Category:American_films> .
#       ?m rdfs:label ?movie .
#       FILTER(LANG(?movie) = "en")
#       ?m dbp:released ?movie_date .
#       FILTER(DATATYPE(?movie_date) = xsd:date)
#       filter (?movie_date > "1910-01-01"^^xsd:date)
#       filter (?movie_date < "2017-12-30"^^xsd:date)
#     }group by ?movie_date order by ?movie_date
# }group by year(?movie_date)'
# predQ = '
# PREFIX dbp: <http://dbpedia.org/property/>
# PREFIX dc: <http://purl.org/dc/terms/>
# PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
# PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
# select (sum(?movies) as ?movies) (year(?movie_date) as ?year) (month(?movie_date) as ?month)
# {
#   SELECT (count(?movie) as ?movies) ?movie_date
#   {
#   ?m dc:subject <http://dbpedia.org/resource/Category:American_films> .
#   ?m rdfs:label ?movie .
#   FILTER(LANG(?movie) = "en")
#   ?m dbp:released ?movie_date .
#   FILTER(DATATYPE(?movie_date) = xsd:date)
#   filter (?movie_date > "1910-01-01"^^xsd:date)
#   filter (?movie_date < "2017-12-30"^^xsd:date)
#   }group by ?movie_date order by ?movie_date
# } order by ?movie_date
# '
# 
# movieTriples <- NULL;
# listCountries <- NULL;
# listYears <- NULL;
# #CRD = paste(PREFIXES, ctry)
# cTriples <- SPARQL::SPARQL(url = endpoint, query = paste(PREFIXES, ctry))$results
# yTriples <- SPARQL::SPARQL(url = endpoint, query = paste(PREFIXES, year))$results
# print(cTriples)
# #cTriples <- SPARQL::SPARQL(url = endpoint, query = cty)$results
# #yTriples <- SPARQL::SPARQL(url = endpoint, query = yr)$results
# 
# preprocessCountry <- function(Countries){
#   
#   #remove duplicate rows
#   Countries$Country <- (gsub("http://dbpedia.org/resource/Cinema_of_the_|http://dbpedia.org/reCsource/Cinema_of_|http://dbpedia.org/resource/", "", trimws(Countries$Country),ignore.case = TRUE))
#   Countries$Country <- (gsub("_", " ", trimws(Countries$Country),ignore.case = TRUE))
# 
#   Countries <- unique(Countries)
#   Countries <- separate_rows(Countries, Country, sep = "\\*")
#   Countries <- separate_rows(Countries,Country, sep = "\\n")
#   Countries <- separate_rows(Countries,Country, sep = "&")
#   Countries <- separate_rows(Countries,Country, sep = " and ")
#   Countries <- separate_rows(Countries,Country, sep = ";")
#   Countries <- separate_rows(Countries,Country, sep = ",")
#   Countries <- separate_rows(Countries,Country, sep = "-")
#   Countries <- separate_rows(Countries,Country, sep = "/")
#   Countries <- unique(Countries)
# 
#   Countries$Country <- (gsub("\\*|\t|-|=|)|;|y|(country|German|Television in|NO country|Janpan*|France rotten tomatoes 73%|05|14|22|CAN|Engl|AUnited States of Americatralia|ada|)|Italy  United States of America|<br|Japan.|MEX", "", trimws(Countries$Country),ignore.case = TRUE))
#   Countries$Country <- (gsub("ico|cio|United Kingdon|German|and|Cinema of Hong Kong|West|Sria|ðŸ‡ºðŸ‡¸United States of America|UnitedStates|ðŸ‡²ðŸ‡½|ðŸ‡¦ðŸ‡·Argentina|U.S|Ã‰tats|EEUU#Newfoundl", "", trimws(Countries$Country),ignore.case = TRUE))
#   Countries <- unique(Countries)
#   #Replace
#   Countries$Country <- trimws(gsub("U.S.|USA|America|UNited States|The United States|United  States|Ameri|United states|United Sates|= United States| United Sates|United State", "United States of America", Countries$Country, ignore.case = TRUE))
#   Countries <- unique(Countries)
# 
#   #Replace
#   Countries$Country[Countries$Country == "Finl"] <- "Finland"
#   Countries$Country[Countries$Country == "🇲🇽"] <- "Mexico"
#   Countries$Country[Countries$Country == "🇺🇸United States of America"] <- "United States of America"
#   Countries$Country[Countries$Country == "of United States of America"] <- "United States of America"
#   Countries$Country[Countries$Country == "🇦🇷Argentina"] <- "Argentina"
#    Countries$Country[Countries$Country == "Pol"] <- "Poland"
#   Countries$Country <- gsub("Irel | the Republic of Irelandand", "Ireland", Countries$Country)
#   Countries$Country[Countries$Country == "UK"] <- "United Kingdom"
#   Countries$Country[Countries$Country == "Australian"] <- "Australia"
#   Countries$Country[Countries$Country == "Ital"] <- "Italy"
#   Countries$Country[Countries$Country == "United Kingdoms"] <- "United Kingdom"
#   Countries$Country[Countries$Country == "Japan."] <- "Japan"
#   Countries$Country[Countries$Country == "South KoreaJapan"] <- "Japan"
#   Countries$Country <- gsub("Georgia ()", "Georgia", Countries$Country)
#   Countries$Country <- gsub("Czech Republic", "Czechoslovakia", Countries$Country)
#  
#   
# 
#   #there is a hidden white space in "United kingdom"
#   Countries$Country <- gsub("United kingdom", "United Kingdom", Countries$Country)
#   Countries$Country <- gsub("United Kingdom", "United Kingdom", Countries$Country)
#   Countries <- unique(Countries)
# 
#   Countries <- filter(Countries, (Countries$Country != "RUnited States of Americaa" & Countries$Country != "North United States of America" & Countries$Country != " = King doms" & Countries$Country != " = Kig doms" & Countries$Country != "United Kigdom" & Countries$Country != "U.K." & Countries$Country != "Lap l" & Countries$Country != "filmed in dalkey co dublin ireland."))
#   Countries <- filter(Countries, (Countries$Country != "ChinaMalaysia" & Countries$Country != "" & Countries$Country != "s" & Countries$Country != "Rw" & Countries$Country != "a" & Countries$Country != "United KingdomUnited States of America" & Countries$Country != "its city states i.e. the ottom Empire" & Countries$Country != "The Vati Empire"))
#   Countries <- filter(Countries, (Countries$Country != "Sorrowful" & Countries$Country != "RomÃ¢nia" & Countries$Country != "International" & Countries$Country != "ish" & Countries$Country != "AUnited States of Americatralia" & Countries$Country != "AUnited States of Americatralian" & Countries$Country != "AUnited States of Americatria"))
#   Countries <- filter(Countries, (Countries$Country != "Irel" & Countries$Country != "RomÃ¢nia" & Countries$Country != "ð¦ð·Argentina" & Countries$Country != "ð²ð½ico" & Countries$Country != "ðºð¸United States of America" & Countries$Country != "File:Flag of United States of America.svg" & Countries$Country != "United Weird Stalker Film States" & Countries$Country != "Variocountries"))
#   Countries$Country <- (gsub(" and|the Republic of |Republic of|Americai|Americaj|United States of Americak|United States of America.", "", trimws(Countries$Country),ignore.case = TRUE))
#   Countries$Country[Countries$Country == "US"] <- "United States of America"
#   Countries <- filter(Countries, (Countries$Country != "" & Countries$Country != "Georgia(" & Countries$Country != ".A." & Countries$Country != "R" & Countries$Country != "Cinema of Taiwan" & Countries$Country != "ðŸ‡ºðŸ‡¸United States of America" & Countries$Country != "."))
#   Countries$Country <- gsub("^(of|Of)","",Countries$Country)
#   Countries$Country[Countries$Country == "Hungar"] <- "Hungary"
#   Countries <- filter(Countries, (Countries$Country != "Occupied Palestinian Territor" & Countries$Country != "filmed in dalke co dublin irel." & Countries$Country != "its cit states i.e. the ottom Empire"))
#   Countries$Country <- trimws(Countries$Country)
#   Countries <- unique(Countries)
# 
#   return(Countries)
# }

# getCountries <- function(mt){
#   lc <- unique(mt$Country)
#   lc <- data.frame(as.character(lc))
#   colnames(lc) <- c("Country")
#   lc <- unique(lc)
#   lc <- lc[order(lc$Country),, drop = FALSE]
#   return(lc)
# }
# 
# getYears <- function(mt){
#   ly <- mt$Year
#   ly <- data.frame(ly)
#   colnames(ly) <- c("Year")
#   ly <- unique(ly)
#   ly <- ly[rev(order(ly$Year)),,drop = FALSE]
# }


# 
# mTriples<- cTriples %>% preprocessCountry
# 
# listCountries <- mTriples %>% getCountries
# 
# listYears <- yTriples %>% getYears
#                             

