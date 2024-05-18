
 
library(plyr)
library(maps)
library(maptools)
library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(networkD3)
library(visNetwork)
library(shinycssloaders)
library(tidyr)
library(scales)
library(rlist)
library(googleVis)
library(stringi)
library(sqldf)
library(lubridate)
library(data.table)
library(wordcloud2)
library(forecast)
library(readr)


endpoint <- "http://dbpedia.org/sparql"
PREFIXES = "PREFIX dbp: <http://dbpedia.org/property/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"

query <- 'PREFIX dbp: <http://dbpedia.org/property/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT Distinct 
(str(?movie)  as ?Movie)  
(str(?director) as ?Director)  
(YEAR(?movie_date) as ?Year)
(str(?Country) as ?Country) 
(str(?actor) as ?Actor) (?m as ?movieLink)

WHERE{ 
  ?m rdfs:label ?movie .
  FILTER(LANG(?movie) = "en") 
  ?m <http://dbpedia.org/ontology/releaseDate> ?movie_date .
  FILTER(DATATYPE(?movie_date) = xsd:date)
  ?m dbp:country ?Country .
  ?m dbp:starring ?a .
  ?a rdfs:label ?actor .
  FILTER(LANG(?actor) = "en")
  ?m dbp:director ?d .
  ?d rdfs:label ?director .
  FILTER(LANG(?director) = "en")
}
order by desc(?movie_date)'

MoviesData <- SPARQL::SPARQL(url = endpoint, query = paste(PREFIXES, query))$results

preprocessCountry <- function(MoviesData){
  
  #remove duplicate rows
  MoviesData$Country <- (gsub("http://dbpedia.org/resource/Cinema_of_the_|http://dbpedia.org/reCsource/Cinema_of_|http://dbpedia.org/resource/", "", trimws(MoviesData$Country),ignore.case = TRUE))
  MoviesData$Country <- (gsub("_", " ", trimws(MoviesData$Country),ignore.case = TRUE))
  
  MoviesData <- unique(MoviesData)
  MoviesData <- separate_rows(MoviesData, Country, sep = "\\*")
  MoviesData <- separate_rows(MoviesData,Country, sep = "\\n")
  MoviesData <- separate_rows(MoviesData,Country, sep = "&")
  MoviesData <- separate_rows(MoviesData,Country, sep = " and ")
  MoviesData <- separate_rows(MoviesData,Country, sep = ";")
  MoviesData <- separate_rows(MoviesData,Country, sep = ",")
  MoviesData <- separate_rows(MoviesData,Country, sep = "-")
  MoviesData <- separate_rows(MoviesData,Country, sep = "/")
  MoviesData <- unique(MoviesData)
  
  MoviesData$Country <- (gsub("\\*|\t|-|=|)|;|y|(country|German|Television in|NO country|Janpan*|France rotten tomatoes 73%|05|14|22|CAN|Engl|AUnited States of Americatralia|ada|)|Italy  United States of America|<br|Japan.|MEX", "", trimws(MoviesData$Country),ignore.case = TRUE))
  MoviesData$Country <- (gsub("ico|cio|United Kingdon|German|and|Cinema of Hong Kong|West|Sria|ðŸ‡ºðŸ‡¸United States of America|UnitedStates|ðŸ‡²ðŸ‡½|ðŸ‡¦ðŸ‡·Argentina|U.S|Ã‰tats|EEUU#Newfoundl", "", trimws(MoviesData$Country),ignore.case = TRUE))
  MoviesData <- unique(MoviesData)
  #Replace
  MoviesData$Country <- trimws(gsub("U.S.|USA|America|UNited States|The United States|United  States|Ameri|United states|United Sates|= United States| United Sates|United State", "United States of America", MoviesData$Country, ignore.case = TRUE))
  MoviesData <- unique(MoviesData)
  
  #Replace
  MoviesData$Country[MoviesData$Country == "Finl"] <- "Finland"
  MoviesData$Country[MoviesData$Country == "🇲🇽"] <- "Mexico"
  MoviesData$Country[MoviesData$Country == "🇺🇸United States of America"] <- "United States of America"
  MoviesData$Country[MoviesData$Country == "of United States of America"] <- "United States of America"
  MoviesData$Country[MoviesData$Country == "🇦🇷Argentina"] <- "Argentina"
  MoviesData$Country[MoviesData$Country == "Pol"] <- "Poland"
  MoviesData$Country <- gsub("Irel | the Republic of Irelandand", "Ireland", MoviesData$Country)
  MoviesData$Country[MoviesData$Country == "UK"] <- "United Kingdom"
  MoviesData$Country[MoviesData$Country == "Australian"] <- "Australia"
  MoviesData$Country[MoviesData$Country == "Ital"] <- "Italy"
  MoviesData$Country[MoviesData$Country == "United Kingdoms"] <- "United Kingdom"
  MoviesData$Country[MoviesData$Country == "Japan."] <- "Japan"
  MoviesData$Country[MoviesData$Country == "South KoreaJapan"] <- "Japan"
  MoviesData$Country <- gsub("Georgia ()", "Georgia", MoviesData$Country)
  MoviesData$Country <- gsub("Czech Republic", "Czechoslovakia", MoviesData$Country)
  
  #there is a hidden white space in "United kingdom"
  MoviesData$Country <- gsub("United kingdom", "United Kingdom", MoviesData$Country)
  MoviesData$Country <- gsub("United Kingdom", "United Kingdom", MoviesData$Country)
  MoviesData <- unique(MoviesData)
  
  MoviesData <- filter(MoviesData, (MoviesData$Country != "RUnited States of Americaa" & MoviesData$Country != "North United States of America" & MoviesData$Country != " = King doms" & MoviesData$Country != " = Kig doms" & MoviesData$Country != "United Kigdom" & MoviesData$Country != "U.K." & MoviesData$Country != "Lap l" & MoviesData$Country != "filmed in dalkey co dublin ireland."))
  MoviesData <- filter(MoviesData, (MoviesData$Country != "ChinaMalaysia" & MoviesData$Country != "" & MoviesData$Country != "s" & MoviesData$Country != "Rw" & MoviesData$Country != "a" & MoviesData$Country != "United KingdomUnited States of America" & MoviesData$Country != "its city states i.e. the ottom Empire" & MoviesData$Country != "The Vati Empire"))
  MoviesData <- filter(MoviesData, (MoviesData$Country != "Sorrowful" & MoviesData$Country != "RomÃ¢nia" & MoviesData$Country != "International" & MoviesData$Country != "ish" & MoviesData$Country != "AUnited States of Americatralia" & MoviesData$Country != "AUnited States of Americatralian" & MoviesData$Country != "AUnited States of Americatria"))
  MoviesData <- filter(MoviesData, (MoviesData$Country != "Irel" & MoviesData$Country != "RomÃ¢nia" & MoviesData$Country != "ð¦ð·Argentina" & MoviesData$Country != "ð²ð½ico" & MoviesData$Country != "ðºð¸United States of America" & MoviesData$Country != "File:Flag of United States of America.svg" & MoviesData$Country != "United Weird Stalker Film States" & MoviesData$Country != "VarioMoviesData"))
  MoviesData$Country <- (gsub(" and|the Republic of |Republic of|Americai|Americaj|United States of Americak|United States of America.", "", trimws(MoviesData$Country),ignore.case = TRUE))
  MoviesData$Country[MoviesData$Country == "US"] <- "United States of America"
  MoviesData <- filter(MoviesData, (MoviesData$Country != "" & MoviesData$Country != "Georgia(" & MoviesData$Country != ".A." & MoviesData$Country != "R" & MoviesData$Country != "Cinema of Taiwan" & MoviesData$Country != "ðŸ‡ºðŸ‡¸United States of America" & MoviesData$Country != "."))
  MoviesData$Country <- gsub("^(of|Of)","",MoviesData$Country)
  MoviesData$Country[MoviesData$Country == "Hungar"] <- "Hungary"
  MoviesData <- filter(MoviesData, (MoviesData$Country != "Occupied Palestinian Territor" & MoviesData$Country != "filmed in dalke co dublin irel." & MoviesData$Country != "its cit states i.e. the ottom Empire"))
  MoviesData$Country <- trimws(MoviesData$Country)
  MoviesData <- unique(MoviesData)
 
  
  return(MoviesData)
}