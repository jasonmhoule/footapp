library(dplyr)
library(rvest)

scrapeAuctionValues <- function(src) {
  if(!(src %in% c("ESPN","Yahoo"))) {
    warning("src must be 'ESPN' or 'Yahoo'; nothing prepared")
    return(NULL)
  }
  
  outputList <- list()
  
  if(src == "ESPN") {
    
    # ESPN source
    for(pageid in list.files(path = "./ESPNdraftfiles/", pattern = "*.html")) {
      first_page <- read_html(paste0("./ESPNdraftfiles/",pageid))
      auctionValue <- first_page %>%
        html_nodes("#playertable_0 .select") %>% 
        html_attr("value") %>% 
        as.integer()
      player <- first_page %>%
        html_nodes("#playertable_0 .flexpop:nth-child(1)") %>% 
        html_text
      outputList[[length(outputList) + 1]] <- data.frame(player,auctionValue, stringsAsFactors = FALSE)
    }
    
  } else if(src == "Yahoo") {
    
    # Yahoo source
    for(pageid in list.files(path = "./Yahoodraftfiles/", pattern = "*.html")) {
      first_page <- read_html(paste0("./Yahoodraftfiles/",pageid))
      auctionValue <- first_page %>%
        html_nodes(".Input-xxs") %>% 
        html_attr("value")
      player <- first_page %>%
        html_nodes(".F-link") %>% 
        html_text
      outputList[[length(outputList) + 1]] <- data.frame(player,auctionValue, stringsAsFactors = FALSE)
    }
  }
  
  outputFrame <- bind_rows(outputList)
  
  # Clean up mismatched names
  if(src == "ESPN") {
    
    # ESPN source
    outputFrame$player <- 
      gsub("(*) (Jr\\.$|Sr\\.$|V$|D/ST$)","\\1",outputFrame$player)
    outputFrame$player <- 
      gsub("'","",outputFrame$player)
    outputFrame$player <- 
      gsub("Robert Kelley","Rob Kelley",outputFrame$player)
    
  } else if(src == "Yahoo") {
    # Yahoo source
    outputFrame$player <- 
      gsub("(*) (Jr\\.$|Sr\\.$|V$|D/ST$)","\\1",outputFrame$player)
    outputFrame$player <- 
      gsub("'","",outputFrame$player)
    
  }
  
  outputFrame
}

library(ffanalytics)
library(tidyr)

auctionPrep <- function(src) {
  if(!(src %in% c("ESPN","Yahoo"))) {
    warning("src must be 'ESPN' or 'Yahoo'; nothing prepared")
    return(NULL)
  }
  
  if(src == "ESPN") {
    
    # ESPN Settings
    load("config/ESPNsettings.Rdata")
    myProjections <- getProjections(scrapeData=runScrape(week = 0, season = 2017, analysts = c(-1, 3, 4, 5, 6, 7, 9, 18, 20, 28), positions = c("QB", "RB", "WR", "TE", "K", "DST")), avgMethod = "average", leagueScoring = userScoring, vorBaseline, vorType, teams = 14, format = "standard", mflMocks = -1, mflLeagues = -1, adpSources = c("ESPN"), writeFile = FALSE)
    auctionvals <- scrapeAuctionValues("ESPN")
    
  } else if(src == "Yahoo") {
    
    # Yahoo settings
    load("config/Yahoosettings.Rdata")
    myProjections <- getProjections(scrapeData=runScrape(week = 0, season = 2017, analysts = c(-1, 3, 4, 5, 6, 7, 9, 17, 18, 19, 20, 28), positions = c("QB", "RB", "WR", "TE", "K", "DST")), avgMethod = "average", leagueScoring = userScoring, vorBaseline, vorType, teams = 8, format = "ppr", mflMocks = -1, mflLeagues = -1, adpSources = c("CBS", "ESPN", "FFC", "MFL", "NFL", "Yahoo"), writeFile = FALSE)
    auctionvals <- scrapeAuctionValues("Yahoo")
    
  }
  
  # Update data frame including projected auction values
  proj <- myProjections$projections %>% 
    left_join(auctionvals, by = "player") %>% 
    replace_na(list(auctionValue = 1)) %>% 
    rename(playername = player)
  
  # Save myProjections$projections as RDS: projdata
  saveRDS(proj, file = "projdata.rds")
  saveRDS(myProjections, file = "fullproj.rds")
  
}