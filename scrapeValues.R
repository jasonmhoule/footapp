library(dplyr)
library(rvest)

scrapeAuctionValues <- function(league) {
  # This function scrapes the AAV from saved html files
  # It requires that html files have been downloaded to a local folder so they can be read
  # 2018: This is still needed since the different number of teams and keepers affect AAV
  # 2018: This is set up for ESPN only since those are the only leagues I am playing in
  
  outputList <- list()
  if(league == "Keeper") {
    fpath = "./Keeperdraftfiles/"
  } else {
    fpath = "./Taydraftfiles/"
  }
  
  # Read in saved AAV files
  for (pageid in list.files(path = fpath, pattern = "*.html")) {
    first_page <- read_html(paste0(fpath, pageid))
    auctionValue <- first_page %>%
      html_nodes("#playertable_0 .select") %>%
      html_attr("value") %>%
      as.integer()
    player <- first_page %>%
      html_nodes("#playertable_0 .flexpop:nth-child(1)") %>%
      html_text
    outputList[[length(outputList) + 1]] <-
      data.frame(player, auctionValue, stringsAsFactors = FALSE)
  }
  
  outputFrame <- bind_rows(outputList)
  
  # Clean up mismatched names
  outputFrame$player <-
    gsub("(*) (Jr\\.$|Sr\\.$|V$|D/ST$|II)", "\\1", outputFrame$player)
  # outputFrame$player <-
  #   gsub("'", "", outputFrame$player)
  # outputFrame$player <-
  #   gsub("Robert Kelley", "Rob Kelley", outputFrame$player)
  outputFrame$player <-
    gsub("DJ Moore", "D.J. Moore", outputFrame$player)
  
  outputFrame
}

library(ffanalytics)
library(tidyr)

auctionPrep <- function(league) {
  # This helper function runs the projections functions from ffanalytics, joins AAV, and saves the resulting projections file
  # 2018: This is set up solely for ESPN since those are the only leagues I am playing in
  # arg league can be "Keeper" or any other value
  
  # Scrape data from all sites possible
  if(!exists("my_scrape")) {
    my_scrape <- scrape_data(
      src = c(
        "CBS",
        "ESPN",
        # "FantasyData",
        "FantasyPros",
        "FantasySharks",
        "FFToday",
        "FleaFlicker",
        "NumberFire",
        "Yahoo",
        "FantasyFootballNerd",
        "NFL",
        "RTSports",
        "Walterfootball"
      ),
      pos = c("QB", "RB", "WR", "TE", "K", "DST"),
      season = 2018,
      week = 0
    )
  }
  
  # Scrape AAV value based on league
  auctionvals <- scrapeAuctionValues(league)
  
  # Compose projected points from projected stats
  scoring_settings <- league_scoring(league)
  myProjections <-
    projections_table(my_scrape, scoring_rules = scoring_settings) %>%
    add_player_info() %>% 
    unite(player, first_name, last_name, sep = " ") %>% 
    filter(avg_type == "average") %>%   # Use average point weighting
    filter(!(team %in% c("FA","FA*"))) # Remove free agents
  
  # Update data frame including projected auction values
  proj <- myProjections %>% 
    left_join(auctionvals, by = "player") %>% 
    replace_na(list(auctionValue = 1, points = 0)) %>% 
    rename(playername = player)
  
  # Save myProjections$projections as RDS: projdata
  saveRDS(proj, file = "projdata.rds")
  saveRDS(myProjections, file = "fullproj.rds")
  
}

league_scoring <- function(league) {
  # Simple function to load in the scoring settings values based on the league settings
  
  if(league == "Keeper") {
    scoring_settings <- list(
      pass = list(
        pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
        pass_int = -2, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
        pass_400_yds = 5
      ),
      rush = list(
        all_pos = TRUE,
        rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
        rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 5),
      rec = list(
        all_pos = TRUE,
        rec = 0, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 2.5,
        rec_150_yds = 0, rec_200_yds = 2.5
      ),
      misc = list(
        all_pos = TRUE,
        fumbles_lost = -1, fumbles_total = -1,
        sacks = 0, two_pts = 2
      ),
      kick = list(
        xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
        fg_50 = 5.0,  fg_miss = -1
      ),
      ret = list(
        all_pos = TRUE,
        return_tds = 6, return_yds = 0
      ),
      idp = list(
        all_pos = TRUE,
        idp_solo = 1, idp_asst = 0.5, idp_sack = 2, idp_int = 3,  idp_fum_force = 3,
        idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2
      ),
      dst = list(
        dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
        dst_blk = 2, dst_ret_yds = 0, dst_pts_allowed = 0
      ),
      pts_bracket = list(
        list(threshold = 0, points = 5),
        list(threshold = 6, points = 4),
        list(threshold = 13, points = 3),
        list(threshold = 17, points = 1),
        list(threshold = 27, points = 0),
        list(threshold = 34, points = -1),
        list(threshold = 45, points = -3),
        list(threshold = 99, points = -5)
      )
    ) }
  else {
    scoring_settings <- list(
      pass = list(
        pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
        pass_int = -2, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
        pass_400_yds = 0
      ),
      rush = list(
        all_pos = TRUE,
        rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
        rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
      rec = list(
        all_pos = TRUE,
        rec = 0.5, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
        rec_150_yds = 0, rec_200_yds = 0
      ),
      misc = list(
        all_pos = TRUE,
        fumbles_lost = -2, fumbles_total = 0,
        sacks = 0, two_pts = 2
      ),
      kick = list(
        xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
        fg_50 = 5.0,  fg_miss = -1
      ),
      ret = list(
        all_pos = TRUE,
        return_tds = 6, return_yds = 0
      ),
      # idp = list(
      #   all_pos = TRUE,
      #   idp_solo = 1, idp_asst = 0.5, idp_sack = 2, idp_int = 3,  idp_fum_force = 3,
      #   idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2
      # ),
      dst = list(
        dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
        dst_blk = 2, dst_ret_yds = 0, dst_pts_allowed = 0
      ),
      pts_bracket = list(
        list(threshold = 0, points = 5),
        list(threshold = 6, points = 4),
        list(threshold = 13, points = 3),
        list(threshold = 17, points = 1),
        list(threshold = 34, points = -1),
        list(threshold = 45, points = -3),
        list(threshold = 99, points = -5)
      )
    )
  }
  
  scoring_settings
}