library(dplyr)
library(ffanalytics)
library(tidyr)

auctionPrep <- function() {
  # This helper function runs the projections functions from ffanalytics, joins AAV, and saves the resulting projections file
  # Uses ESPN AAV and average point weighting

  # Scrape data from all sites possible
  if(!exists("my_scrape")) {
    my_scrape <- scrape_data(
      src = c(
        "CBS",
        "ESPN",
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
  
  # Compose projected points from projected stats
  scoring_settings <- league_scoring()
  myProjections <-
    projections_table(my_scrape, scoring_rules = scoring_settings) %>%
    add_player_info() %>%
    `attr<-`(which = "week", 0) %>% # Needed due to bug
    add_aav(sources = "ESPN") %>% # Add in AAV from ESPN source
    unite(player, first_name, last_name, sep = " ") %>%
    filter(avg_type == "average") %>%   # Use average point weighting
    filter(!(team %in% c("FA","FA*"))) # Remove free agents
  
  # Update data frame including projected auction values
  proj <- myProjections %>% 
    rename(playername = player, auctionValue = aav) %>% 
    replace_na(list(auctionValue = 1, points = 0))
  
  # Save myProjections$projections as RDS: projdata
  saveRDS(proj, file = "projdata.rds")
  saveRDS(myProjections, file = "fullproj.rds")
  
}

# This is included due to an issue with the function in ffanalytics
add_aav <- function (projection_table, sources = c("RTS", "ESPN", "Yahoo", 
                                        "NFL")) 
{
  sources = match.arg(sources, several.ok = TRUE)
  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")
  if (week != 0) {
    warning("AAV data is not available for weekly data", 
            call. = FALSE)
    return(projection_table)
  }
  
  adp_tbl <- get_adp(sources, type = "AAV") %>% select(1, length(.)) %>% 
    rename_at(length(.), ~function(x) return("aav"))
  
  projection_table <- left_join(projection_table, adp_tbl, by = "id")
  
  projection_table %>% `attr<-`(which = "season", season) %>% 
    `attr<-`(which = "week", week) %>% `attr<-`(which = "lg_type", 
                                                lg_type)
}

league_scoring <- function() {
  # Simple function to load in the scoring settings values (default values)
  # See https://ffanalytics.fantasyfootballanalytics.net/articles/scoring_settings.html
  
  scoring_settings <- list(
    pass = list(
      pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
      pass_int = -3, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
      pass_400_yds = 0
    ),
    rush = list(
      all_pos = TRUE,
      rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
      rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
    rec = list(
      all_pos = TRUE,
      rec = 0, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
      rec_150_yds = 0, rec_200_yds = 0
    ),
    misc = list(
      all_pos = TRUE,
      fumbles_lost = -3, fumbles_total = 0,
      sacks = 0, two_pts = 2
    ),
    kick = list(
      xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
      fg_50 = 5.0,  fg_miss = 0.0
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
      dst_blk = 1.5, dst_ret_yds = 0, dst_pts_allowed = 0
    ),
    pts_bracket = list(
      list(threshold = 0, points = 10),
      list(threshold = 6, points = 7),
      list(threshold = 20, points = 4),
      list(threshold = 34, points = 0),
      list(threshold = 99, points = -4)
    )
  )
  
  scoring_settings
}