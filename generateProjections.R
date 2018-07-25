# This is a high level script for scraping projections and augmenting these with AAV based on league.

# First, use these links to save webpages of auction
# prediction values to be scraped. Save enough pages to
# cover the relevant players (generally down to $1)

# ESPN: "ESPNdraftfiles"
# http://games.espn.com/ffl/tools/auctiondraftlist?leagueId=736437&teamId=9

# Yahoo: "Yahoodraftfiles"
# https://football.fantasysports.yahoo.com/f1/310181/6/prerank_auction_costs?filter=ALL&sort=TAC&count=0

# Then source the functions to use
source("scrapeAuctionValues.R")

# Next, run this call with the argument "ESPN" or "Yahoo"
auctionPrep("ESPN")

# Verify the data has been producted by loading it
g <- readRDS("projdata.rds")
