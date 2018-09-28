# This is a high level script for scraping projections and augmenting these with AAV based on league.

# Source the function to use and complete auction prep
source("scrapeValues.R")
auctionPrep()

# Verify the data has been produced by loading it
g <- readRDS("projdata.rds")
