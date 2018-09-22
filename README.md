# Fantasy Football Auction Dashboard

This Shiny dashboard offers decision support for competing in a fantasy football auction draft, including helpful display of information and real-time lineup optimization to calculate "bid-up-to" values. Helper functions allow a user to configure and load fantasy football auction draft data using a variety of league parameters.

## Getting Started

This repo comes bundled with an [RStudio project](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects), which will be the easiest way to deploy the app and its data.

To run the Shiny app, you will need the following packages:

```r
install.packages(c("shiny", "DT", "ggplot2", "dplyr", "lpSolve"), dependencies=TRUE)
```

To scrape your own values, you will also need the `rvest` and `tidyr` packages, along with the [ffanalytics package](https://github.com/isaactpetersen/FantasyFootballAnalyticsR/tree/master/R%20Package) from Github, which may be installed with the `devtools` package.

```r
install.packages(c("rvest", "tidyr"), dependencies=TRUE)
install.packages(c("devtools"), dependencies=TRUE, repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))

devtools::install_github(repo = "FantasyFootballAnalytics/ffanalytics", build_vignettes = TRUE)
```

Much more to come on how to set up and use this app.

## Acknowledgments

* This work depends on the great work by [Fantasy Football Analytics](https://fantasyfootballanalytics.net/) and the [ffanalytics package](https://github.com/isaactpetersen/FantasyFootballAnalyticsR/tree/master/R%20Package).