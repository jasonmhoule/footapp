library(shiny)
teams <- read.csv("config/teamnames.csv", header=FALSE, stringsAsFactors = FALSE)[,1]
shinyUI(fluidPage(
  tags$head(tags$style(HTML(".selectize-dropdown {
                  z-index: 1001;
                  }"))),
  
  # Header command center
  fluidRow(column(7,
                  wellPanel(
                    fluidRow(column(
                      4,
                      selectInput("onPoint",
                                  "Player",
                                  "Select")
                    ),
                    column(
                      3,
                      selectInput("draftTeam",
                                  "Team",
                                  "Select")
                    ),
                    column(
                      2,
                      numericInput("draftCost",
                                  "Cost",
                                  1)
                    ),
                    column(
                      2,
                      fluidRow(
                        checkboxInput("keeper","Keeper?")
                      ),
                      fluidRow(
                        actionButton('addOnPoint',
                                     'Draft Player'))
                    ),
                    column(
                      1,
                      h1(textOutput("bidUpToValue"))
                    )
                    )
                  )
           ),
           column(4,
                  fluidRow(
                    column(5,selectInput("myTeam","My Team",teams)),
                    column(3,numericInput("totalBudget","Total $$",200)),
                    column(4,numericInput("reserveBudget","Reserve $$",15))
                  ),
                  fluidRow(
                    column(2,numericInput("numQB","QBs",1)),
                    column(2,numericInput("numRB","RBs",2)),
                    column(2,numericInput("numWR","WRs",2)),
                    column(2,numericInput("numTE","TEs",1)),
                    column(4,numericInput("numFLEX","FLEXes",1))
                  )
           ),
           column(1,
                  fluidRow(
                    checkboxGroupInput("checks","FLEXable",
                                       c("QB" = "QB",
                                         "RB" = "RB",
                                         "WR" = "WR",
                                         "TE" = "TE"),
                                       selected = c("RB","WR","TE"),
                                       inline = FALSE)
                  )
           )
  ),

  
  sidebarLayout(# Sidebar command center
    sidebarPanel(
      width = 2,
      actionButton('saveDraftBoard',
                   'Save Draft'),
      actionButton('loadDraftBoard',
                   'Load Draft Cache'),
      
      h4("Money Available: ", strong(textOutput("spendable"))),
      h4("My Roster"),
      tableOutput("myRoster"),
      h4("Dream Team"),
      tableOutput("dreamTeam"),
      h4("Some controls")
    ),
    
    # Main nav bar
    mainPanel(
      width = 10,
      navbarPage(
        "Title",
        
        tabPanel("Draft Board",
                 tableOutput('draftBoard')),
        tabPanel(
          "QB/DST/K",
          fluidRow(
            column(4, DT::dataTableOutput('playerBoardQB')),
            column(4, DT::dataTableOutput('playerBoardDST')),
            column(4, DT::dataTableOutput('playerBoardK'))
          )
        ),
        tabPanel(
          "FLEX",
          fluidRow(
            column(4, DT::dataTableOutput('playerBoardRB')),
            column(4, DT::dataTableOutput('playerBoardWR')),
            column(4, DT::dataTableOutput('playerBoardTE'))
          )
        ),
        tabPanel(
          "Calculator",
          "Some measures of money remaining/inflation.",
          plotOutput('pctAAVTracker'),
          fluidRow(
            column(3, sliderInput("overallAAV","Overall AAV Multiplier",0,4,1,step=0.05)),
            column(3, sliderInput("QBAAV","QB AAV Multiplier",0,5,1,step=0.1)),
            column(2, sliderInput("WRAAV","WR AAV Multiplier",0,3,1,step=0.1)),
            column(2, sliderInput("RBAAV","RB AAV Multiplier",0,3,1,step=0.1)),
            column(2, sliderInput("TEAAV","TE AAV Multiplier",0,3,1,step=0.1))
          ),
          plotOutput('Calculator')
        ),
        tabPanel("Under the hood",
                 tableOutput("underTheHood"),
                 numericInput("rosterspots","Non-DST/K Roster Spots",11))
      )
    ))
))
