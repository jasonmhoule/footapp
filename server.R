library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
source("optimRoster.R")

# Read and munge data
prdata <- readRDS("projdata.rds")

  # Fix player names
  prdata$playernamelong <- paste(as.character(prdata$playername),
        " (",
        as.character(prdata$position),
        ", ",
        as.character(prdata$team),
        ")",
        sep="")

  prdata <- prdata %>% 
    select(positionRank = pos_rank,
           playername,
           playernamelong,
           position = pos,
           team,
           points,
           auctionValue)
  
  prdata$available <- 'yes'
  prdata$costEst <- prdata$auctionValue
  
  posmx <- prdata %>% group_by(position) %>% summarise(m = max(auctionValue))
  wt <- NULL
  wt$QB <- seq(from = 0, to = filter(posmx, position == "QB")$m, length.out = 8)
  wt$RB <- seq(from = 0, to = filter(posmx, position == "RB")$m, length.out = 8)
  wt$TE <- seq(from = 0, to = filter(posmx, position == "TE")$m, length.out = 8)
  wt$WR <- seq(from = 0, to = filter(posmx, position == "WR")$m, length.out = 8)
  
  teams <- read.csv("config/teamnames.csv", header=FALSE, stringsAsFactors = FALSE)[,1]

# Server
shinyServer(function(input, output, session) {

  #### Initialization ####
  
  # Initialize input fields
  updateSelectInput(session,
                    "onPoint",
                    choices = c("Select" = "", prdata$playernamelong))
  updateSelectInput(session, "draftTeam", choices = c("Select" = "", teams))
  
  # Setup reactive values
  rv <- reactiveValues(
    draftBoard = data.frame(
      Name = character(),
      Team = character(),
      Position = character(),
      Cost = integer(),
      pctAAV = double(),
      Owner = character(),
      Keeper = character(),
      Points = double()
    ),
    myRoster = data.frame(
      Name = character(),
      Position = character(),
      Cost = integer()
    ),
    QBupdate = 0,
    RBupdate = 0,
    WRupdate = 0,
    TEupdate = 0,
    DSTupdate = 0,
    Kupdate = 0
    )
  rv$baseline <-  data.frame(
    QB = wt$QB,
    RB = wt$RB,
    WR = wt$WR,
    TE = wt$TE
  )
  
  
  #### Main Outputs ####
  # Output sidebar constants
  output$myRoster <- renderTable({
    
    rv$myRoster <- rv$draftBoard[rv$draftBoard$Owner == input$myTeam,
                                 c("Name","Position","Cost")]
    rv$myRoster %>% rename(Pos = Position) %>% arrange(Pos)
    
  }, include.rownames = FALSE)
  
  output$spendable <- renderText({moneyAvailable()})
  
  output$underTheHood <- renderTable({
    
    # Calculate the AAV
    draftslots <- input$rosterspots * length(teams)
    playersdrafted <- sum(rv$draftBoard$Position %in% c("QB", "RB", "WR", "TE"))
    remainingdraftslots <- draftslots - playersdrafted
    mprdata <- prdata[prdata$available == 'yes' & prdata$position %in% c("QB", "RB", "WR", "TE"),]
    sprdata <- mprdata[order(mprdata$auctionValue,decreasing = TRUE),]
    moneyinleague <- 200 * length(teams) #hard coded
    moneyspentindraft <- sum(rv$draftBoard$Cost)
    moneyleftinleague <- moneyinleague - moneyspentindraft
    AAVremainingdraftables <- sum(sprdata$auctionValue[1:remainingdraftslots])
    
    calculatedAAV <- moneyleftinleague/AAVremainingdraftables
    
    # Combine RHS, roster points, and calculated AAV into a table for output
    cbind(c("QB Min", "QB Max", "RB Min", "RB Max", "WR Min", "WR Max", "TE Min", "TE Max", "Total FLEX", "Roster Points",
            "Players Drafted","Remaining Draft Slots", "Money Spent in Draft", "Money Left in League", "AAV remaining","calculatedAAV"),
          c(basicRHS(), sum(rv$dreamTeam$points) + sum(rv$draftBoard[rv$draftBoard$Owner == input$myTeam, "Points"]),
            playersdrafted,remainingdraftslots, moneyspentindraft, moneyleftinleague, AAVremainingdraftables,calculatedAAV))
  })
  
  # Output the draftBoard variable
  output$draftBoard <- renderTable({
    
    rv$draftBoard
    
  })
  
  ## Output the player boards
  output$playerBoardQB <- DT::renderDataTable({
    rv$QBupdate
    rv$draftBoard
    outputPlayerBoard("QB")
    })
  output$playerBoardRB <- DT::renderDataTable({
    rv$RBupdate
    rv$draftBoard
    outputPlayerBoard("RB")
  })
  output$playerBoardWR <- DT::renderDataTable({
    rv$WRupdate
    rv$draftBoard
    outputPlayerBoard("WR")
  })
  output$playerBoardTE <- DT::renderDataTable({
    rv$TEupdate
    rv$draftBoard
    outputPlayerBoard("TE")
  })
  output$playerBoardDST <- DT::renderDataTable({
    rv$DSTupdate
    rv$draftBoard
    outputPlayerBoard("DST")
  })
  output$playerBoardK <- DT::renderDataTable({
    rv$Kupdate
    rv$draftBoard
    outputPlayerBoard("K")
  })
  
  # Player board helper function
  outputPlayerBoard <- function(position) {
    # prdata <- getPrData()
    t <-
      prdata[prdata$position == position, c("available",
                                            "positionRank",
                                            "playername",
                                            "team",
                                            "auctionValue",
                                            "costEst")]
    t <- arrange(t, positionRank)
    colnames(t) <- c("Available", "PRK", "Name", "Tm", "AAV", "Est")
    
    # Setup datatable
    datatable(t,
              rownames = FALSE,
              options = list(pageLength = 25,
                             columnDefs = list(list(
                               visible = FALSE, targets = 0
                             )))) %>% formatStyle(
                               'Available',
                               target = 'row',
                               backgroundColor = styleEqual(c('no','kept'),c('gray','black')),
                               color = styleEqual(c('no','kept'),c('white','white'))
                             ) %>% formatStyle(
                               'Est','Available',
                               color = styleEqual(c('no','kept'),c('yellow','white')),
                               fontWeight = styleEqual(c('no','kept'),c('bold','normal'))
                             ) %>% formatRound(
                               'Est', 1
                             )
  }
  
  # Output pctAAV tracker
  output$pctAAVTracker <- renderPlot({
    validate(
      need(sum(rv$draftBoard$Keeper == 'Drafted')>0, "n/a")
    )
    data <- as.data.frame(cbind(draftPos = 1:sum(rv$draftBoard$Keeper == 'Drafted'),
                                pctAAV = rv$draftBoard[rv$draftBoard$Keeper == 'Drafted',]$pctAAV))

    if (data$pctAAV[length(data$pctAAV)]>5) {
      data <- data[-length(data[,1]),]
    }
    ggplot(data,aes(draftPos,pctAAV)) +
      geom_point() +
      geom_smooth(span=min(1,20/length(data[,1]))) +
      lims(x=c(0, max(100,length(data[,1]))),
           y=c(0, max(2,quantile(data$pctAAV,.85))))
  })
  
  # Output the model calculator
  output$Calculator <- renderPlot({
    
    # Update with any roster update
    rv$QBupdate
    rv$RBupdate
    rv$WRupdate
    rv$TEupdate
    
    # Get player data
    disp <- subset(prdata, position %in% c("QB","RB","WR","TE") &
                     available == 'no',
                   select = c(auctionValue, costEst, position))
                     
    # Combine with basis weights
    for (pos in c("QB","RB","WR","TE")) {
      ww <- data.frame(auctionValue = wt[[pos]],
                  costEst = as.numeric(rv$baseline[pos][,1]),
                  position=pos)
      disp <- rbind(disp,ww)
    }
    disp$auctionValue <- as.numeric(disp$auctionValue)
    disp$costEst <- as.numeric(disp$costEst)
    
    ggplot(disp, aes(auctionValue, costEst)) + geom_smooth() + geom_point() + 
      facet_grid(. ~ position, scales = "free")
    
  })
  
  #### Update Draft Board ####
  # Observing Event: Update Draft Board from inputs
  observeEvent(input$addOnPoint, {
    
    # Validate entry
    validate(
      need(input$onPoint != "",""),
      need(input$draftTeam != "","")
    )
    
    # Format player data and add to draftBoard
    playerData <- prdata[prdata$playernamelong == input$onPoint,][1,]
    rv$draftBoard <- rbind(
      rv$draftBoard,
      data.frame(
        Name = playerData$playername,
        Team = playerData$team,
        Position = playerData$position,
        Cost = input$draftCost,
        pctAAV = input$draftCost / playerData$auctionValue,
        Owner = input$draftTeam,
        Keeper = if(input$keeper) {"Kept"} else {"Drafted"},
        Points = playerData$points
      )
    )
    
    # Update prdata available and costEst fields
    prdata[prdata$playernamelong == input$onPoint,]$available <<- if(input$keeper) {'kept'} else {'no'}
    prdata[prdata$playernamelong == input$onPoint,]$costEst <<- input$draftCost
    
    # Update rv to trigger draftBoard and graph output updates
    switch(as.character(playerData$position),
           "QB" = {rv$QBupdate <- rv$QBupdate + 1},
           "RB" = {rv$RBupdate <- rv$RBupdate + 1},
           "WR" = {rv$WRupdate <- rv$WRupdate + 1},
           "TE" = {rv$TEupdate <- rv$TEupdate + 1},
           "DST" = {rv$DSTupdate <- rv$DSTupdate + 1},
           "K" = {rv$Kupdate <- rv$Kupdate + 1})

    # Use new draft value to recalculate model and update costEst values
    updateCostEst(as.character(playerData$position))
    
    # Reset inputs and remove values
    remainingPlayers <-
      subset(prdata, available == 'yes')
    updateSelectInput(session,
                      "onPoint",
                      choices = c("Select" = "", remainingPlayers$playernamelong))
    updateSelectInput(session, "draftTeam", choices = c("Select" = "", teams))
    updateNumericInput(session, "draftCost", value = 1)
    updateCheckboxInput(session, "keeper", value = FALSE)
  })
  
  # Helper function to recalculate model and update costEst values
  updateCostEst <- function(positn) {
    
    # NA for DST, K
    if(positn %in% c("DST","K")) {return()}
      
    # Pull in draft values for positn, note that 'costEst' is now real price paid
    drafted <- with(prdata, prdata[available == "no" & position == positn,
                                   c("auctionValue","costEst")])
    
    # Combine w/ basis weights to avoid overcorrecting on small sample size
    basis <- cbind(wt[[positn]],rv$baseline[positn])
    colnames(basis) <- colnames(drafted)
    
    # Do not take drafted values into account
    fullset <- basis
    
    # Calculate lm model
    model <- lm(costEst ~ auctionValue, fullset)
    
    # Pull in undrafted values for positn
    undrafted <- with(prdata, prdata[available == "yes" & position == positn,
                                     "auctionValue"])
    
    # Predict new values from lm model and save back
    prdata[prdata$available == "yes" & prdata$position == positn,
           "costEst"] <<- predict.lm(model,data.frame(auctionValue = undrafted))
    
  }
  
  #### Data I/O ####
  # Observing Event: Saving Draft Board from inputs
  observeEvent(input$saveDraftBoard, {
    write.csv(
      x = prdata,
      file = "./cache/prdata.csv",
      row.names = FALSE, quote = TRUE
    )
    write.csv(
      x = isolate({rv$draftBoard}),
      file = "./cache/draftBoard.csv",
      row.names = FALSE, quote = TRUE
    )
  })
  
  # Observing Event: Load Draft Board from cache
  observeEvent(input$loadDraftBoard, {
    prdata <<- read.csv(
      file = "./cache/prdata.csv",
      stringsAsFactors = FALSE
    )
    rv$draftBoard <- read.csv(
      file = "./cache/draftBoard.csv",
      stringsAsFactors = FALSE
    )
    
    rv$draftBoard$Position <<- as.factor(rv$draftBoard$Position)
    fullpos <- c("DST","K","QB","RB","TE","WR")
    levels(rv$draftBoard$Position) <<- c(levels(rv$draftBoard$Position),
                                         fullpos[!fullpos %in% levels(rv$draftBoard$Position)])
    
    remainingPlayers <-
      subset(prdata, available == 'yes')
    updateSelectInput(session,
                      "onPoint",
                      choices = c("Select" = "", remainingPlayers$playernamelong))
  })
  
  # Change overall AAV input: update rv$baselines and update the individual input sliders
  observeEvent(input$overallAAV, {
    rv$baseline$QB <- wt$QB * input$overallAAV * input$QBAAV
    rv$baseline$RB <- wt$RB * input$overallAAV * input$RBAAV
    rv$baseline$WR <- wt$WR * input$overallAAV * input$WRAAV
    rv$baseline$TE <- wt$TE * input$overallAAV * input$TEAAV
    updateCostEst("QB")
    updateCostEst("RB")
    updateCostEst("WR")
    updateCostEst("TE")
    rv$QBupdate <- rv$QBupdate + 1
    rv$RBupdate <- rv$RBupdate + 1
    rv$WRupdate <- rv$WRupdate + 1
    rv$TEupdate <- rv$TEupdate + 1
    
  })
  
  observeEvent(input$QBAAV, {
    rv$baseline$QB <- wt$QB * input$overallAAV * input$QBAAV
    updateCostEst("QB")
    rv$QBupdate <- rv$QBupdate + 1
  })
  
  observeEvent(input$RBAAV, {
    rv$baseline$RB <- wt$RB * input$overallAAV * input$RBAAV
    updateCostEst("RB")
    rv$RBupdate <- rv$RBupdate + 1
  })
  
  observeEvent(input$WRAAV, {
    rv$baseline$WR <- wt$WR * input$overallAAV * input$WRAAV
    updateCostEst("WR")
    rvWRBupdate <- rv$WRupdate + 1
  })
  
  observeEvent(input$TEAAV, {
    rv$baseline$TE <- wt$TE * input$overallAAV * input$TEAAV
    updateCostEst("TE")
    rv$TEupdate <- rv$TEupdate + 1
  })
  
  #### Optimization values ####
  
  # Reactive modules to capture config updates to feed to optimization routines
  moneyAvailable <- reactive({
    pricePaid <- sum(rv$myRoster$Cost)
    input$totalBudget - input$reserveBudget - pricePaid
  })
  
  basicRHS <- reactive({
    
    # Get current numbers of each position on myRoster
    curr_QB <- sum(rv$myRoster$Position == "QB")
    curr_RB <- sum(rv$myRoster$Position == "RB")
    curr_WR <- sum(rv$myRoster$Position == "WR")
    curr_TE <- sum(rv$myRoster$Position == "TE")
    
    # For each position:
      # Min = slots - current
      # Max = slots + flex (if available) - current
    rhs <- c(input$numQB - curr_QB, # QB Min
             input$numQB + input$numFLEX*("QB" %in% input$checks) - curr_QB, # QB Max
             input$numRB - curr_RB, # RB Min
             input$numRB + input$numFLEX*("RB" %in% input$checks) - curr_RB, # RB Max
             input$numWR - curr_WR, # WR Min
             input$numWR + input$numFLEX*("WR" %in% input$checks) - curr_WR, # WR Max
             input$numTE - curr_TE, # TE Min
             input$numTE + input$numFLEX*("TE" %in% input$checks) - curr_TE, # TE Max
             (input$numQB - curr_QB)*("QB" %in% input$checks) +
               (input$numRB - curr_RB)*("RB" %in% input$checks) +  
               (input$numWR - curr_WR)*("WR" %in% input$checks) +
               (input$numTE - curr_TE)*("TE" %in% input$checks) +
               input$numFLEX) # Total FLEXable
    names(rhs) <- NULL
    rhs
  })
  
  
  # Bid-up-to number, responsive on the on-point player
  output$bidUpToValue <- renderText({
    # Validate entry
    validate(
      need(!(input$onPoint %in% c("","Select")),"n/a")
    )
    # Check against position restraints and break if not a fit (bidUpTo = 0)
    j <- basicRHS()[seq(2,8,2)]
    j2 <- c("QB","RB","WR","TE")[j>0]
    if (!(prdata[prdata$playernamelong == input$onPoint,"position"] %in% j2)) {
      return("n/a")
    }
    
    if (input$onPoint %in% rv$dreamTeam$playernamelong) {
      
      # Player in Dream Team
      # Step one, remove player from pool and re-run dream team for secondBest lineup
      # Player data to choose from
      choosePlayers <- subset(prdata,available == 'yes' &
                                !(position %in% c("DST","K")) &
                                !(playernamelong == input$onPoint))
      
      result <- optimRoster(
        prdata = choosePlayers,
        flexVector = input$checks,
        rhs = basicRHS(),
        constraintVal = moneyAvailable(),
        dir = "max"
      )
      
      pointConstraint <- sum(choosePlayers[result$solution==1,c("points")])
      
      # Step two, run below routine using secondBest point constraint
      rv$bidUpTo <- getBidUpTo(input$onPoint, pointConstraint)
      
    } else {
      
      # Player not in Dream Team
      # Run routine on player against Dream Team point baseline constraint
      pointConstraint <- sum(rv$dreamTeam$points)
      rv$bidUpTo <- getBidUpTo(input$onPoint, pointConstraint)
      
    }
  })
  
  # Helper function for bidUpTo
  getBidUpTo <- function(player,pointConstraint) {
    
    # Player data to choose from
    choosePlayers <- subset(prdata,available=='yes' & !(position %in% c("DST","K")))
    
    # Set player's cost to zero
    choosePlayers[choosePlayers$playernamelong == player, "costEst"] <- 0
    
    # Minimize cost against point baseline constraint
    result <- optimRoster(
      prdata = choosePlayers,
      flexVector = input$checks,
      rhs = basicRHS(),
      constraintVal = pointConstraint,
      dir = "min"
    )
    
    # If no solution, bidUpTo = 0
    if (result$status == 2) {
      return(0)
    } else {
      # If solution, subtract minmized roster cost from $ available
      floor(moneyAvailable() - result$objval)
    }
    
  }
  
  
  # Dream team
  output$dreamTeam <- renderTable({
    
    # Make this responsive to updates from any player except DST or K
    rv$QBupdate
    rv$RBupdate
    rv$WRupdate
    rv$TEupdate
    rv$baseline
    
    # Player data to choose from
    choosePlayers <- subset(prdata,available=='yes' & !(position %in% c("DST","K")))
    
    result <- optimRoster(
      prdata = choosePlayers,
      flexVector = input$checks,
      rhs = basicRHS(),
      constraintVal = moneyAvailable(),
      dir = "max"
    )

    rv$dreamTeam <- choosePlayers[result$solution==1,c("playernamelong",
                                                       "playername",
                                                       "position",
                                                       "costEst",
                                                       "points")]
    output <- rv$dreamTeam[,2:4]
    names(output) <- c("Name","Pos","Cost")
    output %>% arrange(Pos)
    
  }, include.rownames = FALSE)

})
