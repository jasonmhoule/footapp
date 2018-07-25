optimRoster <- function(prdata,flexVector,rhs,constraintVal,dir="max") {
  # Function args:
    # prdata, a matrix of projection data
    # flexVector, a vector of strings representing which positions may FLEX
    # rhs, a basic rhs that captures current state (needs to be added to)
    # constraintVal, either cost (if dir="max") or value (if dir="min")
    # dir, either:
      # "max" to maximize value with cost constraint, or
      # "min" to minimize cost with value constraint
  
  # Function returns the output of lpSolve for the program in question
  
  library(lpSolve)
  
  # Initialize values for use in optimization
  
  matrix <- rbind(as.numeric(prdata$position == "QB"), # num QB
                  as.numeric(prdata$position == "QB"), # num QB
                  as.numeric(prdata$position == "RB"), # num RB
                  as.numeric(prdata$position == "RB"), # num RB
                  as.numeric(prdata$position == "WR"), # num WR
                  as.numeric(prdata$position == "WR"), # num WR
                  as.numeric(prdata$position == "TE"), # num TE
                  as.numeric(prdata$position == "TE"), # num TE
                  as.numeric(prdata$position %in% flexVector))  # Num FLEX
  
  direction <- c(">=", # QB Min
                 "<=", # QB Max
                 ">=", # RB Min
                 "<=", # RB Max
                 ">=", # WR Min
                 "<=", # WR Max
                 ">=", # TE Min
                 "<=", # TE Max
                 "==") # FLEX
  
  # Add final constraint and define objective
  if(dir == "max") {
    
    # Maximizing points subject to cost
    obj <- prdata$points
    matrix <- rbind(matrix,
                    prdata$costEst)
    direction <- c(direction,"<=")
    rhs <- c(rhs,constraintVal) # In this case, available money
    
  } else {
    
    # Minimizing cost subject to points
    obj <- prdata$costEst
    matrix <- rbind(matrix,
                    prdata$points)
    direction <- c(direction,">=")
    rhs <- c(rhs, constraintVal) # In this case, point total to exceed
    
  }
  
  lp(dir,obj,matrix,direction,rhs,all.bin=TRUE)
}