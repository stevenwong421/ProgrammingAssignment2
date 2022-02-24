
best <- function(state, outcome) {
      library(dplyr)
      ## Read outcome data
      data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid
      states = data[, 7]
      state_choose = FALSE
      
            if (state %in% states) {
                  state_choose <- TRUE
            }
      
      if (!state_choose) {
            stop ("invalid state")
      } 
      if (!((outcome == "heart attack") | (outcome == "heart failure")
            | (outcome == "pneumonia"))) {
            stop ("invalid outcome")
      }
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      col <- if (outcome == "heart attack") {
            11
      } else if (outcome == "heart failure") {
            17
      } else {
            23
      }
      data=filter(data,data[,7]==state)
      result=arrange(data,data[,col])
      need=result[1,2]
      need
}




