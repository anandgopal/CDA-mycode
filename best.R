setwd("~/Documents/Coursera_R_Peng/Asst_2/ProgAssignment3-data")

best <- function(state, outcome) {  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
    
  outcome_dframe = read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = F)
  outcome_dframe[,11] = as.numeric(outcome_dframe[,11])
  outcome_dframe[,17] = as.numeric(outcome_dframe[,17])
  outcome_dframe[,23] = as.numeric(outcome_dframe[,23])
  
  if(! state %in% outcome_dframe$State) stop("invalid state")
  if(! outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  
  if(outcome == "heart attack"){
    outcome_sub = data.frame(Hospital.Name = outcome_dframe[,2], Death = outcome_dframe[,11], State = as.character(outcome_dframe[,7]))
    outcome_sub = outcome_sub[complete.cases(outcome_sub),]  
    outcome_sub = outcome_sub[outcome_sub$State %in% state,]
    outcome_sub = outcome_sub[order(outcome_sub[,2],outcome_sub[,1]),]
    return_text = as.character(outcome_sub$Hospital.Name[1])
  }

  else if(outcome == "heart failure"){
    outcome_sub = data.frame(Hospital.Name = outcome_dframe[,2], Death = outcome_dframe[,17], State = as.character(outcome_dframe[,7]))
    outcome_sub = outcome_sub[complete.cases(outcome_sub),]  
    outcome_sub = outcome_sub[outcome_sub$State %in% state,]
    outcome_sub = outcome_sub[order(outcome_sub[,2],outcome_sub[,1]),]
    return_text = as.character(outcome_sub$Hospital.Name[1])
  }
  
  else if(outcome == "pneumonia"){
    outcome_sub = data.frame(Hospital.Name = outcome_dframe[,2], Death = outcome_dframe[,23], State = as.character(outcome_dframe[,7]))
    outcome_sub = outcome_sub[complete.cases(outcome_sub),]  
    outcome_sub = outcome_sub[outcome_sub$State %in% state,]
    outcome_sub = outcome_sub[order(outcome_sub[,2],outcome_sub[,1]),]
    return_text = as.character(outcome_sub$Hospital.Name[1])
  }

  return(return_text)  
}



