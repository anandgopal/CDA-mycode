#rankall.R
setwd("~/Documents/Coursera_R_Peng/Asst_2/ProgAssignment3-data")

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  outcome_dframe = read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_dframe[,11] = as.numeric(outcome_dframe[,11])
  outcome_dframe[,17] = as.numeric(outcome_dframe[,17])
  outcome_dframe[,23] = as.numeric(outcome_dframe[,23])
  
  if(! outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  
  if(outcome == "heart attack"){
    outcome_sub = data.frame(Hospital.Name = outcome_dframe[,2], Death = outcome_dframe[,11], State = as.character(outcome_dframe[,7]))
  } else if (outcome == "heart failure"){
    outcome_sub = data.frame(Hospital.Name = outcome_dframe[,2], Death = outcome_dframe[,17], State = as.character(outcome_dframe[,7]))
  } else if (outcome == "pneumonia"){
    outcome_sub = data.frame(Hospital.Name = outcome_dframe[,2], Death = outcome_dframe[,23], State = as.character(outcome_dframe[,7]))
  } 
  
    outcome_sub = outcome_sub[complete.cases(outcome_sub),]  
    outcome_sub = outcome_sub[! outcome_sub$Death == "Not Available",]  
    #outcome_sub = outcome_sub[outcome_sub$State %in% state,]
    outcome_sub = outcome_sub[order(outcome_sub[,3],outcome_sub[,2],outcome_sub[,1]),]  
    
    return_hosp = character(0)
    return_state = character(0)
  
    statelist = unique(as.character(outcome_sub$State))
    
    for(state in statelist){
      outcome_sub_sub = outcome_sub[outcome_sub$State %in% state,]
      if(num == "best"){
        index = length(return_hosp)
        index = index + 1
        return_hosp[index] = as.character(outcome_sub_sub$Hospital.Name[1])
        return_state[index] = state
      } else if (num == "worst"){
        index = length(return_hosp)
        index = index + 1
        return_hosp[index] = as.character(outcome_sub_sub$Hospital.Name[nrow(outcome_sub_sub)])
        return_state[index] = state
      } else {
        index = length(return_hosp)
        index = index + 1
        return_hosp[index] = as.character(outcome_sub_sub$Hospital.Name[as.numeric(num)])
        return_state[index] = state
      }  
    }
    
    return_dframe = data.frame(hospital = return_hosp, state = return_state)  
  
}