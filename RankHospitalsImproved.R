
rank <- function(state,outcome, num = "best"){
  
#by default it finds the best hospital in a state for an outcome   
outcomes <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings =  "Not Available")
#read data 

namez <- names(outcomes)
filteredcolumns <- c(namez[2],namez[7],namez[11],namez[17],namez[23])
outcomesubset <- subset(outcomes, select = filteredcolumns) 
colnames(outcomesubset) <- c("Hospital","State","heart attack","heart failure","pneumonia")

#list the columns, subset Hospital.Name, State, and the 3 possible outcomes, rename columns 

possibleoutcomes <- c("heart attack","heart failure","pneumonia")
statelist <- unique(outcomesubset[2])
#these are the only 3 valid outcomes and only 54 valid states

if (any(statelist == state) == FALSE)
  stop("invalid state") 
if (any(possibleoutcomes == outcome) == FALSE) 
  stop("invalid outcome")

mydata <- subset(outcomesubset, State == state, select = c("Hospital",outcome))
mydata <- mydata[order(mydata[[outcome]],mydata$Hospital),]
mydata <- na.omit(mydata)

ranks <- 1:nrow(mydata)
mydata <- cbind(mydata,ranks)
last <- nrow(mydata)

if (num == "best") 
  return(mydata[1,1])
if (num == "worst") 
  return(mydata[last,1])
if (num > 0 && num <= last) 
  return(mydata[num,1])

else return(NA)
}


