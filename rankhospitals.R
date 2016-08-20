#ranking hospitals by state 

rankhospital <- function(state, outcome, num = "best"){
#arguments: state (e.g. "FL"), outcome (e.g. "heart attack"), num (e.g. 4)
    
data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available",stringsAsFactors = FALSE)
#read file, create NAs, do not string as factors 
statelist <- unique(data[ ,7]) #list of 50 states from column 7 of data 
possibleoutcomes <- c("heart attack", "heart failure", "pneumonia") #outcomes 

#make columns into a list for easy subsetting 
namez <- names(data)

#hospital names is column: [ ,2], i.e. namez[2]
#outcomes are column: 
#heart attack  [ , 11], i.e. namez[11]
#heart failure [ ,17], i.e. namez[17]
#pneumonia [ , 23], i.e. namez[23]

#check if state &outcome is valid: 

if (any(statelist == state) == FALSE)
  stop("invalid state") 
if (any(possibleoutcomes == outcome) == FALSE) 
  stop("invalid outcome")

#subset, sort & alphebetize if tied, remove NA's
if (outcome == "heart attack") {
  mydata <- subset(data, State == state, select= c(namez[2],namez[11]))
  mydata <- mydata[order(mydata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, mydata$Hospital.Name), ]
  mydata <- na.omit(mydata) 
}

if (outcome == "heart failure") {
  mydata <- subset(data, State == state, select=c(namez[2], namez[17]))
  mydata <- mydata[order(mydata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, mydata$Hospital.Name), ]
  mydata <- na.omit(mydata) 
}

if (outcome == "pneumonia") {
  mydata <-subset(data, State == state, select = c(namez[2],namez[23]))
  mydata <-mydata[order(mydata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, mydata$Hospital.Name), ]
  mydata <-na.omit(mydata)
}

#mydata is now a sorted data frame for outcomes. we will bind a "rank" column to this frame
rank <- 1:nrow(mydata) #create ranks equal to number of hospitals 
mydata <- cbind(mydata,rank)   #this will rank each hospital by outcome



#num should read "best" as 1, "worst" as last number in mydata
last <- nrow(mydata)

if (num == "best") 
  return(mydata[1,1])  #same as num = 1 

if (num == "worst")
  return(mydata[last,1])

if (num > 0 && num <= last) 
  return(mydata[num,1])  #just incase you manually enter nrows(mydata), it will == "worst"

else 
  return(NA) #if num is too large for dataset return NA 

}

