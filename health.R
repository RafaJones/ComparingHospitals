
library(dplyr)

fileUrl<- "https://data.cms.gov/api/views/97k6-zzx3/rows.csv?accessType=DOWNLOAD"

if (file.exists("medicareIPPS.csv") == FALSE) {
  download.file(fileUrl, destfile = "./medicareIPPS.csv")
}
ippsdata <- read.csv("./medicareIPPS.csv")

sdata <- ippsdata[,c("DRG.Definition","Provider.State",
                     "Average.Covered.Charges","Average.Total.Payments")]

#remove "$" from each money column and change class to numeric 

sdata$Average.Covered.Charges <- as.numeric(gsub("[,$]","",sdata$Average.Covered.Charges,fixed=FALSE))

sdata$Average.Total.Payments <- as.numeric(gsub("[,$]","",sdata$Average.Total.Payments,fixed=FALSE))

#100 DIAGNOSTIC LEVELS
#add column of differences between charges and total payments 
sdata <- tbl_df(sdata)  #tibble 
sdata <- mutate(sdata, Average.Difference = Average.Covered.Charges - Average.Total.Payments)

sdata <- mutate(sdata, Percent.Paid = Average.Total.Payments / Average.Covered.Charges)


#subsetting data by diagnoses and states where payments was larger than covered charges 
# only .35% of the data is included here 
overpaid <- filter(sdata, Average.Total.Payments >= Average.Covered.Charges)

Diagnosis.Count <- summary(sdata$DRG.Definition)
overpaid.Diagnosis.Count <- summary(overpaid$DRG.Definition)
percent.diagnosis.overpaid <- overpaid.Diagnosis.Count / Diagnosis.Count

most.overpaid.diagnosis <- subset(percent.diagnosis.overpaid, percent.diagnosis.overpaid >= 0.005)
 #too obscure to continue, but it's interesting to see 25 diagnoses 
#  specifically over pay more .5% of the time, especially in NY 

#going to remove overpaid from the data as it will disproportionately skew the data 

s1data <- filter(sdata, Average.Total.Payments <= Average.Covered.Charges)

#diagnoses unique codes for graphing as X values 

numsonly <- function(info) {
  as.numeric(gsub("([0-9]+).*$","\\1",info))
}

s1data <- mutate(s1data, diagnosis.codes = numsonly(DRG.Definition)) 


#will make a 2 panel plot 
#first plot is count of diagnosis versus percent paid, i.e. to answer the question
#Does the diagnoses (similar diagnoses grouped together) affect the % of charges eventually paid 
par(mfrow = c(1,2))

pdf(file = "Diagnosis.Deals.pdf")
with(s1data, plot(diagnosis.codes, Percent.Paid,
                  main = "For Some Diagnoses, States Haggle",
                  ylab = "Payment as % of Charges",
                  xlab = "Diagnosis Code", 
                  col = "blue"))

#second plot is the states, coded here as their numeric values, vs the Percent.Paid 
#separate from the diagnoses, do we see certain states regularly pay a higher percent 
#of what they're charged 

with(s1data, plot(as.numeric(s1data$Provider.State), Percent.Paid, 
                 main = "Some States Get Bigger Discounts",
                 xlab = "States, Numerically",
                 ylab = "Payment as a % of Charges",
                 col = "blue"))
dev.off()