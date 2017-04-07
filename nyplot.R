#Relationship between Average.Covered.Charges and Average.Total.Payments in New York


fileUrl<- "https://data.cms.gov/api/views/97k6-zzx3/rows.csv?accessType=DOWNLOAD"

if (file.exists("medicareIPPS.csv") == FALSE) {
  download.file(fileUrl, destfile = "./medicareIPPS.csv")  #26MB file duplication check 
}
ippsdata <- read.csv("./medicareIPPS.csv", stringsAsFactors = FALSE)

#data from NY
nydata <- subset(ippsdata, Provider.State == "NY", 
                 c("DRG.Definition", 
                   "Provider.Id",
                   "Average.Covered.Charges",
                   "Average.Total.Payments"))

#remove "$" from each money column and change class to numeric 

nydata$Average.Covered.Charges <- as.numeric(gsub("[,$]","",nydata$Average.Covered.Charges,fixed=FALSE))

nydata$Average.Total.Payments <- as.numeric(gsub("[,$]","",nydata$Average.Total.Payments,fixed =FALSE))


#plot the data as a scatterplot 


pdf(file = "nypays.pdf")
with(nydata, plot(log10(Average.Covered.Charges),log10(Average.Total.Payments),
                  xlab = "Charges (log10 Dollars)",
                  ylab = "Payments (log10 Dollars)"))
title(main = "Charges increase Payments in NY ")

#adding various lines to see means, medians, and a basic linear regression 
abline(h = log10(mean(nydata$Average.Total.Payments)), col = "blue")
abline(h = log10(median(nydata$Average.Total.Payments)), col = "red")
abline(v = log10(median(nydata$Average.Covered.Charges)), col = "red")
abline(v = log10(mean(nydata$Average.Covered.Charges)), col = "blue")
abline(lsfit(log10(nydata$Average.Covered.Charges),log10(nydata$Average.Total.Payments)),
       col = "yellow")

#no points are covered by the legend 
legend("bottomright",legend = c("mean","median","lm fit"), pch=19, col = c("blue","red","yellow"))

dev.off()



