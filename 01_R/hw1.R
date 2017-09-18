
## Analytics Edge
# Homework 1 

setwd("C:/study/Analy_Edge/wk1")

readLines("mvtWeek1.csv",n=5)

mvt <- read.csv("mvtWeek1.csv", header = TRUE)
str(mvt)
summary(mvt)

?max
max(mvt$ID)

min(mvt$Beat)

table(mvt$Arrest)

table(mvt$LocationDescription)[names(table(mvt$LocationDescription))=="ALLEY"]

mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert

sort(table(mvt$Month))
sort(table(mvt$Weekday), decreasing = T)

sort(table(mvt$Month[mvt$Arrest == T]))

table(mvt$Arrest[mvt$Year=="2012"])/nrow(mvt[mvt$Year=="2012",])

sort(table(mvt$LocationDescription),decreasing = T)[1:6]

top5_names <- setdiff(names(sort(table(mvt$LocationDescription),decreasing = T)[1:6]), "OTHER")

Top5 <- subset(mvt, LocationDescription %in% top5_names)
Top5$LocationDescription = factor(Top5$LocationDescription)

# One of the locations has a much higher arrest rate than the other locations. Which is it? Please enter the text in exactly the same way as how it looks in the answer options for Problem 4.1. 
table(Top5$LocationDescription[Top5$Arrest==TRUE])/table(Top5$LocationDescription)

# On which day of the week do the most motor vehicle thefts at gas stations happen?
sort(table(mvt$Weekday[mvt$LocationDescription=="GAS STATION"]))

# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
sort(table(mvt$Weekday[mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL"]))


#   Stock Dynamics
IBM <- read.csv("IBMStock.csv", header = T)
GE <- read.csv("GEStock.csv", header = T)           
ProcterGamble <- read.csv("ProcterGambleStock.csv", header = T)
CocaCola <- read.csv("CocaColaStock.csv", header = T)
Boeing <- read.csv("BoeingStock.csv", header = T)

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

dim(IBM)
dim(CocaCola)

plot(CocaColaa$Date, CocaCola$StockPrice, type = "l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
abline(v = as.Date("1983-01-01"))
abline(v = as.Date("1983-12-31"))

# Problem 3.1 - Visualizing Stock Dynamics 1995-2005 
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col = "blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], col = "green")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col = "purple")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col = "orange")
abline(v = as.Date("2000-3-1"))
abline(v = as.Date("2000-3-31"))

abline(v=as.Date("1997-9-1"))
abline(v=as.Date("1997-11-1"))
abline(v=as.Date("2004-1-1"))


# For IBM, compare the monthly averages to the overall average stock price. In which months has IBM historically had a higher stock price (on average)? Select all that apply

sort(tapply(IBM$StockPrice, months(IBM$Date), mean)>mean(IBM$StockPrice))

# ADVANCED; Repeat the tapply function from the previous problem for each of the other four companies.

fn101 <- function(x) {
    print(sort(tapply(x$StockPrice, months(x$Date), mean)>mean(x$StockPrice)))
    print(sort(tapply(x$StockPrice, months(x$Date), mean)))
}

fn101(GE)
fn101(CocaCola)
