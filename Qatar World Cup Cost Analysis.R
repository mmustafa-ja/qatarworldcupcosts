library("readxl")
setwd("E:/University/SEM 5/CTIS365/Project")
compcomparisons <- read_excel("CostComparisons.xlsx")
compcomparisons
worldcups <- data.frame(compcomparisons)
worldcups
names(worldcups)[names(worldcups) == "Cost"] <- "Cost in Billions(USD)"
worldcups
avgspending <- round(mean(worldcups$`Cost in Billions(USD)`),2)
avgspending
ss <- (worldcups$`Cost in Billions(USD)`-avgspending)
ss
SS.squared <- ss * (worldcups$`Cost in Billions(USD)`-avgspending)
worldcup <- cbind(worldcups,ss)
worldcupfinal <- cbind(worldcups,SS.squared)
worldcupfinal
sumofsquares <- sum(worldcupfinal$SS.squared)

sumofsquares
variance <- round(sumofsquares/length(worldcupfinal$Year),2)
variance
sd <- round(sqrt(variance),2)
sd
minspend <- min(worldcupfinal$`Cost in Billions(USD)`)
minspend
maxspend <- max(worldcupfinal$`Cost in Billions(USD)`)
maxspend
range <- maxspend-minspend
med <- median(worldcupfinal$`Cost in Billions(USD)`)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
finalmode <- getmode(worldcupfinal$`Cost in Billions(USD)`)
finalmode
#pretty function adds a top limit to prevent graphs from exceeding the plot
barplot(worldcupfinal$`Cost in Billions(USD)`,xlim=range(pretty(c(0, worldcupfinal$`Cost in Billions(USD)`))),col=c("black","green","red","sky blue","yellow","orange","grey","pink","brown") ,main="World Cup Costs", xlab="Cost in Billions(USD)",ylab="Year",horiz=TRUE,
        names.arg=worldcupfinal$Year)
legend(x = "topright",bty="n",                                     
       paste( "Mean:",avgspending,"\nMedian:",med))
legend(x="topright",bty="n",
       paste("\nSkewness:",round(skewness,2),"\nKurtosis:",round(kurtosis,2),"\nRange:",range))
legend(x="right",bty="n",
       paste("\nStandard Deviation:",sd))


#Skewness and Kurtosis
worldcupfinal$`(X-mu)^3` <- (worldcupfinal$`Cost in Billions(USD)`-avgspending)^3
worldcupfinal$`(X-mu)^4` <- (worldcupfinal$`Cost in Billions(USD)`-avgspending)^4
sumforskewness <- sum(worldcupfinal$`(X-mu)^3`)
sumforskewness
skewness <- sumforskewness/((sd^3)*9)
skewness
sumforkurtosis <- sum(worldcupfinal$`(X-mu)^4`)
kurtosis <- sumforkurtosis/((sd^4)*9)
kurtosis


#Pie chart: breakdown of spending of 2020 Qatar World Cup
library(ggplot2)
library(plotrix)
setwd("E:/University/SEM 5/CTIS365")
worldcupspend <- read_excel("2022 World Cup estimated Spend.xlsx")
spenddf <- data.frame(worldcupspend)

spenddf
labelsector <- c(worldcupspend$Sector)
labelsector
pie(spenddf$Spending.in.USD.Million.,
    labels=(labelsector),
    main="Sector-wise spending for the FIFA World Cup 2022",
    explode=0.1,
    border="black",
    clockwise=TRUE
)


