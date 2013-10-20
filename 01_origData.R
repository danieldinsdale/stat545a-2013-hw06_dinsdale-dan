library(ggplot2)
library(lattice)
library(plyr)
require(graphics)
library(RColorBrewer)
gDat <- read.delim("gapminderDataFiveYear.txt")
#display.brewer.pal(n = 5, name = 'Dark2')
#p1BrewColors <- brewer.pal(n = 5, name = "Dark2")
#using my homework 4
lifeExpCont <- ddply(gDat, .(continent, year), summarize, meanLifeExp = mean(lifeExp))
p1 <- ggplot(lifeExpCont, aes(x = year, y = meanLifeExp, colour = continent))
p1 + geom_line() +  ylab("Mean Life Expectancy (Years)") + xlab("Year") 
ggsave("meanLifeExp.png")
p2 <- ggplot(gDat, aes(x = year, y = lifeExp, colour = continent))
p2 + geom_smooth(method = "lm") +  ylab(" Life Expectancy (Years)") + xlab("Year")
ggsave("LifeExpErrorIntervalsLinearModel.png")
p3 <- ggplot(gDat, aes(x = lifeExp, fill = continent))
p3 + geom_bar(binwidth = 1.5) + ylab("Count") + xlab("Life Expectancy")
ggsave("LayeredHistogram.png")
gDat <- within(gDat, continent <- reorder(continent, lifeExp, mean)) #reorder continents
gDat <- arrange(gDat, continent) #reorder complete
write.table(gDat, "gapminderOrderedContinents.txt", quote = FALSE,
            sep = "\t", row.names = FALSE)
#levels(jDat$continent)
#reference http://www.stat.ubc.ca/~jenny/STAT545A/block04_dataAggregation.html
gFit <- function(x){
  lm(lifeExp ~ I(year - 1952), x)
}
gFun <- function(x) {
  estCoefs <- coef(lm(lifeExp ~ I(year - 1952), x))
  estResid <- summary(gFit(x))$sigma
  estAll <- c(estCoefs, estResid)
  names(estAll) <- c("intercept", "slope", "residual")
  return(estAll)
}
jDat <- droplevels(subset(gDat, continent != "Oceania"))
gCoefs <- ddply(jDat, ~country + continent, gFun)
gCoefs <- arrange(gCoefs, continent)
write.table(gCoefs, "gapminderWithInterceptsOrdered.txt", quote = FALSE,
            sep = "\t", row.names = FALSE)
