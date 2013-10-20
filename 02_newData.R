library(ggplot2)
library(lattice)
library(plyr)
require(graphics)
library(RColorBrewer)
gDat <- read.delim("gapminderOrderedContinents.txt")
gDat <-
  within(gDat, {
    continent <- factor(as.character(continent), levels = unique(gDat$continent))
  })
str(gDat$continent)
jDat <- droplevels(subset(gDat, continent != "Oceania"))
gCoefs <- read.delim("gapminderWithInterceptsOrdered.txt") 
gCoefs <-
  within(gCoefs, {
    continent <- factor(as.character(continent), levels = unique(gCoefs$continent))
  }) #orders continents as they are in file
str(gCoefs)
#to find the 3 best and worst countries per continent based on intercept
AsiaBadInt <- arrange(subset(gCoefs, continent == "Asia"), intercept)
AfricaBadInt <- arrange(subset(gCoefs, continent == "Africa"), intercept)
EuropeBadInt <- arrange(subset(gCoefs, continent == "Europe"), intercept)
AmericasBadInt <- arrange(subset(gCoefs, continent == "Americas"), intercept)
finalAsia <- c("Afghanistan", "Yemen, Rep.", "Nepal", "Israel", "Japan", "Hong Kong, China")
finalAfrica <- c("Gambia", "Sierra Leone", "Guinea", "Mauritius", "Zimbabwe", "Reunion")
finalEurope <- c("Turkey", "Bosnia and Herzegovina", "Albania", "Netherlands", "Iceland", "Norway")
finalAmericas <- c("Bolivia", "Haiti", "Guatemala", "Puerto Rico", "United States", "Canada")
finalCountry <- c(finalAsia, finalAfrica, finalEurope, finalAmericas)
bestWorstCoefs <- subset(gCoefs, country %in% finalCountry)
bestWorstCoefs <- within(bestWorstCoefs, country <- reorder(country, intercept))
bestWorstCountry <- subset(gDat, country %in% finalCountry)
bestWorstCountry <- within(bestWorstCountry, country <- reorder(country, lifeExp, mean))

write.table(bestWorstCoefs, "gapminderbestworstcoef.txt", quote = FALSE,
            sep = "\t", row.names = FALSE)
write.table(bestWorstCountry, "gapminderbestworstlife.txt", quote = FALSE,
            sep = "\t", row.names = FALSE)
p4 <- ggplot(subset(bestWorstCountry, continent == "Asia"), aes(x = year-1952, y = lifeExp, colour = country))
p4 + geom_point() + geom_abline(aes(intercept=intercept, slope=slope), data=subset(bestWorstCoefs, continent == "Asia")) + ylab("Life Expectancy") + xlab("Year-1952") + facet_grid(~country)
ggsave("asiaExtremeCountriesWithLM.png")

p5 <- ggplot(subset(bestWorstCountry, continent == "Africa"), aes(x = year-1952, y = lifeExp, colour = country))
p5 + geom_point() + geom_abline(aes(intercept=intercept, slope=slope), data=subset(bestWorstCoefs, continent == "Africa")) + ylab("Life Expectancy") + xlab("Year-1952") + facet_grid(~country)
ggsave("africaExtremeCountriesWithLM.png")
#PROBLEMWITHAFRICA
p6 <- ggplot(subset(bestWorstCountry, continent == "Americas"), aes(x = year-1952, y = lifeExp, colour = country))
p6 + geom_point() + geom_abline(aes(intercept=intercept, slope=slope), data=subset(bestWorstCoefs, continent == "Americas")) + ylab("Life Expectancy") + xlab("Year-1952") + facet_grid(~country)
ggsave("americasExtremeCountriesWithLM.png")

p7 <- ggplot(subset(bestWorstCountry, continent == "Europe"), aes(x = year-1952, y = lifeExp, colour = country))
p7 + geom_point() + geom_abline(aes(intercept=intercept, slope=slope), data=subset(bestWorstCoefs, continent == "Europe")) + ylab("Life Expectancy") + xlab("Year-1952") + facet_grid(~country)
ggsave("europeExtremeCountriesWithLM.png")

####
gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderCountryColors.txt"
countryColors <- read.delim(file = gdURL, as.is = 3) # protect color
str(countryColors)
jColors <- countryColors$color
names(jColors) <- countryColors$country
head(jColors)
p8 <- ggplot(jDat, aes(x = lifeExp, y = gdpPercap))
p8 + geom_point() + facet_wrap(~ continent) + aes(colour = country) + scale_color_manual(values = jColors) + theme(legend.position = "none") 
ggsave("continentGDPvdLifeExpwithColours.png")
jDat1 <- jDat[with(jDat, order(year, -1 * pop)), ]
p9 <- ggplot(subset(jDat, year == 2007), aes(x = lifeExp, y = gdpPercap))
p9 + geom_point(aes(size = sqrt(pop/pi)), pch = 21, show_guide = FALSE) + scale_size_continuous(range=c(1,40))+ facet_wrap(~ continent) + aes(fill = country) + scale_fill_manual(values = jColors)
ggsave("GDPVsLifeExp2007.png")
p10 <- ggplot(subset(jDat, year == 1952), aes(x = lifeExp, y = gdpPercap))
p10 + scale_y_continuous(limits = c(0, 20000)) +geom_point(aes(size = sqrt(pop/pi)), pch = 21, show_guide = FALSE) + scale_size_continuous(range=c(1,40))+ facet_wrap(~ continent) + aes(fill = country) + scale_fill_manual(values = jColors)
ggsave("GDPVsLifeExp1952WithAlteredYLimit.png")
