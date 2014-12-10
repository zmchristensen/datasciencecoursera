library(dplyr)

## Read the PM and Source data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Get the baltimore emissions
baltimore <- filter(NEI, NEI$fips == "24510")

## Sum the data by year
yearly_sums <- aggregate(baltimore$Emissions, by = list(baltimore$year), FUN = sum)
colnames(yearly_sums) <- c("Year", "Emissions")

## Scale the Y-axis so the X-intercept = 0
yMin <- 0
yMax <- max(yearly_sums$Emissions) * 1.1

## Open png, create plot, and close png
png("plots/plot2.png", width = 480, height = 480)

plot(yearly_sums[,1], yearly_sums[,2], main = "Total Yearly Fine PM Emissions, Baltimore City",
     xlab = "Year", ylab = "Total Emissions (in tons)", type = "b", col = "aquamarine3", 
     lwd = 4, ylim = c(yMin, yMax))

dev.off()