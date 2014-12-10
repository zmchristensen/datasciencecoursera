library(dplyr)

## Read the PM and Source data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Get the SCC ids for coal related emissions
ids <- filter(SCC, grepl("Coal", SCC$EI.Sector, ignore.case = TRUE))[,1]

## Filter the data
coal_data <- filter(NEI, NEI$SCC %in% ids)

## Sum the data by year
yearly_sums <- aggregate(coal_data$Emissions, by = list(coal_data$year), FUN = sum)
colnames(yearly_sums) <- c("Year", "Emissions")

## Scale the Y-axis so the X-intercept = 0
yMin <- 0
yMax <- max(yearly_sums$Emissions) * 1.1

png("plots/plot4.png", width = 480, height = 480)

## Supress scientific notation on Y-axis
options("scipen" = 6)
plot(yearly_sums[,1], yearly_sums[,2], main = "Total Yearly Fine PM Emissions, Coal",
     xlab = "Year", ylab = "Total Emissions (in tons)", type = "b", col = "aquamarine3",
     lwd = 4, ylim = c(yMin, yMax))

dev.off()
