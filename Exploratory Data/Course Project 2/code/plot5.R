library(dplyr)

## Read the PM and Source data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Get the baltimore emissions
baltimore <- filter(NEI, NEI$fips == "24510")

## Get the SCC ids for motor vehicle related emissionsSCC
ids <- filter(SCC, grepl("vehicle", SCC$EI.Sector, ignore.case = TRUE))[,1]

## Filter the data
baltimore_vehicle_data <- filter(baltimore, baltimore$SCC %in% ids)

## Sum the data by year
yearly_sums <- aggregate(baltimore_vehicle_data$Emissions, 
                         by = list(baltimore_vehicle_data$year), FUN = sum)
colnames(yearly_sums) <- c("Year", "Emissions")

## Scale the Y-axis so the X-intercept = 0
yMin <- 0
yMax <- max(yearly_sums$Emissions) * 1.1

png("plots/plot5.png", width = 480, height = 480)

## Supress scientific notation on Y-axis
options("scipen" = 6)
plot(yearly_sums[,1], yearly_sums[,2], main = "Yearly Motor Vehicle Fine PM Emissions, Baltimore",
     xlab = "Year", ylab = "Total Emissions (in tons)", type = "b", col = "aquamarine3",
     lwd = 4, ylim = c(yMin, yMax))

dev.off()
