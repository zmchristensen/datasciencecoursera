library(dplyr)

## Read the PM and Source data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Get the baltimore emissions
baltimore <- filter(NEI, NEI$fips == "24510")
la <- filter(NEI, NEI$fips == "06037")

## Get the SCC ids for motor vehicle related emissionsSCC
ids <- filter(SCC, grepl("vehicle", SCC$EI.Sector, ignore.case = TRUE))[,1]

## Filter the data
baltimore_vehicle_data <- filter(baltimore, baltimore$SCC %in% ids)
la_vehicle_data <- filter(la, la$SCC %in% ids)

## Sum the data by year
baltimore_yearly_sums <- aggregate(baltimore_vehicle_data$Emissions, 
                         by = list(baltimore_vehicle_data$year), FUN = sum)
colnames(baltimore_yearly_sums) <- c("Year", "Emissions")

la_yearly_sums <- aggregate(la_vehicle_data$Emissions, 
                                   by = list(la_vehicle_data$year), FUN = sum)
colnames(la_yearly_sums) <- c("Year", "Emissions")


## Scale the Y-axis so the X-intercept = 0
yMin <- 0
yMax <- max(baltimore_yearly_sums$Emissions, la_yearly_sums$Emissions) * 1.3

png("plots/plot6.png", width = 480, height = 480)

## Supress scientific notation on Y-axis
options("scipen" = 6)
plot(baltimore_yearly_sums[,1], baltimore_yearly_sums[,2], main = "Yearly Motor Vehicle Fine PM Emissions",
     xlab = "Year", ylab = "Total Emissions (in tons)", type = "b", col = "aquamarine3",
     lwd = 4, ylim = c(yMin, yMax))
lines(la_yearly_sums[,1], la_yearly_sums[,2], col = "cyan4", type = "b", lwd = 4)
legend("topright", lwd = 4, legend = c("Baltimore", "Los Angeles"), col = c("aquamarine3", "cyan4"))

dev.off()
