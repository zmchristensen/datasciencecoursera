library(dplyr)
library(ggplot2)

## Read the PM and Source data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Get the baltimore emissions
baltimore <- filter(NEI, NEI$fips == "24510")

## Sum the data by year
yearly_type_sums <- aggregate(baltimore$Emissions, by = list(baltimore$type, baltimore$year), FUN = sum)
colnames(yearly_type_sums) <- c("Type", "Year", "Emissions")

## Open png, create plot, and close png
plot <- qplot(Year, Emissions, data = yearly_type_sums, 
      main = "Total Yearly Emissions, Baltimore, By Source Type", xlab = "Year", 
      ylab = "Total Emissions (in tons)", geom = "line", group = Type, colour = Type)

ggsave(plot, width=6, height=6, dpi = 144, filename = "plots/plot3.png") 
