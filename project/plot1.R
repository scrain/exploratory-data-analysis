source('load-data.R')

if (!exists("NEI")) {
  NEI <- readRDS("summarySCC_PM25.rds")
}

# Get total emissions by year (converting to kilotons)
TotalEmissions <- aggregate(NEI$Emissions/1000, by=list(NEI$year), FUN=sum)
colnames(TotalEmissions) <- c('Year', 'TotalPM')

png('plot1.png', width=800, height=600)

barplot(TotalEmissions$TotalPM, 
        TotalEmissions$Year,
        main = 'Total Emissions by Year',
        xlab = 'Year',
        ylab = 'PM2.5 (kilotons)',
        names.arg = TotalEmissions$Year)

dev.off()