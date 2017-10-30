source('load-data.R')

if (!exists("NEI")) {
  NEI <- readRDS("summarySCC_PM25.rds")
}

# Baltimore City subset 
bc_NEI = subset(NEI, fips == '24510')

# Get total emissions by year (converting to kilotons)
TotalEmissions <- aggregate(bc_NEI$Emissions/1000, by=list(bc_NEI$year), FUN=sum)
colnames(TotalEmissions) <- c('Year', 'TotalPM')

png('plot2.png', width=800, height=600)

barplot(TotalEmissions$TotalPM, 
        TotalEmissions$Year,
        main = 'Total Emissions by Year for Baltimore City',
        xlab = 'Year',
        ylab = 'PM2.5 (kilotons)',
        names.arg = TotalEmissions$Year)

dev.off()