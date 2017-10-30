source('load-data.R')

require(ggplot2)

if (!exists("NEI")) {
  NEI <- readRDS("summarySCC_PM25.rds")
}
if (!exists("SCC")) {
  SCC <- readRDS("Source_Classification_Code.rds")
}

# Baltimore City subset 
bc_NEI <- subset(NEI, fips == 24510)
bc_NEI$Year <- factor(bc_NEI$year)

# Get only motor vehicle related Source Classification Codes 
mv_SCC <- SCC[grepl('(vehicle)', SCC$EI.Sector, ignore.case = TRUE),]

# Create merged NEI/SCC dataset for just these sources
mv_Data <- merge(bc_NEI, mv_SCC, by = 'SCC')
mv_Data$Year <- factor(mv_Data$year)

png('plot5.png', width=800, height=600)

y_breaks <- c(0.000001,  0.0001, 0.01, 1.0, 100.0)

p <- ggplot(mv_Data, aes(Year, Emissions)) + 
  scale_y_log10(labels=y_breaks, breaks=y_breaks) +
  stat_boxplot(geom ='errorbar')  +  # whiskers
  geom_boxplot(outlier.alpha=0.6) +  
  ylab('PM2.5 (tons)') + 
  ggtitle('Comparison of Yearly Emissions from Motor Vehicle Related Sources for Baltimore City') + 
  guides(fill=FALSE) + # remove legend
  theme(plot.title = element_text(hjust = 0.5)) # center title

print(p)

dev.off()

print(p)
