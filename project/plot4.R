source('load-data.R')

require(ggplot2)

if (!exists("NEI")) {
  NEI <- readRDS("summarySCC_PM25.rds")
}
if (!exists("SCC")) {
  SCC <- readRDS("Source_Classification_Code.rds")
}

# Get only coal related Source Classification Codes 
coal_SCC <- SCC[grepl('(coal)', SCC$Short.Name, ignore.case = TRUE),]

# Create merged NEI/SCC dataset for just coal sources
coal_Data <- merge(NEI, coal_SCC, by = 'SCC')
coal_Data$Year <- factor(coal_Data$year)

png('plot4.png', width=800, height=600)

y_breaks <- c(0.000000001,  0.00001, 0.1, 1000.0)

p <- ggplot(coal_Data, aes(Year, Emissions)) + 
  scale_y_log10(labels=y_breaks, breaks=y_breaks) +
  stat_boxplot(geom ='errorbar')  +  # whiskers
  geom_boxplot(outlier.alpha=0.6) +  
  ylab('PM2.5 (tons)') + 
  ggtitle('Comparison of Yearly Emissions from Coal Combustion Related Sources for the United States') + 
  theme(plot.title = element_text(hjust = 0.5)) # center title

print(p)

dev.off()

print(p)
