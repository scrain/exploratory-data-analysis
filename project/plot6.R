source('load-data.R')

require(ggplot2)

# Get NEI subsets and add fipsName for use in the plot later
bc_NEI <- subset(NEI, fips == '24510')  
bc_NEI$fipsName = 'Baltimore City'
la_NEI <- subset(NEI, fips == '06037')  
la_NEI$fipsName = 'Los Angeles County'

# combine into a single data.frame
bc_la_NEI      <- rbind(la_NEI, bc_NEI)

# Get only motor vehicle related Source Classification Codes 
mv_SCC <- SCC[grepl('(vehicle)', SCC$EI.Sector, ignore.case = TRUE),]

# Create merged NEI/SCC dataset for just these sources
mv_Data <- merge(bc_la_NEI, mv_SCC, by = 'SCC')
mv_Data$Year <- factor(mv_Data$year)

png('plot6.png', width=800, height=600)

y_breaks <- c(0.00001, 0.001, 0.1, 10, 1000.0)

p <- ggplot(mv_Data, aes(Year, Emissions)) + 
  scale_y_log10(labels=y_breaks, breaks=y_breaks) +
  facet_grid(. ~ fipsName) + 
  stat_boxplot(geom ='errorbar')  +  # whiskers
  geom_boxplot(aes(fill=fips), outlier.alpha=0.6) +  
  ylab('PM2.5 (tons)') + 
  ggtitle('Comparison of Yearly Emissions from Motor Vehicle Related Sources') + 
  guides(fill=FALSE) + # remove legend
  theme(plot.title = element_text(hjust = 0.5)) # bugs me that titles aren't centered by default

print(p)

dev.off()

print(p)
