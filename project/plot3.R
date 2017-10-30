source('load-data.R')

require(ggplot2)

if (!exists("NEI")) {
  NEI <- readRDS("summarySCC_PM25.rds")
}

# Baltimore City subset 
bc_NEI <- subset(NEI, fips == 24510)
bc_NEI$Year <- factor(bc_NEI$year)

png('plot3.png', width=800, height=600)

y_breaks <- c(0.0000001, 0.00001, 0.001, 0.1, 10.0, 1000.0)

p <- ggplot(bc_NEI, aes(Year, Emissions)) + 
       scale_y_log10(labels=y_breaks, breaks=y_breaks) +
       facet_grid(. ~ type) + 
       stat_boxplot(geom ='errorbar') +  # whiskers
       geom_boxplot(aes(fill = type), outlier.alpha=0.6) +  
       ylab('PM2.5 (tons)') + 
       ggtitle('Comparison of Yearly Emissions for Baltimore City by Type') + 
       guides(fill=FALSE) + # remove legend
       theme(plot.title = element_text(hjust = 0.5)) # center title

print(p)

dev.off()

print(p)
