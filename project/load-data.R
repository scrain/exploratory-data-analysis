#
# Conditionally download, unzip and prepare the data for plotting
#
if (!dir.exists('data/')) {
  dir.create('data/')
}

if (!file.exists('data/NEI_data.zip')) {
  download.file('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip', 'data/NEI_data.zip')
  unzip('data/NEI_data.zip')
}

if (!exists("NEI")) {
  NEI <- readRDS("summarySCC_PM25.rds")
}

if (!exists("SCC")) {
  SCC <- readRDS("Source_Classification_Code.rds")
}

