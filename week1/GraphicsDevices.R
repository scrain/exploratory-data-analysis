slide4 <- function() {
  library(datasets)
  with(faithful, plot(eruptions, waiting)) ## Make plot appear on screen device 
  title(main = "Old Faithful Geyser data") ## Annotate with a title
}

slide5 <- function() {
  library(datasets)
  
  pdf(file = "myplot.pdf") ## Open PDF device; create 'myplot.pdf' in my working directory
  
  with(faithful, plot(eruptions, waiting)) ## Make plot appear on screen device 
  title(main = "Old Faithful Geyser data") ## Annotate with a title
  
  dev.off() ## Close the PDF file device
  ## Now you can view the file 'myplot.pdf' on your computer
}

slide9 <- function() {
  library(datasets)
  with(faithful, plot(eruptions, waiting)) ## Create plot on screen device 
  title(main = "Old Faithful Geyser data") ## Add a main title 
  dev.copy(png, file = "geyserplot.png")   ## Copy my plot to a PNG file 
  dev.off()                                ## Don't forget to close the PNG device!  
}