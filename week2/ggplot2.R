library(ggplot2)
load("maacs_full.Rda")

# ggplot hello world
slide8 <- function() {
  qplot(displ, hwy, data=mpg)
}

# modifying aesthetics
slide9 <- function() {
  qplot(displ, hwy, data=mpg, color=drv)
}

slide9b <- function() {
  qplot(displ, cty, data=mpg, color=year)
}

# adding a geom
slide10 <- function() {
  qplot(displ, hwy, data=mpg, geom = c("point", "smooth"))
}

# histograms
slide11 <- function() {
  qplot(hwy, data=mpg, fill=drv)
}

# Facets
slide12a <-function() {
  qplot(displ, hwy, data=mpg, facets =.~drv)
}

# Facets
slide12b <-function() {
  qplot(hwy, data=mpg, facets =drv ~., binwidth=2)
}

# histogram of ENO 
# ENO = exhaled nitric oxide
# mopos = sensitized to mouse allergen (y/n)
# pm25 = fine particulate matter
#
slide15 <- function() {
  qplot(log(eno), data=maacs)
}

# histogram by group
slide16 <- function() {
  qplot(log(eno), data=maacs, fill=mopos)
}
# Density smooth
slide17a <- function() {
  qplot(log(eno), data=maacs, geom="density")
}
slide17b <- function() {
  qplot(log(eno), data=maacs, geom="density", color=mopos)
}

# scatterplots: eNO vs PM2.5
slide18a <- function() {
  qplot(log(pm25), log(eno), data=maacs)
}
slide18b <- function() {
  qplot(log(pm25), log(eno), data=maacs, shape=mopos)
}
slide18c <- function() {
  qplot(log(pm25), log(eno), data=maacs, shape=mopos, color=mopos)
}
slide19 <- function() {
  qplot(log(pm25), log(eno), data=maacs, shape=mopos, color=mopos, geom=c('point', 'smooth'), method='lm')
}
slide20 <- function() {
  qplot(log(pm25), log(eno), 
        data=maacs, 
        shape=mopos, color=mopos, 
        geom=c('point', 'smooth'), 
        method='lm',
        facets=.~mopos)
}

slide27 <- function() {
  qplot(logpm25, NocturnalSympt, data=maacs, facets = . ~ bmicat, geom=c('point','smooth'), method='lm')
}

# First plot with point layer
slide30 <- function() {
  g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) 
  g + geom_point() 
}

# Adding More Layers: Smooth
slide31a <- function() {
  g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) 
  g + geom_point() + geom_smooth()
}
slide31b <- function() {
  g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) 
  g + geom_point() + geom_smooth(method='lm')
}

# Adding more layers: Facets
slide32 <- function() {
  g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) 
  g + geom_point() + facet_grid(. ~ bmicat) + geom_smooth(method = "lm")
}

# Modifying Aesthetics
slide34a <- function() {
  g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) 
  g + geom_point(color = 'steelblue', # constant color
                 size = 4, alpha = 1/2)  
}
slide34b <- function() {
  g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) 
  g + geom_point(aes(color = bmicat), # variable color
                 size = 4, alpha = 1/2)  
}

# Modifying labels
slide35 <- function() {
  g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) 
  g + geom_point(aes(color = bmicat)) + 
      labs(title = "MAACS Cohort")  + 
      labs(x = expression("log " * PM[2.5]), y = "Nocturnal Symptoms") 
}

# Customizing the Smooth
slide36 <- function() {
  g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) 
  g + geom_point(aes(color = bmicat), size = 2, alpha = 1/2) +
      geom_smooth(size = 1, linetype = 2, method = "lm", se = FALSE)
}

# Changing the theme
slide37 <- function() {
  g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) 

  g + geom_point(aes(color = bmicat)) + 
     theme_bw(base_family = 'Times') 
}

slide38a <- function() {
  testdat <- data.frame(x = 1:100, y = rnorm(100)) 
  testdat[50,2] <- 100  ## Outlier! 
  plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3)) 
}
slide38b <- function() {
  testdat <- data.frame(x = 1:100, y = rnorm(100)) 
  testdat[50,2] <- 100  ## Outlier! 
  g <- ggplot(testdat, aes(x = x, y = y)) 
  g + geom_line() 
}

# outlier missing
slide39a <- function() {
  testdat <- data.frame(x = 1:100, y = rnorm(100)) 
  testdat[50,2] <- 100  ## Outlier! 
  g <- ggplot(testdat, aes(x = x, y = y)) 
  
  g + geom_line() + ylim(-3,3)
}
# outlier included
slide39b <- function() {
  testdat <- data.frame(x = 1:100, y = rnorm(100)) 
  testdat[50,2] <- 100  ## Outlier! 
  g <- ggplot(testdat, aes(x = x, y = y)) 
  
  g + geom_line() + coord_cartesian(ylim = c(-3, 3))
}

## Calculate the deciles of the data
cutpoints <- quantile(maacs$logno2_new, seq(0, 1, length = 11), na.rm = TRUE)
## Cut the data at the deciles and create a new factor variable 
maacs$no2dec <- cut(maacs$logno2_new, cutpoints)

slide43 <- function(){
  ## Setup ggplot with data frame 
  g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
  
  ## Add layers 
  g + geom_point(alpha = 1/3)  + 
    facet_wrap(bmicat ~ no2dec, nrow = 5, ncol = 5) +
    geom_smooth(method="lm", se=FALSE, col="steelblue") +
    theme_bw(base_family = "Avenir", base_size = 10) +
    labs(x = expression("log " * PM[2.5])) +
    labs(y = "Nocturnal Symptoms") +
    labs(title = "MAACS Cohort")
  
}
