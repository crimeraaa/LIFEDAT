##############################
# R plots
# Christian Supsup
# 28 May 2026
#############################

######################
## A. Basic plotting using these commands with internal datasets.
######################

hist(precip)  ## histogram

boxplot(PlantGrowth[[1]]~PlantGrowth[[2]]) ## boxplot

plot( x = faithful$waiting, y = faithful$eruptions) ## scatter plot

## Alternative version using "formula" syntax
plot(faithful$eruptions~faithful$waiting)
barplot(VADeaths)
barplot(VADeaths, beside = T)

######################
## B. Colors in R
######################
## There are 657 named colors in R
## Each color also has (1) number, (20) hexadecimal code, and (3) RGB value (red-green-blue)
colors()

myCol <- colors()[c(119,448)]
print(myCol)

plot(cars, col = myCol, pch = 19, cex = 3)

## Some functions to translate between color specifications
## col2rgb
lg_RGB <- col2rgb("limegreen") 
print(lg_RGB)

## rbg
lg_HEX <- rgb(t(lg_RGB),  maxColorValue = 255)
print(lg_HEX)

par(mfrow = c(2,1))
hist(rnorm(100), col = "limegreen")
hist(rnorm(100), col = lg_HEX)

######################
## C. Color palette functions
######################
## Palette functions produce a desired number of “discrete” colors across a color range specified by the palette
topo4 <- topo.colors(4)
print(topo4)

par(mfrow = c(1,3))
barplot(1:4, col = topo4, main = "4 values, 4 colors")
barplot(1:8, col = topo8, main = "8 values, 8 colors")
barplot(1:8, col = topo4, main = "8 values, 4 colors")

## Custom palletes
## Some sample data
x <- runif(100)
dat <- data.frame(x = x,y = x^2 + 1)

## Create a function to generate a continuous color palette using colorRampPalette
rbPal <- colorRampPalette(c('green','magenta'))

## This adds a column of color values based on the y values
dat$Col <- rbPal(10)[as.numeric(cut(dat$y,breaks = 10))]
head(dat)

plot(dat$x, dat$y , pch = 20, col = dat$Col, cex = 3)

######################
## D. Saving figures using code
######################

## 1. Open a new "external" graphical device, specifying filename and other parameters
## 2. Specify the file type: pdf(), jpeg(), png(), tiff(), postscript(), etc.
## 3. Create your plot as you would do with the default viewer
## 4. Close the "external" graphical device: dev.off()

## Call a plot
boxplot(PlantGrowth[[1]]~PlantGrowth[[2]], col="violetred")	

## Now export
pdf(file="plantboxes.pdf", width = 5, height = 7)
boxplot(PlantGrowth[[1]]~PlantGrowth[[2]], col="violetred")
dev.off()

## What filetype should I use?
## Raster graphics (e.g. png, jpeg)
## Vector graphics (e.g. PDF, postscript)

######################
## E. Figure regions & multiple plotting
######################
par() ## controls many details of plotting parameters

## Specify plot margins around plot area with argument
mar = c()

par(mar = c(bottom, left, top, right))
par(mar = c(5, 4, 4, 2))

## Specify outer margins around the figure area with 
oma = c()

## Multiple Plotting with par()
## Multi-panel plots can be created with mfrow or mfcol arguments to par()
par(mfrow=c(2,2), bg="white") ## set 4 plots on device

plot(iris$Sepal.Length[iris$Species == "setosa"], 
     iris$Sepal.Width[iris$Species == "setosa"], 
     pch = 15, col = "blue")

plot(iris$Sepal.Length[iris$Species == "versicolor"], 
     iris$Sepal.Width[iris$Species == "versicolor"], 
     pch = 16, col = "green")

plot (iris$Sepal.Length[iris$Species == "virginica"], 
      iris$Sepal.Width[iris$Species == "virginica"], 
      pch = 17, col = "red")

######################
## F. ggplot2
######################

https://ggplot2.tidyverse.org/articles/ggplot2.html
