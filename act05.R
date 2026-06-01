################################################################################
# Activity 05 - Data Visualization
# Jerome Poblete
# 2026 May 28
################################################################################

# A. Geom and Aesthetics
# install.packages(ggplot2)
library(ggplot2)
rm(list = ls())


## 1. Read in the cleaned patients dataset, "patient_data.csv", into a new
##    object called "patients".
patients <- read.csv("patient_data.csv")


## 2. Generate a scatterplot of BMI versus Weight using the patient dataset and
##    add a color scale based on the Height variable
##
##    `ggplot()` is the main workhorse. By default, it creates a scatterplot.
##    It directly uses the named variables `data` and `mapping`.
##
##    `aes()` lets us define which group/s to use and in which dimensions.
##    The named arguments `x` and `y` are frequently used. Some may be omitted,
##    e.g. `y` if you want to make a histogram. If desired, can we also forward
##    the named variable `color` or `colour`. All of the values to the arguemnts
##    can be "quosures", i.e. unquoted strings that represent fields of the main
##    data. E.g. we can just type `BMI` rather than `bmi_weight$data$BMI`. This
##    is primarily a feature of tidyverse, which ggplot2 is a part of.
##
##    Note that to actually draw the points we need to chain `geom_point()`
##    when we want to 'print' the scatterplot.
bmi_weight <- ggplot(patients, aes(BMI, Weight, colour=Height)) + geom_point()


## Draw the scatterplot. Other lines similar to this will do the same.
# bmi_weight


## 3. Using an additional geom, add an extra layer of a fit line to the previous
##    plot
##
##    Can't add the named argument `fill=Height` to above initialization of
##    `bmi_weight` because `geom_smooth()` drops it and warns us.
bmi_weight + geom_smooth(method="lm", colour="red")


## 4. Does the fit in the previous plot look good? Look at the help page for
##    “geom_smooth” and adjust the method to fit a straight line without
##    standard error bounds.
bmi_weight + geom_smooth(method="lm", se=FALSE, colour="red")


## 6. Generate a boxplot of Score values comparing smokers and non-smokers.
##
##    Since this is a boxplot, we don't need to draw the points.
smokes_scores <- ggplot(patients, aes(Smokes, Score))
smokes_scores + geom_boxplot()


## 7. Split the previous boxplot into male and female groups with different
##    colors.
##
##    Don't use the named argument `colour` so we can see the midlines.
smokes_scores + geom_boxplot(aes(fill=Sex))


## 8. Generate a histogram of BMIs with each bar colored blue, choosing a
##    suitable bin width.
bmi_only <- ggplot(patients, aes(BMI))
bmi_only + geom_histogram(binwidth=1, fill="blue")


## 9. Instead of a histogram, generate a density plot of BMI.
bmi_only + geom_density(fill="blue")


## 10. Generate density plots of BMIs colored by Sex.
bmi_only + geom_density(aes(colour=Sex, fill=Sex))


# B. Facets
#
## 1. Generate a scatterplot of BMI versus Weight, add a color scale to the
##    scatterplot based on the Height variable, and split the plot into a grid
##    of plots separated by Smoking status and Sex.
##
##    To split a plot into grids, use the `facet_grid()` function. It allows us
##    to form a matrix-like plot. So here we want to classify each person's
##    BMI vs. Weight point based on their Smoker and Sex statuses.
##
##    Note that the formula syntax is equivalent to passing in the named
##    arguments, e.g. `b ~ a` is the same as passing `x = a, y = b`. Read it as
##    "regress 'y' on 'x'`.
##
##    See: https://www.econometrics.blog/post/the-r-formula-cheatsheet/
bmi_weight + facet_grid(Sex ~ Smokes)


## 2. Generate a boxplot of BMIs comparing smokers and non-smokers, color the
##    boxplot by Sex, and include a separate facet for people of different ages.
##
##    `facet_wrap()` is similar to `facet_grid()` but it formats the output
##    plot slightly differently to optimize for rectangular displays. It works
##    best for one (1) variable with many levels.
bmi_smokes <- ggplot(patients, aes(BMI, Smokes))

## Don't use the named argument `colour` so we can see the midlines.
bmi_smokes + geom_boxplot(aes(fill=Sex)) + facet_wrap(~ Age)


## 3. Produce a similar boxplot of BMIs, but this time group data by Sex, color
##    by Age, and facet by Smoking status.
##
##    Note that because `Age` is a discrete (i.e. integer) variable, we need
##    to use the `factor` function. TODO: But why doesn't `Sex` require it?
bmi_smokes + geom_boxplot(aes(y=Sex, fill=factor(Age))) + facet_wrap(~ Smokes)


# C. Scales and hemes
#
## 1. Generate a scatterplot of BMI versus Weight from the patients dataset.
## 2. Starting from the previous plot, adjust the BMI axis to show only labels
##    for 20, 30, 40 and the weight axis to show breaks for 60 to 100 in steps
##    of 5, adding the units (kilograms) to the axis label. Look at the help
##    page for “scale_x_continuous” and “scale_y_continuous”
##
##    Add `fill` so we don't have to explicitly specify it later on.
##    This is very important for `scale_fill_brewer()` to work as it needs,
##    well, a fill.
bmi_weight <- bmi_weight + geom_point(aes(fill=Height))
bmi_weight +
  ## For the x-axis, omitting our `limits` still works but we just don't see
  ## the labels `20` and `40` because no value goes that far away from 30. This
  ## may be an issue of my PC display but I won't risk it. I've hardcoded the
  ## limits so all 3 axis ticks are guaranteed to appear.
  scale_x_continuous(breaks=c(20, 30, 40), limits=c(20, 40)) +

  ## The `labels` named argument for `scale_*` can be a function which takes in
  ## the entire axis, as a vector, as its sole input. For the y-axis case since
  ## we specified `breaks` the actual axis will be just that.
  scale_y_continuous(breaks=seq(60, 100, by=5), labels=function(x) {
    paste(x, rep("kg", length(x)))
  })


## 3. Create a violin plot of BMI by Age where violins are filled using a
##    sequential color palette. Look at the help page for “scale_fill_brewer”
##
##    You need to explicitly specify the fill, of course. See the named argument
##    `palette` for `scale_{colour,fill}_{brewer,distiller}()`.
bmi_age <- ggplot(patients, aes(BMI, Age, fill=factor(Age)))
bmi_age + geom_violin() + scale_fill_brewer(palette="Purples")


## 4. Create a scatterplot of BMI versus Weight and add a continuous color scale
##    for height. Make the color scale with a midpoint (set to the mean point)
##    color of grey and extremes of green (low) and red (high). Look at the help
##    page for “scale_colour_gradient2”
##
##    Since we initialized `bmit_weight` with `aes(fill=Height)` we don't
##    need to specify it further.
##
##    Note that in order for our color scale (low, mid, high) to work, we NEED
##    to supply `midpoint` otherwise it will default to 0 and all our colors
##    will use `high`.
bmi_weight +
  scale_colour_gradient2(midpoint=mean(patients$Height),
                         low="green",
                         mid="grey",
                         high="red")


## 5. Recreate the scatterplot of BMI by weight this time, coloring by age
##    group, and add a straight line fit (but no standard error/confidence
##    intervals) for each age group.
##
##    Note that since the variable `bmi_weight` was initialized with
##    `aes(..., fill=Height)`, we need to override that.
bmi_weight2 <-
  ggplot(patients,
         aes(BMI,
             Weight,
             colour=Age,
             fill=factor(Age))) +
  geom_point() +
  scale_colour_gradient2(midpoint=mean(patients$Age),
                         low="green",
                         mid="grey",
                         high="red") +
  geom_smooth(method="lm", se=FALSE)

bmi_weight2

## 6. Remove the legend title from the previous plot, change the background
##    colors of legend keys to white, and place the legend at the bottom of the
##    plot. Look at the help page for “theme”
bmi_weight2 <-
  bmi_weight2 +
  theme(legend.title=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.position="bottom")

bmi_weight2

## 7. Add a title to the plot and remove the minor grid lines. Save the plot to
##    a 5 by 5 inch image file.
bmi_weight2 <- bmi_weight2 + labs(title="BMI x Weight")
bmi_weight2


## I had to google to get this function :( But I read the documentation on my
## own and figured out which arguments to use
ggsave("BMI_Plot.png", plot=bmi_weight2, width=5, height=5, units="in")
