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
##    THe named arguments `x` and `y` are frequently used. Some may be omitted,
##    e.g. `y` if you want to make a histogram. If desired, can we also forward
##    the named variable `color` or `colour`.
##
##    Note that to actually draw the points we need to chain `geom_point()`
##    when we want to 'print' the scatterplot.
bmi_weight <- ggplot(data=patients, mapping=aes(x=BMI, y=Weight, colour=Height))

## Draw the scatterplot. Other lines similar to this will do the same.
bmi_weight + geom_point()

## 3. Using an additional geom, add an extra layer of a fit line to the previous
##    plot
##
bmi_weight + geom_point() + geom_smooth(method = "lm", colour="red")

## 4. Does the fit in the previous plot look good? Look at the help page for
##    “geom_smooth” and adjust the method to fit a straight line without
##    standard error bounds.
bmi_weight + geom_point() + geom_smooth(method = "lm", se = FALSE, colour="red")

## 6. Generate a boxplot of Score values comparing smokers and non-smokers.
##
##    Since this is a boxplot, we don't need to draw the points.
smokes_scores <- ggplot(data=patients, mapping=aes(x=Smokes, y=Score))
smokes_scores + geom_boxplot()

## 7. Split the previous boxplot into male and female groups with different
##    colors.
smokes_scores + geom_boxplot(mapping=aes(fill=Sex))

## 8. Generate a histogram of BMIs with each bar colored blue, choosing a
##    suitable bin width.
bmi <- ggplot(data=patients, mapping=aes(x=BMI))
bmi + geom_histogram(binwidth=1, fill="purple")

## 9. Instead of a histogram, generate a density plot of BMI.
bmi + geom_density()

## 10. Generate density plots of BMIs colored by Sex.
bmi + geom_density(mapping=aes(fill=Sex))

# B. Facets

## 1. Generate a scatterplot of BMI versus Weight, add a color scale to the
##    scatterplot based on the Height variable, and split the plot into a grid
##    of plots separated by Smoking status and Sex.
##
##    To split a plot into grids, use the `facet_grid()` function. It allows us
##    to form a matrix-like plot. So here we want to classify each person's
##    BMI vs. Weight point based on their Smoker and Sex statuses.
##
##    Note that the formula syntax is equivalent to passing in the named
##    arguments, e.g. `b ~ a` is the same as passing `x = a, y = b`.
bmi_weight + geom_point() + facet_grid(Sex ~ Smokes)

## 2. Generate a boxplot of BMIs comparing smokers and non-smokers, color the
##    boxplot by Sex, and include a separate facet for people of different ages.
##
##    `facet_wrap()` is similar to `facet_grid()` but it formats the output
##    plot slightly differently to optimize for rectangular displays. It works
##    best for one (1) variable with many levels.
bmi + geom_boxplot(mapping=aes(fill=Sex)) + facet_wrap(~ Age)

## 3. Produce a similar boxplot of BMIs, but this time group data by Sex, color
##    by Age, and facet by Smoking status.
bmi + geom_boxplot(mapping=aes(y=Sex, fill=Age)) # + facet_wrap(~ Sex)

# C. Scales and hemes

## 1. Generate a scatterplot of BMI versus Weight from the patients dataset.
## 2. Starting from the previous plot, adjust the BMI axis to show only labels
##    for 20, 30, 40 and the weight axis to show breaks for 60 to 100 in steps
##    of 5, adding the units (kilograms) to the axis label. Look at the help
##    page for “scale_x_continuous” and “scale_y_continuous”

## 3. Create a violin plot of BMI by Age where violins are filled using a
##    sequential color palette. Look at the help page for “scale_fill_brewer”

## 4. Create a scatterplot of BMI versus Weight and add a continuous color scale
##    for height. Make the color scale with a midpoint (set to the mean point)
##    color of grey and extremes of green (low) and red (high). Look at the help
##    page for “scale_colour_gradient2”

## 5. Recreate the scatterplot of BMI by weight this time, coloring by age
##    group, and add a straight line fit (but no standard error/confidence
##    intervals) for each age group.

## 6. Remove the legend title from the previous plot, change the background
##    colors of legend keys to white, and place the legend at the bottom of the
##    plot. Look at the help page for “theme”

## 7. Add a title to the plot and remove the minor grid lines. Save the plot to
##    a 5 by 5 inch image file.