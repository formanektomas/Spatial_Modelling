#####################################################################
#          Introduction to ggplot2: basic plots & features
#
# Goal of this script:
# Repetition of ggplot2 basics (maps and infomaps covered separately)
# 
# https://ggplot2.tidyverse.org/
#
#####################################################################
#
library(ggplot2) 
#
#### Examples based on the Diamonds dataset from {ggplot2}
#
rm(list=ls())
?diamonds
head(diamonds)
summary(diamonds)
str(diamonds)
#
# For plot readability, we shall use only 3.000 rows of the dataset (randomly selected)
#
?sample
?base:::sample
set.seed(10) # to ensure consistent random selection for the whole class
DiamondDF <- diamonds[sample(nrow(diamonds),3000),] 
#
#####################################################################
#
# Example 1: Basic scatterplot price ~ carat
#
# [A]
# Simple plot using the R base plotting device:
plot(DiamondDF$price~DiamondDF$carat) # {graphics} package
#
# Plot repeated using {ggplot2}:
ggplot(DiamondDF, aes(x=carat,y=price))+
  # First, we create ggplot object and populate it with a data frame,
  # along with basic aesthetics parameters (what is on the x and y axes)
  geom_point() 
  # this layer defines plot type / scatterplot
#
# 
# [B]
# Adding colours - different colour for each "cut" ("Fair", "Good", ...)
ggplot(DiamondDF, aes(x=carat,y=price, colour=cut))+ # amend basic aesthetics
  geom_point()
#
# Alternatively:
ggplot(DiamondDF)+ # only the dataset is defined here
  geom_point(aes(x=carat, y=price, colour=cut)) # aes() for geom_points defined
# .. useful if different types of "lines/dots/other elements" appear on the plot
ggplot(DiamondDF)+ # only the dataset is defined here
  geom_point(aes(x=carat, y=price, colour=cut))+
  geom_smooth(aes(x=carat, y=price), colour="red", method = "lm")
#
# [C]
# Adding title to the plot
# .. Each plot feature is a "layer" 
# .. that may be expressed as additional line of code
ggplot(DiamondDF, aes(x=carat,y=price))+ # note the `+` syntax
  geom_point(aes(colour=cut))+
  ggtitle("Price ~ carat plot example") # new layer  
#
# [D] 
# Adding facets - plotting data in "tiles" - for each cut-type
ggplot(DiamondDF, aes(x=carat,y=price))+ 
  geom_point(aes(colour=cut))+
  ggtitle("Price ~ carat plot example")+
  facet_wrap(~cut) # defines facets, you can try adding: scales = "free"
#
# [E]
# Say, we want all facets in one column:
ggplot(DiamondDF, aes(x=carat,y=price))+ 
  geom_point(aes(colour=cut))+
  ggtitle("Price ~ carat plot example")+
  facet_wrap(~cut, ncol=1) 
#
## Quick assignment 1
?facet_wrap
## Using the ?facet_wrap command, how do you amend the previous plot
## so that facets are arranged into 3 rows (and corresponding number
## of columns)?
#
# [F]
# Say, we want to add linear trend to the facetted plot:
ggplot(DiamondDF, aes(x=carat,y=price))+ 
  geom_point(aes(colour=cut))+
  ggtitle("Price ~ carat plot example")+
  facet_wrap(~cut, ncol=1)+
  geom_smooth(method= "lm", colour="red") # new layer - linear trend
#
## Quick assignment 2
?geom_smooth
## Remove confidence intervals (standard error-based) from the previous plot
#
## Quick assignment 3
## How would you re-combine the facetted plot [F] into a single plot?
#
#
#####################################################################
#
#
#
# Additional ggplot2 examples:
#
#
# Histograms: 
ggplot(DiamondDF, aes(x=price)) + 
  geom_histogram()
#
# Organize histogram into 10 'bins'
ggplot(DiamondDF, aes(x=price)) + 
  geom_histogram(bins = 10)
#
# Show the composition of each bin:
ggplot(DiamondDF) +
  geom_histogram(aes(x=price, fill=cut), bins = 10)
#
#
#
# Density plots are better suited than histograms - for continuous variables:
ggplot(DiamondDF, aes(x=price, fill=cut)) +
  geom_density()
#
# We can use some transparency to distinguish between the distributions:
ggplot(DiamondDF, aes(x=price,fill=cut)) +
  geom_density(alpha=0.5)
#
# Or use coloured lines instead of fill:
ggplot(DiamondDF, aes(x=price,colour=cut)) +
  geom_density()
#
#
# Box plot
ggplot(DiamondDF, aes(x=cut, y=price, fill=cut)) + 
  geom_boxplot() +
  theme_minimal() # different "themes" are available.
# .. just an illustration of how geom_boxplot() works,
# .. the important "carat" aspect is ignored here
#
#
#
#
#