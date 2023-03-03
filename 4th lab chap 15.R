# A linear model analysis estimates the values of the intercept and gradient in order to predict values of y for given values of x.
# Example data from the Australian forestry industry, recording the density and hardness of 36 samples of wood from different tree species

# LIBRARIES ____________
library(tidyverse)
library(rstatix)
library(performance)
# ______________________

janka <- read_csv ("data/janka.csv")

# PLOT _________________
janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()


# Calculating
# Correlation coefficient
# cor() does not have a data option so need to use the with() function
with(janka, cor(dens, hardness))

janka_ls1 <- lm(hardness ~ dens, data = janka) #This linear model will estimate a 'line of best fit' using the method of 'least squares' to minimise the error sums of squares (the average distance between the data points and the regression line)

# specify linear model method for line fitting

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

summary(janka_ls1) #In many ways the intercept makes more intuitive sense in a regression model than a difference model. Here the intercept describes the value of y (timber hardness) when x (wood density) = 0. The standard error is standard error of this calculated mean value. The only wrinkle here is that that value of y is an impossible value - timber hardness obviously cannot be a negative value (anti-hardness???). This does not affect the fit of our line, it just means a regression line (being an infinite straight line) can move into impossible value ranges.
#One way in which the intercept can be made more valuable is to use a technique known as 'centering'. By subtracting the average (mean) value of x from every data point, the intercept (when x is 0) can effectively be right-shifted into the centre of the data.
#_______________________









