#linear models to calculate estimates, estimates of mean difference and confidence intervals

#LIBRARIES__________----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom)
#install.packages("broom.helpers")
#___________________----

#ESTIMATES__________----
lsmodel0 <- lm(formula = height ~ 1, data = darwin)
summary(lsmodel0) #Table of coefficients - The 18.9 is the estimate of the model coefficient (in this case it is the overall mean), together with its standard erro
#The 1 indicates we want an estimate for the intercept
#The line specifies we want to analyse height a a function of an explanatory variable using '~'


lsmodel1 <- lm(height ~ type, data=darwin)
broom::tidy(lsmodel1)
summary(lsmodel1)
#___________________----


#PLOTS______________----
darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

broom::tidy(lsmodel1, conf.int=T)
#___________________----
#When i'm plotting and it shits the bed, use dev.off(), which basically resets the graphic


GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)
#This produces a graph of the estimated mean difference with an approx 95% CI

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means
#provides the mean, standard error and 95% confidence interval estimates of all levels from the model at once


#We now need to check our assumptions
#1) That the residual/unexplained variance in our data is approximately normally distributed
#2) That the residual/unexplained variance is approximately equal between our groups
# Residuals are the differences between the observed values and the fitted values produced by the model - in this case the heights of the plants against the treatment means.
# We can do this in several ways, in base R with the plot() function, and by using the performance::check_model() function.

performance::check_model(lsmodel1) #Using tidyverse


performance::check_model(lsmodel1, check=c("normality","qq"))
#Checking normal distribution with base R


plot(lsmodel1, which=c(2,2)) #base R, not tidyverse
#Plots a Q-Q graph
#A QQ plot is a classic way of checking whether a sample distribution is the same as another (or theoretical distribution).
# The qqplot distributes your data on the y-axis, and a theoretical normal distribution on the x-axis. If the residuals follow a normal distribution, they should meet to produce a perfect diagonal line across the plot.


performance::check_model(lsmodel1, check="homogeneity")
#Checking for equal variance


performance::check_model(lsmodel1, check="outliers")
#Check if and or how outliers effect the mean

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)
#Visual representation of the plant heights