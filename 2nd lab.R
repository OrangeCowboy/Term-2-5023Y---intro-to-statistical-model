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

summary(janka_ls1)


dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()
#This centres the x values then fits a new linear model.

confint(janka_ls1)

predict(janka_ls1)

resid(janka_ls1)


augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")
#Plotted wih black fitted regression line and red dashed lines representing the residuals

# A line connecting all the data points in order 
p1 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_ls1 %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")


library(patchwork)
p1+p2+p3


#The following code is a quicker function
model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

plot(janka_ls1, which=c(2,2))
#normal distribution base R

plot(janka_ls1, which=c(1,3)) #Equal variance
#performance::check_model(janka_ls1, check="homogeneity") in tidyverse

coef(janka_ls1) #Prediction

predict(janka_ls1, newdata=list(dens=c(22,35,65))) #base r
broom::augment(janka_ls1, 
               newdata=tibble(dens=c(22,35,65))) #tidyverse

#adding confidence intervals
broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)
#standard error

broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")
#95% confidence intervals

#I really like the emmeans package - it is very good for producing quick predictions for categorical data - it can also do this for continuous variables. By default it will produce a single mean-centered prediction. But a list can be provided - it will produce confidence intervals as standard.
emmeans::emmeans(janka_ls1, 
                 specs = "dens", 
                 at = list(dens = c(22, 35, 65)))

pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))


#Linear model analyses can extend beyond testing differences of means in categorical groupings to test relationships with continuous variables. This is known as linear regression, where the relationship between the explanatory variable and response variable are modelled with the equation for a straight line. The intercept is the value of y when x = 0, often this isn't that useful, and we can use 'mean-centered' values if we wish to make the intercept more intuitive. As with all linear models, regression assumes that the unexplained variability around the regression line, is normally distributed and has constant variance.
#Once the regression has been fitted it is possible to predict values of y from values of x, the uncertainty around these predictions can be captured with confidence intervals.





