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







