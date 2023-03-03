# T-testing
# The one sample t-test: takes the mean of a sample and compares it with the null hypothesis of zero
# The two sample t-test which compares the difference between the means of two samples against a null hypothesis of no difference between the means of the two populations.

#LIBRARIES__________----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom)
#install.packages("broom.helpers")
#___________________----


#PLOT_______________----
df <- c(1:30)#Values for critical t at each degree of freedom up to 30

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

lsmodel1 <- lm(height ~ type, data = darwin)

broom::tidy(lsmodel1)

tidy_model1 <- broom::tidy(lsmodel1)

#The structure of our linear model so far has produced the output for a standard two-sample Student's t-test. However, when we first calculated our estimates by hand - we started by making an average of the paired differences in height. To generate the equivalent of a paired t-test, we simply have to add the factor for pairs to our linear model formula

darwin %>% 
  mutate(pair = as_factor(pair)) %>% 
  lm(height ~ type + pair, data = .) %>% 
  broom::tidy()
#Table of coefficients, the intercept is the height of the crossed plant from pair 1:
#The second row now compares the mean heights of Crossed and Selfed plants when they are in the same pair. Paired t-test
#Rows three to 16 compare the average difference of each pair (Crossed and Selfed combined) against pair 1

lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows
#Generates confidence intervals for the paired t-test

