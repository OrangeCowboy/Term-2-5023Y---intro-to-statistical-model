#LOAD THE LIBRARIES ----
library(tidyverse)
library(kableExtra)


darwin <- read_csv ("data/darwin_raw.csv")

#___________



#READ THE DATA ----

# check the structure of the data
glimpse(darwin)

# check data is in a tidy format
head(darwin)

#__________


# clean up column names 

darwin <- janitor::clean_names(darwin)

# check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# check for typos by looking at distinct characters/values

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary

summary(darwin)


# VISUALISATION ----

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_violin()


# MATHS STUFF ----

darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))

# make a new object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

            
# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary

difference_summary %>% 
  mutate(se= sd/sqrt(n))

# The average difference in height is 2.62cm, meaning the cross breed plant, on average, grows to a taller height than the control group 2.62 ± 1.22 inches (mean ± SE).

#Create a sequence of 100 equally spaced numbers between -4 and 4
x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x)

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI
