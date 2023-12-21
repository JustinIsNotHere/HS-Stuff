library(tidyverse)
library(readxl)
library(janitor)
data <- read_excel("R Weather Graphic Input.xlsx", sheet = "Crater Lake")
data <- data %>%
  clean_names()

past <- data %>%
  
  mutate(newDay = seq(1, length(day))) %>% # label days as 1:365 (will represent x-axis)         
  ungroup() %>%
  mutate(upper = record_high_f, # identify max value for each day
         lower = record_low_f, # identify min value for each day
         avg = (average_high_f+average_low_f)/2,  # calculate mean value for each day
         se = sd(avg)/sqrt(length(avg))) %>%  # calculate standard error of mean
  mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
         avg_lower = avg-(2.101*se))  # calculate 95% CI for mean

present <- data %>%
  
  mutate(newDay = seq(1, length(day))) %>% # create matching x-axis as historical data
  ungroup() 
# function to turn y-axis labels into degree formatted values
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

# create dataframe that represents the lowest temp for each day for the historical data
pastlows <- past %>%
  group_by(newDay) %>%
  summarise(pastlow = record_low_f) # identify lowest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
presentlows <- present %>%
  left_join(pastlows) %>%  # merge historical lows to current year low data
  mutate(record = ifelse(low_f<record_low_f, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y")  # filter for days that represent current year record lows

# create dataframe that represents the highest temp for each day for the historical data
pasthighs <- past %>%
  group_by(newDay) %>%
  summarise(pasthigh = record_high_f)  # identify highest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
presenthighs <- present %>%
  left_join(pasthighs) %>%  # merge historical highs to current year low data
  mutate(record = ifelse(high_f>=record_high_f, "Y", "N")) %>% # identifies if current year was record high
  filter(record == "Y")  # filter for days that represent current year record highs

# create y-axis variable
a <- dgr_fmt(seq(-20,100, by=10))

# create a small dataframe to represent legend symbol for 2014 Temperature
legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))

p <- ggplot(data, aes(newDay, avg)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "burlywood", alpha=10)

print(p)

p <- p + 
  geom_linerange(past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "burlywood2")

print(p)

p <- p + 
  geom_line(present, mapping=aes(x=newDay, y=high_f, group=1)) +
  geom_vline(xintercept = 0, colour = "burlywood3", linetype=1, size=1)

print(p)

p <- p + 
  geom_line(present, mapping=aes(x=newDay, y=low_f, group=1)) +
  geom_vline(xintercept = 0, colour = "burlywood4", linetype=1, size=1)

print(p)

p <- p + 
  geom_hline(yintercept = -20, colour = "gray", linetype=1) +
  geom_hline(yintercept = -10, colour = "gray", linetype=1) +
  geom_hline(yintercept = 0, colour = "gray", linetype=1) +
  geom_hline(yintercept = 10, colour = "gray", linetype=1) +
  geom_hline(yintercept = 20, colour = "gray", linetype=1) +
  geom_hline(yintercept = 30, colour = "gray", linetype=1) +
  geom_hline(yintercept = 40, colour = "gray", linetype=1) +
  geom_hline(yintercept = 50, colour = "gray", linetype=1) +
  geom_hline(yintercept = 60, colour = "gray", linetype=1) +
  geom_hline(yintercept = 70, colour = "gray", linetype=1) +
  geom_hline(yintercept = 80, colour = "gray", linetype=1) +
  geom_hline(yintercept = 90, colour = "gray", linetype=1) +
  geom_hline(yintercept = 100, colour = "gray", linetype=1)

print(p)

p <- p + 
  geom_vline(xintercept = 31, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "gray", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "gray", linetype=3, size=.5) 

print(p)

p <- p +
  coord_cartesian(ylim = c(-20,100)) +
  scale_y_continuous(breaks = seq(-20,100, by=10), labels = a) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))

print(p)

p <- p +
  geom_point(data=presentlows, aes(x=newDay, y=record_low_f), colour="blue3") +
  geom_point(data=presenthighs, aes(x=newDay, y=record_high_f), colour="firebrick3")

print(p)

p <- p +
  ggtitle("Crater Lake's Weather in 2022") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 27, y = 110, label = "Temperature", size=4, fontface="bold")

print(p)