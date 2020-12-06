# load ggplot2, ggthemes, tidyr
library(ggplot2) # graph
library(ggthemes) # import pre-built themes
library(tidyr) # change the format of the data 
require(lubridate)
library(plyr)
library(devtools)
library(ggpubr)
library(scales)
library(quantmod)
library(CGPfunctions)
library(tidyverse) # metapackage of all tidyverse packages
library(ggplot2)
library(dplyr)
library(reshape2) # Melt
library(plyr)
library(scales) # visualisation
library(corrplot) # visualisation
library(GGally) # visualisation
library(ggalt) # encircle
library(maps) #maps
library(treemap)
library(ggdendro) # Dendogram
library(crosstalk)
library(plotly)
library(scales)
library(zoo)
library(lubridate)
library(egg)

# Source of the data: https://rb.gy/3kotcd
# Stacked histogram (Different Colors)
# Import data
data <- read.csv("Data_Vis_HW1_Dataset.csv")
colnames(data)
as.factor(data$Year)
as.factor(data$Month)
data$Month

# Time Series (DID NOT PUT IN THE CHART)
ggplot(data, aes(x = Year,y = National)) +
  geom_line(color = "black", 
            size=1) +
  geom_smooth() +
  labs(title = "Unemployment Rate in the United States from 2003 to 2020",
       x = "Year",
       y = "Unemployment Rate (%)") +
  theme_minimal()

############# AGGREGATE BY RACE ############
Race_stat <- data %>%
  group_by(Year, Month) %>%
  summarize(Year, Month, BM, BW, HM, HW, WM, WW)

Race_stat <- gather(Race_stat, Race, Rate, BM:WW, factor_key=TRUE)
Race_stat

############### AGGREGATE BY EDUCATION #######
Educ_stat <- data %>%
  group_by(Year,Month) %>%
  summarize(Year, Month,Less_High_School, High_School, Some_College, Bachelor)

Educ_stat <- gather(Educ_stat, Education, Rate, Less_High_School:Bachelor, factor_key=TRUE)
Educ_stat


################### RACE TRENDLINE CHART

p1 <- ggplot(data=Race_stat, aes(x=Year, y=Rate)) +  # Initialize plot 
  geom_point(aes(color = Race)) +
  geom_line(aes(group = Race), size=0.75, alpha=0.2) +
  labs(title= "Trendline of Unemployment Rate (%)\nFrom 2003 to 2020 by Each Race") +
  theme_minimal()
p1

######################### EDUCATION TRENDLINE CHART
p2 <- ggplot(data=Educ_stat, aes(x=Year, y=Rate)) +  # Initialize plot 
  geom_point(aes(color = Education)) +
  geom_line(aes(group = Education), size=0.75, alpha=0.2) +
  labs(title= "Trendline of Unemployment Rate (%) \n From 2003 to 2020 by Different Educaton Levels") +
  theme_minimal()
p2


##### RACE VIOLIN PLOT
fig(12,8)
p3 <- ggplot(Race_stat, aes(x=Race,y=Rate, fill=Race)) + 
  geom_violin()+
  geom_jitter(shape=16, size=1, position=position_jitter(0.1))+
  labs(x="",
       y="Unemployment Rate (%)", 
       title="Violin Plot of Unemployment Rate by Each Race", caption = "BM = Black Men \n BW = Black Women \n HM = Hispanic Men \n HW = Hispanic Women \n WM = White Men \n WW = White Women")+  
  theme_bw()+
  theme(plot.title = element_text(size=15)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=8),
        axis.title=element_text(size=18))
p3

########### EDUCATION BAR CHART
fig(12,8)
p4 <- ggplot(Educ_stat, aes(factor(Education), Rate)) + 
  geom_boxplot(aes(fill=factor(Education))) +
  labs(x="Education Levels",
       y="Unemployment Rate (%)", 
       fill="Race",
       title="Bar Chart of Average Unemployment Rate in America\nfrom 2003 to 2020 by Different Education Levels", caption = 'Less_High_School = Did not graduate high school\n High_School = Graduated high school but did not attend college\n Some_College = Attended college but did not graduate college\n Bachelor = graduated from college')+  
  theme_bw()+
  theme(plot.title = element_text(size=12),
        axis.text.x= element_blank(),
        axis.text.y= element_text(size=8),
        axis.title=element_text(size=18))
p4

##############################################################
library("cowplot")
ggdraw() +
  draw_plot(p1, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p2, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p3, x = 0, y = 0, width = .5, height = 0.5) +
  draw_plot(p4, x = 0.5, y = 0, width = .5, height = 0.5)



