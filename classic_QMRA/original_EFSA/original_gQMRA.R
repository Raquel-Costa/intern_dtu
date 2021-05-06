# define a working directory. The needed files should be stored in this directory
#setwd("path")

library(readxl)
library(tidyverse)
#Option 1 BLS concentration data <- inputs2.xlsx
#Option 2 US concentration data <- inputs.xlsx
#Option 3 mix BLS and US data <- inputs3.xlsx
#xfile="inputs3.xlsx"
step=0.1
DoseCont <- seq(0, 12, step)
source("original_dose_response.R") #Stochastic dose response model to be run one time
source("original_exposure_assessment.R") #full script from initial conc to number of cases

options(digits=4)
#contamfun is defined in script2
rescases=contamfun(runs=1000000,shift=0,meanTemp=5.9,
                   sdTemp=2.9,
                   Mode_prop_rtime=0.3,
                   Max_prop_rtime=1.1)
View(rescases)
sum(rescases$cases)

#make a plot for the number of expected cases
##separate path column into 3 different columns and keep the one on age and the one on gender
rescases_gender <- as.data.frame(str_split_fixed(rescases$Path, " ", 3))[c(1,2)]
names(rescases_gender) = c("gender", "Age")

###change some information to obtain a legend with the correct order in the plot that will be constructed
rescases_gender <- rescases_gender %>%
  mutate_all(funs(str_replace(., ">75", "75+")))
rescases_gender <- rescases_gender %>%
  mutate_all(funs(str_replace(., "5-14", "05-14")))
rescases_gender <- rescases_gender %>%
  mutate_all(funs(str_replace(., "1-4", "01-04")))

rescases_gender <- rescases_gender %>%
  cbind(rescases[4:5])

risk_plot <- ggplot(rescases_gender, aes(x=Age, y=risk, color=gender, group=gender))+
  geom_point()+
  geom_line()+
  labs(title = "Risk of illness per gender and age",
       y= "Risk of illness (%)")

ggplotly(risk_plot) %>% 
  layout(margin = margin(l =1),
         font=list(size = 12),
         yaxis = list(title = paste0(c(rep("&nbsp;", 2),
                                       "Risk",
                                       rep("&nbsp;", 2),
                                       rep("\n&nbsp;", 1)),
                                     collapse = "")))

ggplot(rescases_gender, aes(x=Age, y=cases, fill=gender))+
  geom_col(position = "dodge")+
  labs(title = "Number of expected cases per gender and age",
       y= "Number of expected cases")+
  guides(fill=guide_legend(title="Gender"))+
  theme(plot.title = element_text(margin = margin(10,0,10,0)),
        axis.title.x = element_text(vjust=-0.35),
        axis.title.y = element_text(vjust=1))+
  geom_text(aes(label=cases), position=position_dodge(width=0.9), vjust=-0.25)