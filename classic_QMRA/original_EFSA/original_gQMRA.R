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
