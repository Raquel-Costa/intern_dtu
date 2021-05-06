#clean environment
rm(list = ls())

#set working directory to where the data is saved
setwd("C:/Users/rakac/OneDrive - Universidade de Lisboa/Faculdade/6 ano/Classic QMRA/intern_dtu/classic_QMRA")

#libraries
library(readxl) #used to read data
library(dplyr)
library(tidyr) #used to get the gather function
library(mc2d) #used to get the functions to obtain random samples
library(MCMCglmm) #used to get the functions to obtain random samples
library(Jmisc)
library(ggplot2) #make plots
library(plotly) #make plots interactive
library(stringr) #separate a column with characters into multiple columns
library(ggsci) #add color palette on plots

#Serra da Estrela cheese data
prev = read_excel("serra_da_estrela_cheese.xlsx", sheet = "prevalence") #data on prevalence of L.monocytogenes in the cheese
conso = read_excel("serra_da_estrela_cheese.xlsx", sheet = "consumption") #data on eating occasions per year of the cheese
size = read_excel("serra_da_estrela_cheese.xlsx", sheet = "serving_size") #data on the size of one serving of the cheese
conc = read_excel("serra_da_estrela_cheese.xlsx", sheet = "concentration") #data on the concentration of L.monocytogenes in the cheese
EGR5 = read_excel("serra_da_estrela_cheese.xlsx", sheet = "EGR5") #data on the exponential growth rate of L.monocytogenes in cheese at 5ºC
temp = read_excel("serra_da_estrela_cheese.xlsx", sheet = "storage_temperature") #data on the domestic storage temperature of the cheese
time = read_excel("serra_da_estrela_cheese.xlsx", sheet = "storage_time") #data on the domestic storage time of the cheese
DR = read_excel("serra_da_estrela_cheese.xlsx", sheet = "dose_response") #data on the r parameter of the dose response model

ggplot(conso,aes(x=Age, y=eating_occasions_year, fill = Gender)) +
  geom_col(position="dodge") +
  labs(title = "Number of eating occasions per year per age and gender",
       y= "Number of eating occasions per year")+
  theme(plot.title = element_text(margin = margin(10,0,10,0)),
        axis.title.x = element_text(vjust=-0.35),
        axis.title.y = element_text(vjust=3))

#be able to always reproduce the same values
set.seed(1)

#Define the model variables
runs=1000000 #number of times that some functions, which get random samples, will be run
shift = 0 #variable that allows testing alternative scenarios by changing this value to a number different than zero
options(digits=4) #when needed use 4 decimal places


#Dose response model (Hazard characterization) -------------------------------------------------------------------------------------------

##function that applies the Pouillot et al. 2015 lognormal exponential dose response model
DRLNDose <- function(r, Dose, meanlog, sdlog) {
  dnorm(r, meanlog, sdlog) * ((r>=0) *1 + (r<0) * (-expm1(-10^Dose * 10^r)))
}
DR_fun <- function(Dose,meanlog,sdlog,low=-Inf,up=Inf,Print=FALSE,tol=1E-20,...){
  res <- "try-error"
  while(res=="try-error"){
    Int <- try(integrate(DRLNDose,
                         lower=low, upper=up,
                         rel.tol=tol,
                         Dose=Dose, meanlog=meanlog, sdlog=sdlog, ...),silent=TRUE)
    res <- class(Int)
    tol <- tol * 10
  }
  if(Print) print(Int)
  return(Int$value)
}

##defining the posible ingested doses from 0 to 12 with increments of 0.1
step = 0.1
DoseCont <- seq(0, 12, step)

##apply the dose response function which receives the doses defined above and the mean and the standard deviation of the r parameter
cond_risk_fun=function(i){
  sapply(DoseCont,DR_fun,meanlog=DR$Mean[i],sdlog=DR$RefSdLog[i])
}

##apply the dose response function to each population
cond_risk=sapply(1:length(unique(DR$Population)),cond_risk_fun)

##create a data frame with the probabilities of illness for each dose by population
df_DR=data.frame(cond_risk)%>%
  gather("population",prob,1:ncol(cond_risk))

##add a commun with the population information
path=data_frame(population=unique(df_DR$population),Path=DR$Path)
df_DR=left_join(df_DR,path,key=population)

##add a column with the dose information (repeat 14 times as there are 14 populations)
df_DR$DoseCont=rep(DoseCont,14)

##add a column with the gender information
df_DR$Gender=rep(c("Female","Male"),each=nrow(df_DR)/2)

print(df_DR)


##make a plot for the probability of illness by each dose
###separate path column into 3 different columns and keep the one on age and the one on gender
df_DR_gender <- as.data.frame(str_split_fixed(df_DR$Path, " ", 3))[c(1,2)]
names(df_DR_gender) = c("gender", "Age")

###change some information to obtain a legend with the correct order in the plot that will be constructed
df_DR_gender <- df_DR_gender %>% 
  mutate_all(funs(str_replace(., ">75", "75+")))
df_DR_gender <- df_DR_gender %>% 
  mutate_all(funs(str_replace(., "5-14", "05-14")))
df_DR_gender <- df_DR_gender %>% 
  mutate_all(funs(str_replace(., "1-4", "01-04")))

###add the df_DR information to the table created before with the gender and age seperatly
df_DR_gender <- df_DR_gender %>% 
  cbind(df_DR[c(2,4)])

###build a plot
dose_resp <- ggplot(df_DR_gender, aes(x = DoseCont, y = prob*100, color = Age)) +
  geom_line() +
  facet_grid(~gender)+
  theme(panel.spacing = unit(1, "lines"),
        plot.title = element_text(size=11, hjust = 0.5),
        legend.title=element_text(size=10))+
  labs(title = "Dose-response model by gender and age",
       x = "",
       y="")+
  scale_color_npg()

###make the plot interactive
ggplotly(dose_resp) %>%
  layout(margin = margin(l =1),
         font=list(size = 10),
         yaxis = list(title = paste0(c(rep("&nbsp;", 2),
                                       "Probility of illness (%)",
                                       rep("&nbsp;", 2),
                                       rep("\n&nbsp;", 1)),
                                     collapse = "")),
         xaxis = list(title = paste0(c(rep("&nbsp;", 55),
                                       "Dose (log10 CFU)",
                                       rep("&nbsp;", 2),
                                       rep("\n&nbsp;", 1)),
                                     collapse = "")))


#Exposure Assessment ----------------------------------------------------------------------------------------------------------------------
##Create function that converts the mean and variance of a number (Y) to the mean and standard deviation of log(Y)

convert_m_v=function(m,v){
  phi = sqrt(v + m^2)
  mu=log(m^2/phi) 
  sigma = sqrt(log(phi^2/m^2))
  c(mu,sigma)
}



##create a function that will run multiple times with a shift variable, to allow alternative scenario testing, equal to 0
contamfun = function(runs, shift = 0){
  

  ##--Exponential Growth Rate--
  
  #function that receives the EGR at 5ºC information and gets random values from the truncated distribution
  EGR5fun=function(i){
    m=EGR5$mean[i]
    s=EGR5$standard_deviation[i]
    parm=convert_m_v(m,s^2)
    rtnorm(runs,mean=parm[1],sd=parm[2],
           lower=log(EGR5$min[i]),upper=log(EGR5$max[i])) 
  }
  
  #apply the function above to the first and only row (because we only have 1 food product) of the data regarding the EGR at 5ºC
  EGR5r=exp(EGR5fun(1)) 
  
  
  #funtion that receives the domestic storage temperature information and gets random values from the pert distribution
  Tempfun=function(i){
    rpert(runs,mode=temp$mean[i], min=temp$min[i], max=temp$max[i])
  }
  
  #apply the function above to the first and only row (because we only have 1 food product) of the data regarding the domestic storage temperature
  Tempr=Tempfun(1)
  
  #apply the EGR formula where Tmin is -1.18 as FDA and FSIS (2003) and if the domestic storage temperature is lower than the minimum growth temperature, the growth is equal to zero
  Tmin=-1.18
  EGRr=EGR5r*((Tempr-Tmin)/(5-Tmin))^2
  EGRr[Tempr<Tmin]<-0
  
  
  
  ##--Concentration of L.monocytogenes at consumption--
  
  #function that receives the max, min and mode time for domestic storage in days and gets random values from the pert distribution
  s_timefun=function(i){
    rpert(runs, min=time$min[i], mode=time$mode[i],
          max=time$max[i])
  }
  
  #apply the function above to the first and only row (because we only have 1 food product) of the data regarding the domestic storage time
  s_time=s_timefun(1)
  
  
  #function that receives the concentration of L.monocytogenes data and gets random values from the beta distribution regarding the initial L.monocytogenes concentration
  C0fun=function(i){ 
    rbetagen(runs, shape1=conc$shape1[i],
             shape2=conc$shape2[i],
             min=conc$min[i], 
             max=conc$max[i]+shift) 
  }
  
  #apply the function above to the first and only row (because we only have 1 food product) of the data regarding the initial concentration of L.monocytogenes
  C0r=C0fun(1)
  
  
  #apply the concentration at consumption formula with the initial concentration, aximum concentration, time and EGR
  rosso=function(time,egrm,lag=0,x0,xmax){ 
    x0=10^x0 
    xmax=10^xmax
    den=1+(xmax/x0 -1)*exp(-egrm*(time-lag))
    log10(xmax/den)
  } 
  f_concfun=function(i){
    Nmax=EGR5$Nmax.mean[i]+shift
    rosso(s_time,EGRr,C0r,lag=0,Nmax)
  }
  f_concr=f_concfun(1)
  
  
  ##--Ingested dose--
  
  #function that receives the min, max and mode of a serving portion of the cheese and gets random values from the pert distribution
  serving_fun=function(i){
    rpert(runs, min=size$min, max=size$max, mode=size$mode)
  }
  
  #apply the function above to all the rows to get the serving size for each age group (the same because our data doesn't make this differentiation)
  serving = sapply(1:14, serving_fun)
  
  #calculate the prevalence of L.monocytogenes in the Serra da Estrela cheese and create a data frame with the information
  prevalence_calculus = prev[1,1]/prev[1,2]
  prevalence = data.frame(prevalence = prevalence_calculus, Path = DR$Path,
                                TEO = conso$eating_occasions_year)
  
  #calculate the probability of each expected dose (121 possible doses - from 0 to 12 by increments of 0.1) being ingested
  ecdf_fun=function(j){
    dosei=f_concr+t(log10(serving[,j]))
    pp=ecdf(dosei)
    pCont1=pp((DoseCont-(step/2)))
    pCont2=pp((DoseCont+(step/2)))
    pdf=(pCont2-pCont1)
    pdf[1]<-1-sum(pdf[-1])
    return(pdf)
  }
  prob_ingestion <- sapply(1:14, ecdf_fun)
  
  #create a data frame with the probabilities of ingested each dose but with all the populations in the same column
  df_pdf_dose= data.frame(population = rep(c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14"), each=121),
                          prob = c(prob_ingestion))
  
  #add a column with the population information
  path=data_frame(population=unique(df_pdf_dose$population),Path=DR$Path)
  df_pdf_dose=left_join(df_pdf_dose,path,key=population)
  
  #create a new column with the cumulative probabilities by population (which means rows 121, 242, 363 ... where the dose is equal to 12, the probability is equal to one)
  df_pdf_dose=df_pdf_dose%>%
    group_by(Path)%>%
    mutate(cdf=cumsum(prob))
  print(df_pdf_dose)
  
  #make a plot for the probability of ingesting each dose
  ##separate path column into 3 different columns and keep the one on age and the one on gender
  df_pdf_dose_gender <- as.data.frame(str_split_fixed(df_pdf_dose$Path, " ", 3))[c(1,2)]
  names(df_pdf_dose_gender) = c("gender", "Age")

  ###change some information to obtain a legend with the correct order in the plot that will be constructed
  df_pdf_dose_gender <- df_pdf_dose_gender %>%
    mutate_all(funs(str_replace(., ">75", "75+")))
  df_pdf_dose_gender <- df_pdf_dose_gender %>%
    mutate_all(funs(str_replace(., "5-14", "05-14")))
  df_pdf_dose_gender <- df_pdf_dose_gender %>%
    mutate_all(funs(str_replace(., "1-4", "01-04")))

  ###add the df_DR information to the table created before with the gender and age seperatly
  df_pdf_dose_gender <- df_pdf_dose_gender %>%
    cbind(df_pdf_dose[2]) %>%
    cbind(df_DR[4])

  ###build a plot
  dose_prob <- ggplot(df_pdf_dose_gender, aes(x = DoseCont, y = prob*100, color = Age)) +
    geom_line() +
    facet_grid(~gender)+
    theme(panel.spacing = unit(1, "lines"),
          plot.title = element_text(size=11, hjust = 0.5),
          legend.title=element_text(size=10))+
    labs(title = "Probability of ingesting each dose of L.monocytogenes by gender and age",
         x = "",
         y="")+
    scale_color_npg()

  ###make the plot interactive
  dose_prob_int <- ggplotly(dose_prob) %>%
    layout(margin = margin(l =1),
           font=list(size = 10),
           yaxis = list(title = paste0(c(rep("&nbsp;", 2),
                                         "Probility of ingestion (%)",
                                         rep("&nbsp;", 2),
                                         rep("\n&nbsp;", 1)),
                                       collapse = "")),
           xaxis = list(title = paste0(c(rep("&nbsp;", 55),
                                         "Dose (Log10 CFU)",
                                         rep("&nbsp;", 2),
                                         rep("\n&nbsp;", 1)),
                                       collapse = "")))
  
  
  print(dose_prob_int)


  
  ##Risk characterization-----------------------------------------------------------------------------------------------------------------------
  #create a new data frame that receives the information regarding the probability of ingesting each dose by population
  df_pdf_risk=df_pdf_dose
  
  #add a new column called risk that corresponds to the probability of ingesting each dose by population multiplied by the probability of illness for each by population dose obtained from the dose response model performed at the beginning
  df_pdf_risk$risk=df_pdf_dose$prob*df_DR$prob
  
  print(df_pdf_risk)
  
  #create a new data frame that sums all the risks for that population (calculated on the previous step) obtaining 1 risk value for each population
  risk=df_pdf_risk%>%
    group_by(Path)%>%
    summarise(risk=sum(risk))
  
  #add this information with the prevalence table created previously
  risk=left_join(prevalence,risk,key=Path)
  
  #calculate the risk as the the value obtained before times the prevalence and the number of cases as the risk times the number of eating occasions per year
  risk%>%
    mutate(risk=risk*positive_samples,cases=round(risk*TEO))
  
}



#run contamfun to obtain the final results
rescases=contamfun(runs=runs,shift=shift)

#see the number of expected cases by population
View(rescases)

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

#see the total number of expected cases
sum(rescases$cases)



