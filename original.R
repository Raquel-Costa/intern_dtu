library(readr)
library(readxl)
library(msm)
library(tidyverse)
library(plotly)
library(mc2d)
library(knitr)
library(ggplot2)
library(readr)



#################################################
#Load the input file of the dose response relation
#################################################

path="original.xlsx"
df_DR=read_rds(path)

#################################################
#Extract inputs from xfile
#################################################

prev=read_excel("original.xlsx",sheet="prev")
conc=read_excel("original.xlsx",sheet="conc")
EGR=read_excel("original.xlsx",sheet="EGR")
ROP=read_excel("original.xlsx",sheet="ROP")
DRP=read_excel("original.xlsx",sheet="DR")
conso=read_excel("original.xlsx",sheet="conso")
size=read_excel("original.xlsx",sheet="size")
r_time=read_excel("original.xlsx",sheet="r_time")
#################################################
#################################################
#defined needed fuctions
#################################################

#Convert mean and varaiance of Y to 
#mean and std of log(Y)
convert_m_v=function(m,v){
  phi = sqrt(v + m^2)
  mu=log(m^2/phi) #mean of log(Y)  
  sigma = sqrt(log(phi^2/m^2))#
  c(mu,sigma)
}
#Primary growth model
rosso=function(time,egrm,lag=0,x0,xmax){ 
  x0=10^x0 
  xmax=10^xmax
  den=1+(xmax/x0 -1)*exp(-egrm*(time-lag))
  log10(xmax/den)
} 
#################################################
#################################################
####Start of the definition of contamfunc
####shift is argument that shifts the xmax (maximum population density)
### meanTemp and sdTemp are the parameters of the normal distribution of 
###the consumer refrigerators
###Mode_prop_rtime and Max_prop_rtime are respectively the mode and the maximum of the proportion 
###of the shelflife used as time of storage
###runs is the number of iterations
runs =4
shift =0
meanTemp=5.9
sdTemp=2.9
Mode_prop_rtime=0.3
Max_prop_rtime=1.1
contamfun=function(runs,shift=0,meanTemp=5.9,
                   sdTemp=2.9,
                   Mode_prop_rtime=0.3,
                   Max_prop_rtime=1.1) {
  ######################################
  #Initial concentration for each
  #of the 13 RTE subcategories
  ######################################
  C0fun=function(i) rbetagen(runs, 
                             shape1=conc$shape1[i], 
                             shape2=conc$shape2[i], 
                             min=conc$min[i],  
                             max=conc$max[i]+shift) 
  #the function C0fun is applied to each of the thirteen
  #RTE food categories thanks to sapply
  C0r=sapply(1:nrow(conc),C0fun)
  #C0r is a matrix with a number of lines equal to runs
  #and a number of columns eaqual to the number of RTE food category
  #dimension is runs x 13
  ######################################
  #Exponential Growth Rate
  #for each of the 13 RTE subcategories
  ######################################
  
  EGR5fun=function(i){
    m=EGR$m[i]
    s=EGR$sd[i]
    parm=convert_m_v(m,s^2)
    rtnorm(runs,mean=parm[1],sd=parm[2],
           lower=log(EGR$min[i]),upper=log(EGR$max[i])) 
  }
  
  EGR5r=exp(sapply(1:nrow(conc),EGR5fun))
  #dimension of EGR5r is runs x 13
  ######################################
  #Refrigerator Temperature
  #EGR
  ######################################
  
  Tempr=rtnorm(runs,mean=meanTemp,sd=sdTemp,
               lower=-2,upper=15)
  #dimension of Tempr is runs x 1
  Tmin=-1.18
  EGRr=EGR5r*((Tempr-Tmin)/(5-Tmin))^2
  EGRr[Tempr<Tmin]<-0
  
  #dimension of EGRr is runs x 13
  ######################################
  #Time of storage
  #
  ######################################
  
  ##########
  #remaining shelf life
  ##########
  r_timefun=function(i){
    m=r_time$m[i]
    rexp(runs,rate=1/m)
  }
  r_timer=sapply(1:length(unique(r_time$group)),r_timefun)
  #dimension of r_timer is runs x 13
  #########
  #Proportion of r_time
  #########
  
  propr=rpert(runs, min=0, mode=Mode_prop_rtime, max=Max_prop_rtime)
  #dimension of propr is runs x 1
  #########
  #s_time
  #########
  s_time=r_timer*propr
  #dimension of s_time runs x 13
  ######################################
  #Concnetration at time of consumption
  #f_conc
  ######################################
  f_concfun=function(i){
    Nmax=EGR$Nmax.mean[i]+shift
    rosso(s_time[,i],EGRr[,i],C0r[,i],lag=0,Nmax)
  }
  f_concr=sapply(1:length(unique(r_time$group)),f_concfun)
  
  #dimension of f_concr is runs x 13
  ##############################
  #Probability density function of the doses
  ##############################
  
  #pdf doses for each of the 14 sub-populations
  
  #start by collecting the portion sizes specific to each sub-population i
  doser_fun=function(i){ 
    #affect the portion size to the 13 RTE foods category
    #see table size in input file
    #in this table we have only 6 portion sizes in 
    #column 3: Smoked fish	
    #column 4: Gravad fish	
    #column 5:Cooked meat	
    #column 6:Sausage	
    #column 7:PÃ¢tÃ©	
    #column 8:Soft and semi-soft cheese
    #here we need 13 columns respectively for:
    #1-Smoked fish	ROP
    #2-Hot smoked fish	ROP
    #3-Gravad fish	ROP
    #4-Cooked meat	ROP
    #5-Sausage	ROP
    #6-PÃ¢tÃ©	ROP
    #7-Cold smoked fish	normal
    #8-Hot smoked fish	normal
    #9-Gravad fish	normal
    #10-Cooked meat	normal
    #11-Sausage	normal
    #12-PÃ¢tÃ©	normal
    #13-Soft and semi-soft cheese	normal
    #the following duplicates the needed columns to have portion sizes
    #for all the RTE food categorie
    sizer=t((size[i,3:8]))
    sizer=c(sizer[1],sizer)
    sizer=c(sizer[-7],sizer)
    
    #dimension of dosei is runs x 13
    #the same is applied for the consumption (number of eating occasions)
    consoi=t((conso[i,3:8]))
    consoi=c(consoi[1],consoi)
    consoi=c(consoi[-7],consoi)
    consoi=consoi*ROP$p*ROP$p2
    consoi=consoi/sum(consoi)
    #expected doses are calculated 
    dosei=t(t(f_concr)+log10(sizer))
    #dimension of dosei is runs x 13
    #ecdf function provide for each RTE food categories
    #the probability to observe a set of doses, see DoseCont definition
    ecdf_fun=function(j){  
      x=dosei[,j]
      pp=ecdf(x)
      pCont1=pp((DoseCont-(step/2)))
      pCont2=pp((DoseCont+(step/2)))
      pdf=(pCont2-pCont1)
      pdf[1]<-1-sum(pdf[-1])
      pdf*consoi[j]
    } 
    apply(sapply(1:13,ecdf_fun),1,sum)
  } 
  
  #Overall prevalence per population
  #for each food the function prev_fun calculate
  #the overall prevalence
  prev_fun=function(i){ 
    #determining the total eating occasion for the 13 RTE foods
    consoi=t((conso[i,3:8]))
    consoi=c(consoi[1],consoi)
    consoi=c(consoi[-7],consoi)
    #consoi is multiplied by p and p2 which reprenet respectively
    #p and p2 are extracted from the table ROP
    #p is proportion of ROP and normal pacjaging within eact RTE food category
    #p2 is proportion hot smoked and cold smoked
    #p2 is equal to 1 for the other food categories
    consoi=consoi*ROP$p*ROP$p2
    #consoi is now a proportion of consumption of one sub-category...
    consoi=consoi/sum(consoi)
    
    prevfood_fun=function(j){
      prev$S[j]/prev$N[j]*consoi[j] 
    }  
    #the overal prevalence is now calculated
    sum(sapply(1:13,prevfood_fun))
  } 
  #derivation of the total number of eating oaccasions for all the RTF categories
  teo=apply(conso[,3:8],1,sum)
  #the overal prevalence is now calculated for each subpopulation
  overall_prev=sapply(1:14,prev_fun)
  overall_prev=data_frame(prev=overall_prev,Path=DRP$Path,
                          population=DRP$population,
                          teo)
  DoseCont <- seq(0, 12, 0.1)
  step = 0.1
  #use of the pdf_dose function for all the subpopulations
  pdf_dose=sapply(1:14,doser_fun)
  #creating and organisation of a data-frame incuding the pdf of the doses
  df_pdf_dose=data.frame(pdf_dose)%>%
    gather("population",prob,1:ncol(pdf_dose))
  
  path=data_frame(population=unique(df_pdf_dose$population),Path=DRP$Path)
  df_pdf_dose=left_join(df_pdf_dose,path,key=population)
  df_pdf_dose=df_pdf_dose%>%
    group_by(Path)%>%
    mutate(cdf=cumsum(prob))
  
  #df_pdf_dose is a data frame where the vector DoseCont <- seq(0, 12, step) with step=.1
  #is repeated 14 times (subpopulations) and for each dose we attribute its probability...
  
  #df_pdf_risk contains the probability of listeriosis per case
  df_pdf_risk=df_pdf_dose
  #risk new variable in the dataframe is calculated by multiplying the probability of a dose by
  #the conditional probability of listerioisis condititional to the same dose (see script 1)
  df_pdf_risk$risk=df_pdf_dose$prob*df_DR$prob
  risk=df_pdf_risk%>%
    group_by(Path)%>%
    summarise(risk=sum(risk))
  risk=left_join(overall_prev,risk,key=Path)
  risk%>%
    mutate(risk=risk*prev,cases=round(risk*teo))
  # the output is a table with 14 lines (14 populations) including the overall prevalence
  #risk per serving, teo, and the expected number of cases
} 
####End contam function

