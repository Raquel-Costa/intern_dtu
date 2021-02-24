######################################
#defined functions
######################################

#Stochastic dose response model as in Pouillot et al. (2015)
DRLNDose <- function(r, Dose, meanlog, sdlog) {
  dnorm(r, meanlog, sdlog) * ((r>=0) *1 + (r<0) * (-expm1(-10^Dose * 10^r)))
}
DR <- function(Dose,meanlog,sdlog,low=-Inf,up=Inf,Print=FALSE,tol=1E-20,...){
  #This function provide the marginal prob of invasive listeriosis
  #in a given population and for a given Dose  in log 10 cfu.
  #the default use the parameters stored in DR sheet of the input Excel files
  
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
######################################
#Conditional dose response
#per age and gender(14 subpopulations)
######################################

DRP=read_excel("original.xlsx",sheet="DR")
#step=0.1
#DoseCont <- seq(0, 12, step)
cond_risk_fun=function(i){
  sapply(DoseCont,DR,meanlog=DRP$Mean[i],sdlog=DRP$RefSdLog[i])
}
cond_risk=sapply(1:length(unique(DRP$population)),cond_risk_fun)

df_DR=data.frame(cond_risk)%>%
  gather("population",prob,1:ncol(cond_risk))

path=data_frame(population=unique(df_DR$population),Path=DRP$Path)
df_DR=left_join(df_DR,path,key=population)
df_DR$DoseCont=rep(DoseCont,14)
df_DR$Gender=rep(c("Female","Male"),each=nrow(df_DR)/2)
library(readr)

path="save_df_DR"
write_rds(df_DR, path)
