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

DoseCont = seq(0,12,0.1)

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

library(readr)

path="save_df_DR"
write_rds(df_DR, path)
