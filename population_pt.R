#Libraries
library(data.table)
library(dplyr)
library(plyr)

#Import data base regarding portuguese population between 2009 and 2012 from https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en
population_pt <- fread("population_pt.csv")
population_pt$Value = as.numeric(gsub(",", "", population_pt$Value))


population_pt_mean <- population_pt %>% 
  select(TIME, AGE, SEX, Value) %>% 
  group_by(AGE, SEX) %>% 
  summarise_at(vars(-TIME), funs(mean(., na.rm=TRUE)))

by_gender <- population_pt_mean %>% 
  split(list(.$SEX))

pop_0_4_f = sum(by_gender$Females[c(100, 1, 12, 23, 34), 3])
pop_0_4_m = sum(by_gender$Males[c(100, 1, 12, 23, 34), 3])

pop_5_14_f = sum(by_gender$Females[c(45, 56, 67, 78, 89, 9, 2:7), 3])
pop_5_14_m = sum(by_gender$Males[c(45, 56, 67, 78, 89, 9, 2:7), 3])

pop_15_24_f = sum(by_gender$Females[c(8:11, 13:17), 3])
pop_15_24_m = sum(by_gender$Males[c(8:11, 13:17), 3])

pop_25_44_f = sum(by_gender$Females[c(18:22, 24:33, 35:39), 3])
pop_25_44_m = sum(by_gender$Males[c(18:22, 24:33, 35:39), 3])

pop_45_64_f = sum(by_gender$Females[c(40:44, 46:55, 57:61), 3])
pop_45_64_m = sum(by_gender$Males[c(40:44, 46:55, 57:61), 3])

pop_65_74_f = sum(by_gender$Females[c(62:66, 68:72), 3])
pop_65_74_m = sum(by_gender$Males[c(62:66, 68:72), 3])

pop_75_plus_f = sum(by_gender$Females[c(73:77, 79:88, 90:99, 101), 3])
pop_75_plus_m = sum(by_gender$Males[c(73:77, 79:88, 90:99, 101), 3])


pop_pt = data.frame(Age_group = c("0-4", "0-4", "5-14", "5-14", "15-24", "15-24", "25-44", "25-44", "45-64", "45-64",
                                  "65-74","65-74", "75 plus", "75 plus"), 
                    gender = rep(c("Female", "Male"), len = 14),
                    Population = c(pop_0_4_f, pop_0_4_m, pop_15_24_f, pop_15_24_m, pop_25_44_f, pop_25_44_m, pop_45_64_f, pop_45_64_m,
                                   pop_5_14_f, pop_5_14_m, pop_65_74_f, pop_65_74_m, pop_75_plus_f, pop_75_plus_m))

