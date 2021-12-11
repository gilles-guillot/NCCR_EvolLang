library(pacman)
p_load(tidyverse,
       readxl,
       usethis,
       GGally)

dat = read_csv("./aquisition.csv")

dat %>% stack() %>% ggplot(aes(x = ind, y = values)) +
  geom_boxplot()

# flag outlier
dat = dat %>% mutate(outlier = case_when(TV.in.min >= 600 ~ TRUE,
                                         TRUE ~ FALSE ))
dat %>% filter(outlier==TRUE)

dat %>%  
  filter(outlier == FALSE) %>%
  stack() %>%
  ggplot(aes(x = ind, y = values)) +
  geom_boxplot()

dat %>% 
  ggplot(aes(x=Age.in.days,y=Aquisistion..cum.in.words.)) + 
           geom_point(size=3,alpha=.2) + 
  geom_vline(xintercept = 603) + 
  geom_vline(xintercept = 660) 

               

dat = dat %>% mutate(Words.per.Day =  Aquisistion..cum.in.words./  Age.in.days , 
                     Period = factor(case_when(Age.in.days <= 603 ~ 1,
                                        Age.in.days <= 660 ~ 2,
                                        Age.in.days > 660 ~ 3)))

dat = dat %>% mutate(Words.per.Day_period =  
                       case_when(Period == 1 ~ Aquisistion..cum.in.words./  (Age.in.days - min(Age.in.days)) ,
                                 Period == 2 ~ (Aquisistion..cum.in.words. - 57) / (Age.in.days - 603) ,
                                 Period == 3 ~ (Aquisistion..cum.in.words. - 98)/(Age.in.days - 660) ))
                                                                             
dat %>% 
  ggplot(aes(x=Age.in.days,y=Aquisistion..cum.in.words.,color=Period)) + 
  geom_point(size=5,alpha=.5) + 
  geom_vline(xintercept = 603) + 
  geom_vline(xintercept = 660) 

              

dat %>% 
  ggplot(aes(x=Age.in.days,y=Words.per.Day_period,color=Period)) + 
  geom_point(size=5,alpha=.5) + 
  geom_vline(xintercept = 603) + 
  geom_vline(xintercept = 660) 

dat %>% 
  ggplot(aes(x=Age.in.days,y=Words.per.Day)) + 
  geom_point(size=3,alpha=.2) + 
  facet_wrap(~ Period)

# dat %>% 
#   ggplot(aes(x=Age.in.days,y=Words.per.Day)) + 
#   geom_point(size=3,alpha=.2) + 
#   geom_vline(xintercept = 603) + 
#   geom_vline(xintercept = 660) 

# dat %>% 
#   ggplot(aes(x=Age.in.days,y=Words.per.Day_period,color=Period)) + 
#   geom_point(size=3,alpha=.2) + 
#   facet_wrap(~ Period)


dat %>% ggpairs()

dat = dat %>% pivot_longer(cols = ends_with("min"),names_to = "Type", values_to = "Mean.Exposure")


dat %>% ggplot(aes(x=Mean.Exposure, y=Words.per.Day)) + 
  geom_point() + 
  facet_wrap(~ Type + Period,scale="free")
  



