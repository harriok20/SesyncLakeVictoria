# 1) load data
dataset <- read.csv('~/SESYNK LAKEVICTORIA.csv', na.strings = c('NA', '')) 


# 2) subset to include only the variables we need
subset<- cbind(hhid,hv002,hv009,hv024,hv201,hv202,hv204,hv205,
           hv206,hv219,hv220,hv226,hv230a,hv230b,hv232,hv235,hv236,
           hv237a,hv237b,hv237c,hv237d,hv237e,hv237f,hv237g,hv237h,
           hv237i,hv237j,hv237k,hv237x,hv237z,
          hv238,hv270)

# 3) means and sd for multiple columns
dataset %>%
  summarise_at(vars(hv220, hv009), list(mean = mean, sd=sd), na.rm=T)

# 4) means and sd for individual columns:

# 4a) household head age (mean and SD)
meanage<-mean(hv220, na.rm=TRUE)
sd(dataset$hv220, na.rm=TRUE)

# 4b)household size (mean and SD)
meanhhsize<-mean(hv009, na.rm=TRUE)             
sd(dataset$hv009)

# 5) Frequencies & proportions
gender<-table(dataset$hv219) #frequency
gender
prop.table(gender)           #proportion

# a.electricity 
#[useNA=no tells R to not include the NAs]
electricity<-table(dataset$hv206, useNA='no')   
electricity
prop.table(electricity)          

# b.wealth index
wealth<-table(dataset$hv270) 
wealth
prop.table(wealth)          

# c.toilet use
toilet<-table(dataset$hv225) 
toilet
prop.table(toilet) 

# d.cooking fuel
cook_fuel<-table(dataset$hv226) 
cook_fuel
prop.table(cook_fuel) 

# e.source of drinking water 
drinking<-table(dataset$hv201) 
drinking
drink<-prop.table(drinking) 

# 6) Bar graphs

library (readr)
dataset2<- read.csv(file = '~/SESYNK LAKEVICTORIA.csv')
library(ggplot2)

# a. Source of drinking water
# First, lets rename one of the response options for a variable 
#[Change "river/dam/lake/ponds/stream/canal/irrigation channel" 
#to "river/lake"]

library(dplyr)
dataset3 <- dataset %>%
  mutate(hv201 = gsub('river/dam/lake/ponds/stream/canal/irrigation channel', 
                      'river/lake', hv201)) 

ggplot(dataset3, aes(hv201)) + 
  geom_bar (aes(y=(..count..)/sum
(..count..))) + xlab("Source of drinking water")+
scale_y_continuous (labels=scales::percent, name="percent") +
  theme (axis.text.x=element_text(angle=45, hjust=1))

# b.do you treat your water to make it safe?
  ggplot(dataset3, aes(hv237)) + 
    geom_bar (aes(y=(..count..)/sum (..count..))) +
    labs(title='Do you do anything to your water to make it safe for drinking?',
         x= 'Treatment of water for dinking') + 
    scale_y_continuous (labels=scales::percent, name="percent")
  theme (axis.text.x=element_text(angle=45, hjust=1))
  
# c.water treatment = boiling
ggplot(dataset2, aes(hv237a)) + 
geom_bar (aes(y=(..count..)/sum (..count..))) +
  labs(title='How do you treat your water to make it safe to drink?',
       x= 'By boiling') + 
scale_y_continuous (labels=scales::percent, name="percent")
theme (axis.text.x=element_text(angle=45, hjust=1))
  
#d. water treatment = Adding bleach/chlorine
ggplot(dataset2, aes(hv237b)) + 
  geom_bar (aes(y=(..count..)/sum (..count..))) +
  labs(title='How do you treat your water to make it safe to drink?',
       x= 'Adding bleach/chlorine') + 
 scale_y_continuous (labels=scales::percent, name="percent")
theme (axis.text.x=element_text(angle=45, hjust=1))

# e.water treatment = Straining through cloth
ggplot(dataset2, aes(hv237c)) + 
  geom_bar (aes(y=(..count..)/sum (..count..))) + 
  labs(title='How do you treat your water to make it safe to drink?',
       x= 'Straining through cloth') + 
  scale_y_continuous (labels=scales::percent, name="percent")
theme (axis.text.x=element_text(angle=45, hjust=1))

# f.water treatment = Using water filter

ggplot(dataset2, aes(hv237d)) + 
  geom_bar (aes(y=(..count..)/sum(..count..))) + 
  labs(title='How do you treat your water to make it safe to drink?',
       x= 'Using water filter')+ 
 scale_y_continuous (labels=scales::percent, name="percent")
theme (axis.text.x=element_text(angle=45, hjust=1))

# g.water treatment = Solar disinfection

ggplot(dataset2, aes(hv237e)) + 
  geom_bar (aes(y=(..count..)/sum(..count..))) + 
  labs(title='How do you treat your water to make it safe to drink?',
       x= 'Solar disinfection')+
  scale_y_continuous (labels=scales::percent, name="percent")
theme (axis.text.x=element_text(angle=45, hjust=1))

# h.water treatment = stand and settle
ggplot(dataset2, aes(hv237f)) + 
  geom_bar (aes(y=(..count..)/sum(..count..))) + 
  labs(title='How do you treat your water to make it safe to drink?',
  x= 'Allowing it to stand and settle')+
  scale_y_continuous (labels=scales::percent, name="percent")
theme (axis.text.x=element_text(angle=45, hjust=1))  

# i.water treatment = Covering the water container

ggplot(dataset2, aes(hv237g)) + 
  geom_bar (aes(y=(..count..)/sum(..count..))) + 
  labs(title='How do you treat your water to make it safe to drink?',
       x= 'Covering the water container')+
  scale_y_continuous (labels=scales::percent, name="percent")
theme (axis.text.x=element_text(angle=45, hjust=1))



#######extra coding examples##################
# get numbers for hv206 where all "no" are excluded
table(dataset$hv206, exclude='no') # excludes all "no" responses

# get number where only female in hv219
dataset %>%
  filter(hv219 == 'female') %>%
  dplyr::select(hv206)%>%
  na.omit()%>%
  group_by(hv206) %>%
  summarize(n())

dataset %>%
  distinct(hv206)

