dataset <- read.csv('~/SESYNK LAKEVICTORIA.csv') #load data
#subset to include only the variables we need
subset<- cbind(hhid,hv002,hv009,hv024,hv201,hv202,hv204,hv205,
           hv206,hv219,hv220,hv226,hv230a,hv230b,hv232,hv235,hv236,
           hv237a,hv237b,hv237c,hv237d,hv237e,hv237f,hv237g,hv237h,
           hv237i,hv237j,hv237k,hv237x,hv237z,
          hv238,hv270)

#household head age (mean and SD)
meanage<-mean(hv220, na.rm=TRUE)
sd(dataset$hv220, na.rm=TRUE)

#household size (mean and SD)
meanhhsize<-mean(hv009, na.rm=TRUE)             
sd(dataset$hv009)

#frequencies & proportions
gender<-table(dataset$hv219) #frequency
gender
prop.table(gender)           #proportion

#electricity
electricity<-table(dataset$hv206) 
electricity
prop.table(electricity)          

#wealth index
wealth<-table(dataset$hv270) 
wealth
prop.table(wealth)          

#Toilet use
toilet<-table(dataset$hv225) 
toilet
prop.table(toilet) 

#cooking fuel
cook_fuel<-table(dataset$hv226) 
cook_fuel
prop.table(cook_fuel) 
