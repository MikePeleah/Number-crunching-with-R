#####################################################
######### Tajikistan - employment and wages #########
#####################################################

# set working directory
setwd("C:/Users/uyanga.gankhuyag/OneDrive - United Nations Development Programme/Data/Data crunching with R")

# load package
library(WDI)

######### Tajikistan - Migrants and Remittances #########
WDIsearch(string = c("migrant"), field = "name", short = TRUE,
          cache = NULL)
WDIsearch(string = c("remittance"), field = "name", short = TRUE,
          cache = NULL)
WDIsearch(string = c("exchange"), field = "name", short = TRUE,
          cache = NULL)

# International migrant stock, total                    SM.POP.TOTL    
# Personal remittances, received (current US$)	        BX.TRF.PWKR.CD.DT
# Official exchange rate (LCU per US$, period average)  PA.NUS.FCRF


# remittances and migrants
mig<-WDI(country=c("TJ"),indicator="SM.POP.TOTL",start=1990,end=2019,
         extra=F, cache=NULL) #   "International migrant stock, total"
rem<-WDI(country=c("TJ"),indicator="BX.TRF.PWKR.CD.DT",start=1990,end=2019,
         extra=F, cache=NULL) # Remittance inflows, current USD
xrate<-WDI(country=c("TJ"),indicator="PA.NUS.FCRF",start=1990,end=2019,
           extra=F, cache=NULL) #   "Official exchange rate (LCU per US$, period average)"

# merge remittance and exchange rate dframes
rem<-merge(rem,xrate)

# change column names
colnames(rem)[4]<-"rem.usd"
colnames(rem)[5]<-"xrate"

# calculate remittances in Tajik somoni
rem$rem.som<-rem$rem.usd*rem$xrate

# extrapolate migration in missing years
library(ggplot2)
p1 <- ggplot(mig, aes(x = year, y=SM.POP.TOTL)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0)); p1

## intrapolate migration in missing years based on UNFPA
library(zoo)
mig$pred1<-na.spline(mig$SM.POP.TOTL)

## check the intrapolation
p1 +
  geom_line(aes(y = mig$pred1), color="red")

## estimate migration in missing years based on ILO estimations
mig$ilo<-NA
mig$ilo[mig$year==2008]<-720000
mig$ilo[mig$year==2008]/mig$pred1[mig$year==2008]
mig$ilo<-mig$pred1*2.588153 # 

# merge remittance and migrant dframes
migrem<-merge(mig,rem)

# calculate average remittance per UNPFA estimate of migrants, monthly
migrem$rem.pc<-migrem$rem.som/migrem$pred1/12

# calculate average remittance per ILO estimate of migration, monthly
migrem$rem.pc2<-migrem$rem.som/migrem$ilo/12

# plot remittances
ggplot(rem, aes(x = year, y=rem.usd)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0))+
  scale_x_continuous(breaks = rem$year)
  
###############################################################
### plot wage v employment, by sectors, including migration ###
###############################################################

e=read.csv("employment wage taj.csv",sep=",",na="")
# read employment file

library(ggplot2)
# call graphing package

theme_set(theme_bw())  
# pre-set the black & white theme

ggplot(e, aes(x=log(employment), y=log(wage))) +
  geom_point() 
# basic scatterplot

ggplot(e, aes(x=log(employment), y=log(wage),label=sector)) +
  geom_label() 
# label plot

ggplot(e, aes(x=log(employment), y=log(wage),label=sector)) +
  geom_label(position=position_jitter(width=0,height=0.2)) 
# jittered label plot

ggplot(e, aes(x=log(employment), y=log(wage),label=sector)) + 
  geom_label(aes(size=5),position=position_jitter(width=0,height=0.2)) 
# labels are bigger

ggplot(e, aes(x=log(employment), y=log(wage),label=sector,fill=tradables)) + 
  geom_label(aes(size=5),position=position_jitter(width=0,height=0.2)) 
# distinguish sectors by colour

ggplot(e, aes(x=log(employment), y=log(wage),label=sector,fill=tradables)) + 
  geom_label(aes(size=5),position=position_jitter(width=0,height=0.2)) + 
  scale_fill_gradient(high="red",low="yellow")
# control the colour of sectors

ggplot(e, aes(x=log(employment), y=log(wage),label=sector,fill=tradables)) + 
  geom_label(aes(size=3),alpha=0.4, position=position_jitter(width=0,height=0.1)) + 
  scale_fill_gradient(high="red",low="yellow")+
  theme(legend.position = "none") +
  labs(title="Tajikistan employment and wage by sectors, 2018",
       y="Average monthly wage in Somoni, log scale", 
       x="Total employment, log scale")
# reduce jitter and make labels semi-transparent

ggplot(e, aes(x=log(employment), y=log(wage),label=sector,fill=tradables)) + 
  geom_label(aes(size=5), alpha=0.4, position=position_jitter(width=0,height=0.1)) + 
  scale_fill_gradient(high="red",low="yellow")+
  theme(legend.position = "none") 
# delete the legend

ggplot(e, aes(x=log(employment), y=log(wage),label=sector,fill=tradables)) + 
  geom_label(aes(size=3),alpha=0.4, position=position_jitter(width=0,height=0.05)) + 
  scale_fill_gradient(high="red",low="yellow")+
  theme(legend.position = "none") +
  labs(title="Tajikistan employment and wage by sectors",
       subtitle="2019",
       y="Average monthly wage in Somoni, log scale", 
       x="Total employment, log scale",
       caption="Source: Tajikistan Statistics Agency, World Bank WDI")
# give titles to the graph and axes
  
ggplot(e, aes(x=log(employment), y=log(wage),label=sector,fill=tradables)) + 
  geom_label(aes(size=3),alpha=0.4, position=position_jitter(width=0,height=0.05)) + 
  scale_fill_gradient(high="red",low="yellow")+
  geom_hline(yintercept=7.5)+
  geom_vline(xintercept=11)+
  theme(legend.position = "none") +
  labs(title="Tajikistan employment and wage by sectors",
       subtitle="2019",
       y="Average monthly wage in Somoni, log scale", 
       x="Total employment, log scale",
       caption="Source: Tajikistan Statistics Agency, World Bank WDI")
# separate the graph into quadrants

