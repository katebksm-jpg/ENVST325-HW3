#Kate Broeksmit - Activity 3

# In Class Prompts --------------------------------------------------------
#installing packages and reading data

install.packages(c("ggplot2","dplyr"))
install.packages("lubridate")
library(ggplot2)
library(dplyr)
library(lubridate)
datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

colnames(datCO2)[4]<-"CO2"

datCO2$Entity <- as.factor(datCO2$Entity)

US <- datCO2 %>%
  filter(Entity == "United States")

plot(US$Year, US$CO2, type="b",
     pch=19,
     ylab="Annual fossil fuel emissions (,billons of tons CO2)",
     xlab="Year",
     yaxt="n")

axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2),
     las=2 )


ggplot(US, aes(x=Year, y=CO2))+geom_point()+
  geom_line()+
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+theme_classic()

#create new filter
NorthA <- datCO2 %>%
  filter(Entity=="United States" |
           Entity=="Mexico"|
           Entity=="Canada")

#make plot
ggplot(NorthA, 
       aes(x=Year, y=CO2, color=Entity))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("red","royalblue","darkgoldenrod"))

#read in and date parse temperature anomoly data

tempanom <- read.csv("/cloud/project/activity03/climate-change.csv")
tempanom$date <- ymd(tempanom$Day)

unique(tempanom$Entity)

NSHemisphere <- tempanom %>%
  filter(Entity=="Northern Hemisphere"|
           Entity=="Southern Hemisphere")
#in Class prompt 1

#make plot in Base R
#create data frames for Northern and Southern Hemispheres
NHem <- tempanom[tempanom$Entity=="Northern Hemisphere",]
SHem <- tempanom[tempanom$Entity=="Southern Hemisphere",]

#create plot for Northern Hem
plot(NHem$date, NHem$temperature_anomaly,
     type="b",
     pch=19,
     ylab="Temperature Anomaly (Degrees C)",
     xlab="Year",
     col="green4")

#add Southern Hem to plot 
points(SHem$date, SHem$temperature_anomaly,
       type="b",
       pch=19,
       col="royalblue")
legend("topleft",
       c("Northern Hemisphere","Southern Hemisphere"), 
       col=c("green4","royalblue"),
               pch=19, bty="n")

#question - do I need to edit this graph (transparency?)


#make plot in GGplot
ggplot(NSHemisphere, aes(x=date, y=temperature_anomaly, color=Entity))+
  geom_point()+
  geom_line()+
  labs(x="date", y="Temperature Anomaly (degrees C)")+
  scale_color_manual(values=c("green4","royalblue"))+
  theme_classic()

#in class prompt 2 - total all time emissions
#create new data frame

Allemissions <- NorthA %>%
  filter(Entity=="United States"|Entity=="Canada"|
           Entity=="Mexico")%>%
  group_by(Entity)%>%
  summarise(total_emissions=sum(CO2))

ggplot(data=Allemissions, aes(x=Entity, y=total_emissions, fill=Entity))+
  geom_col()+
  labs(title="Distribution of All Time Emissions", x="Country",
       y="CO2 Emissions (tons CO2)")+
  theme_classic()


# Homework Prompts --------------------------------------------------------



  



