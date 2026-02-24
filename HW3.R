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
     col="#008B0080")

#add Southern Hem to plot 
points(SHem$date, SHem$temperature_anomaly,
       type="b",
       pch=19,
       col="#4169E180")
legend("topleft",
       c("Northern Hemisphere","Southern Hemisphere"), 
       col=c("#008B0080","#4169E180"),
               pch=19, bty="n")

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
#question 1 - 
#find the names of the countires included 
unique(datCO2$Entity)

#create new data set for the 4 countires with the highest emissions
Highestemit <- datCO2[datCO2$Year>=1990 & datCO2$Entity=="India"|
                       datCO2$Year>=1990 & datCO2$Entity=="China"|
                       datCO2$Year>=1990 & datCO2$Entity =="United States"|
                       datCO2$Year>=1990 & datCO2$Entity=="Russia",]


#add new column to data set that converts raw billions into billions by dividing by 1 billion

Highestemit$billions <- Highestemit$CO2/1000000000

#create plot

ggplot(data = Highestemit, aes(x=Year, y=billions, color=Entity))+
  geom_line(linewidth = 1.5)+
  labs(title="CO2 Emissions Trends Since 1990 Among Today's Four Highest-Emitting Countries",
       subtitle = "China surpasses the US, while US emissions begin to decline",
       x="Year", y="Annual Emissions (billions of tons of CO2)") +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5))+
  theme_classic()+
  scale_color_manual(values=c("brown2", "#018960", "gray","royalblue"))


#question2 - communicating change in world temperature and emissions

#world emissions - create new data set

WorldCO2 <- datCO2[datCO2$Year>=1880 & datCO2$Entity=="World",]

#add column to data set for billions 
WorldCO2$billions <- WorldCO2$CO2/1000000000

#graph new data set w/ billions of tons of CO2 as y axis
ggplot(data=WorldCO2, aes(x=Year, y=billions))+
  geom_area(fill="#6A89A7")+
  labs(title="Global Carbon Dioxide Emissions Have Risen Dramatically Since 1880", x="Year",
       y="Global Emissions (billions of tons of CO2)")+
         theme_classic()
  

#part 2: graph global temperature anomaly  

#find the diffrent entities in data frame 
unique(tempanom$Entity)

#find earliest recorded day
min(tempanom$Day)

#make new data frame for World Temperatures 
WorldTemp <- tempanom[tempanom$Entity=="World",]

#create plot for the temperature anomalies 
ggplot(dat=WorldTemp, aes(x=date, y=temperature_anomaly))+
  geom_line(aes(color = temperature_anomaly))+
  geom_point(aes(color=temperature_anomaly)) + 
  scale_color_distiller(palette = "RdBu", direction = 1)+
  scale_color_gradient2(low = "royalblue", high = "red", mid="lightgray",midpoint = 0) +
  labs(title="Global Temperature Anomalies Since 1880", 
       subtitle="Anomalies showcase a long-term warming trend", 
       x="Year", y="Temperature Anomaly(°C)")+
  theme_classic()


#Question 3

#Read in Data
disaster<- read.csv("/cloud/project/activity03/number-of-natural-disaster-events.csv")

#See the different Disasters Included 
unique(disaster$Entity)


#Data Frame that filters for categories that will duplicate/double count disasters
FilteredDis <- disaster %>%
  filter(Year!=2025 & Entity!="All disasters" &
           Entity!="All disasters excluding earthquakes" &
           Entity!="All disasters excluding extreme temperature")


#Create a plot that shows total disasters by type
ggplot(dat=FilteredDis, aes(x=Year, y=Disasters, fill=Entity))+
  geom_area()+
  labs(title="Total Global Disasters", 
       x="Year", y="Number of Disasters") +
  theme_classic()

#this plot is very hard to read and inludes years that are not reliable in terms of reporting
#so I will now create a plot that is more focused

#find which disasters are most frequent
mostdis <- FilteredDis %>%
  filter(FilteredDis$Year>=2000) %>%
  group_by(Entity) %>%
  summarise(total_disaster=sum(Disasters))

#create new data frame for 5 most common disasters since 2000 
mostcommon <- FilteredDis %>%
  filter(FilteredDis$Year>=2000 & Entity=="Flood"|
           FilteredDis$Year>=2000 & Entity=="Extreme weather"|
           FilteredDis$Year>=2000 & Entity=="Earthquake"|
           FilteredDis$Year>=2000 & Entity=="Drought"|
           FilteredDis$Year>=2000 & Entity=="Extreme temperature")

#create a graph for most common disasters 

#load package with color palletes for the graph
install.packages("rcartocolor")
library(rcartocolor)

#find the Hexcodes for colors
clrs <- carto_pal(6, "Fall")

ggplot(dat=mostcommon, aes(x=Year, y=Disasters, fill=Entity))+
  geom_col()+
  labs(title="Floods and Extreme Weather Lead in Disaster Frequency", subtitle="Comparing the five most common natural disasters recorded worldwide since 2000",
       x="Year", y="Number of Disasters") +
  theme_classic()+
  scale_x_continuous(breaks = seq(2000, 2024, by = 4))+
  scale_fill_manual(values = c("Flood"= "#59788E", "Earthquake"="#5c7256",
                               "Drought"= "#e19464", "Extreme weather"="#E4deb1", 
                               "Extreme temperature"="tomato2"))









