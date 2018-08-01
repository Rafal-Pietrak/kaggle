install.packages("tidyverse")       #data manipulating
install.packages('ggplot2')        #visualisation
install.packages('RColorBrewer')   #Color palette
install.packages('readr')          #the read_csv function
install.packages('ggfittext')
install.packages('treemapify')
install.packages("reshape2")


#zmiana języka błędów
Sys.setenv(LANG="en")


require("dplyr")
require("ggplot2")
require("RColorBrewer")
require('readr')
require('treemapify')
require("reshape2")

#data import

setwd("/Users/rafalpietrak/Programowanie w R/Sety danych")
summer <- read.table("summer.csv",header = TRUE,sep=",")
winter <- read.table("winter.csv",header=TRUE, sep=",")

#adding column with type of olympics when combining sets
summer$type <- rep("summer",nrow(summer))
winter$type <- rep("winter",nrow(winter))

#Loading dictionary file

dict <- read_csv("dictionary.csv")
head(dict)
dict$GDP <- dict$`GDP per Capita`
dict$`GDP per Capita`<- NULL

#combining sets

all <- bind_rows(summer,winter)

#----Questions----
#Questions which I would like to answer :

# 1.How many countries were present at olympics games ?
# 2.How many medals were gained throughout history ?
# 3.How Poles performed ?  

#----Answers----

#1.

summer %>% group_by(Year,Country) %>% 
  summarise(Total=n()) %>%
  ggplot(mapping=aes(x=Year,y=Total))+
  geom_point(shape=21, fill="blue", color="#56B4E9", size=1)  +
  scale_x_continuous(minor_breaks = seq(min(summer$Year) , max(summer$Year), 4), breaks = seq(min(summer$Year), max(summer$Year), 4))+theme_minimal()
  

winter %>% group_by(Year,Country) %>% 
  summarise(Total=n()) %>%
  ggplot(mapping=aes(x=Year,y=Total))+
  geom_point(shape=21, fill="green", color="#56B4E9", size=1)  +
  scale_x_continuous(minor_breaks = seq(min(winter$Year) , max(winter$Year), 4), breaks = seq(min(winter$Year), max(winter$Year), 4))+theme_minimal()


all %>% group_by(Year,Country,type) %>% 
  summarise(Total=n()) %>%
  ggplot(mapping=aes(x=Year,y=Total,colour = type))+
  geom_point(shape=21, size=1)  +
  scale_x_continuous(minor_breaks = seq(min(all$Year) , max(all$Year), 4), breaks = seq(min(all$Year), max(all$Year), 4))+theme_minimal()







n_medal <- all %>% group_by(Year,type) %>% 
  summarise(Total=n())
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#2. 

n_medal %>% 
  ggplot(mapping=aes(x=Year,y=Total,fill=type)) + geom_bar(stat="identity") +geom_text(aes(label=Total), vjust=-1)+scale_fill_brewer(palette = "Paired")+
  scale_x_continuous(minor_breaks = seq(min(n_medal$Year),max(n_medal$Year),4),breaks = seq(min(n_medal$Year), max(n_medal$Year), 4))+
  ylab(expression("vol")) + xlab(expression("Year"))+theme_minimal()+
  ggtitle("Volume of gained medals (years 1896 - 2012)")
 

#pokazuje wszystkie dostępne palety w pakiecie RColorBrewer
display.brewer.all()



#3. Now Let's check how many Polish representats were awarded with medal
#   for each type of olympics ? 

Poles_summer <- all %>% filter(Country=="POL",type=="summer") %>%  group_by(Year,type,Medal) %>% 
  summarise(Total=n())

Poles_winter <- all %>% filter(Country=="POL",type=="winter") %>%  group_by(Year,type,Medal) %>% 
  summarise(Total=n())


#Showing polish medals from summer Olympics

Poles_summer %>% 
  ggplot(mapping=aes(x=Year,y=Total,fill=Medal)) + geom_bar(stat="identity")+geom_text(aes(label=Total), size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = "Paired")+
  scale_x_continuous(minor_breaks = seq(min(Poles$Year),max(Poles$Year),4),breaks = seq(min(Poles$Year), max(Poles$Year), 4))+
  ylab(expression("vol")) + xlab(expression("Year"))+theme_minimal()+
  ggtitle("Structure of medals of Poles on summer Olympics (years 1896 - 2012)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Showing polish medals from winter Olympics


Poles_winter %>% 
  ggplot(mapping=aes(x=Year,y=Total,fill=Medal)) + geom_bar(stat="identity")+geom_text(aes(label=Total), size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = "Paired")+
  scale_x_continuous(minor_breaks = seq(min(Poles_winter$Year),max(Poles_winter$Year),4),breaks = seq(min(Poles_winter$Year), max(Poles_winter$Year), 4))+
  ylab(expression("vol")) + xlab(expression("Year"))+theme_minimal()+
  ggtitle("Structure of medals of Poles on winter Olympics (years 1896 - 2012)")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


#Looks like polish winter successes seems to occure in last for winter games,
# especially in 2012 and 2016


#But how Poland looks compared to rest of world comparing number of medals and GDP ? 
#Lets prepare tree map


#adding full country name to our data from dictionary
country_medals <- all %>% group_by(Country) %>% summarise(medals = n())
country_medals <- country_medals %>%
  mutate(Country_full = factor(Country,
                               levels=c(dict$Code), #we collect the full country names from dictionary
                               labels=c(dict$Country))) #add full country name to our database	
#adding country's Population to our combined set

country_medals <- country_medals %>% inner_join(dict,by = c("Country"="Code"))
country_medals$Population.x<- NULL
country_medals$Country.y<- NULL
colnames(country_medals) <- c("Country","medals","Country_full","Population","GDP")


#Creating new categorical variable with GDP value

country_medals$gdp_cat[country_medals$GDP > 50000] <- ">50"
country_medals$gdp_cat[country_medals$GDP > 35000 & country_medals$GDP <= 50000]  <- "35-50"
country_medals$gdp_cat[country_medals$GDP > 20000 & country_medals$GDP <= 35000] <- "20-35"
country_medals$gdp_cat[country_medals$GDP > 10000 & country_medals$GDP <= 20000] <- "10-20"
country_medals$gdp_cat[country_medals$GDP > 5000 & country_medals$GDP <= 10000] <- "5-10"
country_medals$gdp_cat[country_medals$GDP > 2000 & country_medals$GDP <= 5000] <- "2-5"
country_medals$gdp_cat[country_medals$GDP < 2000 ] <- "< 2"
country_medals$gdp_cat[is.na(country_medals$GDP)] <- "no data"


#Preparing variable for treemap

country_medals <- mutate(country_medals, Country_full = as.character(Country_full))
country_medals <- mutate(country_medals, GDP = as.factor(GDP))
country_medals <- mutate(country_medals, medals = as.numeric(medals))

country_medals$gdp_cat<- as.factor(country_medals$gdp_cat)
#Treemap with countries gained medals

country_medals$label <- paste(country_medals$Country_full, country_medals$medals, sep = ", ")


ggplot(country_medals, aes(area = medals,fill=country_medals$gdp_cat, label = label)) +
  geom_treemap() +
  geom_treemap_text(
    fontface = "italic",
    colour= "white",
    place = "centre",
    grow = TRUE
  )+scale_fill_brewer(palette = "Set2")+theme(legend.position = "bottom")+
  labs(
    title = "Countries by all medals won in history",
    caption = "The area of each tile represents the country's amount of gained medals grouped by categories of GDP", fill="GDP in k USD"
  )


# Let's check whether is a trend in number of medals compared to GDP per Capita ?

#conclusion_1: it seems that with higher GDP per Capita, amount of medals increase...
country_medals$GDP <- as.numeric(as.character(country_medals$GDP))
country_medals %>% ggplot(aes(x=as.numeric(as.character(GDP)),y=medals,na.rm=TRUE))+scale_x_continuous(breaks = c(25, 50, 75,100))+geom_point()+xlab("GDP per Capita")+
ylab("Number of medals")+geom_smooth(span=0.1,method=lm,se=T, size=2,colour="green")+theme_minimal()

# Loess-ważona regresja lokalnie wielomianowa 
# (local polynomial regression fitting) Przeprowadzana dla każdego punktu, polega na 
# wygładzeniu linii regresji w kierunku zera. 


country_medals$GDP <- as.numeric(as.character(country_medals$GDP))

country_medals %>% ggplot(aes(x=as.numeric(as.character(GDP)),y=medals,na.rm=TRUE))+scale_x_continuous(breaks = c(25, 50, 75,100))+geom_point()+xlab("GDP per Capita")+
  ylab("Number of medals")+geom_smooth(span=0.1,method=loess,se=T, size=2,colour="green")+theme_minimal()


#conclusion_2: however using loess polynomial regression fitting, amount of medals start to grow
# from specific moment 





