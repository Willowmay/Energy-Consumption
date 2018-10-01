install.packages("dplyr")
library(dplyr)

#read in the energy data. some cells that take a string are left blank and dont show up during an is.na search. So adding na.strings=" will make those cells appear. 
energy<- read.csv("~/Downloads/energy.csv", na.strings = "")

#look at data
energy
summary(energy)
str(energy)
head(energy)
colnames(energy)
dim(energy)
nrow(energy)
ncol(energy)
tail(energy)
View(energy)


#the column names are obnoxious and full of ...'s. we will need to rename. 
colnames(energy) [1] <- "Location"
colnames(energy) [4] <- "Primary Electricity(%)"
colnames(energy)[5] <- "Natural Gas(%)"
colnames(energy) [6] <- "Oil(%)"
colnames(energy) [7] <- "Coal(%)"
colnames(energy) [8] <- "WindWater(%)"
colnames(energy) [9] <- "Fuelwood(%)"
colnames(energy) [10] <- "Animal Power(%)"
colnames(energy) [11] <- "Human Power(%)"
#check to see if it worked: 
colnames(energy)
#it worked!!!

#dealing with missing values: identifying all columns and missing values. 
which(is.na(energy$Location))
which(is.na(energy$Code))
which(is.na(energy$Year))
which(is.na(energy$`Primary Electricity(%)`))
which(is.na(energy$`Natural Gas(%)`))
which(is.na(energy$`Oil(%)`))
which(is.na(energy$`Coal(%)`))
which(is.na(energy$`WindWater(%)`))
which(is.na(energy$`Fuelwood(%)`))
which(is.na(energy$`Animal Power(%)`))
which(is.na(energy$`Human Power(%)`))

#we need to change these missing values to 0 because of entry error and inconsistency: some cells have 0's to represent nothing, and some have blanks. 
#after imputing missing values we checked those columns
energy$`Primary Electricity(%)`[is.na(energy$`Primary Electricity(%)`)] <-0
which(is.na(energy$`Primary Electricity(%)`))
energy$`Primary Electricity(%)`

energy$`Natural Gas(%)`[is.na(energy$`Natural Gas(%)`)] <-0
which(is.na(energy$`Natural Gas(%)`))
energy$`Natural Gas(%)`

energy$`Oil(%)`[is.na(energy$`Oil(%)`)] <-0
which(is.na(energy$`Oil(%)`))
energy$`Oil(%)`

energy$`WindWater(%)`[is.na(energy$`WindWater(%)`)] <-0
which(is.na(energy$`WindWater(%)`))
energy$`WindWater(%)`

energy$`Human Power(%)`[is.na(energy$`Human Power(%)`)] <-0
which(is.na(energy$`Human Power(%)`))
energy$`Human Power(%)`


# the Code England and Wales is not present, as it was left blank in the initial document. We have made it appear as NA, and for analysis purposes we are going to assign it the code E&W. 
#first we need to change the coloumn to a character because it is currently a factor, and it errors out if we try to run the below code. . 
class(energy$Code)
energy$Code <- as.character(energy$Code)
energy$Code <- ifelse(is.na(energy$Code), 
                      'E&W', energy$Code)
unique(energy$Code)
#check to make sure it worked!
unique(energy$Code)
#note the second entry! It worked! 

View(energy)



#Q1:What is the long-term trend of Natural Gas, Oil and Coal ( non-renewables) consumption amongst countries?
library(ggplot2)
ggplot(energy, aes(Year)) + 
  geom_line(aes(y = energy$`Coal(%)`, colour = Location))
#Coal trends show a much larger rate of consumption over time, with all countries in the data having records of use starting at 1800. All country consumption trends peak between 1900 and 1950, before decreasing. 

ggplot(energy, aes(Year)) + 
  geom_line(aes(y = energy$`Oil(%)`,  colour = Location))
#Oil consumption shows an aggressive rising and falling trend between countries. The patterns demonstrate sharp increases and decreases over time. While not represented in this data, a possible explanation to these trends may be due to the price and access over time. I.E. oil prices high = lower consumption. Oil prices low = high consumption. Overall, around 2000 we can see a steady decline in oil. Additionally, these trends can be explained by the adaptation of new resources. 

ggplot(energy, aes(Year)) + 
  geom_line(aes(y = energy$`Natural Gas(%)`, colour = Location))  
#The use of natural gas did not begin until the early 1900’s, with Canada showing the earliest use. The other countries began using it more around 1950. Netherlands has the highest rate of consumption of natural gas over time, but then shows a steep decline. This may be due to the Netherlands adopting more renewable energy sources, reducing its dependency on gases. The other countries show a steady increase in usage. 


#Q2:What is the trend of Fuelwood consumption by Canada from 1800-2000?

chi <- energy %>% filter(Location == "Canada", Year < 2000)
mean(chi$`Fuelwood(%)`)
summary(chi$`Fuelwood(%)`)
qplot(chi$Year, chi$`Fuelwood(%)`) + geom_line()
#Canada’s fuelwood consumption trend begins very high in 1800, and steadily decreases over time, presumably as other, more efficient fuel sources are being exploited. It levels levels off around 1970, before maintaining steady inactivity into 2000 and beyond. 


#Q3:What percentage of human power in total energy consumption are used average in this time period for each country?

barplot(by(energy$`Human Power(%)`,energy$Location,mean), xlab = "Location", ylab = "Average Human Power %")
#Average human power usage in total energy consumption varies in different country
#We don’t have Uruguay’s human power usage information 


#Q4: What is the coal consumption trend from 1800 in Spain? 

trend <- energy %>% filter(Location== "Spain")
attach(trend)
plot(Year, `Coal(%)`)

