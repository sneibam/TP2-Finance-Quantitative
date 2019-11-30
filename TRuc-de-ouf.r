getwd()
table1 <- read.csv("Europe_3_Factors.csv", header = F)
table1 <- table1[4:354,]
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
#install.packages("xts")
library(xts)

FF3 <- read.csv("dataFF3.csv", header = FALSE)
colnames(FF3) <- c("Date", "Rates")
colnames(table1) <- c("Date", "Mkt-RF", "SMB", "HML", "RF")

## importer les donnés de taux d'interêts de la Banque Centrale Europeen 
data1 <- read.csv("data_rates.csv", header = FALSE)
data1 <- data1[-c(1:5), c(1, 9)]
colnames(data1) <- c("Date","Rates_Deposit_Facility_BCE")


#### on recoupere les prémieres 4 chiffres dont correspond à l'année et 5iéme et 6iéme - à mois 
year <- substr(table1$Date, 1, 4)
month <- substr(table1$Date, 5,6)
#### on garde ne que année et mois pour le table FF3 
FF3$Date <- substr(FF3$Date, 1,7)

FF3 <- FF3[-c(3,4,5)]
## on crée les collones "year" et "month" et "date_nouveau"(dont sera le merge de deux collones "year", "month") dans le tableau table1
table1$year <- year
table1$month <- month
table1$date_nouveau<- paste(table1$year,table1$month,sep = "-")
## on enleve les colones inutiles 
table1$Date<- NULL
table1$year <-NULL
table1$month <- NULL

## on renome la dernière colone de table1 pour avoir le méme nom avec le df FF3
colnames(table1) <- c("Mkt-RF", "SMB", "HML", "RF", "Date")

### on fait un leftjoin des dfs FF3 et table1 en gardent touts les observations dans le tableau FF3
#Data=merge(x = FF3, y = table1, by = "Date", all.x = TRUE)

FF3 <- get(load("~/Downloads/FF3.rdata"))
head(FF3)
Data <- left_join(FF3, table1, by = c("Date"="Date"))


Data$HML=as.numeric(as.character(Data$HML))
Data$SMB=as.numeric(as.character(Data$SMB))
Data$`Mkt-RF`=as.numeric(as.character(Data$`Mkt-RF`))
Data$RF=as.numeric(as.character(Data$RF))

pca <- prcomp(Data[,c("Mkt-RF", "SMB", "HML", "RF", "Rates")], center = TRUE, scale. = TRUE)
summary(Data)


#### transformer notre tableau Data dans un dataframe 
Data <- as.data.frame(Data)
## Création de notre variable target à partir de nos données rates et rf
Data <- Data %>% mutate(target = as.double(Data$Rates) - as.double(as.character(Data$RF)))


### model linéer pour vérifier la significance des variables 
linear_model <- lm(formula = target ~ `Mkt-RF`+SMB+HML,data = Data)
summary(linear_model)



#### Rates ECB


data1$Date <- as.Date(data1$Date, format = "%Y-%m-%d")
data1$Rates_Deposit_Facility_BCE <- as.numeric(as.character(data1$Rates_Deposit_Facility_BCE))

data1 <- data1 %>% group_by(Date=floor_date(Date, "month")) %>%
  summarize(Rates_Deposit_Facility_BCE=mean(Rates_Deposit_Facility_BCE))

data1 <- data1 %>% drop_na(Rates_Deposit_Facility_BCE)
data1$Date <- substr(data1$Date, 1,7)

Data <- left_join(Data, data1, by = c("Date"="Date"))

#### Exchange rates USD to Euros

data_euro_dollar <- read.csv("eurofxref-hist.csv")
data_euro_dollar <- data_euro_dollar[,1:2]

data_euro_dollar$Date <- as.Date(data_euro_dollar$Date, format = "%Y-%m-%d")
data_euro_dollar <- data_euro_dollar %>% group_by(Date=floor_date(Date, "month")) %>%
  summarize(USD=mean(USD))


data_euro_dollar <- data_euro_dollar %>% drop_na(USD)
data_euro_dollar$Date <- substr(data_euro_dollar$Date, 1,7)

Data <- left_join(Data, data_euro_dollar, by = c("Date"="Date"))

### MCOIL eu

mcoilbrenteu <- read.csv("MCOILBRENTEU.csv")
mcoilbrenteu$Date <- as.Date(mcoilbrenteu$DATE, format = "%Y-%m-%d")

mcoilbrenteu$Date <- substr(mcoilbrenteu$Date, 1,7)

Data <- left_join(Data, mcoilbrenteu[-c(1)], by = c("Date"="Date"))


### Natural gas price

natgasprice <- read.csv("natural_gas_prices.csv")
colnames(natgasprice) <- c("Date", "Natural gas price")
Data <- left_join(Data, natgasprice, by = c("Date"="Date"))


########    n'executes  pas
#table1[,1] 

#format(as.Date("192607", "%Y%m"),format = "%Y-%m-%d")
library(zoo)
as.Date(192608,origin = "1970-01-01")


