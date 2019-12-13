getwd()
table1 <- read.csv("Europe_5_Factors.csv", header = F)
#table1 <- table1[4:354,]
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(hablar)
library(corrplot)
#install.packages("xts")
library(xts)

FF3 <- read.csv("dataFF3.csv", header = FALSE)
colnames(FF3) <- c("Date", "Rates")
colnames(table1) <- c("Date", "Mkt-RF", "SMB", "HML", "RMW", "CMA", "RF" )

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
colnames(table1) <- c("Mkt-RF", "SMB", "HML", "RMW", "CMA", "RF","Date")

### on fait un leftjoin des dfs FF3 et table1 en gardent touts les observations dans le tableau FF3
#Data=merge(x = FF3, y = table1, by = "Date", all.x = TRUE)

FF3 <- get(load("FF3.rdata"))
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

### Taux d'interet credit

data3 <- read.csv("data_credit.csv")
data3$Date <- paste(data3$Date, "01")
data3$Date <- as.Date(data3$Date, format = "%Y%b%d")
data3$Date <- substr(data3$Date, 1,7)
colnames(data3) <- c("Date", "Credit_rates")
Data <- left_join(Data, data3, by = c("Date"="Date"))


### Debt securities

debt_securities <- read.csv("debt_securities.csv", header = FALSE)
debt_securities <- debt_securities[-c(1:5), c(1,4)]
colnames(debt_securities) <- c("Date", "Debt_securities")

debt_securities$Date <- paste(debt_securities$Date, "01")
debt_securities$Date <- as.Date(debt_securities$Date, format = "%Y%b%d")
debt_securities$Date <- substr(debt_securities$Date, 1,7)

Data <- left_join(Data, debt_securities, by = c("Date"="Date"))


Data$Debt_securities=as.numeric(as.character(Data$Debt_securities))


# Cafe

coffee_prices <- read.csv("coffee-prices-historical-chart-data.csv", header = F, sep=",")
colnames(coffee_prices) <- c("Date", "Coffee_price")
coffee_prices <- coffee_prices[-1,]

coffee_prices$Date <- as.Date(coffee_prices$Date, format="%Y-%m-%d")

coffee_prices$Coffee_price <- as.numeric(as.character(coffee_prices$Coffee_price))

coffee_prices <- coffee_prices %>% group_by(Date=floor_date(Date, "month")) %>%
  summarize(Coffee_price=mean(Coffee_price))


coffee_prices <- coffee_prices %>% drop_na(Coffee_price)
coffee_prices$Date <- substr(coffee_prices$Date, 1,7)

Data <- left_join(Data, coffee_prices, by = c("Date"="Date"))

## Sugar Price
sugar_prices <- read.csv("sugar-240.csv", header = F, sep=";")
sugar_prices <- sugar_prices[4:242,1:2]
colnames(sugar_prices) <- c("Date", "Sugar_price")
sugar_prices$Sugar_price <- as.numeric(as.character(sugar_prices$Sugar_price))
sugar_prices$Date <- paste(sugar_prices$Date, "-01", sep="")
sugar_prices$Date <- as.Date(sugar_prices$Date, format="%b-%y-%d")
sugar_prices$Date <- substr(sugar_prices$Date, 1,7)
Data <- left_join(Data, sugar_prices, by = c("Date"="Date"))


## MSCI Europe

msci_europe <- read.csv("MSCI.csv", header = F, sep = ",")
msci_europe <- msci_europe[-1, c(1, 7)]
colnames(msci_europe) <- c("Date", "MSCI_Variation")
msci_europe$MSCI_Variation <- as.character(msci_europe$MSCI_Variation)
msci_europe$MSCI_Variation <- substr(msci_europe$MSCI_Variation, 1, nchar(msci_europe$MSCI_Variation)-1)
msci_europe$MSCI_Variation <- gsub(",", ".", msci_europe$MSCI_Variation)
msci_europe$MSCI_Variation <- as.numeric(msci_europe$MSCI_Variation)

msci_europe$Date <- paste(msci_europe$Date, "01")
msci_europe$Date <- as.Date(msci_europe$Date, format="%b %y %d")
msci_europe$Date <- substr(msci_europe$Date, 1,7)

Data <- left_join(Data, msci_europe, by = c("Date"="Date"))

# MSCI financial services
msci_fin <- read.csv("MSCI Europe Financials Price.csv", header = F, sep = ",")
msci_fin <- msci_fin[-1, c(1, 7)]
colnames(msci_fin) <- c("Date", "msci_fin_variation")
msci_fin$msci_fin_variation <- as.character(msci_fin$msci_fin_variation)
msci_fin$msci_fin_variation <- substr(msci_fin$msci_fin_variation, 1, nchar(msci_fin$msci_fin_variation)-1)
msci_fin$msci_fin_variation <- gsub(",", ".", msci_fin$msci_fin_variation)
msci_fin$msci_fin_variation <- as.numeric(msci_fin$msci_fin_variation)

msci_fin$Date <- paste(msci_fin$Date, "01")
msci_fin$Date <- as.Date(msci_fin$Date, format="%b %y %d")
msci_fin$Date <- substr(msci_fin$Date, 1,7)

Data <- left_join(Data, msci_fin, by = c("Date"="Date"))

# MSCI industrial services
msci_ind <- read.csv("MSCI Europe Industrials EUR Historical Data.csv", header = F, sep = ",")
msci_ind <- msci_ind[-1, c(1, 7)]
colnames(msci_ind) <- c("Date", "msci_ind_variation")
msci_ind$msci_ind_variation <- as.character(msci_ind$msci_ind_variation)
msci_ind$msci_ind_variation <- substr(msci_ind$msci_ind_variation, 1, nchar(msci_ind$msci_ind_variation)-1)
msci_ind$msci_ind_variation <- gsub(",", ".", msci_ind$msci_ind_variation)
msci_ind$msci_ind_variation <- as.numeric(msci_ind$msci_ind_variation)

msci_ind$Date <- paste(msci_ind$Date, "01")
msci_ind$Date <- as.Date(msci_ind$Date, format="%b %y %d")
msci_ind$Date <- substr(msci_ind$Date, 1,7)

Data <- left_join(Data, msci_ind, by = c("Date"="Date"))

# MSCI healthcare services
msci_health <- read.csv("MSCI Europe Health Care EUR Historical Data.csv", header = F, sep = ",")
msci_health <- msci_health[-1, c(1, 7)]
colnames(msci_health) <- c("Date", "msci_health_variation")
msci_health$msci_health_variation <- as.character(msci_health$msci_health_variation)
msci_health$msci_health_variation <- substr(msci_health$msci_health_variation, 1, nchar(msci_health$msci_health_variation)-1)
msci_health$msci_health_variation <- gsub(",", ".", msci_health$msci_health_variation)
msci_health$msci_health_variation <- as.numeric(msci_health$msci_health_variation)

msci_health$Date <- paste(msci_health$Date, "01")
msci_health$Date <- as.Date(msci_health$Date, format="%b %y %d")
msci_health$Date <- substr(msci_health$Date, 1,7)

Data <- left_join(Data, msci_health, by = c("Date"="Date"))

# Nestle SA

nestle <- read.csv("Nestle.csv", header = F, sep = ",")
nestle <- nestle[-1, c(1, 7)]
colnames(nestle) <- c("Date", "Nestle_share_price_variation")
nestle$Nestle_share_price_variation <- as.character(nestle$Nestle_share_price_variation)
nestle$Nestle_share_price_variation <- substr(nestle$Nestle_share_price_variation, 1, nchar(nestle$Nestle_share_price_variation)-1)
nestle$Nestle_share_price_variation <- gsub(",", ".", nestle$Nestle_share_price_variation)
nestle$Nestle_share_price_variation <- as.numeric(nestle$Nestle_share_price_variation)

nestle$Date <- paste(nestle$Date, "01")
nestle$Date <- as.Date(nestle$Date, format="%b %y %d")
nestle$Date <- substr(nestle$Date, 1,7)

Data <- left_join(Data, nestle, by = c("Date"="Date"))

Data <- na.omit(Data)

#Data <- Data %>% drop_na(MSCI_Variation)
#Data <- Data %>% drop_na(EuroStoxx_Variation)

# ACP

pca=prcomp(Data[,c("target","Mkt-RF", "SMB", "HML", "RMW", "CMA", 
                   "Rates_Deposit_Facility_BCE", "USD", "MCOILBRENTEU", "Natural gas price", 
                   "Debt_securities", "Credit_rates", "Coffee_price", "Sugar_price", "MSCI_Variation",
                   "Nestle_share_price_variation", "msci_fin_variation", "msci_ind_variation", "msci_health_variation")], center=TRUE, scale. = TRUE)
summary(pca)

library("FactoMineR")
res.pca <- PCA(Data[,c("SMB", "HML", "CMA", 
                       "RMW", "MCOILBRENTEU", "Coffee_price",
                       "Sugar_price","Debt_securities",
                       "Rates_Deposit_Facility_BCE", "MSCI_Variation",
                       "msci_ind_variation")], graph = TRUE)
print(res.pca)


## Eigen values
library("factoextra")
library(ggplot2)
library("corrplot")

M <- cor(as.matrix(Data[,colnames(Data) %in% c("SMB", "HML", "CMA", 
                                               "RMW", "MCOILBRENTEU", "Coffee_price",
                                               "Sugar_price","Debt_securities",
                                               "Rates_Deposit_Facility_BCE", 
                                               "msci_ind_variation")]))
corrplot(M)


eig.val <- get_eigenvalue(res.pca)
eig.val
# Interpretation: Le critère de Kaiser (1961) nous indique que si la valeur propre est supérieure à 1, la composante principale concernée représente plus de variance par rapport à une seule variable d’origine. On remarque que les valeurs absolues qui expliques les trois prmeieres dimensions sont representées par supérieures à 1. 
## Eigen values plot
eig.viz<-fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
eig.viz
## Variables extraction from PCA
var <- get_pca_var(res.pca)
var
# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)

## Cercle de corrélation
# Coordonnées des variables
head(var$coord, 10)
fviz_pca_var(res.pca, col.var = "black")
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:3)
# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
# Les varaibles MSCI_industry, Credit_rates, MSCI_financial_services, MSCI_Variation, USD,  Natural_gas price sont celles qui contribuent le plus a la variance expliquee des 2 premiers axes.
# Nous allons donc garder uniquement ces variables pour la modelisation
fviz_pca_var(res.pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description de la dimension 1
res.desc$Dim.1
# Description de la dimension 2
res.desc$Dim.2


