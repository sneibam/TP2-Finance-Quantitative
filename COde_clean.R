getwd()
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(factoextra)
library(ggplot2)
library(corrplot)
library(FactoMineR)



###############################################################################
############################ .   Partie d'importation des données 


######### .  on import les donées du Fama and  French 
table1 <- read.csv("Europe_5_Factors.csv", header = F)
table1 <- table1[-c(1:3),]

## on renome la dernière colone de table1 pour avoir le méme nom avec le df FF3
colnames(table1) <- c("Date","Mkt-RF","SMB","HML","RMW","CMA","RF")

#### on recoupere les prémieres 4 chiffres dont correspond à l'année et 5iéme et 6iéme - à mois 
year <- substr(table1$Date, 1, 4)
month <- substr(table1$Date, 5,6)


## on crée les collones "year" et "month" et "date_nouveau"(dont sera le merge de deux collones "year", "month") dans le tableau table1
table1$year <- year
table1$month <- month
table1$date_nouveau<- paste(table1$year,table1$month,sep = "-")




## on enleve les colones inutiles 
table1$Date<- NULL
table1$year <-NULL
table1$month <- NULL
colnames(table1) <- c("Mkt-RF","SMB","HML","RMW","CMA","RF","Date")



################### .     importation de FF3 pour  notre fund "JPM INV-JPM EUROPE SEL EQ-A" 
########## .     
FF3 <- get(load("~/Downloads/FF3.rdata"))
head(FF3)




################## importer les donnés de taux d'interêts pour les deposites facility  de la Banque Centrale Europeen
data1 <- read.csv("data_rates.csv", header = FALSE)
data1 <- data1[-c(1:5), c(1, 9)]
colnames(data1) <- c("Date","Rates_Deposit_Facility_BCE")

################ transformer les données quotidien aux données mensuels 
data1$Date <- as.Date(data1$Date, format = "%Y-%m-%d")
data1$Rates_Deposit_Facility_BCE <- as.numeric(as.character(data1$Rates_Deposit_Facility_BCE))

data1 <- data1 %>% group_by(Date=floor_date(Date, "month")) %>%
  summarize(Rates_Deposit_Facility_BCE=mean(Rates_Deposit_Facility_BCE))

data1 <- data1 %>% drop_na(Rates_Deposit_Facility_BCE)
data1$Date <- substr(data1$Date, 1,7)



############### Exchange rates USD to Euros
data_euro_dollar <- read.csv("eurofxref-hist.csv")
data_euro_dollar <- data_euro_dollar[,1:2]

data_euro_dollar$Date <- as.Date(data_euro_dollar$Date, format = "%Y-%m-%d")
data_euro_dollar <- data_euro_dollar %>% group_by(Date=floor_date(Date, "month")) %>%
  summarize(USD=mean(USD))

data_euro_dollar <- data_euro_dollar %>% drop_na(USD)
data_euro_dollar$Date <- substr(data_euro_dollar$Date, 1,7)


################ Prix du petrol sur le marché Europeén 
mcoilbrenteu <- read.csv("MCOILBRENTEU.csv")
mcoilbrenteu$Date <- as.Date(mcoilbrenteu$DATE, format = "%Y-%m-%d")

mcoilbrenteu$Date <- substr(mcoilbrenteu$Date, 1,7)



############### Prix du gaz naturel sur le marché Europeén
natgasprice <- read.csv("natural_gas_prices.csv")
colnames(natgasprice) <- c("Date", "Natural gas price")


###################   Loans for consumption excluding revolving loans and overdrafts, Over 1 and up to 5 years
data_credit <- read.csv("data_credit.csv", header = F, sep = ",")
colnames(data_credit) <- c("Date", "Credit Rates in %")
data_credit$Date <- paste(data_credit$Date, "01")
data_credit$Date <- as.Date(data_credit$Date, format = "%Y%b%d")
data_credit$Date <- substr(data_credit$Date, 1,7)
data_credit <- data_credit[-1,]



##################  Debt Securities other than shares, excluding financial derivatives, Euro, [Millions of Euro]
######  
debt_securities <- read.csv("debt_securities.csv", header = F, sep = ",")
debt_securities <- debt_securities[-c(1:5),c(1,4)]
colnames(debt_securities) <- c("Date", "Debt Securities")
debt_securities$Date <- paste(debt_securities$Date, "01")
debt_securities$Date <- as.Date(debt_securities$Date, format = "%Y%b%d")
debt_securities$Date <- substr(debt_securities$Date, 1,7)


###################   Prix du café sur le marché Europeén 
coffee <- read.csv("coffee-prices-historical-chart-data.csv", header = F, sep = ",")
colnames(coffee) <- c("Date", "Coffee Price")
coffee <- coffee[-1,]
coffee$Date <- as.Date(coffee$Date, format = "%Y-%m-%d")

coffee$`Coffee Price` <- as.numeric(as.character(coffee$`Coffee Price`))

coffee <- coffee %>% group_by(Date=floor_date(Date, "month")) %>%
  summarize(`Coffee Price`=mean(`Coffee Price`))

coffee <- coffee %>% drop_na(`Coffee Price`)
coffee$Date <- substr(coffee$Date, 1,7)


############  Prix du sucre sur le marché Europeén 

sugar <- read.csv2("sugar-240.csv")
sugar <- sugar[-c(1,2,3),-3]

colnames(sugar)<- c("Date", "Sugar_Price")

sugar$Date <- paste(sugar$Date, "-01", sep = '')
sugar$Date <- as.Date(sugar$Date, format = "%b-%y- %d")
sugar$Date <- substr(sugar$Date, 1,7)



#############################################################################
################## .   Partie du merging le table du Fama and French avec  le table  FF3  et les autres variable explicatives 


### on fait un leftjoin des dfs FF3 et table1 en gardent touts les observations dans le tableau FF3
Data <- left_join(FF3, table1, by = c("Date"="Date"))

#### joining table taux d'interêts pour les deposites facility avec dataframe Data 
Data <- left_join(Data, data1, by = c("Date"="Date"))

#### joining table Exchange rates USD to Euros data frame Data 
Data <- left_join(Data, data_euro_dollar, by = c("Date"="Date"))

### joining  Prix du petrol sur le marché Europeén  avec Data
Data <- left_join(Data, mcoilbrenteu[-c(1)], by = c("Date"="Date"))

###    Joining  Prix du gaz naturel sur le marché Europeén avec Data 
Data <- left_join(Data, natgasprice, by = c("Date"="Date"))

##### Joining Loans for consumption avec Data
Data <- left_join(Data, data_credit, by = c("Date"="Date"))

####   Joining Debt Securities avec Data 
Data <- left_join(Data, debt_securities, by = c("Date"="Date"))

####  Joining table Prix du café avec Data
Data <- left_join(Data, coffee, by = c("Date"="Date"))

##### Joining Prix du sucre avec Data
Data <- left_join(Data, sugar, by = c("Date"="Date"))


#### transformer notre tableau Data dans un dataframe 
Data <- as.data.frame(Data)

## Création de notre variable target à partir de nos données rates et rf
Data <- Data %>% mutate(target = as.double(Data$Rates) - as.double(as.character(Data$RF)))



#### on change le format des indicateurs dans le format numeric 
Data$RMW=as.numeric(as.character(Data$RMW))
Data$CMA=as.numeric(as.character(Data$CMA))
Data$HML=as.numeric(as.character(Data$HML))
Data$SMB=as.numeric(as.character(Data$SMB))
Data$`Mkt-RF`=as.numeric(as.character(Data$`Mkt-RF`))
Data$RF=as.numeric(as.character(Data$RF))
Data$`Debt Securities`<- as.numeric(as.character(Data$`Debt Securities`))
Data$`Credit Rates in %`  =as.numeric(as.character(Data$`Credit Rates in %`))
Data$Sugar_Price=as.numeric(as.character(Data$Sugar_Price))


 ############################################################
######################    Partie de la modelisation  

res.pca <- PCA(Data[,c("Mkt-RF","SMB","HML","RMW","CMA", "target")], graph = TRUE)
print(res.pca)
#summary(res.pca)

## Eigen values

eig.val <- get_eigenvalue(res.pca)
eig.val
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
head(var$coord, 4)
fviz_pca_var(res.pca, col.var = "black")
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description de la dimension 1
res.desc$Dim.1
# Description de la dimension 2
res.desc$Dim.2


#### Linear model 3 Factors
linear_model_3Factors<-lm(target~`Mkt-RF`+SMB+HML,data=Data)
summary(linear_model_3Factors)


#### Linear model 5 Factors
linear_model_5Factors<-lm(target~`Mkt-RF`+SMB+HML+RMW+CMA,data=Data)
summary(linear_model_5Factors)


### model linéer pour vérifier la significance des variables 
linear_model <- lm(formula = target ~ `Mkt-RF` + SMB +HML + USD + Rates_Deposit_Facility_BCE + `Coffee Price` + `Debt Securities` + `Credit Rates in %` + Sugar_Price,data = Data)
summary(linear_model)





