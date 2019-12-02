
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
table1 <- read.csv("C:\\Users\\eya.b\\Documents\\Eya\\Finance quantitative\\Europe_5_Factors_EUR.csv", header = F)
FF3 <- read_excel("C:\\Users\\eya.b\\Documents\\Eya\\Finance quantitative\\FF3.xlsx", sheet = "Feuil3",col_names=FALSE)
colnames(FF3) <- c("Date", "Rates")
colnames(table1) <- c("Date","Mkt-RF","SMB","HML","RMW","CMA","RF")

## importer les donnés de taux d'interêts de la Banque Centrale Europeen 
data1 <- read.csv("C:\\Users\\eya.b\\Documents\\Eya\\Finance quantitative\\data_rates.csv", header = FALSE)
data1 <- data1[-c(1:5), c(1, 9)]
colnames(data1) <- c("Date","Rates_Deposit_Facility_BCE")


####Converting columns to format Year/Month
## We get the characters that correspond to the year from the column Date
year <- substr(table1$Date, 1, 4)
## We get the characters that correspond to the month from the column Date
month <- substr(table1$Date, 5,6)
table1$year <- year
table1$month <- month
table1$date_nouveau<- paste(table1$year,table1$month,sep = "-")
## We remove non useful columns
table1$Date<- NULL
table1$year <-NULL
table1$month <- NULL
FF3$Date <- substr(FF3$Date, 1,7)
## We rename columns into 
colnames(table1) <- c("Mkt-RF","SMB","HML","RMW","CMA","RF","Date")

### Left merge
#table1$Date<-as.Date(table1$Date)
#FF3$Date<-as.Date(FF3$Date)
Data=left_join(FF3,table1,by=c("Date"="Date"))

#### transformer notre tableau Data dans un dataframe 
#Data <- as.data.frame(Data)
## Création de notre variable target à partir de nos données rates et rf
Data <- Data %>% mutate(target = as.double(Data$Rates) - as.double(as.character(Data$RF)))





### Interprétation dur l'intercept: l'intercept est singnificatif

#### Rates ECB


data1$Date <- as.Date(data1$Date, format = "%Y-%m-%d")
data1$Rates_Deposit_Facility_BCE <- as.numeric(as.character(data1$Rates_Deposit_Facility_BCE))

data1 <- data1 %>% group_by(Date=floor_date(Date, "month")) %>%
  summarize(Rates_Deposit_Facility_BCE=mean(Rates_Deposit_Facility_BCE))

data1 <- data1 %>% drop_na(Rates_Deposit_Facility_BCE)
data1$Date <- substr(data1$Date, 1,7)

Data <- left_join(Data, data1, by = c("Date"="Date"))

#### Exchange rates USD to Euros

data_euro_dollar <- read.csv("C:\\Users\\eya.b\\Documents\\Eya\\Finance quantitative\\eurofxref-hist.csv")
data_euro_dollar <- data_euro_dollar[,1:2]

data_euro_dollar$Date <- as.Date(data_euro_dollar$Date, format = "%Y-%m-%d")
data_euro_dollar <- data_euro_dollar %>% group_by(Date=floor_date(Date, "month")) %>%
  summarize(USD=mean(USD))


data_euro_dollar <- data_euro_dollar %>% drop_na(USD)
data_euro_dollar$Date <- substr(data_euro_dollar$Date, 1,7)

Data <- left_join(Data, data_euro_dollar, by = c("Date"="Date"))

### MCOIL eu .  prix du petrol sur le marché Europeen 

mcoilbrenteu <- read.csv("C:\\Users\\eya.b\\Documents\\Eya\\Finance quantitative\\MCOILBRENTEU.csv")
mcoilbrenteu$Date <- as.Date(mcoilbrenteu$DATE, format = "%Y-%m-%d")

mcoilbrenteu$Date <- substr(mcoilbrenteu$Date, 1,7)

Data <- left_join(Data, mcoilbrenteu[-c(1)], by = c("Date"="Date"))


### Prix du gaz naturel sur le marché Europeen

natgasprice <- read.csv("C:\\Users\\eya.b\\Documents\\Eya\\Finance quantitative\\natural_gas_prices.csv")
colnames(natgasprice) <- c("Date", "Natural gas price")
Data <- left_join(Data, natgasprice, by = c("Date"="Date"))


######   Loans for consumption excluding revolving loans and overdrafts, Over 1 and up to 5 years

data_credit <- read.csv("C:\\Users\\eya.b\\Documents\\Eya\\Finance quantitative\\data_credit.csv", header = TRUE, sep = ",")
colnames(data_credit) <- c("Date", "Credit Rates in %")



data_credit$Date <- paste(data_credit$Date, "01")
data_credit$Date <- as.Date(data_credit$Date, format = "%Y%b%d")
data_credit$Date <- substr(data_credit$Date, 1,7)



##################  Debt Securities other than shares, excluding financial derivatives, Euro, [Millions of Euro]
######  

debt_securities <- read.csv("C:\\Users\\eya.b\\Documents\\Eya\\Finance quantitative\\debt_securities.csv", stringsAsFactors = FALSE, header = F, sep = ",")
debt_securities <- debt_securities[-c(1:5),c(1,4)]
colnames(debt_securities) <- c("Date", "Debt Securities")

debt_securities$Date <- paste(debt_securities$Date, "01")
debt_securities$Date <- zoo::as.Date(debt_securities$Date, format = "%Y%b%d")
debt_securities$Date <- substr(debt_securities$Date, 1,7)


## Creation of our target variable target variable target= rates - rf
#Data$target= as.double(Data$Rates) - as.double(Data$RF)

### Prinipal component analysis
Data$HML=as.numeric(as.character(Data$HML))
Data$SMB=as.numeric(as.character(Data$SMB))
Data$`Mkt-RF`=as.numeric(as.character(Data$`Mkt-RF`))
Data$RF=as.numeric(as.character(Data$RF))


library("FactoMineR")
res.pca <- PCA(Data[,c("Mkt-RF","SMB","HML","RMW","CMA", "target")], graph = TRUE)
print(res.pca)

## Eigen values
library("factoextra")
library(ggplot2)
library("corrplot")
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
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
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