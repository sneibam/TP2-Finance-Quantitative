# Modelisation

linear_model_3factors <- lm(formula = target ~ `Mkt-RF`+SMB+HML,data = Data)
summary(linear_model_3factors)

linear_model_5factors <- lm(formula = target ~ `Mkt-RF`+SMB+HML+RMW+CMA,data = Data)
summary(linear_model_5factors)

# Modele lineaire avec toutes nos variables
linear_model_all <- lm(formula = target ~ `Mkt-RF`+SMB+HML+RMW+CMA
                       +Rates_Deposit_Facility_BCE+USD+MCOILBRENTEU+
                        `Natural gas price`+Credit_rates+Debt_securities+
                         Coffee_price+Sugar_price+MSCI_Variation
                       + MSCI_Variation+ msci_fin_variation+msci_ind_variation+ msci_health_variation,data = Data)
summary(linear_model_all)

# Modele lineaire avec uniquement les variables significatives dans nos 2 axes
linear_model <- lm(formula = target~Coffee_price+MCOILBRENTEU+`Mkt-RF`+Sugar_price+RMW+USD, data = Data)
summary(linear_model)

linear_model_msci <- lm(formula = target ~ `Mkt-RF`+MSCI_Variation+msci_ind_variation+msci_health_variation+msci_fin_variation,data = Data)
summary(linear_model_msci)

# Metrics

metrics <- function(model, model_name) {
  return (c(model_name,summary(model)$r.squared, summary(model)$adj.r.squared, AIC(model), BIC(model)))
}

metrics_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Model", "R2", "R2 Adj", "AIC", "BIC"))
#colnames(metrics_df) <- c("Model", "R2", "R2 Adj", "AIC", "BIC")

metrics_df[nrow(metrics_df)+1,] <- metrics(linear_model_3factors, "Modele 3 facteurs")
metrics_df[nrow(metrics_df)+1,] <- metrics(linear_model_5factors, "Modele 5 facteurs")
metrics_df[nrow(metrics_df)+1,] <- metrics(linear_model_all, "Modele Multifactoriel")
metrics_df[nrow(metrics_df)+1,] <- metrics(linear_model_msci, "Modele MSCI")


Data$predictions <- fitted.values(linear_model_3factors)

rmse(Data$target, Data$predictions)



lmMod <- lm(formula = target ~ ., select(Data, -c(1,2, 8)))
selectedMod <- step(lmMod)
summary(selectedMod)
AIC(selectedMod)
BIC(selectedMod)

# 1. Stepwise, Forward Backward + Compare BIC AIC , RMSE

res <- residuals(linear_model)
par(mfrow=c(1,2))
hist(residuals(linear_model))

qqnorm(res)
qqline(res, col = 2)


mean(res**2)


data_mat <- as.matrix(Data[,-1])
M <- cor(data_mat)
corrplot(M, method = "circle")