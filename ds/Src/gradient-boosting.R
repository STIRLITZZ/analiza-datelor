library(gbm)

# Separă setul de date în antrenare și testare
set.seed(123)
indices_antrenare <- sample(1:nrow(meredes_luc), 0.8 * nrow(meredes_luc))
set_antrenare <- meredes_luc[indices_antrenare, ]
set_testare <- meredes_luc[-indices_antrenare, ]

# Definirea formulei
formula <- "price ~ mileage + tax + mpg + engineSize + year + categorizat_numeric + l_per_100km"

# Crearea modelului de gradient boosting
model_gbm <- gbm(formula, data = set_antrenare, distribution = "gaussian", n.trees = 100, interaction.depth = 3, shrinkage = 0.1)

# Afișarea rezultatelor
summary(model_gbm)

# Realizarea predicțiilor pe setul de testare
predictii_gbm <- predict(model_gbm, newdata = set_testare, n.trees = 100, type = "response")

# Calculul Mean Squared Error (MSE) pentru gradient boosting
mse_gbm <- mean((set_testare$price - predictii_gbm)^2)
cat("Mean Squared Error (Gradient Boosting):", mse_gbm, "\n")

rsquared <- 1 - (sum((set_testare$price - predictii_gbm)^2) / sum((set_testare$price - mean(set_testare$price))^2))
cat("R-squared:", rsquared, "\n")


# Setează opțiunile pentru a afișa numerele fără notație exponențială
options(scipen = 999)

# Creează un data frame cu prețurile reale și predicțiile modelului
rezultate <- data.frame(Real = set_testare$price, Predictii = predictii_gbm)

# Creează un Scatter Plot
plot(rezultate$Real, rezultate$Predictii, main = "Prețurile Reale și Predicțiile Modelului GBM",
     xlab = "Preț Real", ylab = "Predicție", col = "blue", pch = 16)

# Adaugă o linie de identitate (linie de 45 de grade)
abline(a = 0, b = 1, col = "red", lty = 2)

print(importanta_caracteristici_gbm$rel.inf)


