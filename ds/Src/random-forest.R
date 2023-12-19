# Separă setul de date în antrenare și testare
set.seed(123)  # Pentru reproducibilitate
indices_antrenare <- sample(1:nrow(meredes_luc), 0.8 * nrow(meredes_luc))
set_antrenare <- meredes_luc[indices_antrenare, ]
set_testare <- meredes_luc[-indices_antrenare, ]

library(randomForest)

# Definirea formulei
formula <- "price ~ mileage + tax + mpg + engineSize + year + categorizat_numeric + l_per_100km"

# Crearea modelului Random Forest
model_rf <- randomForest(formula, data = set_antrenare, ntree = 100, importance = TRUE)

# Afișarea rezumatului modelului
print(model_rf)

# Realizarea predicțiilor pe setul de testare
predictii_rf <- predict(model_rf, newdata = set_testare)

# Calculul Mean Squared Error (MSE) pentru Random Forest
mse_rf <- mean((set_testare$price - predictii_rf)^2)
cat("Mean Squared Error (Random Forest):", mse_rf, "\n")

ss_total <- sum((set_testare$price - mean(set_testare$price))^2)
ss_residual <- sum((set_testare$price - predictii_rf)^2)
rsquared_rf <- 1 - (ss_residual / ss_total)

cat("R-squared (Random Forest):", rsquared_rf, "\n")

print(model_rf$importance)


# Extrage coeficienții pentru afișarea explicită
coeficienti <- coef(model_rf)

# Creați un tabel pentru afișarea caracteristicilor și coeficienților
tabel_coeficienti <- data.frame(Caracteristica = names(coeficienti), Coeficient = coeficienti)

# Afișează tabela cu coeficienții caracteristicilor
print(tabel_coeficienti)

coeficienti <- coef(model_liniar)[c("mileage", "tax", "mpg", "engineSize", "year", "categorizat_numeric", "l_per_100km")]
tabel_coeficienti <- data.frame(Caracteristica = names(coeficienti), Coeficient = coeficienti)

culori <- c("blue", "green", "red", "purple", "orange", "green", "purple")
grafic_coeficienti <- ggplot(tabel_coeficienti, aes(x = Caracteristica, y = Coeficient, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Coeficienții Caracteristicilor în Model",
       x = "Caracteristici", y = "Coeficienti") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafic_coeficienti)

