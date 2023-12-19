# Separă setul de date în antrenare și testare
set.seed(123)  # Pentru reproducibilitate
indices_antrenare <- sample(1:nrow(meredes_luc), 0.8 * nrow(meredes_luc))
set_antrenare <- meredes_luc[indices_antrenare, ]
set_testare <- meredes_luc[-indices_antrenare, ]

# Definirea formulei
formula <- "price ~ mileage + tax + mpg + engineSize + year + categorizat_numeric + l_per_100km"

# Crearea modelului de regresie liniară
model_liniar <- lm(formula, data = set_antrenare)

# Afișarea rezultatelor
print(summary(model_liniar))

# Realizarea predicțiilor pe setul de testare
set_testare$predictii <- predict(model_liniar, newdata = set_testare)

# Calculul Mean Squared Error (MSE) pentru regresia liniară
mse_liniar <- mean((set_testare$price - set_testare$predictii)^2)
cat("Mean Squared Error (Regresie Liniară):", mse_liniar, "\n")

# Calculul R-squared pentru regresia liniară pe setul de testare
rsquared_liniar <- summary(model_liniar)$r.squared
cat("R-squared pentru regresia liniară pe setul de testare:", rsquared_liniar, "\n")

# Creează un Scatter Plot între Prețurile Reale și Predicțiile Modelului
ggplot(data = set_testare, aes(x = price, y = predictii)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot între Prețurile Reale și Predicțiile Modelului",
       x = "Preț Real", y = "Predicție Model") +
  theme_minimal() +
  scale_x_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k"))  # Afiseaza in mii


coeficienti <- coef(model)[c("mileage", "tax", "mpg", "engineSize", "year", "categorizat_numeric", "l_per_100km")]

# Creați un tabel pentru afișarea caracteristicilor și coeficienților
tabel_coeficienti <- data.frame(Caracteristica = names(coeficienti), Coeficient = coeficienti)

# Afișează tabela cu coeficienții caracteristicilor
print(tabel_coeficienti)

# Setarea culorilor pentru bar plot
culori <- c("blue", "green", "red", "purple", "orange", "green", "purple", "red")

# Creează un bar plot pentru coeficienții modelului
grafic_coeficienti <- ggplot(tabel_coeficienti, aes(x = Caracteristica, y = Coeficient, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Coeficienții Caracteristicilor în Model",
       x = "Caracteristici", y = "Coeficienti") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afișează graficul
print(grafic_coeficienti)

