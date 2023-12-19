library(stats)
library(dplyr)
library(ggplot2)
library(caret)
library(rsample)
library(vip)
library(ggplot2)
library(scales)
library(lattice)
library(caTools)
library(randomForest)
library(glmnet)
library(gbm)

# Setarea unei semințe pentru reproducibilitate
set.seed(123)

# Procentul de date pentru setul de antrenare(de exemplu, 80 %)
procent_antrenare < -0.8

# Numărul total de rânduri
numar_randuri < -nrow(meredes_luc)

# Numărul de rânduri pentru setul de antrenare
numar_antrenare < -round(procent_antrenare * numar_randuri)

# Indecșii rândurilor pentru setul de antrenare
indice_antrenare < -sample(1:numar_randuri, numar_antrenare, replace = FALSE)

# Setul de Antrenare
set_antrenare < -meredes_luc[indice_antrenare, ]

# Setul de Testare
set_testare < -meredes_luc[-indice_antrenare, ]

# Selectează caracteristicile relevante
caracteristici_relevante <-meredes_luc%>%
  select(price, mileage, tax, mpg, engineSize, categorizat_numeric)

# Afișează glimpse pentru setul de caracteristici relevante
glimpse(caracteristici_relevante)

# Separă setul de antrenare
set_antrenare < -subset(date, sample.split(date, SplitRatio = 0.8)$Resample == TRUE)

# Sumarul modelului
summary(model)

set_testare$predictii < -predict(model, newdata = set_testare)
ggplot(data = set_testare, aes(x = price, y = predictii)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot între Prețurile Reale și Predicțiile Modelului",
       x = "Preț Real", y = "Predicție Model") +
  theme_minimal() +
  scale_x_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k"))  # Afiseaza in mii
coeficienti < -coef(model)[-1]  # Exclude interceptul, dacă este prezent

# Creează un bar plot pentru coeficienții modelului
barplot(coeficienti, names.arg = names(coeficienti),
        main = "Caracteristici alese de model",
        xlab = "Caracteristici", ylab = "Coeficienti")
caracteristici_alese < -names(model$coefficients)[-1]

# Afișează lista de caracteristici alese
cat("Caracteristici alese de model:\n")
cat(caracteristici_alese, sep = ", ")

coeficienti < -coef(model)[c("mileage", "tax", "mpg", "engineSize", "year", "categorizat_numeric", "l_per_100km")]
# Creați un tabel pentru afișarea caracteristicilor și coeficienților
tabel_coeficienti < -data.frame(Caracteristica = names(coeficienti), Coeficient = coeficienti)
# Afișează tabela cu coeficienții caracteristicilor
print(tabel_coeficienti)

# Setarea culorilor pentru bar plot
culori < -c("blue", "green", "red", "purple", "orange", "green", "purple")
# Creează un bar plot pentru coeficienții modelului
grafic_coeficienti < -ggplot(tabel_coeficienti, aes(x = Caracteristica, y = Coeficient, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Coeficienții Caracteristicilor în Model",
       x = "Caracteristici", y = "Coeficienti") +
  theme_minimal() +
  scale_fill_manual(values = culori)
theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afișează graficul
print(grafic_coeficienti)
# Creează un bar plot pentru coeficienții modelului cu denumirile de jos pe diagonala
grafic_coeficienti < -ggplot(tabel_coeficienti, aes(x = Caracteristica, y = Coeficient, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Coeficienții Caracteristicilor în Model",
       x = "Caracteristici", y = "Coeficienti") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afișează graficul
print(grafic_coeficienti)

# Calculează reziduurile
reziduuri < -residuals(model)

# Setează reziduurile la zero
model$fitted.values < -model$fitted.values + reziduuri

# Afișează noul sumar al modelului
summary(model)

# Definește modelul Random Forest
model_rf < -randomForest(price ~mileage + year + tax + engineSize + categorizat_numeric + l_per_100km + mpg + binar_fuelType, data = set_antrenare)

# Sumarul modelului Random Forest
print(model_rf)

# Realizează predicții pe setul de testare
predictii_rf < -predict(model_rf, newdata = set_testare)

# Creați graficul pentru compararea dintre prețurile reale și predicțiile modelului Random Forest
ggplot(data = set_testare, aes(x = price, y = predictii_rf)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot între Prețurile Reale și Predicțiile Modelului Random Forest",
       x = "Preț Real", y = "Predicție Model RF") +
  theme_minimal() +
  scale_x_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k"))  # Afiseaza in mii

# Afișează importanța caracteristicilor sub formă de alt tip de grafic
barplot(importanta_caracteristici[, 1], horiz = TRUE, names.arg = rownames(importanta_caracteristici),
        main = "Importanța Caracteristicilor în Random Forest",
        xlab = "Importanță", ylab = "Caracteristici", las = 2)

# Obține coeficienții modelului Random Forest
coeficienti_rf < -as.data.frame(model_rf$importance)

# Afișează tabelul cu coeficienții
print(coeficienti_rf)


# Obține importanța caracteristicilor din modelul Random Forest
importanta_rf < -as.data.frame(importanta_caracteristici$MeanDecreaseGini)

# Obține importanța caracteristicilor din modelul Random Forest
importanta_rf < -as.data.frame(importanta_caracteristici$MeanDecreaseGini)
importanta_rf$Caracteristica < -rownames(importanta_rf)
colnames(importanta_rf) < -c("Importanta", "Caracteristica")

# Creează un bar plot pentru importanța caracteristicilor în Random Forest
grafic_importanta_rf < -ggplot(importanta_rf, aes(x = Caracteristica, y = Importanta, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Importanța Caracteristicilor în Random Forest",
       x = "Caracteristici", y = "Importanță") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Setarea culorilor pentru bar plot
culori < -c("blue", "green", "red", "purple", "orange", "green", "purple")
# Creează un bar plot pentru coeficienții modelului
grafic_coeficienti < -ggplot(tabel_coeficienti, aes(x = Caracteristica, y = Coeficient, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Coeficienții Caracteristicilor în Model",
       x = "Caracteristici", y = "Coeficienti") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afișează graficul
print(grafic_coeficienti)
culori < -c("blue", "green", "red", "purple", "orange", "green", "purple")
# Creează un bar plot pentru coeficienții modelului
grafic_coeficienti < -ggplot(tabel_coeficienti, aes(x = Caracteristica, y = Coeficient, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Coeficienții Caracteristicilor în Model",
       x = "Caracteristici", y = "Coeficienti") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afișează graficul
print(grafic_coeficienti)
# Afișează importanța caracteristicilor sub formă de grafic
#grafic_rf < -barplot(importanta_caracteristici[, 1], names.arg = rownames(importanta_caracteristici),
#        main = "Importanța Caracteristicilor în Random Forest",
#        xlab = "Caracteristici", ylab = "Importanță", las = 2, mar = c(5, 5, 4, 2))
# Obține importanța caracteristicilor din modelul Random Forest
importanta_caracteristici < -importance(model_rf)

# Afișează importanța caracteristicilor sub formă de grafic
grafic_rf < -barplot(importanta_caracteristici[, 1], names.arg = rownames(importanta_caracteristici),
                     main = "Importanța Caracteristicilor în Random Forest",
                     xlab = "Caracteristici", ylab = "Importanță", las = 2, mar = c(5, 5, 4, 2))

# Obține importanța caracteristicilor din modelul Random Forest
importanta_rf < -as.data.frame(importanta_caracteristici$MeanDecreaseGini)
# Afișează importanța caracteristicilor sub formă de grafic
#grafic_rf < -barplot(importanta_caracteristici[, 1], names.arg = rownames(importanta_caracteristici),
#        main = "Importanța Caracteristicilor în Random Forest",
#        xlab = "Caracteristici", ylab = "Importanță", las = 2, mar = c(5, 5, 4, 2))
# Obține importanța caracteristicilor din modelul Random Forest
importanta_caracteristici < -importance(model_rf)
importanta_caracteristici < -importance(model_rf)

# Afișează importanța caracteristicilor sub formă de grafic
grafic_rf < -barplot(importanta_caracteristici$MeanDecreaseGini, names.arg = rownames(importanta_caracteristici),
                     main = "Importanța Caracteristicilor în Random Forest",
                     xlab = "Caracteristici", ylab = "Importanță", las = 2, mar = c(5, 5, 4, 2))

# Obține importanța caracteristicilor din modelul Random Forest
importanta_rf < -as.data.frame(importanta_caracteristici$MeanDecreaseGini)
importanta_rf$Caracteristica < -rownames(importanta_rf)
colnames(importanta_rf) < -c("Importanta", "Caracteristica")

# Creează un bar plot pentru importanța caracteristicilor în Random Forest
grafic_importanta_rf < -ggplot(importanta_rf, aes(x = Caracteristica, y = Importanta, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Importanța Caracteristicilor în Random Forest",
       x = "Caracteristici", y = "Importanță") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afișează graficul
print(grafic_importanta_rf)
importanta_caracteristici < -importance(model_rf)

# Filtrează importanța pentru caracteristicile dorite
caracteristici_selectate < -c("mileage", "tax", "mpg", "engineSize", "year", "categorizat_numeric", "l_per_100km")
importanta_selectata < -importanta_caracteristici[caracteristici_selectate, , drop = FALSE]

# Afișează importanța caracteristicilor sub formă de tabel
print(importanta_selectata)
culori < -c("blue", "green", "red", "purple", "orange", "cyan", "pink")

# Creează un dataframe pentru importanța caracteristicilor selectate
importanta_selectata_df < -data.frame(Caracteristica = rownames(importanta_selectata), Importanta = importanta_selectata[, 1])

# Creează un bar plot pentru importanța caracteristicilor
grafic_importanta_rf < -ggplot(importanta_selectata_df, aes(x = Caracteristica, y = Importanta, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Importanța Caracteristicilor în Random Forest",
       x = "Caracteristici", y = "Importanță") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afișează graficul
print(grafic_importanta_rf)

# Creează un bar plot pentru importanța caracteristicilor
grafic_importanta_rf < -ggplot(importanta_selectata_df, aes(x = Caracteristica, y = Importanta, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Importanța Caracteristicilor în Random Forest",
       x = "Caracteristici", y = "Importanță") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-7, suffix = "k"))  # Afiseaza in mii

# Afișează graficul
print(grafic_importanta_rf)
# Creează un bar plot pentru importanța caracteristicilor
grafic_importanta_rf < -ggplot(importanta_selectata_df, aes(x = Caracteristica, y = Importanta, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Importanța Caracteristicilor în Random Forest",
       x = "Caracteristici", y = "Importanță") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k"))  # Afiseaza in mii

# Afișează graficul
print(grafic_importanta_rf)

# Creează un bar plot pentru importanța caracteristicilor
grafic_importanta_rf < -ggplot(importanta_selectata_df, aes(x = Caracteristica, y = Importanta, fill = Caracteristica)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Importanța Caracteristicilor în Random Forest",
       x = "Caracteristici", y = "Importanță") +
  theme_minimal() +
  scale_fill_manual(values = culori) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-7, suffix = "k"))  # Afiseaza in mii

# Afișează graficul
print(grafic_importanta_rf)
grafic_importanta_rf_puncte < -ggplot(importanta_selectata_df, aes(x = Caracteristica, y = Importanta, color = Caracteristica)) +
  geom_point(size = 3) +
  labs(title = "Importanța Caracteristicilor în Random Forest",
       x = "Caracteristici", y = "Importanță") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afișează graficul cu puncte
print(grafic_importanta_rf_puncte)
model_gbm < -gbm(price ~mileage + year + tax + engineSize + categorizat_numeric + l_per_100km + mpg + binar_fuelType, data = set_antrenare)

# Obține importanța caracteristicilor
importanta_caracteristici_gbm < -summary(model_gbm)

# Afișează importanța caracteristicilor
print(importanta_caracteristici_gbm)

# Evaluarea performanței pentru modelul Random Forest
predictii_rf < -predict(model_rf, newdata = set_testare)

# Calculul Mean Squared Error(MSE) pentru Random Forest
mse_rf < -mean((set_testare$price - predictii_rf) ^ 2)
cat("Mean Squared Error (Random Forest):", mse_rf, "\n")

# Calculul R - squared pentru Random Forest
rsquared_rf < -1 - (sum((set_testare$price - predictii_rf) ^ 2) / sum((set_testare$price - mean(set_testare$price)) ^ 2))
cat("R-squared (Random Forest):", rsquared_rf, "\n")

# Extrage datele de importanță
importanta_data < -as.data.frame(importanta_caracteristici_gbm$rel.inf)

# Crează un bar plot cu caracteristici pe axa x și importanța lor pe axa y
barplot(t(importanta_data), beside = TRUE, col = rainbow(ncol(importanta_data)), names.arg = colnames(importanta_data), las = 2, main = "Importanța Caracteristicilor în Modelul GBM", xlab = "Caracteristici", ylab = "Importanță")

# Adaugă o legendă
legend("topright", legend = rownames(importanta_data), fill = rainbow(ncol(importanta_data)))
model_gbm < -gbm(price ~mileage + year + tax + engineSize + categorizat_numeric + l_per_100km + mpg + binar_fuelType, data = set_antrenare)

# Obține importanța caracteristicilor
importanta_caracteristici_gbm < -summary(model_gbm)

# Afișează importanța caracteristicilor
print(importanta_caracteristici_gbm)

# Afișează importanța caracteristicilor
print(t(importanta_caracteristici_gbm$rel.inf))

# Obține importanța caracteristicilor
importanta_caracteristici_gbm < -summary(model_gbm)
model_gbm < -gbm(price ~mileage + year + tax + engineSize + categorizat_numeric + l_per_100km + mpg + binar_fuelType, data = set_antrenare)

# Obține importanța caracteristicilor
importanta_caracteristici_gbm < -summary(model_gbm)

# Afișează importanța caracteristicilor
print(t(importanta_caracteristici_gbm$rel.inf))

# Obține importanța caracteristicilor
importanta_caracteristici_gbm < -summary(model_gbm)
model_gbm < -gbm(price ~mileage + year + tax + engineSize + categorizat_numeric + l_per_100km + mpg + binar_fuelType, data = set_antrenare)
model_gbm < -gbm(price ~mileage + year + tax + engineSize + categorizat_numeric + l_per_100km + mpg + binar_fuelType,
                 data = set_antrenare,
                 distribution = "poisson")

# Obține importanța caracteristicilor
importanta_caracteristici_gbm < -summary(model_gbm)

# Afișează importanța caracteristicilor
print((importanta_caracteristici_gbm$rel.inf))

# Afișează importanța caracteristicilor
print(importanta_caracteristici_gbm$rel.inf)

# Obține importanța caracteristicilor
importanta_caracteristici_gbm < -summary(model_gbm)

# Extrage importanța relativă a caracteristicilor
importanta_relativa < -importanta_caracteristici_gbm$rel.inf

# Creează un grafic de bare cu orientare schimbată
barplot(importanta_relativa, horiz = TRUE, col = rainbow(length(importanta_relativa)))

# Adaugă denumirile caracteristicilor pe axa y
axis(2, at = 1:length(importanta_relativa), labels = names(importanta_relativa))
importanta_caracteristici_gbm < -summary(model_gbm)

# Extrage importanța relativă a caracteristicilor
importanta_relativa < -importanta_caracteristici_gbm$rel.inf

# Creează un grafic de bare cu denumirile scrise pe verticală
barplot(importanta_relativa, horiz = FALSE, col = rainbow(length(importanta_relativa)),
        names.arg = names(importanta_relativa), las = 2)

# Obține importanța caracteristicilor
importanta_caracteristici_gbm < -summary(model_gbm)

# Extrage importanța relativă a caracteristicilor
importanta_relativa < -importanta_caracteristici_gbm$rel.inf

# Creează un grafic de bare cu denumirile scrise pe verticală
barplot(importanta_relativa, horiz = FALSE, col = rainbow(length(importanta_relativa)),
        names.arg = names(importanta_relativa), las = 2)

# Adaugă denumirile caracteristicilor
text(x = seq_along(importanta_relativa), y = importanta_relativa,
     label = names(importanta_relativa), pos = 3, col = "black", cex = 0.7)
plot(model_gbm, i = "cv", main = "Coeficienți caracteristici GBM")
 
