library(dplyr)
library(ggplot2)
library(caret)
library(rsample)
library(vip)
library(ggplot2)
library(scales)
library(lattice)

ggplot(data = meredes_luc, aes(x = year, y = price)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_point() +
  labs(title = "Relația dintre Anul Modelului și Preț")

ggplot(data = meredes_luc, aes(x = categorizat, y = price)) +
  geom_boxplot() +
  labs(title = "Relația dintre Preț și Model")

ggplot(data = meredes_luc, aes(x = engineSize, y = price)) +
  geom_point() +
  labs(title = "Relația dintre Preț și Capacitatea Motorului")

ggplot(data = meredes_luc, aes(x = engineSize, y = price)) +
  geom_point(color = "purple") +
  labs(title = "Relația dintre Preț și Volumul Motorului")

ggplot(data = meredes_luc, aes(x = l_per_100km, y = price)) +
  geom_point(color = "orange") +
  labs(title = "Relația dintre Preț și Consumul de Combustibil")

ggplot(data = meredes_luc, aes(x = price, y = tax, color = tax)) +
  geom_point() +
  labs(title = "Relația dintre Preț și Taxă, colorat în funcție de Taxă")

ggplot(data = meredes_luc, aes(x = mileage, y = price)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot pentru Relația dintre Mileage și Price",
       x = "Mileage", y = "Price")
