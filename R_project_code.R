library(tidyverse)
library(modelr)
library(scatterplot3d)
library(rsample) 
library(rpart.plot) 
library(caret) 
library(quantmod)
library(modelr)
library(ISLR)
library(caret)
library(dplyr)


# CITIRE DATE DIN CSV 
Cars<- read.csv("C:\\DARIA\\1.FSEGA\\AN_3\\FSEGA_3\\SEMESTRUL2\\BigData\\PROIECT\\CarsData.csv")

# CONVERTIRE VARIABILE IN FACTORI
# Setarea nivelelor sa fie unice (pentru eroare)
all_engine_sizes <- unique(Cars$engineSize)
all_years <- unique(Cars$year)
all_transmissions <- unique(Cars$transmission)
all_manufacturers <- unique(Cars$Manufacturer)

Cars$engineSize <- factor(Cars$engineSize, levels = all_engine_sizes)
Cars$year <- factor(Cars$year, levels = all_years)
Cars$transmission <- factor(Cars$transmission, levels = all_transmissions)
Cars$Manufacturer <- factor(Cars$Manufacturer, levels = all_manufacturers)
Cars$model <- as.factor(Cars$model)
Cars$fuelType <- as.factor(Cars$fuelType)
Cars$tax <- as.factor(Cars$tax)


# PLOTURI
ggplot(Cars) + 
  geom_boxplot(aes(x=year, y=price, fill=year)) +
  theme(text= element_text(size=20))

ggplot(Cars, aes(x = year, y = price)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Prețurile mașinilor în funcție de anul de fabricație", x = "An", y = "Preț") +
  theme(text = element_text(size = 20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotire etichete pe axa x 

ggplot(Cars) + 
  geom_boxplot(aes(x=fuelType, y=price, fill=fuelType)) +
  theme(text= element_text(size=20))

ggplot(Cars) + 
  geom_boxplot(aes(x=Manufacturer, y=price, fill=Manufacturer)) +
  theme(text= element_text(size=20))

ggplot(Cars) + 
  geom_boxplot(aes(x=transmission, y=price, fill=transmission)) +
  theme(text= element_text(size=20))

ggplot(Cars) + 
  geom_boxplot(aes(x=engineSize, y=price, fill=engineSize)) +
  theme(text= element_text(size=20))

ggplot(Cars) + 
  geom_boxplot(aes(x=model, y=price, fill=model)) +
  theme(text= element_text(size=20))



#........VIEW PRICE EXPRESSED AS FUNCTION OF OTHER VARIABLES - SUMMARY
mod_price_all <- lm(price ~ ., data = Cars)
summary(mod_price_all)

# R = 11.95%
mod_price_model <- lm(price ~ model, data = Cars)
summary(mod_price_model)

# R = 31.52%
mod_price_year <- lm(price ~ year, data = Cars)
summary(mod_price_year)

# R = 30.61%
mod_price_transmission <- lm(price ~ transmission, data = Cars)
summary(mod_price_transmission)

# R = 17.47%
mod_price_mileage <- lm(price ~ mileage, data = Cars)
summary(mod_price_mileage)

# R = 5.29%
mod_price_tax <- lm(price ~ tax, data = Cars)
summary(mod_price_tax)

# R = 5.29%
mod_price_fuelType <- lm(price ~ fuelType, data = Cars)
summary(mod_price_fuelType)

# R = 8.73%
mod_price_mpg <- lm(price ~ mpg, data = Cars)
summary(mod_price_mpg)

# R = 49.1%
mod_price_engineSize <- lm(price ~ engineSize, data = Cars)
summary(mod_price_engineSize)

# R = 28.49%
mod_price_Manufacturer <- lm(price ~ Manufacturer, data = Cars)
summary(mod_price_Manufacturer)

#......PRET IN FUNCTIE DE ENGINE, YEAR, MILEAGE - SUMMARY - R = 80.45%
mod_price_complex <- lm(price ~ year + transmission + engineSize + Manufacturer, data = Cars)
summary(mod_price_complex)
confint(mod_price_complex)


#---------------------ANTRENARE---------------------

#..... Pas 1: Impartirea setului de date in antrenare si test 

set.seed(123)  # For reproducibility
split <- initial_split(Cars, prop = 0.7)
train_set <- training(split)
test_set <- testing(split)


#..... Rezolvare eroare: Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
# factor engineSize has new levels 4.3 atunci cand se fac predictii - se datoreaza faptului ca nivelul 4.3 nu 
# exista in setul de test - se pune text gol pentru nivelele care nu exista

# Indentificarea nivelelor lipsa din setul de test
missing_levels <- setdiff(unique(test_set$engineSize), unique(train_set$engineSize))

# Creare randuri dummy pentru nivelele existente in setul de antrenament si lipsa in setul de test
dummy_data <- lapply(missing_levels, function(level) {
  sample_row <- train_set[1, ]
  sample_row$engineSize <- level
  return(sample_row)
})

# Unim randurile dummy la training-set 
dummy_data <- do.call(rbind, dummy_data)
train_set <- rbind(train_set, dummy_data)


#..... Pas 2: Creare si fitting pe modele pentru variabila dependenta (price) 
#exprimata in functie de alte variabile pe setul de antrenament (train_set)

#1: pretul exprimat in functie de motor, an, kilometraj impreuna pe train_Set
model_complex <- lm(price ~ year + transmission + engineSize + Manufacturer, data = train_set)
summary(model_complex)

#2: pretul exprimat in functie de EngineSize pe train_set
model_engineSize <- lm(price ~ engineSize, data = train_set)

#3: pretul exprimat in functie de transmission pe train_set
model_transmission <- lm(price ~ transmission, data = train_set)

#4: pretul exprimat in functie de year pe train_set
model_year <- lm(price ~ year, data = train_set)

#5: pretul exprimat in functie de manufacturer pe train_set
model_manufacturer <- lm(price ~ Manufacturer, data = train_set)


#..... Pas 3: Creare predictii si evaluare model pe setul de test 
#1 : Predictii
# Predicțiile și evaluarea modelului complex
predictions_complex <- predict(model_complex, test_set)
rmse_complex <- RMSE(predictions_complex, test_set$price)
print(paste("RMSE pentru modelul complex: ", rmse_complex))

# Predicțiile și evaluarea modelului pe EngineSize
predictions_engineSize <- predict(model_engineSize, test_set)
rmse_engineSize <- RMSE(predictions_engineSize, test_set$price)
print(paste("RMSE pentru modelul EngineSize: ", rmse_engineSize))

# Predicțiile și evaluarea modelului pe Transmission
predictions_transmission <- predict(model_transmission, test_set)
rmse_transmission <- RMSE(predictions_transmission, test_set$price)
print(paste("RMSE pentru modelul Transmission: ", rmse_transmission))

# Predicțiile și evaluarea modelului pe Year
predictions_year <- predict(model_year, test_set)
rmse_year <- RMSE(predictions_year, test_set$price)
print(paste("RMSE pentru modelul Year: ", rmse_year))

# Predicțiile și evaluarea modelului pe Manufacturer
predictions_manufacturer <- predict(model_manufacturer, test_set)
rmse_manufacturer <- RMSE(predictions_manufacturer, test_set$price)
print(paste("RMSE pentru modelul Manufacturer: ", rmse_manufacturer))

#2: Facem merge la predictii cu test_set pentru a putea plota
test_set$predicted_price_complex = predictions_complex
test_set$predicted_price_engineSize = predictions_engineSize
test_set$predicted_price_transmission = predictions_transmission
test_set$predicted_price_year = predictions_year
test_set$predicted_price_manufacturer = predictions_manufacturer

#3: Plotarea datelor din setul de test vs datele prezise 

# PLOT PENTRU PREDICTIONS_COMPLEX
ggplot(test_set, aes(x = price, y = predicted_price_complex)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "blue", fill='orange', size = 1) +
  labs(title = "Prețurile actuale vs Prețurile prezise (Model Complex)",
       x = "Preț actual",
       y = "Preț prezis") +
  theme_minimal()

# PLOT PENTRU PREDICTIONS_ENGINESIZE
ggplot(test_set, aes(x = engineSize, y = price)) +
  geom_boxplot(aes(fill = as.factor(engineSize)), alpha = 0.5) +            # Prețuri actuale
  geom_jitter(aes(y = predicted_price_engineSize), color = "red", width = 0.2) +  # Prețuri prezise
  labs(title = "Prețurile actuale și cele prezise în funcție de EngineSize",
       x = "EngineSize",
       y = "Preț") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotire etichete pe axa x 

# PLOT PENTRU PREDICTIONS_TRANSMISSION
ggplot(test_set, aes(x = transmission, y = price)) +
  geom_boxplot(aes(fill = as.factor(transmission)), alpha = 0.5) +            # Prețuri actuale
  geom_jitter(aes(y = predicted_price_transmission), color = "red", width = 0.2) +  # Prețuri prezise
  labs(title = "Prețurile actuale și cele prezise în funcție de transmission",
       x = "Transmission",
       y = "Preț") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotire etichete pe axa x 

# PLOT PENTRU PREDICTIONS_MANUFACTURER
ggplot(test_set, aes(x = Manufacturer, y = price)) +
  geom_boxplot(aes(fill = as.factor(Manufacturer)), alpha = 0.5) +            # Prețuri actuale
  geom_jitter(aes(y = predicted_price_manufacturer), color = "red", width = 0.2) +  # Prețuri prezise
  labs(title = "Prețurile actuale și cele prezise în funcție de Manufacturer",
       x = "Manufacturer",
       y = "Preț") +
  theme_minimal() 

# PLOT PENTRU PREDICTIONS_YEAR
ggplot(test_set, aes(x = year, y = price)) +
  geom_boxplot(aes(fill = as.factor(year)), alpha = 0.5) +  # Prețuri actuale
  geom_jitter(aes(y = predicted_price_year), color = "red", width = 0.2) +  # Prețuri prezise
  labs(title = "Prețurile actuale și cele prezise în funcție de anul fabricației",
       x = "Anul fabricației",
       y = "Preț") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotire etichete pe axa x 


#----------------ARBORI DE DECIZIE -----------------

str(Cars) # Pentru a vizualiza structura setului de date al mașinilor

#vizualizarea atributelor numerice
Cars %>% 
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = TRUE) +
  facet_wrap(~metric, scales = "free")


# Impartirea setului de date
set.seed(123)
cars_split <- initial_split(Cars, prop = 0.7)
cars_train <- training(cars_split)
cars_test <- testing(cars_split)

#............GENERARE ARBORI IN FUNCTIE DE MAI MULTE VARIABILE............

#1: Generare arbore de decizie pentru engineSize, year, transmission si manufacturer
m1 <- rpart(
  formula = price ~ .,
  data = cars_train,
  method = "anova"
)
m1 # afisarea arborelui rezultat (in mod text)

rpart.plot(m1) #afisarea grafica a arborelui rezultat
plotcp(m1)
m1$cptable 


#........ Continuare pentru primul m1 ..........
m2 <- rpart(
  formula = price ~ ., 
  data = cars_train,
  method = "anova",
  control = list(cp = 0, xval = 10)  # se creste arborele pana la obtinerea valorii zero pentru parametrul alpha
)  # m2 este un arbore ne-taiat
plotcp(m2)
abline(v = 12, lty = "dashed") # la linia punctata se obtine arborele m1

# se obtine un arbore cu parametri minsplit si maxdepth specificati
m3 <- rpart(
  formula = price ~ .,
  data = cars_train, 
  method = "anova",
  control = list(minsplit = 10, maxdepth = 12, xval = 10)
)
m3
rpart.plot()
plotcp(m3)


#cautam cele mai bune valori pentru parametri minsplit si maxdepth
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)
head(hyper_grid)
models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = price ~ .,
    data = cars_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}
get_cp <- function(x) {
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
}
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}


mutated_grid <- hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  )  
mutated_grid %>%
  arrange(error) %>%
  top_n(-5, wt=error)

optimal_tree <- rpart(
  formula = price ~ .,
  data = cars_train,
  method = "anova",
  control = list(minsplit = 6, maxdepth = 13, cp = 0.0108982)
)
rpart.plot(optimal_tree)
plotcp(optimal_tree)

pred <- predict(m1, newdata = cars_test)
RMSE(pred = pred, obs = cars_test$price)
optimal_tree
rpart.plot(m1)

