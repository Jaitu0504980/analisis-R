############## SCRIPT MODELO ENTRENADO CON POBLACIÓN CHINA Y VALIDADO CON MEXICANA ########

datos_mexicanos <- diabetes
datos_chinos <- diabetes_chinos
datos_chinos <- datos_chinos %>%
  rename(Triglycerides = TG_mg_dl)
rm(norm_mexicanos)

datos_chinos <- datos_chinos %>%
  rename(HbA1c = HbA1c...)
datos_mexicanos <- datos_mexicanos %>%
  rename(Edad = AGE)
datos_mexicanos <- datos_mexicanos %>%
  rename(IMC = BMI)

datos_chinos <- datos_chinos %>%
  mutate(diabetes_gestacional = ifelse(diabetes_gestacional == "SI", 1,
                                       ifelse(diabetes_gestacional == "NO", 0, NA)))


library(dplyr)
library(class)

# Función para imputar NA con mediana
imputar_mediana <- function(df) {
  for (i in seq_along(df)) {
    if (is.numeric(df[[i]])) {
      mediana <- median(df[[i]], na.rm = TRUE)
      df[[i]][is.na(df[[i]])] <- mediana
    }
  }
  return(df)
}

# Define variables predictoras (ajusta si es necesario)
vars <- c("Edad", "IMC", "HbA1c", "Triglycerides")

# Guarda variable objetivo ANTES de seleccionar columnas predictoras
train_target <- datos_chinos$diabetes_gestacional
test_target <- datos_mexicanos$diabetes_gestacional

# Selecciona variables predictoras y limpia NAs con mediana
datos_chinos_predictoras <- datos_chinos %>%
  select(all_of(vars)) %>%
  imputar_mediana()

datos_mexicanos_predictoras <- datos_mexicanos %>%
  select(all_of(vars)) %>%
  imputar_mediana()

# Normalización basada en rango de datos chinos
mins <- sapply(datos_chinos_predictoras, min)
maxs <- sapply(datos_chinos_predictoras, max)

normalize_with_ref <- function(df, mins, maxs) {
  sweep(sweep(df, 2, mins, "-"), 2, maxs - mins, "/")
}

norm_chinos <- normalize_with_ref(datos_chinos_predictoras, mins, maxs)
norm_mexicanos <- normalize_with_ref(datos_mexicanos_predictoras, mins, maxs)

# Limita valores fuera de rango [0,1] en mexicanos
norm_mexicanos[norm_mexicanos < 0] <- 0
norm_mexicanos[norm_mexicanos > 1] <- 1

# Entrena y predice con KNN
set.seed(123)
k_value <- 25

predictions <- knn(train = norm_chinos, test = norm_mexicanos, cl = train_target, k = k_value)

# Evalúa resultados
tab <- table(Predicted = predictions, Actual = test_target)
print(tab)

accuracy <- function(conf_matrix) {
  sum(diag(conf_matrix)) / sum(conf_matrix) * 100
}

cat("Precisión (Accuracy) con k =", k_value, ":", round(accuracy(tab), 2), "%\n")

# Búsqueda automática del mejor k
for (k in seq(1, 30, 2)) {
  pred_k <- knn(train = norm_chinos, test = norm_mexicanos, cl = train_target, k = k)
  acc_k <- accuracy(table(pred_k, test_target))
  cat("k =", k, "- Accuracy:", round(acc_k, 2), "%\n")
}

table(datos_chinos$diabetes_gestacional)






################### SCRIPT CON REPETICIONES######################




library(dplyr)
library(class)

# Función para imputar NA con mediana
imputar_mediana <- function(df) {
  for (i in seq_along(df)) {
    if (is.numeric(df[[i]])) {
      mediana <- median(df[[i]], na.rm = TRUE)
      df[[i]][is.na(df[[i]])] <- mediana
    }
  }
  return(df)
}

# Función de normalización
normalize_with_ref <- function(df, mins, maxs) {
  sweep(sweep(df, 2, mins, "-"), 2, maxs - mins, "/")
}

# Función de accuracy
accuracy <- function(conf_matrix) {
  sum(diag(conf_matrix)) / sum(conf_matrix) * 100
}

# Variables predictoras
vars <- c("Edad", "IMC", "HbA1c", "Triglycerides")
k_value <- 29
n_reps <- 1000  # Número de repeticiones

# Imputación y selección en mexicanos (esto no cambia)
datos_mexicanos_predictoras <- datos_mexicanos %>%
  select(all_of(vars)) %>%
  imputar_mediana()
test_target <- datos_mexicanos$diabetes_gestacional

# Guarda resultados
resultados_accuracy <- numeric(n_reps)

for (i in 1:n_reps) {
  set.seed(i)  # Semilla distinta en cada repetición
  
  # Submuestreo aleatorio del 80% de los datos chinos
  idx <- sample(1:nrow(datos_chinos), size = floor(0.8 * nrow(datos_chinos)))
  datos_train <- datos_chinos[idx, ]
  train_target <- datos_train$diabetes_gestacional
  
  # Imputación y selección en subset chino
  datos_train_predictoras <- datos_train %>%
    select(all_of(vars)) %>%
    imputar_mediana()
  
  # Normalización con base en subset chino
  mins <- sapply(datos_train_predictoras, min)
  maxs <- sapply(datos_train_predictoras, max)
  
  norm_train <- normalize_with_ref(datos_train_predictoras, mins, maxs)
  norm_mexicanos <- normalize_with_ref(datos_mexicanos_predictoras, mins, maxs)
  norm_mexicanos[norm_mexicanos < 0] <- 0
  norm_mexicanos[norm_mexicanos > 1] <- 1
  
  # KNN
  predictions <- knn(train = norm_train, test = norm_mexicanos, cl = train_target, k = k_value)
  
  acc <- accuracy(table(predictions, test_target))
  resultados_accuracy[i] <- acc
}

# Mostrar resultados finales
cat("Resultados de Accuracy en", n_reps, "repeticiones:\n")
print(resultados_accuracy)

cat("\nResumen estadístico del Accuracy:\n")
print(summary(resultados_accuracy))

cat("\nAccuracy promedio:", round(mean(resultados_accuracy), 2), "%\n")

##################### SCRIPT ENTRENADO Y VALIDADO CON POBLACIÓN MEXICANA #######################
library(dplyr)

# Función para imputar mediana
imputar_mediana <- function(df) {
  for (i in seq_along(df)) {
    if (is.numeric(df[[i]])) {
      mediana <- median(df[[i]], na.rm = TRUE)
      df[[i]][is.na(df[[i]])] <- mediana
    }
  }
  return(df)
}

# 1. Seleccionamos variables y imputamos antes de normalizar
datos_imputados <- diabetes %>%
  select(AGE, BMI, RDW_.1.., VLDL, LDH, HbA1c) %>%
  imputar_mediana()

# 2. Normalizamos ya sin NA
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

norm_dat <- datos_imputados %>%
  mutate(across(everything(), ~ normalize(.x)))

# 3. Dividir en entrenamiento y test
set.seed(123)
dat_samp <- sample(2, nrow(norm_dat), replace = TRUE, prob = c(0.67, 0.33))

dat_training <- norm_dat[dat_samp == 1, ]
dat_test <- norm_dat[dat_samp == 2, ]

dat_target_group <- diabetes$diabetes_gestacional[dat_samp == 1]
dat_test_group <- diabetes$diabetes_gestacional[dat_samp == 2]

# Verificar que no hay NA
sum(is.na(dat_training))
sum(is.na(dat_test))


library(class)
dat_pred <- knn(train = dat_training, test = dat_test, cl = dat_target_group, k=25) #Enseñar como definir valor de k
dat_pred

summary(dat_pred)
summary(dat_test_group)

tab <- table(dat_pred, dat_test_group)
tab

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# Repetir la función 50,000 veces y calcular el promedio
set.seed(123)  # Para reproducibilidad, opcional
rep <- 100
accuracies <- replicate(rep, loop())
mean_accuracy <- mean(accuracies)


library(class)

# Suponiendo que ya tienes estas variables listas:
# dat_training: Datos de entrenamiento (solo variables numéricas, ya normalizadas si es necesario).
# dat_test: Datos de prueba (mismo formato que dat_training).
# dat_target_group: Etiquetas del conjunto de entrenamiento.
# dat_test_target: Etiquetas reales del conjunto de prueba.

loop <- function() {
  pred <- knn(train = dat_training, test = dat_test, cl = dat_target_group, k = 25)
  
  # Calcular precisión correctamente
  accuracy <- sum(pred == dat_test_group) / length(dat_test_group)
  return(accuracy)
}


# Ejecutar la función repetidamente
rep <- 100  # Número de repeticiones
accuracies <- replicate(rep, loop())

# Mostrar resultados
print(accuracies)
mean(accuracies)  # Promedio de precisión




library(class)

k_values <- seq(1, 31, 2)  # probar k impares del 1 al 31
accuracies <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  pred <- knn(train = dat_training, test = dat_test, cl = dat_target_group, k = k_values[i])
  tab <- table(pred, dat_test_group)
  accuracies[i] <- sum(diag(tab)) / sum(tab)
}

plot(k_values, accuracies, type = "b", xlab = "Valor de k", ylab = "Precisión")


#################### PRUEBA DE LOS 50 000 EXPERIMENTOS ###########################

loop <- function() {
  set.seed(sample(1:50000, 1))  # Semilla aleatoria para cada repetición
  
  dat_samp <- sample(2, nrow(norm_dat), replace = TRUE, prob = c(0.67, 0.33))
  dat_training <- norm_dat[dat_samp == 1, ]
  dat_test <- norm_dat[dat_samp == 2, ]
  
  dat_target_group <- diabetes$diabetes_gestacional[dat_samp == 1]
  dat_test_group <- diabetes$diabetes_gestacional[dat_samp == 2]
  
  if (nrow(dat_test) == 0 || nrow(dat_training) == 0) {
    return(NA)  # Evitar error si algún set queda vacío
  }
  
  pred <- knn(train = dat_training, test = dat_test, cl = dat_target_group, k = 25)
  accuracy <- sum(pred == dat_test_group) / length(dat_test_group)
  return(accuracy)
}

# Número de repeticiones
rep <- 1000  # Puedes ajustar este número (cuantas más, mejor análisis)

# Ejecutar simulaciones
accuracies <- replicate(rep, loop())

# Mostrar los resultados
print(accuracies)

# Resumen estadístico
summary(accuracies)

# Promedio de precisión
mean(accuracies, na.rm = TRUE)

# Desviación estándar (para ver la variabilidad)
sd(accuracies, na.rm = TRUE)

sum(is.na(accuracies))

# Histograma de la precisión
hist(accuracies, main = "Distribución de precisión en repeticiones",
     xlab = "Precisión", col = "lightblue", border = "black")

# O también una caja de bigotes
boxplot(accuracies, main = "Caja de bigotes de precisión", ylab = "Precisión")

##################### ESTADÍSTICA DESCRIPTIVA DE VALORES PARAMÉTRICOS########################
library(gtsummary)
class(diabetes$RDW_.1..)
diabetes$RDW_.1.. <- as.numeric(diabetes$RDW_.1..)
diabetes |> 
  tbl_summary(
    by = diabetes_gestacional,
    include = c(BMI, AGE, HbA1c, GLU.1HR, Cholesterol),
    type = list(all_continuous() ~ "continuous2"),
    statistic =
      all_continuous() ~ c("{N_nonmiss}",
                           "{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}"),
    missing = "no"
  ) |> 
  modify_header(label = "*Variable*") |> # update the column header
  bold_labels() |>
  add_n() |> # add column with total number of non-missing observations
  add_p(
    # perform t-test for all variables
    test = everything() ~ "t.test",
    # assume equal variance in the t-test
    test.args = everything() ~ list(var.equal = TRUE)
  )
