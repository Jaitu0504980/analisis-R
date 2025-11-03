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
