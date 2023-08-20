# Analisis de Dataset YOUTUBE
# 
# 2023-08-16  version actual:   2023-08-16
#Dataset: https://www.kaggle.com/datasets/nelgiriyewithana/global-youtube-statistics-2023
###############################################
# 
# Bibliotecas a cargar

install.packages("googledrive")
install.packages("writexl")

library(ggplot2)
library(dplyr)
library(readr)



url <- "https://raw.githubusercontent.com/cavilla1994/SI-YOUTUBE/main/Global%20YouTube%20Statistics.csv"
data <- read.csv(url, header = TRUE)

#Consulta de clase de la base de datos
data.class(data)
View(data)
#Consulta de los nombres de las columnas y de que tipo es:

colnames(data)

tipos_de_datos <- sapply(data, class)
tipos_de_datos

#Calculos estadisticos basicos

media <- mean(data$subscribers) # Calculo de la media
desvio <- sd(data$subscribers)   #Calculo del desvio
rango <- IQR(data$subscribers)  #Calculo rango intercuartil
cuartiles <- quantile(data$subscribers, probs = c(0.25, 0.5, 0.75))
mediana <- median(data$subscribers)

# Crear una lista para los valores
valores <- list(Media = media, Desvio = desvio, Rango = rango, Cuartiles = cuartiles, Mediana = mediana)
valores

#Columnas con NAN
media <- mean(data$country_rank)
media


#Utilizando una funcion de R como Summary

summary(data$subscribers)  #Me da los mismos valores que se calcularon previamente, pero no pierdo tiempo escribiendo !



#Buscando valores NA, NAN
#NA: datos faltantes o no disponibles
#NAN: Es un valor especial que representa un resultado indefinido o no numérico en cálculos matemáticos.

#Contamos cuantos valores NAN hay en cada columna
contar_nan <- function(data) {
  result <- colSums(sapply(data, is.nan))
  df_result <- data.frame(Columna = names(result), NaN_Count = result)
  return(df_result)
}

resultado_nan <- contar_nan(data)
print(resultado_nan)

#Eliminamos filas con datos NAN 
eliminar_filas_nan <- function(data) {
  data_sin_nan <- data[complete.cases(data), ]
  return(data_sin_nan)
}


# Obtener las filas eliminadas
filas_elimindas <- data[!(rownames(data) %in% rownames(data_clear)), ]

# Guardar las filas eliminadas en un nuevo archivo CSV
ruta_archivo_eliminadas <- "C:/Users/Jonás/Desktop/filas_eliminadas.csv"
write.csv(filas_elimindas, file = ruta_archivo_eliminadas, row.names = FALSE)



data_clear

write.csv(data_clear, "data_clear.csv")
drive_upload("data_clear.csv", path = "data_clear")


# Definir la ubicación y nombre del archivo de Excel en tu escritorio
archivo_excel <- file.path("~/Desktop", "data_clear.xlsx")

# Guardar el archivo en formato Excel
write.xlsx(data_clear, archivo_excel, row.names = FALSE)

print("Archivo de Excel guardado en tu escritorio en formato Excel.")



cs########################################################

#







# Instalacion de librerias
install.packages("corrplot")
install.packages("FactoMineR")

# Carga de librerias
library(corrplot)
library(FactoMineR)

# Se seleccionan las columnas que solo tengan formato numerico (las tipo caracter no aplican)
columnas_numericas <- sapply(data_clear, is.numeric)
matriz_correlacion <- cor(data_clear[, columnas_numericas], use = "complete.obs")
corrplot(matriz_correlacion, method = "circle")

# Filtrar solo columnas numéricas para el PCA
data_numeric <- data_clear[, columnas_numericas]

# Realizar un PCA solo con variables numéricas
pca_resultado <- PCA(data_numeric, graph = FALSE)

# Imprimir información del PCA
summary(pca_resultado)

# Gráficos de los ejes factoriales
plot(pca_resultado)



