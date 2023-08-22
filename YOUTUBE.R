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
library(corrplot)
library(FactoMineR)


url <- "https://raw.githubusercontent.com/cavilla1994/SI-YOUTUBE/main/Global%20YouTube%20Statistics.csv"
data <- read.csv(url, header = TRUE)

#Consulta de clase de la base de datos
data.class(data)

#Consulta de los nombres de las columnas y de que tipo es:
colnames(data)

#caracteristica de los datos
tipos_de_datos <- sapply(data, class)
tipos_de_datos


#Veo los primeros registros y los ultimos registros
head(data) #Primeros

tail(data) #Ultimos


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

#Elimino columnas que considero que no me van a servir para un analisis

columns_to_remove <- c("Urban_population", "Latitude", "Longitude", "Gross.tertiary.education.enrollment....")
data <- data[, !(names(data) %in% columns_to_remove)]

View(data)


#Buscando valores NA, NAN
#NA: datos faltantes o no disponibles
#NAN: Es un valor especial que representa un resultado indefinido o no numérico en cálculos matemáticos.

#Contamos cuantos valores NAN hay en cada columna
contar_nan <- function(data) {
  result <- colSums(sapply(data, function(col) sum(is.na(col))))
  df_result <- data.frame(Columna = names(result), NaN_Count = result)
  return(df_result)
}
resultado_nan <- contar_nan(data)
print(resultado_nan)  #Visualizamos cuales columnas tienen mayor cantidad de valores NA



# Creamos la funcion para eliminar filas con NA
eliminar_filas_na_columna <- function(data, columna) {
  data_clear <- data[complete.cases(data) | !is.na(data[, columna]), ]
  return(data_clear)
}

# Nombre de la columna con NA que deseas filtrar
columna_con_na <- "subscribers_for_last_30_days" #Se observa que es la que mayor NA aporta al dataset, decidimos eliminarla
                                                 #y analizar esos valores por separado,  dado que afectaban al dataset


# Generamos los dos dataset.
#Data_clear -> la cual reperesenta el data set para trabajar.
#filas_eliminadsa -> se creara un dataset extra para analizar los Na.

data_clear <- eliminar_filas_na_columna(data, columna_con_na)
filas_eliminadas <- data[!(rownames(data) %in% rownames(data_clear)), ]


#Guardamos ambos dataset como csv

write.csv(data_clear, file = "C:/Users/Jonas-PC/Documents/GitHub/SI-YOUTUBE/data_clear.csv", row.names = FALSE)

write.csv(filas_eliminadas, file = "C:/Users/Jonas-PC/Documents/GitHub/SI-YOUTUBE/filas_eliminadas.csv", row.names = FALSE)


########################################################

#Analisis otro level.


#Grafico de barras: Cantidad de videos por categoria

videos_por_categoria <- data %>%
  group_by(category) %>%
  summarise(num_videos = n())
ggplot(videos_por_categoria, aes(x = reorder(category, -num_videos), y = num_videos)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Cantidad de Videos por Categoría",
       x = "Categoría", y = "Cantidad de Videos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Grafico de barras: Regiones mas populares por suscriptores
regiones_populares <- data %>%
  group_by(Country) %>%
  summarise(total_subscribers = sum(subscribers)) %>%
  arrange(desc(total_subscribers)) %>%
  top_n(10)
ggplot(regiones_populares, aes(x = reorder(Country, -total_subscribers), y = total_subscribers)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Regiones más Populares por Suscriptores",
       x = "País", y = "Total de Suscriptores") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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



