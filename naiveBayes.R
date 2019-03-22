#-----------------------------------------------------------------------------------------------------------------------------------------------
# Universidad del Valle de Guatemala
# Autores: Andrea Maria Cordon Mayen, 16076
#          Cristopher Sebastian Recinos Ramírez, 16005
# Fecha: 18/03/2019
# arboles.R
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Librerias a utilizar
library(caret)
install.packages("fmsb")
library(fmsb)
install.packages("e1071")
library(e1071)
install.packages("mlr")
library(mlr)
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Set de ambientes y variables generales
# --------------------------------------

# Set del working directory de Andrea
setwd("~/2019/UVG/Primer Semestre/Minería de Datos/Laboratorios/Laboratorio5/HDT5NaiveBayes/Datos")

# Set del working directory de Sebastian
setwd("C:/Users/sebas/Documents/UVG/201901/Mineria/Laboratorio5/HDT5NaiveBayes/Datos")

# Se cargan todos los datos 
datitus <- read.csv("train2.csv")
str(datitus) # Tipos de variables de las columnas de la base de datos

# Se seleccionan las variables numericas
datos <- datitus[, c("Type", "Age", "Breed1", "Breed2", "Gender", "Color1", "Color2", "Color3", "MaturitySize", "FurLength", "Vaccinated", "Dewormed", "Sterilized", "Health", "Quantity", "Fee", "State", "AdoptionSpeed")]

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Naive Bayes
# ------------

porcentaje<-0.7
set.seed(123)

# Datos de entrenamiento y prueba
corte <- sample(nrow(datos), nrow(datos)*porcentaje)
train <- datos[corte,]
test <- datos[-corte,]
test
test$AdoptionSpeed

# Adoption Speed
modeloNB <- naiveBayes(as.factor(AdoptionSpeed)~Type + Breed1, data = train)
modeloNB
prediccion <- predict(modeloNB, newdata = test)
prediccion
table(prediccion, test$AdoptionSpeed)

# Matriz de confusión
confusionMatrix(as.factor(prediccion), as.factor(test$AdoptionSpeed))


