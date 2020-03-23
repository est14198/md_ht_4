#Christopher Sandoval 13660
#Maria Fernanda Estrada 14198


# ---- Análisis del modelo ----
data_training <- read.csv("train.csv", stringsAsFactors = FALSE)

# Filtrando datos
data_training_filtered <- data_training[, c(2,19,20,35,45,48,52,71,81) ]
data_training_filtered <- data_training_filtered[data_training_filtered$SalePrice < 225000 & data_training_filtered$SalePrice > 25000 ,]

# Generacion del modelo
fitLMPW<-lm(SalePrice~., data = data_training_filtered)
summary(fitLMPW)

# Ejemplos de relacion entre una variable y SalePrice
library(ggplot2)
ggplot(data = data_training_filtered, mapping = aes(x = BedroomAbvGr  , y = SalePrice)) +
geom_point(color = "firebrick", size = 2) +
geom_smooth(method = "lm", se = TRUE, color = "black") +
labs(title = "Sale Price ~ BsmtFinSF1", x = "BsmtFinSF1", y = "SalePrice") +
theme_bw() + theme(plot.title = element_text(hjust = 0.5))


# ---- Análisis de las variables ----

# Residuos
head(fitLMPW$residuals)
hist(fitLMPW$residuals)
boxplot(fitLMPW$residuals)
qqnorm(fitLMPW$residuals)
qqline(fitLMPW$residuals, col="red")
# Pruebas de normalidad
install.packages("nortest")
library(nortest)
lillie.test(fitLMPW$residuals)

# Matriz de correlacion
install.packages("corrplot")
library(corrplot)
matriz_cor <- cor(data_training_filtered)
corrplot(matriz_cor)


# ---- Aplicacion del modelo al set de prueba ----
data_test <- read.csv("test.csv", stringsAsFactors = FALSE)
data_test_filtered <- data_test[, c(2,19,20,35,45,48,52,71) ]
predLM<-predict(fitLMPW, newdata = data_test_filtered)
library(caret)
data_sample <- read.csv("sample_submission.csv", stringsAsFactors = FALSE)
RMSE(data_sample$SalePrice,predLM)
plot(data_sample$SalePrice,col="blue")
points(predLM, col="red")
summary(data_sample$SalePrice-predLM)
hist(data_sample$SalePrice-predLM)
