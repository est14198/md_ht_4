#Christopher Sandoval 13660
#Maria Fernanda Estrada 14198


#Análisis del modelo
data_training <- read.csv("train.csv", stringsAsFactors = FALSE)

data_training_filtered <- data_training[, c("SalePrice", "OverallQual", "TotalBsmtSF", "X1stFlrSF", "GrLivArea", "YearBuilt")]

fitLMPW<-lm(SalePrice~., data = data_training_filtered)

summary(fitLMPW)

library(ggplot2)
ggplot(data = data_training_filtered, mapping = aes(x = X1stFlrSF, y = SalePrice)) +
geom_point(color = "firebrick", size = 2) +
geom_smooth(method = "lm", se = TRUE, color = "black") +
labs(title = "Sale Price ~ X1stFlrSF", x = "X1stFlrSF", y = "SalePrice") +
theme_bw() + theme(plot.title = element_text(hjust = 0.5))


#Análisis de las variables

head(fitLMPW$residuals)

plot(data_training_filtered)

hist(fitLMPW$residuals)
boxplot(fitLMPW$residuals)
qqnorm(fitLMPW$residuals)
qqline(fitLMPW$residuals, col="red")
install.packages("nortest")
library(nortest)
lillie.test(fitLMPW$residuals)


install.packages("corrplot")
library(corrplot)
matriz_cor <- cor(data_training_filtered)
matriz_cor
corrplot(matriz_cor)

data_test <- read.csv("test.csv", stringsAsFactors = FALSE)
data_test_filtered <- data_test[, c("OverallQual", "TotalBsmtSF", "X1stFlrSF", "GrLivArea", "YearBuilt")]

predLM<-predict(fitLMPW, newdata = data_test_filtered)
library(caret)
data_sample <- read.csv("sample_submission.csv", stringsAsFactors = FALSE)
RMSE(data_sample$SalePricen,predLM)
plot(data_sample$SalePrice,col="blue")
points(predLM, col="red")
summary(test$Petal.Length-predLM)

