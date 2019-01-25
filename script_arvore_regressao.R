library("rpart")
library("rpart.plot")
dados <- read.table("D:/Desktop/trabIA/trab2/data_abalone.csv", header = TRUE, sep=",")
modelo_ad <- rpart(rings ~ sex + length + diameter + heigth + whole_weight + shucked_weight + viscera_weight + shell_weight,
                   data = dados,  method = "anova", parms = list(split = "Information"))
rpart.plot(modelo_ad, type = 3,cex = 0.55)
dados_teste <- read.table("D:/Desktop/trabIA/trab2/data_abaloneTest.csv", header = TRUE, sep =",")

## faz a previsão de classe para os dados de teste
y_estimado <- predict(modelo_ad, dados_teste, "vector")
y_estimado

c <- 0

## calcula erro quadrático
for(i in 1:length(y_estimado)){
  c <- c + (as.vector(y_estimado)[i] - as.vector(dados_teste[['rings']])[i])^2
}
erro <- c/length(y_estimado)

a <- 0

## calcula a distancia absoluta media
for(i in 1:length(y_estimado)){
  a <- a + abs(as.vector(y_estimado)[i] - as.vector(dados_teste[['rings']])[i])

}
distancia <- a/length(y_estimado)

erro
distancia

