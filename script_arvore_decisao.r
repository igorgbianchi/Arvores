library("rpart")
library("rpart.plot")
dados <- read.table("D:/Desktop/trabIA/trab2/data_car.csv", header = TRUE, sep=",")
modelo_ad <- rpart(Class ~ buying + maint + doors + persons + lug_boot + safety, 
                   data = dados, method = "class", parms = list(split = "Information"))
plot_ad <- rpart.plot(modelo_ad, type = 3, cex = 0.5)
dados_teste <- read.table("D:/Desktop/trabIA/trab2/data_carTest.csv", 
                          header = TRUE, sep =",")

## faz a previsão de classe para os dados de teste
y_estimado <- predict(modelo_ad, dados_teste, "class")
y_estimado
c <- 0

## calcula a quantidade de acertos em relação ao que foi previsto e os dados de teste
for(i in 1:length(y_estimado)){
  if(as.vector(y_estimado)[i] == as.vector(dados_teste[['Class']])[i]){
    c <- c + 1
  }
}

## calcula precisão
precisao <- c/length(y_estimado)

## calcula taxa de erro
erro <- 1 - precisao
precisao
erro
