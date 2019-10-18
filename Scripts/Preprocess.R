#------------------------------------------------------------------------------------ 
#Goal: Preprocess
#Description: Limpar os dados e transformar variáveis
#Developer: Letícia Marçal
#------------------------------------------------------------------------------------

#library
library(caret)
library(ggplot2)
library(lattice)
library(corrplot)
library(dplyr)

#upload arquivo
existing_products <- read.csv("C:/Users/letic/Documents/UbiqumR/Course2_task3/Data/Raw/existingproductattributes2017.csv")

#separar os atributos nominais em diferentes colunas. Se colocar ., a função
#vai escolher todos os atributos nominais. Se quiser espeficificar qual quer
#transformar, só escrever o nome no atributo na fórmula. Nesse caso estamos separando
#em diferente colunas, porque só queremos olhar para quatro dos 13 tipos de produtos
newDataFrame <- dummyVars(" ~ .", data = existing_products)
cleanData <- data.frame(predict(newDataFrame, newdata = existing_products))

#mudar nomes
colnames(cleanData)[5] <- 'Laptop'
colnames(cleanData)[6] <- 'Netbook'
colnames(cleanData)[7] <- 'PC'
colnames(cleanData)[10] <- 'Smartphone'

#Tranformar o número do produto em ID
rownames(cleanData) <- cleanData$ProductNum

#excluir coluna BestSellersRank (23), porque tem 15 missing values. Exclui também
#todas as colunas com os tipos de produtos que não interessam. E tipo de produto
#e número do produto
cleanData <- cleanData[,-c(1, 2, 3, 4, 8, 9, 11, 12, 13, 23)]

#correlation matrix
corrData <- cor(cleanData)

#plotar o correlation matrix
corrplot(corrData)

#excluir mais colunas depois do correlationmatrix
cleanData <- cleanData[,-c(5, 6, 8, 10, 12, 13, 14, 15, 16, 17, 18)]

#boxplot as colunas
boxplot(cleanData$x4StarReviews)
boxplot(cleanData$x4StarReviews)$out

boxplot(cleanData$x2StarReviews)
boxplot(cleanData$x2StarReviews)$out

boxplot(cleanData$PositiveServiceReview)
boxplot(cleanData$PositiveServiceReview)$out

boxplot(cleanData$Volume)
boxplot(cleanData$Volume)$out

#encontrei um cluster de 8 rows repetidos que não fazem sentido.
#vou excluir por ID e colocar do dataset readyData
readyData <- cleanData[-c(34, 35, 36, 37, 38, 39, 40, 41), ]

#criar coluna de ID, porque eu exclui antes. É preciso para filtrar
readyData$product_id <- rownames(readyData)

#quero excluir as linhas eu dizem que o volume é zero, pois tem erro
#como é possível ter avaliação, se o volume é zero?
#usando o pacote ‘dplyr’ para filtrar as linhas, mas usando coluna
readyData <- filter(.data = readyData, !(product_id %in% c(129, 132, 166)))

#bloxpotar para ver outliers agora sem os valores que eu excluí
boxplot(readyData$x4StarReviews)
boxplot(readyData$x4StarReviews)$out

boxplot(readyData$x2StarReviews)
boxplot(readyData$x2StarReviews)$out

boxplot(readyData$PositiveServiceReview)
boxplot(readyData$PositiveServiceReview)$out

boxplot(readyData$Volume)
boxplot(readyData$Volume)$out

#excluir outliers por ID
readyData <- filter(.data = readyData, !(product_id %in% c(118, 123, 148, 150, 198)))


#boxplot depois de tirar o oulier
boxplot(readyData$x4StarReviews)
boxplot(readyData$x2StarReviews)
boxplot(readyData$PositiveServiceReview)
boxplot(readyData$Volume)
