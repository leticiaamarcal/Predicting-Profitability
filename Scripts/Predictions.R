#------------------------------------------------------------------------------------ 
#Goal: Fazer Predictions
#Description: aplicar modelo criado para fazer predictions 
#Developer: Letícia Marçal
#------------------------------------------------------------------------------------

#linkar com o preprocess
source("Scripts/TrainModels.R")

#upload data
newData <- read.csv("C:/Users/letic/Desktop/newproductattributes2017.csv")

#dummy
newDFP <- dummyVars(" ~ .", data = newData)
newProducts <- data.frame(predict(newDFP, newdata = newData))

#eliminar colunas
newProducts <- newProducts[,-c(1, 2, 3, 4, 8, 9, 11, 12, 14, 15, 17, 19, 21, 22, 23, 24, 25, 26, 27, 29)]

#mudar nome das colunas
colnames(newProducts)[1] <- 'Laptop'
colnames(newProducts)[2] <- 'Netbook'
colnames(newProducts)[3] <- 'PC'
colnames(newProducts)[4] <- 'Smartphone'

#transformar em ID
rownames(newProducts) <- newProducts$ProductNum

#excluir colunas de ProductNum
newProducts <- newProducts[,-5]

#Random Forest e SVM Linear foram os melhores modelos, então vou fazer
#a previsão com eles

###predictions com Random Forest
newProducts$VolumeRF <- predict(RFmodel, newProducts)

###predictions com SVM Linear
newProducts$VolumeSVM <- predict(SVMmodel, newProducts)

##vou testar os outros modelos just in case

## k-NN
newProducts$VolumeKNN <- predict(KNNmodel, newProducts)

## Linear Regression LRmodel
newProducts$VolumeLR <- predict(LRmodel, newProducts)
