#------------------------------------------------------------------------------------ 
#Goal: Train Models
#Description: criar modelos, encontrar métricas e testá-los
#Developer: Letícia Marçal
#------------------------------------------------------------------------------------

#linkar com o preprocess
source("Scripts/Preprocess.R")

#excluir coluna product_id
readyData <- readyData[, -9]

#set seed
set.seed(123)

#partindo o dataset
inTrain <- createDataPartition(y = readyData$Volume, p = .75, list = FALSE)

#criando train and test set
training <- readyData[ inTrain,]
testing <- readyData[-inTrain,]

#criar crossvalidation para colocar dentro do modelo a seguir
crossV <- trainControl(method = 'repeatedcv', repeats = 10)

#set seed
set.seed(123)

#treinar o modelo - Linear Regression / 2s, 4s e Positiv
LRmodel_24P <- train(Volume ~ x4StarReviews + x2StarReviews + PositiveServiceReview,
                 data = readyData, 
                 method = 'lm', 
                 preProc = c('center','scale'), 
                 tuneLength = 2, 
                 trControl = crossV)

#Métricas
# RMSE: 179.9835
# Rsquared: 0.922842
# MAE: 116.7756

#fazer a prediction no test
LRmodel_24P_pred_test <- predict(LRmodel_24P, testing)

#postresample
postResample(testing$Volume, LRmodel_24P_pred_test)

#métricas
# RMSE: 78.3168290
# Rsquared: 0.9903485
# MAE: 62.9497862


#set seed
set.seed(123)

#treinar o modelo - Linear Regression / 2s e 4s 
LRmodel_24 <- train(Volume ~ x4StarReviews + x2StarReviews,
                     data = readyData, 
                     method = 'lm', 
                     preProc = c('center','scale'), 
                     tuneLength = 2, 
                     trControl = crossV)

#Métricas
# RMSE: 310.4431  
# Rsquared: 0.7626305  
# MAE: 201.9217

#fazer a prediction no test
LRmodel_24_pred_test <- predict(LRmodel_24, testing)

#postresample
postResample(testing$Volume, LRmodel_24_pred_test)

#métricas
# RMSE: 370.0506789 
# Rsquared: 0.5725199   
# MAE: 176.1371951  

#set seed
set.seed(123)

#treinar o modelo - Linear Regression / todos atributos
LRmodel <- train(Volume ~ ., 
                 data = readyData, 
                 method = 'lm', 
                 preProc = c('center','scale'), 
                 tuneLength = 2, 
                 trControl = crossV)

#métricas
# RMSE: 195.8234
# Rsquared: 0.8793424
# MAE: 125.2005

#set seed
set.seed(123)

#fazer a prediction no test
LRmodel_pred_test <- predict(LRmodel, testing)

#postreample
postResample(testing$Volume, LRmodel_pred_test)

#métricas
# RMSE: 79.4592628  
# Rsquared: 0.9894325
# MAE: 62.1087898

#set seed again pra ver se não foi só coincidência as métricas estarem tão boas
set.seed(128)

#fazer a prediction no test
LRmodel_pred_test <- predict(LRmodel, testing)

#postresample
postResample(testing$Volume, LRmodel_pred_test)

#métricas
# RMSE:79.4592628  
# Rsquared:0.9894325
# MAE:62.1087898

######treinar com SVM Linear

#set seed
set.seed(123)

#treinar o modelo SVM
SVMmodel <- train(Volume ~ ., 
                 data = readyData, 
                 method = 'svmLinear', 
                 preProc = c('center','scale'), 
                 tuneLength = 2, 
                 trControl = crossV)

#métricas
# RMSE: 176.2364
# Rsquared: 0.9227287
# MAE: 112.2782

#fazer a prediction no test
SVMmodel_pred_test <- predict(SVMmodel, testing)

#postResample
postResample(testing$Volume, SVMmodel_pred_test)

#métricas
# RMSE: 60.1533528
# Rsquared: 0.9910691
# MAE: 49.1541237 

#####Treinar Random Forest (rf)

#set seed
set.seed(123)

#treinar o modelo RF
RFmodel <- train(Volume ~ ., 
                 data = readyData, 
                 method = 'rf', 
                 preProc = c('center','scale'), 
                 tuneLength = 2, 
                 trControl = crossV)

#métricas
# mtry  RMSE      Rsquared   MAE      
# 2     201.8294  0.8980015  123.63107
# 7     159.4774  0.9245442   94.30795


#fazer a prediction no test
RFmodel_pred_test <- predict(RFmodel, testing)

#postresample
postResample(testing$Volume, RFmodel_pred_test)

#métricas
# RMSE: 86.6370612
# Rsquared: 0.9787145
# MAE: 40.0490696

#####treinar k-NN

#set seed
set.seed(123)

#treinar o modelo k-NN
KNNmodel <- train(Volume ~ ., 
                  data = readyData, 
                  method = 'knn', 
                  preProc = c('center','scale'), 
                  tuneLength = 2, 
                  trControl = crossV)

#métricas
# k  RMSE      Rsquared   MAE     
# 5  210.5469  0.8527509  125.7040
# 7  208.1914  0.8670417  123.2562

#fazer a prediction no test
KNNmodel_pred_test <- predict(KNNmodel, testing)

#postresample
postResample(testing$Volume, KNNmodel_pred_test)

#métricas
# RMSE: 126.1550107 
# Rsquared: 0.9599681 
# MAE: 0.9599681 



