library(ggplot2)
library(readr)
dataset= read.csv('C:/Users/USER INFO/Desktop/R_Fct/winequality-red.csv',sep=';')
#--------------------------------#
#Histograme
hist(dataset$quality)
#Proportion de nos classes
prop.table(table(dataset$quality))
#--------------------------------#
#Construction d'un modèle d'arbre de décision
#Séparation des données en training et test
library(caTools)
set.seed(123)
split=sample.split(dataset,SplitRatio=0.66)
training_set=subset(dataset, split==TRUE)
test_set=subset(dataset,split==FALSE)
prop.table(table(training_set$quality))
#Construction de l'arbre de décision ) partir du training set
library(rpart)
model_control=rpart.control(xval=10,cp=0.00001)
fit_original = rpart(quality~.,control=model_control,data=training_set)
#Étape de Prédiction
pred_test_ori=predict(fit_original,newdata=test_set,type="class")
#Matrice de confusion
cm=table(test_set$quality,pred_test_ori)
cm
#--------------------------------------#
#Fonction accuracy
accuracy=function(table,total){
  n=dim(table)[1]
  sum=0
  for (i in 1:n){
    sum=sum + table[i,i]
  }
  acc=sum/total
  return(acc)
}
acc_original_test=accuracy(cm,dim(test_set[1]))
acc_original_test[1]
#--------------------------------------#
pred_train_ori=predict(fit_original,newdata = training_set,type="class")
cm2=table(training_set$quality,pred_train_ori)
cm2
acc_original_train=accuracy(cm2,dim(training_set))
acc_original_train[1]
#------------------------------------#
#Nous sommes dans le cas d'un surapprentissage
#On cherche maintenant à limiter la taille des classes
#Pour cela on joint les classes minoritaire entre elles
binned_data=data.frame(dataset)
binned_data= within(binned_data,{
  quality[quality == 3]=4
  quality[quality == 8]=7
})
hist(binned_data$quality)
binned_data$quality=factor(binned_data$quality,levels=c(4,5,6,7),labels=c('4','5','6','7'))
prop.table(table(binned_data$quality))
#Sépare les données une 2ème fois     
set.seed(123)
split=sample.split(binned_data,SplitRatio = 0.66)
binned_train_set=subset(binned_data,split==TRUE)
binned_test_set=subset(binned_data,split==FALSE)
prop.table(table(binned_train_set$quality))
#Maintenant on utilise les méthodes de re-équilibrage
#On utilise le Oversampling
library(caret)
upsample_data=upSample(binned_train_set,binned_train_set$quality)
head(upsample_data)
upsample_data$Class=NULL
prop.table(table(upsample_data$quality))
#-----------------------------------------#
#Création d'un arbre de décision basée sur les données équilibrer
fit_upsample = rpart(quality ~.,control = model_control, data = upsample_data )
pred_upsample_test = predict(fit_upsample, newdata = binned_test_set, type = "class")
cm=table(binned_test_set$quality,pred_upsample_test)
pred_upsample_train=predict(fit_upsample,newdata = upsample_data,type="class")
cm2=table(upsample_data$quality,pred_upsample_train)
paste('La précision de lensemble de test est',accuracy(cm,dim(binned_test_set)[1]))
paste('La précision de lensemble apprentissage est',accuracy(cm2,dim(upsample_data)[1]))
#------------------------------------------#
#Nouvelle technique d'équilibrage des données 
split2=sample.split(binned_train_set,SplitRatio = 0.5)
binned_new=subset(binned_data,split2==TRUE)
binned_old=subset(binned_data,split2==FALSE)
library(caret)
newdata1=upSample(binned_new,binned_new$quality)
newdata1$Class=NULL
dim(newdata1)
upsample_data=rbind(newdata1,binned_old)
dim(upsample_data)
prop.table(table(upsample_data$quality))
#--------------------------------------------#
#Création d'un nouveau modèle suivant ses données d'apprentissage
cm=NULL
cm2=NULL
model_control=rpart.control(xval=10,minisplit=12,cp=0.00001)
fit_upsample=rpart(quality~.,control=model_control,data=upsample_data)
plot(fit_upsample)
pred_upsample_test=predict(fit_upsample,newdata = binned_test_set,type="class")
cm=table(binned_test_set$quality,pred_upsample_test)
pred_upsample_train=predict(fit_upsample,newdata = upsample_data,type='class')
cm2=table(upsample_data$quality,pred_upsample_train)
paste("l'accuracy de notre test est",accuracy(cm,dim(binned_test_set)[1]))
paste("l'accuracy de nos données d'apprentissage est",accuracy(cm2,dim(upsample_data)[1]))
#----------------------------------------------#
#Voyons voir pour SMOTE les Résultats que l'on aura
library(DMwR)
set.seed(123)
newdata1=SMOTE(quality~.,prec.over=600,perc.under = 500,data=binned_new)
prop.table(table(newdata1$quality))
newdata1=rbind(newdata1,binned_old)
fit1=rpart(quality~.,control = model_control,data = newdata1)
pred_fit1=predict(fit1,newdata=binned_test_set,type="class")
cm=table(binned_test_set$quality,pred_fit1)
pred_train=predict(fit1,newdata = newdata1,type='class')
cm2=table(newdata1$quality,pred_train)
paste("l'accuracy de notre test est",accuracy(cm,dim(binned_test_set)[1]))
paste("l'accuracy de nos données d'apprentissage est",accuracy(cm2,dim(newdata1)[1]))
#----------------------------------------------#
newdata1=downSample(binned_train_set,binned_train_set$quality)
newdata1$Class=NULL
dim(newdata1)
newdata1=rbind(newdata1,binned_old)
prop.table(table(newdata1$quality))
fit2=rpart(quality~.,control = model_control,data = newdata1)
pred_fit2=predict(fit2,newdata=binned_test_set,type="class")
cm=table(binned_test_set$quality,pred_fit2)
pred_train=predict(fit2,newdata = newdata1,type="class")
cm2=table(newdata1$quality,pred_train)
paste("l'accuracy de notre test est",accuracy(cm,dim(binned_test_set)[1]))
paste("l'accuracy de nos données d'apprentissage est",accuracy(cm2,dim(newdata1)[1]))
#-----------------------------------------------#
