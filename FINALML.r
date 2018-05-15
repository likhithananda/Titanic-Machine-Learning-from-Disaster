#install.packages("corrplot")
#install.packages("randomForest")
#install.packages("class")
#install.packages("e1071")
#install.packages("pROC")



#Read the dataset
TitanicData <- read.csv("C:/Users/likhi/Desktop/train.csv")

#PREPROCESSING DATA

#Removing the irrelavent columns from TitanicData
TitanicData <- TitanicData[-c(1,4,9,10,11)]
View(TitanicData)
#This is the summary of the Titanic dataset
summary(TitanicData)

#Handling the Null values in Embarked
TitanicData$Embarked <- as.numeric(as.factor(TitanicData$Embarked))
TitanicData <- TitanicData[!is.na(TitanicData$Embarked),]
#Replacing the NAs in age with the most frequent age value
mostfreq <- tail(names(sort(table(TitanicData$Age))), 1)


TitanicData[is.na(TitanicData)] <- mostfreq

#Categorizing gender column as 1 for male and 2 for female
TitanicData[,3] <- ifelse(TitanicData[,3]=="male",1, 2)
#Categorizing age as <=20 =1 >20 & <45=2, >45 =3
TitanicData[,4] <- ifelse( TitanicData[,4] <= 20,1, ifelse(TitanicData[,4] <= 45,2,3))

#To View Correlation plot
library(corrplot)
p <- cor(TitanicData)
corrplot(p,method = "pie")

folds <- 10;
id <- sample(1:folds, nrow(TitanicData), replace = TRUE);
list <- 1:folds;

#SVM
library(e1071)
SVMPrec = 0.0
SVMrec = 0.0
SVMFMeas = 0.0
SVMAccuracy = 0.0

for(f in 1:folds){
  Ttrain  = subset(TitanicData, id %in% list[-i]);
  Ttest = subset(TitanicData, id %in% c(i));
  Ttrain$Survived = as.factor(Ttrain$Survived);
  Smodel	<- svm(Survived	~	.,	data	=	Ttrain,	type='C',kernel = "polynomial",degree=2)
  Spredict	<- predict(Smodel,	Ttest[,-1])
  S_table <- table(Ttest$Survived, Spredict)
  #View(S_table)
  SVMPrec = SVMPrec + diag(S_table)/rowSums(S_table)
  SVMPrec = mean(SVMPrec)
  SVMAccuracy = SVMAccuracy + sum(diag(S_table))/sum(S_table)
  
  SVMrec = SVMrec + diag(S_table)/colSums(S_table)
  SVMrec = mean(SVMrec)
}
SVMAccuracy = SVMAccuracy*100/folds
SVMPrec <- SVMPrec/folds
SVMrec <- SVMrec/folds
SVMFMeas <- 2*SVMPrec*SVMrec/(SVMPrec+SVMrec)
cat("SVM acuracy is:", SVMAccuracy)
cat("SVM Precision:",SVMPrec)
cat("SVM Recall:",SVMrec)
cat("SVM F Measure:",SVMFMeas)
require(pROC)
respSVM <- predict(Smodel, Ttest, type="response")
plot(roc(Ttest$Survived,as.numeric(respSVM)),col="red")


#KNN

library(class)

sum=0
Sums <- c(0,0,0,0)
for (f in 1:folds)
{
  #Splitting the Data
  KNNTrain<- subset(TitanicData, id %in% list[-i])
  KNNTest <- subset(myData, id %in% c(i))
  #Building the model
  KNNbuild<- knn(KNNTrain[,2:7], KNNTest[,2:7],KNNTrain[,1],k=10)
  KNNTable<- table("Predictions" = KNNbuild, Actual = KNNTest[,1])
  err <- (sum(diag(KNNTable)) / sum(KNNTable))*100.0
  print(err)
  sum <- sum + err
  df <- table("Predictions" = KNNbuild, Actual = KNNTest[,1])
  KNNRec <- (diag(df) / colSums(df))
  KNNRec <- mean(KNNRec)
  KNNPrec <- diag(df) / rowSums(df)
  KNNPrec <- mean(KNNPrec)
  KNNFMeas <- (2*KNNPrec*KNNRec)/(KNNPrec+KNNRec)
  KNNFMeas <- mean(KNNFMeas)
  accuracy <- (sum(diag(df)) / sum(df))*100.0
  Sums[1] <- Sums[1] + accuracy
  Sums[2] <- Sums[2] + KNNPrec
  Sums[3] <- Sums[3] + KNNRec
  Sums[4] <- Sums[4] + KNNFMeas
  
}

AccuracyKNN <- Sums[1]/folds
print(AccuracyKNN)
cat("KNN Accuracy is:",AccuracyKNN)
KNNPrec <- Sums[2]/folds
cat("Precision for KNN is:",KNNPrec)
KNNRec <- Sums[3]/folds
cat("Recall for KNN is:",KNNRec)
KNNFMeas <- Sums[4]/folds
cat("Fscore for KNN is:",KNNFMeas)

#Random Forest
library(randomForest)
set.seed(9)

RFAccuracy <- 0.0;
RFPrec <- 0;
RFRec <- 0;

for (f in 1:folds) {
  RFTrain  <- subset(myData, id %in% list[-i]);
  RFTest <- subset(myData, id %in% c(i));
  modelRF <- randomForest(as.factor(Survived)~., RFTrain, importance = FALSE, ntree=200, proximity = TRUE);
  PredictionRF <- predict(modelRF, RFTest);
  RFTable <- table(PredictionRF, RFTest$Survived);
  RFAccuracy <- RFAccuracy + sum(diag(RFTable))/sum(RFTable);
  RFPrec <- RFPrec + diag(RFTable)/rowSums(RFTable)
  RFPrec <- mean(RFPrec)
  RFRec <- RFRec + diag(RFTable)/colSums(RFTable)
  RFRec <- mean(RFRec)
}

RFAccuracy <- RFAccuracy*100/folds;
RFPrec <- RFPrec/folds
RFRec <- RFRec/folds
RFFMeas <- (2*RFRec*RFPrec)/(RFRec+RFPrec)
cat("Random Forest Accuracy:",RFAccuracy)
cat("Random FOrest Precision:",RFPrec)
cat("Random FOrest Recall:",RFRec)
cat("Random FOrest F Measure:",RFFMeas)
#View(RFTable)
plot(modelRF, log = "y")
importance(modelRF)
require(pROC)
respRF <- predict(modelRF, RFTest, type="response")
plot(roc(RFTest$Survived,as.numeric(respRF)),col="blue")