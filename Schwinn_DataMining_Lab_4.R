#############################
#Data Mining Practical Session
#
#Subject: RPredict Futbol match outcomes using 
#Random Forests, Multilayer Perceptron Neural 
#Network (MLP), K-Nearest Neighbor Classifiers, 
#Naive Bayesian Classifier, and Multinomial 
#Logisitc Regression (MLogit Regression).
#
#Author: Austin Schwinn
#
#Feb 8, 2017
#############################
#############################
#Exercise 1: 
#Football data mining


#Visualization
#descriptive statistics
#random forests
#snowfall

#install.packages('rstudioapi')
#install.packages('Rcpp')
#install.packages('ggplot2')
#install.packages('Hmisc')
#install.packages('randomForest')
#install.packages("snowfall")
#install.packages('caret')
#install.packages('doParallel')
#install.packages('RSNNS')
#install.packages('e1071')
#install.packages('klaR')
#install.packages('nnet')
#install.packages('hmisc')
#install.packages('raster')
library(rstudioapi)
library(Rcpp)
library(ggplot2)
library(Hmisc)
library(randomForest)
library(snowfall)
library(caret)
library(doParallel)
library(RSNNS)
library(e1071)
library(klaR)
library(nnet)
library(raster)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load in dataset
#Data is currently stored in R format
rm(list=ls())
load(file="Data and Dependencies/football_data.Rdata")
ls()
str(dtset)

#Col 1 is target variable. Current format is 1,2,X
#Replace this with W,L,D for win, lose, draw
y   <- factor(dtset[,1], labels=c("W","L","D"))
#Place all explanatory features in seperate DF from labels
Xf  <- dtset[,2:ncol(dtset)]

#Our aim is to select and identify factors most strongly
#associated with match outcomes
#Before model specification, we do exploratory analysis to 
#determine the main characteristics of the data.

#Investigate target variable distribution with table of absolute
#and percentage frequencies,
AbsFreq <- table(y)
PerFreq <- round(prop.table(AbsFreq)*100,1)
cbind(AbsFreq, PerFreq)

#The distribution of the target variable shows breakdown of
#47% wins for the hometeam, about half and 27% and 25% for losses
#and draws, which are relatively similar.
#This would support the idea of home field advantage

#Visualize distribution with bar chart
Freq <- data.frame(PerFreq)
ggplot(Freq, aes(x="", fill=y, weight=Freq))+ geom_bar(width=1)+ 
  scale_fill_manual(values=c("green","yellow","red"))+
  scale_y_continuous("Percentage frequency")+ scale_x_discrete(name="")+
  theme(text=element_text(size=24))

#Focusing on covariates in matrix Xf, calc selected
#univariate descriptive statistics
describe(Xf)  

#For example, the descriptive statistic for O_OCCAS_C var (number
#of scoring opportunities by the home team) are mean 4.942, median = 5,
#min=0, max=14. Focusing on the relationship between the target var
#and the covariates, we investigate how the distribution of the target
#variable changes for dif values of a single covariate

#O_OCCAS_C var is number of scoring opportunities by the home team
#Focus on relationship between target var and single feature
x.name  <- "O_OCCAS_C"
x       <- Xf[,names(Xf) %in% x.name]
pf      <- prop.table(table(x,y),1)[,c(1,3,2)]
dtst    <- data.frame(PctFreq=c(t(pf)),
                      x=rep(as.numeric(rownames(pf)),each=ncol(pf)),
                      Outcome=ordered(rep(1:3,nrow(pf)), labels=colnames(pf)))
#using geom_area, we an can easily draw and aread graph of W,L,D 
#percentages as the value of O_OCCAS_C varies
ggplot(dtst, aes(x=x, y=PctFreq, group=Outcome, fill=Outcome))+
  geom_area(position="fill")+ scale_x_continuous(x.name)+
  scale_y_continuous("Percentage_frequency")+
  scale_fill_manual(values = c("green","yellow","red"))+
  theme(text=element_text(size=24))
#This plot shows that when the number of home team scoring opportunities
#increases, the frequency of wins for the home team increases.

####
#Random Forest

#Observe relationships between features and labels representing match outcome.
#Since the number of observed covariates is higher than the number of same units
#(n < p), we have decided to include a preliminary variable selection step in 
#our procedure. To identify the variable importance measure(VIM), we use the
#Random Forest algorith because it is well suited to treat n<p.

#Randomly permute rows of Xf
Zf <- Xf[sample(nrow(Xf)),]

#Covariate and pseudo-covariate matrices along with target vector
dtset.pseudo <- data.frame(cbind(Xf,Zf,y))

#construct random forest
rf <- randomForest(y~., data=dtset.pseudo, ntree=500)

#Set the number of tress in our RF to 500 to limit comp time, but
#a higher number of would be preferable.
#Note: the mtry parameter (denoting the number h of covariates randomly
#selected at each node) is set to teh default value floor(sqrt(ncol(x)))
#(the square root of the number of covs) because the bias-correction algo
#is fairly insensitive to this parameter

#The (biased) mean decrease in node impurity (the Gini VIM) is estimated
#using the importance command with type=2 (type=1 is MDA VIM)
VIMs <- importance(rf,type=2)

#The differences for step 4:
p <- ncol(Xf)
VIMs.unb <- VIMs[1:p,] - VIMs[(p+1):(2*p),]

#The above step must be repeated S times, which may take a long time
#when the number of covariates is in the order of hundreds or greater.
VIMs.unb <- function(k){
  set.seed(k)
  Zf            <- Xf[sample(nrow(Xf)),]
  dtset.pseudo  <- data.frame(cbind(Xf,Zf,y))
  rf            <- randomForest(y~.,data=dtset.pseudo, ntree=500)
  VIMs          <- importance(rf,type=2)
  VIMs[1:p,]-VIMs[(p+1):(2*p),]
}

#Show how the bias-correction algorithm can be parallelized using the
#snowfall package, a usability wrapper for the snow library
#initialize snowfall with parallel computing enabled and request
#a socket cluster with 6 cpus
sfInit(parallel=TRUE, cpus=6,type="SOCK")

#load the rf package onto all computing sockets
sfLibrary(randomForest)

#export objects to each node
sfExport("Xf","y", "p")

#apply VIMs.unb to vector of 1 to 100
VIMs.list <- sfLapply(x=1:100, VIMs.unb)
sfStop()

#convert VIMs.list into matrix of dim (S x p) and calculate unbiased
#Gini VIMs of the p covariates by averaging the values of the differences
#contained in VIMs.list over the S replications
VIMs      <- t(matrix(unlist(VIMs.list),p))
GINI.unb  <- apply(VIMs,2,mean)

#The box-plots of the bias-corrected Gini VIMs for the 481 covariates
#obtained with the S=100 replications displayed
idx <- order(GINI.unb, decreasing=T)
Xs  <- Xf[,idx[1:13]]
vm  <- c(VIMs[,idx])
grp <- c(t(matrix(rep(1:ncol(VIMs),nrow(VIMs)),ncol(VIMs))))
dt  <- data.frame(vm,grp=factor(grp))
ggplot(dt, aes(grp,vm))+ geom_boxplot(outlier.size=0)+
  scale_x_discrete(breaks=c(1,100,200,300,400,p),name="")+
  scale_y_continuous(name="Gini_VIM_corrected")+
  geom_hline(yintercept=0,colour="red", lty=2, lwd=1)+
  theme(text=element_text(size=24))

#highlight presence of 3 groups of covariates: 4 highly
#informative covs (blue), 9 covs with medium importance (red),
#and a residual group (green) of covs of low VIMS (<.5)
dt <- data.frame(id=1:50,
                 VIM=GINI.unb[idx[1:50]],
                 grp=c(rep(1:4),rep(2,9),rep(3,50-4-9)),
                 names=c(names(Xs),rep("",50-13)),
                 cols=c(rep("red",4),rep("blue",9),rep("gray50",50-4-9)))

ggplot(dt, aes(x=id, y=VIM, label=names, colour=cols))+
  geom_point() + scale_color_discrete(l=60)+scale_fill_identity()+
  geom_text(angle=45,hjust=-.05, vjust=0, size=4.2)+
  scale_y_continuous(name="Gini_VIM_corrected", limits=c(0,3.05))+
  scale_x_continuous(name="")+
  theme(legend.position = "none", text=element_text(size=24))

#According to these results, the top 13 explanatory variables are:
#1) O_OCCAS_C (# of scoring opportunities created by home team)
#2) O_OCCAS_O (# of scoring opportunities created by away team)
#3) O_TIRI1_C (# of shots on goal for home team)
#4) O_PALLE2_O (# of ball kicks to bypass the midfield away team)
#5) O_TIRI1_0 (# of shots on goal by away team)
#6) D_RECUPERI14_0 (# of defensive heading in penalty area by away team)
#7) O_CROSS_C (# of crosses by home team)
#8) G_COLPTESTA_C (# of head shot by home team in away team penalty area)
#9) 0_%ATT_):(goal attack % by away team)
#10) D_%PROT_C (% of penalty area defense by home team)
#11) O_CROSS16_C (# of crosses on free kicks by home team)
#12) O_PAS4_O (# of long passes forard from the defense of away team)
#13) D_PALLE17_O (# of balls lost during forward action by home team)

#Store the selected covariates in matrix Xs
Xs <- Xf[,idx[1:13]]
save(Xs,y,file="selected_covariates.RData")

####
#PCA

#After selecting 13 vars that influence on match results, combine 
#variables into smaller dimension space using PCA. 
#Will train 2 seperate PCAs, 1 for home and 1 for away team

#Build PCA funtion
PCA <- function(X,b){
  pca         <- princomp(X, cor=T, scores=T)
  loadings    <- pca$loadings
  obj.scores  <- pca$scores[,1:b]
  Rot         <- varimax(loadings[,1:b])
  list(eigvals=(pca$sdev)^2, loadings=loadings, obj.scores=obj.scores,
       loadings.rot=Rot$loadings, obj.scores.rot=scale(obj.scores)%*%Rot$rotmat)
}

#PCA for the home team
X <- Xs[,grep("_C$", names(Xs))]

#Use pca function on home team
dims          <- 3
pca           <- PCA(X,dims)
objs.rot.home <- pca$obj.scores.rot

#Visualize PCA
par(mfrow=c(1,2),cex=1.7)
p <- length(pca$eigvals)
win.graph(800,600,10)
plot(pca$eigvals, xaxp=c(1,p,p-1), type='b',  main="HOme Team Scree Plot",
     xlab='Principal Components', ylab='Eigenvalues')
lines(c(0,p+1),c(1,1),lty="dashed")
text(pca$eigvals, as.character(round(pca$eigvals, digits=2)),
     cex=0.6, pos=c(4,4,4,4,3,3))

win.graph(800,600,10)
plot(100*cumsum(pca$eigvals)/p, xaxp=c(1,p,p-1), type="b",
     xlab='Principle Components', ylab = "CVAF (%)",
     main='Cumulative Variance Accounted For')
text(100*cumsum(pca$eigvals)/p,
     as.character(round((cumsum(pca$eigvals)/p)*100, digits=1)),
     cex=0.6, pos=c(4,4,4,2,1,1))

#Change signs of first and 3 rotated componenets
objs.rot.home[,1]     <- -objs.rot.home[,1]
pca$loadings.rot[,1]  <- -pca$loadings.rot[,1]
print(pca$loadings.rot, cutoff=0)

#PCA for away team
X <- Xs[,grep("_O$", names(Xs))]

#Use pca function on home team
dims          <- 3
pca           <- PCA(X,dims)
objs.rot.away <- pca$obj.scores.rot

#Visualize PCA
par(mfrow=c(1,2),cex=1.7)
p <- length(pca$eigvals)
win.graph(800,600,10)
plot(pca$eigvals, xaxp=c(1,p,p-1), type='b',  main="Away Team Scree Plot",
     xlab='Principal Components', ylab='Eigenvalues')
lines(c(0,p+1),c(1,1),lty="dashed")
text(pca$eigvals, as.character(round(pca$eigvals, digits=2)),
     cex=0.6, pos=c(4,4,4,4,3,3))

win.graph(800,600,10)
plot(100*cumsum(pca$eigvals)/p, xaxp=c(1,p,p-1), type="b",
     xlab='Principle Components', ylab = "CVAF (%)",
     main='Cumulative Variance Accounted For')
text(100*cumsum(pca$eigvals)/p,
     as.character(round((cumsum(pca$eigvals)/p)*100, digits=1)),
     cex=0.6, pos=c(4,4,4,2,1,1))

#Change signs of first and 3 rotated componenets
objs.rot.away[,1]     <- -objs.rot.away[,1]
pca$loadings.rot[,1]  <- -pca$loadings.rot[,1]
print(pca$loadings.rot, cutoff=0)

Xc        <- data.frame(objs.rot.home, objs.rot.away)
names(Xc) <- c("air.attack.home", "shot.attack.home", "defense.home",
               "defense.away", "shot.attack.away", "counterattack.away")
round(cor(Xc),3)

#Train/test split
set.seed(1991)
dtset.ind <- data.frame(Xc,y)
#Withhold 80 matches for test
index       <- sample(1:nrow(dtset.ind),80)
dtset.train <- dtset.ind[-index,]
dtset.test  <- dtset.ind[index,]

#For parameter controling train function, repeat cross-validation 
#resampling method with 15 epochs with sets of 10 folds
ctrl.train <- trainControl(method='repeatedcv', number=10,repeats=15)

####
#Random Forest

#Make rf classifier
my.rf <- function(train, test){
  clus <- parallel::makeCluster(spec=6, type='PSOCK')
  registerDoParallel(clus)
  
  #Train the RF with varied mrty and store best result based on accuracy
  fit.rf <- caret::train(y ~ ., data=train, method='rf', metric='Accuracy',
              tuneGrid=expand.grid(.mtry=1:6), trControl=ctrl.train,
              ntree=1000)
  stopCluster(clus)
  
  #output of RFs with varying mtry parameter
  print(fit.rf)
  plot(fit.rf)
  
  #Predict on test with trained RF model
  return(predict(fit.rf$finalModel, newdata = test[,1:6], type='class'))
}

#Use random forest function to predict using RF
y.rf <- my.rf(dtset.train,dtset.train)

####
#Multilayer Perceptron (MLP) Neural Network

#MLP classifier with single layer of hidden neurons
my.nnet <- function(train,test){
  clus <- parallel::makeCluster(spec=6, type='PSOCK')
  registerDoParallel(clus)
  
  #Train MLP with varied number of hidden neurons and store best result
  fit.nnet <- caret::train(y~., data=train, method='mlp', metric='Accuracy',
                tuneGrid=expand.grid(.size=1:15), learnFunc='SCG',
                trControl=ctrl.train)
  stopCluster(clus)
  
  #Describe optimal NNET structure
  summary(fit.nnet$finalModel)
  
  #Predict off trained MLP
  probs.nnet <- predict(fit.nnet$finalModel, newdata=test[,1:6])
  head(probs.nnet)
  
  #Original predict output is type class probabilities
  #For test, coerce prediction result to categorical
  out.nnet <- apply(probs.nnet,1,which.max)
  return(factor(out.nnet, levels=1:3, labels = levels(test$y)))
}

#Use mlp function to predict using neural net
y.nnet <- my.nnet(dtset.train,dtset.train)

####
#K-Nearest Neighbor (KNN)

#Start KNN classifier
my.knn <- function(train,test){
  clus <- parallel::makeCluster(spec=6, type = 'PSOCK')
  print(clus)
  registerDoParallel
  
  #Train KNN with k ranging from 5 to 100 and choose k w/ max accuracy
  fit.knn <- caret::train(y~., data=train, method='knn',
                tuneGrid=expand.grid(.k=5:100), metric = 
                "accuracy", trControl=ctrl.train)
  stopCluster(clus)
  
  #Predict using trained knn
  return(predict(fit.knn$finalModel, newdata=test[,1:6], 
            type='class'))
}

#Use knn function to predict 
y.knn <- my.knn(dtset.train,dtset.train)

####
#Naive Bayesian Classifier
my.NB <- function(train,test){
  #Train NBC
  fit.NB <- naiveBayes(y~., data=train)
  
  #Predict off trained NBC
  return(predict(fit.NB, newdata = test[,1:6], type='class'))
}

#Use NB function to predict
y.NB <- my.NB(dtset.train,dtset.test)

####
#Multinomial Logisitic Regression (MLogit Regression)
my.mlog <- function(train,test){
  #Train MLogit Reg
  fit.mlog <- multinom(y~., data=train)
  print(t(coef(fit.mlog)))
  
  #Predict on trained MLogit Reg
  return(predict(fit.mlog, newdata = test[,1:6]))
}

#Predict using mlog function
y.mlog <- my.mlog(dtset.train,dtset.test)

#Save all classifier models in R file
save(fit.rf, fit.nnet, fit.knn, fit.NB, fit.mlog,
     train, test, file="Data and Dependencies/classifieres.RData")

####
#Evaluate the potential models with confusion matrix


#Initialize dictionary for results
cmResults             <- data.frame(matrix(NA, nrow=5, 
                            ncol=7))
colnames(cmResults)   <- c('model', 'itter','acc', 'kappa',
                           'sensW','sensL','sensD')
modNames              <- c('rf','nnet','knn','NB','mlog')

####
#NOTE: 
#This loop takes a long time to run. As an alternative,
# my results are saved as a CSV that can be loaded here:
#cmResults <- read.csv("Data and Dependencies/cmResults.csv")
#If loading previous results, skip to line 489 past
# saving results from the loop
####

#Iterate through models and store results to compare
x <- 1
#Run 100 variations
for(i in 1:100){
  #Create new train/test split
  index       <- sample(1:nrow(dtset.ind),80)
  dtset.train <- dtset.ind[-index,]
  dtset.test  <- dtset.ind[index,]
  
  #Use each of the model functions for each iteration
  y.rf    <- my.rf(dtset.train,dtset.test)
  y.nnet  <- my.nnet(dtset.train,dtset.test)
  y.knn   <- my.knn(dtset.train,dtset.test)
  y.NB    <- my.NB(dtset.train,dtset.test)
  y.mlog  <- my.mlog(dtset.train,dtset.test)
  
  #Predict on new variation
  models    <- list(y.rf,y.nnet,y.knn,y.NB,y.mlog)
  #Results for each model
  for(j in 1:length(models)){
    cMat                  <- caret::confusionMatrix(models[[j]], 
                                dtset.test$y)
    cmResults [x,'model'] <- modNames[j]
    cmResults [x,'itter'] <- i
    cmResults [x,'acc']   <- cMat$overall['Accuracy']
    cmResults [x,'kappa'] <- cMat$overall['Kappa']
    cmResults [x,'sensW'] <- data.frame(cMat$byClass)$Sensitivity[1]
    cmResults [x,'sensL'] <- data.frame(cMat$byClass)$Sensitivity[2]
    cmResults [x,'sensD'] <- data.frame(cMat$byClass)$Sensitivity[3]
    x                     <- x+1
  }
}

#Save results
write.csv(cmResults,"Data and Dependencies/cmResults.csv")

####
#Compare the differenet algorithms to see what model predicts
#the best results
cmResults <- read.csv("Data and Dependencies/cmResults.csv")

#Prepare acc/kappa data to proper df format for plot
acc_kap_box <- data.frame("model"=factor(rep(1:5,200),labels=c("RF",
                "NNET","KNN","NBayes","MLogit")),"metric"=factor(
                c(rep(1,nrow(cmResults)),rep(2,nrow(cmResults))),
                labels=c("Accuracy","Kappa")),"perf"=c(cmResults$acc,
                cmResults$kappa))

#Create box/whisker plot with accuracy and kappa
acc_kap_plot <- ggplot(aes(y=perf,x=model), data=acc_kap_box)+
                  geom_boxplot(aes(fill=model))+
                  coord_flip()+
                  facet_wrap(~metric,ncol=1,scales="fixed")+
                  scale_fill_discrete(guide=F)+
                  scale_x_discrete(name="")+scale_y_continuous(name="")+
                  theme(text=element_text(size=24))
print(acc_kap_plot)        

#Prepare acc/kappa data to proper df format for plot
sens_box <- data.frame("model"=factor(rep(1:5,300),labels=c("RF",
              "NNET","KNN","NBayes","MLogit")),"metric"=factor(
              c(rep(1,nrow(cmResults)),rep(2,nrow(cmResults)),
              rep(3,nrow(cmResults))),labels=c("Sensitivity W",
              "Sensitivity L","Sensitivity D")),"perf"=c(cmResults$sensW,
              cmResults$sensL,cmResults$sensD))

#Box/whisker plot for sensitivity
sens_plot <- ggplot(aes(y=perf,x=model), data=sens_box)+
                geom_boxplot(aes(fill=model))+
                coord_flip()+
                facet_wrap(~metric,ncol=1,scales="fixed")+
                scale_fill_discrete(guide=F)+
                scale_x_discrete(name="")+scale_y_continuous(name="")+
                theme(text=element_text(size=24))
print(sens_plot)  

#create table of mean, standard deviation. and coefficient of
#variation from results accuracy and kappa
accKap <- cbind(aggregate(cmResults[,c('acc','kappa')],list(cmResults$model),mean),
            aggregate(cmResults[,c('acc','kappa')],list(cmResults$model),sd)[,2:3],
            aggregate(cmResults[,c('acc','kappa')],list(cmResults$model),cv)[,2:3])

#Label
rownames(accKap)  <- accKap$Group.1
accKap            <- accKap[,2:ncol(accKap)]
colnames(accKap)  <- c("Acc.AVG","Kappa.AVG","Acc.STDV","Kappa.STDV",
                       "Acc.CV","Kappa.CV")

#Compare accuracy and kappa average, standard deviation, coeff of var
print(accKap)

#create table of mean, standard deviation. and coefficient of
#variation from results of Win, Loss, and Draw Sensitivities
sens <- cbind(aggregate(cmResults[,c('sensW','sensL','sensD')],list(cmResults$model),mean),
          aggregate(cmResults[,c('sensW','sensL','sensD')],list(cmResults$model),sd)[,2:4],
          aggregate(cmResults[,c('sensW','sensL','sensD')],list(cmResults$model),cv)[,2:4])

#Label
rownames(sens)  <- sens$Group.1
sens            <- sens[,2:ncol(sens)]
colnames(sens)  <- c("SensW.AVG","SensL.AVG",'SensD.avg',"SensW.STDV","SensL.STDV",
                     'SensD.STDV',"SensW.CV","SensL.CV",'SensD.CV')


#Compare accuracy and kappa average, standard deviation, coeff of var
print(sens)

#As we compare results, it looks like MLogit has the best accuracry and spread
#For sensitivitiy, the field is pretty even but it looks like MLogit
#still slightly outperforms the other models so we will use 
#MLogit going forward as it is the best balance between accuracy,
#simplicity, and stability
#Additionally, it is interesting that MLogit, an intrinsicly linear
#model outperforms more complex models. This alludes to the data
#following at least somewhat of a linear trend.

####
#Model Redefinement

#Our case study is not strongly imbalanced so we are going to adjust weights
#slightly. W freq is about 2x L and D so adjust home team win to .5 weight 
#of L and D weighted at 1
weight                      <- rep(1,length(dtset.train$y))
weight[dtset.train$y=="W"]  <- .5

#Fit model
fit.mlogit.bal <- multinom(y~., weights=weight, data=dtset.train)

#Compute relative risk ratios
print(t(exp(coef(fit.mlogit.bal))))

#Predict using MLogit model
y.mlogit.bal  <- predict(fit.mlogit.bal, newdata=dtset.test)
CM.mlogit     <- caret::confusionMatrix(y.mlogit.bal, dtset.test$y)
print(CM.mlogit)

#The accuracy of the model stayed close to level and improved the loss
#sensitivity but slightly worsened W sensitivity

####
#Deployment

#Now that have the fitted model, final step is to extract some insightful
#information about key factors that affect match outcome. We will use
#relative risk ratio for this

#Compute relative risk ratios
cf.mlogit <- coef(fit.mlogit.bal)
t(exp(cf.mlogit))

#Results show that shot attack home decreases the change of losing the most
#which makes sense that the more the home team shoots, the more
#likely they are to win. Interestingly, shot attacks seem to decrease the 
#home teams chance of winning, so this could be one strategy for the 
#home team to avoid. Away team attack shots has the biggest impact on losing
#but this is out of team's control