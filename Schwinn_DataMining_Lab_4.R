#############################
#Data Mining Practical Session
#
#Subject: Recommender Systems and Crime Analysis
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
#install.packages('ggplot2')
#install.packages('Hmisc')
#install.packages('randomForest')
#install.packages("snowfall")
library(rstudioapi)
library(ggplot2)
library(Hmisc)
library(randomForest)
library(snowfall)

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

#PCA for the home team
X <- Xs[,grep("_C$", names(Xs))]

#Build PCA funtion
PCA <- function(X,b){
  pca         <- princomp(X, cor=T, scores=T)
  loadings    <- pca$loadings
  obj.scores  <- pca$scores[,1:b]
  Rot         <- varimax(loadings[,1:b])
  list(eigvals=(pca$sdev)^2, loadings=loadings, obj.scores=obj.scores,
       loadings.rot=Rot$loadings, obj.scores.rot=scale(obj.scores)%*%Rot$rotmat)
}

#Use pca function on home team
dims          <- 3
pca           <- PCA(X,dims)
objs.rot.home <- pca$obj.scores.rot

#Visualize PCA
par(mfrow=c(1,2),cex=1.7)
p <- length(pca$eigvals)
win.graph(800,600,10)
plot(pca$eigvals, xaxp=c(1,p,p-1), type='b',  main="Scree Plot",
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
