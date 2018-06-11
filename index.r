
data(mtcars)
library("MASS")
library("neuralnet")
set.seed(123)
DataFrame<-Boston
help("Boston")
#structure of boston data
str(DataFrame)
hist(DataFrame$medv)
dim(DataFrame)
head(DataFrame)
apply(DataFrame,2,range)
maxValue<-apply(DataFrame,2,max)
minValue<-apply(DataFrame,2,min) 
DataFrame<-as.data.frame(scale(DataFrame,center=minValue,scale=maxValue))
ind<-sample(1:nrow(DataFrame),400)
trainDF<-DataFrame[ind,]
testDF<-DataFrame[-ind,]
allVars<-colnames(DataFrame)
predictorVars<-allVars[!allVars%in%"medv"]
predictorVars<-paste(predictorVars,collapse="+")
form=as.formula(paste("medv~",predictorVars,collapse="+"))
neuralModel<-neuralnet(formula=form,hidden=c(4,2),linear.output=T,data=trainDF)
plot(neuralModel)
predictions<-compute(neuralModel,testDF[,1:13])
str(predictions)
predictions<-predictions$net.result*(max(testDF$medv)-min(testDF$medv))+min(testDF$medv)+min(testDF$medv)
actualValues<-(testDF$medv)*(max(testDF$medv)-min(testDF$medv))+min(testDF$medv)
MSE<-sum((predictions-actualValues)^2)/nrow(testDF)
MSE
plot(testDF$medv,predictions,col='blue',main='Real Vs Predicted',pch=1,cex=0.9,type='p' )
abline(0,1,col="black")
