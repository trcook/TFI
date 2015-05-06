require(foreign)
require(caret)
require(caretEnsemble)

train<-read.csv("train.csv")
test<-read.csv("test.csv")
train$Open.Date<-as.integer(as.POSIXct(as.Date(train$Open.Date,format="%m/%d/%Y"),tz = "GMT"))
test$Open.Date<-as.integer(as.POSIXct(as.Date(test$Open.Date,format="%m/%d/%Y"),tz = "GMT"))
levels(train$Type)<-c(levels(train$Type),"MB")
partition<-createDataPartition(train$revenue,2)

gbmGrid <-  expand.grid(interaction.depth = 3,
                        n.trees = 500,
                        shrinkage = 0.01
)
rpartGrid<-expand.grid(cp=.01,minsplit=10)


tc<-trainControl(method = 'cv',number = 10,repeats = 10)

tl=list(
  gbm=caretModelSpec(method='gbm', tuneGrid=expand.grid(interaction.depth = 3, n.trees = 500, shrinkage = 0.01 )  ),
  ANFIS=caretModelSpec(method='ANFIS',tuneGrid=expand.grid(num.labels=c(5),max.iter=10))
  #bstTree=caretModelSpec(method='bstTree',tuneGrid=data.frame(mstop=50,maxdepth=3,nu=.001))
  #rpart=caretModelSpec(method='rpart', tuneGrid=data.frame(cp=.01))
  #rf=caretModelSpec(method='rf',tuneGrid=data.frame(mtry=10))
  #dnn=caretModelSpec(method='dnn',tuneGrid=data.frame(layer1=50,layer2=100,layer3=50,hidden_dropout=.1,visible_dropout=.0001))
)

require(doSNOW)
cl<-makeCluster(4)
registerDoSNOW(cl)
model_list <- caretList(
  revenue~., data=train[,c(2,6:16,43)],
  tuneList=tl,
  trControl = tc
  # methodList=c('gamboost')
)


greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)
glmensemble<-caretStack(
  model_list,
  method='glm',
  trControl=trainControl(method='repeatedcv',number=5,repeats = 10)
)


sample<-read.csv(file.path(root_dir,"sampleSubmission.csv"))
submit<-predict(glmensemble,test)
sample$Prediction<-submit
write.csv(sample,file="submit.csv",row.names=F,col.names=F)