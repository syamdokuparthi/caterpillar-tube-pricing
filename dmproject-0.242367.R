library(data.table)
library(xgboost)
library(Matrix)
library(methods)
train  <- read.csv("C:/Users/SyamPrasad/Documents/Dmproject-R/train.csv",header = T)
test  <- read.csv("C:/Users/SyamPrasad/Documents/Dmproject-R/test.csv",header = T)
cost  <- read.csv("C:/Users/SyamPrasad/Documents/Dmproject-R/cost.csv",header = T)
ids  <- test$id
cost  <- cost$cost
train$id  <- -(1:nrow(train))
test$cost  <- 0

data  <- rbind(train,test)
train  <- data[which(data$id < 0), ]
test  <- data[which(data$id > 0), ]

train$id  <- NULL 
test$id  <- NULL
test$cost  <- NULL


tr.mf  <- model.frame(as.formula(paste("cost ~",paste(names(train),collapse = "+"))),train)
tr.m  <- model.matrix(attr(tr.mf,"terms"),data = train)
tr  <- Matrix(tr.m)
t(tr)


te.mf  <- model.frame(as.formula(paste("~",paste(names(test),collapse = "+"))),test)
te.m  <- model.matrix(attr(te.mf,"terms"),data = test)
te  <- Matrix(te.m)
t(te)




cost.log  <- log(cost+1) 

tr.x  <- xgb.DMatrix(tr,label = cost.log)
te.x  <- xgb.DMatrix(te)



par  <-  list(booster = "gbtree",
              objective = "reg:linear",
              min_child_weight = 10,
              ets = 0.3,
              gamma = 1,
              subsample = 0.85,
              colsample_bytree = 0.75,
              max_depth = 15,
              verbose = 1,
              scale_pos_weight = 1)

learn_rate= 200



x.mod.t  <- xgb.train(params = par, data = tr.x , nrounds = learn_rate)
pred  <- predict(x.mod.t,te.x)
head(pred)

for(i in 1:50){
  x.mod.t  <- xgb.train(par,tr.x,learn_rate)
  pred  <- cbind(pred,predict(x.mod.t,te.x))
  
}
pred.sub[which(pred.sub < 0)]=0
pred.sub  <- exp(rowMeans(pred))-1



sub.file = data.frame(id = ids, cost = pred.sub)
sub.file = aggregate(data.frame(cost = sub.file$cost), by = list(id = sub.file$id), mean)

write.csv(sub.file, "C:/Users/SyamPrasad/Documents/Dmproject-R/submit.csv", row.names = FALSE, quote = FALSE)

