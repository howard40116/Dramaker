###改成1-10
x1=read.csv("C:/Users/Student/Desktop/summary.csv", sep = ",")
x2=x1[,1:2]
x3=x1[,8:ncol(x1)]
x4=cbind(x2,x3)

imdb=numeric(nrow(x4))
for(i in 1:nrow(x4))
{
if(x4$IMDB分數[i]>=1 && x4$IMDB分數[i] <6){imdb[i]=1	
}else if(x4$IMDB分數[i]>=6 && x4$IMDB分數[i] <6.5){imdb[i]=2
}else if(x4$IMDB分數[i]>=6.5 && x4$IMDB分數[i] <6.8){imdb[i]=3
}else if(x4$IMDB分數[i]>=6.8 && x4$IMDB分數[i] <7){imdb[i]=4
}else if(x4$IMDB分數[i]>=7 && x4$IMDB分數[i] <7.2){imdb[i]=5
}else if(x4$IMDB分數[i]>=7.2 && x4$IMDB分數[i] <7.4){imdb[i]=6
}else if(x4$IMDB分數[i]>=7.4 && x4$IMDB分數[i] <7.6){imdb[i]=7
}else if(x4$IMDB分數[i]>=7.6 && x4$IMDB分數[i] <7.9){imdb[i]=8
}else if(x4$IMDB分數[i]>=7.9 && x4$IMDB分數[i] <8.3){imdb[i]=9
}else {imdb[i]=10}
}
imdb

x5=cbind(x4,imdb)

###############改成1-10的決策樹(CART)
x1=na.exclude(x5)
np=ceiling(0.1*nrow(x1))
test.index=sample(1:nrow(x1),np)
x1.test=x1[test.index,]
x1.train=x1[-test.index,]
library(rpart)
x1.tree=rpart(imdb ~ animal + army + art + baseball + bear + beer + blue + boss + brother + buffalo + camera + castle + 
cave + cell + cigarette + city + cliff + college + colony + company + computer + desert + dorm + dragon + driver + duke + evil + family + father + fire + 
fish + friend + galaxy + gun + helicopter + horse + hotel + human + hunter + husband + iron + 
judge + jungle + keyword + kid + knife + laboratory + lie + love + magic + manager + mask + meeting + missal + money + 
monster + mother + mountain + music + news + ninja + office + paint + party + plane + planet + president + prince + princess + 
queen + raven + rain + resistance + restaurant + river + robot + science + 
sea + ship + sister + slave + snow + soldier + study + train + village + 
water + white + wife , data=x1.train)

plot(x1.tree); text(x1.tree)

imdb.train=x1$imdb[-test.index]
train.pred=predict(x1.tree,newdata=x1.train,type="vector")
train.MAPE=mean(abs(imdb.train-train.pred)/imdb.train)
cat("MAPE(train)=",train.MAPE*100,"%\n")

imdb.test=x1$imdb[test.index]
test.pred=predict(x1.tree,newdata=x1.test,type='vector')
test.MAPE=mean(abs(imdb.test-test.pred)/imdb.test)
test.MAPE.output=test.MAPE*100
cat("MAPE(test)=",test.MAPE*100,"%\n")




###############改成A-J的決策樹(貝式分類)
library(e1071)

x1=read.csv("C:/Users/Student/Desktop/catetable.csv")
x1=na.exclude(x1)
np=ceiling(0.1*nrow(x1))
test.index=sample(1:nrow(x1),np)
x1.test=x1[test.index,]
x1.train=x1[-test.index,]
x1.tree=naiveBayes(x1.train$imdb ~ . , data=x1.train)

#predict(x1.tree, x1.train)
#predict(x1.tree, x1.train, type = "raw")
pred <- predict(x1.tree, x1.train)
table.train=table(pred, x1.train$imdb)
#table.train
correct.train=sum(diag(table.train))/sum(table.train)
correct.train


#predict(x1.tree, x1.test)
#predict(x1.tree, x1.test, type = "raw")
pred <- predict(x1.tree, x1.test)
table.test=table(pred, x1.test$imdb)
#table.test
correct.test=sum(diag(table.test))/sum(table.test)
correct.test






#########################A-J決策樹類別版本

x1=read.csv("C:/Users/Student/Desktop/catetable.csv", sep = ",")
x1=na.exclude(x1)
np=ceiling(0.1*nrow(x1))
test.index=sample(1:nrow(x1),np)
x1.test=x1[test.index,]
x1.train=x1[-test.index,]
library(tree)
x1.tree=tree(imdb ~ animal + army + art + baseball + bear + beer + blue + boss + brother + buffalo + camera + castle + 
cave + cell + cigarette + city + cliff + college + colony + company + computer + desert + dorm + dragon + driver + duke + evil + family + father + fire + 
fish + friend + galaxy + gun + helicopter + horse + hotel + human + hunter + husband + iron + 
judge + jungle + keyword + kid + knife + laboratory + lie + love + magic + manager + mask + meeting + missal + money + 
monster + mother + mountain + music + news + ninja + office + paint + party + plane + planet + president + prince + princess + 
queen + raven + rain + resistance + restaurant + river + robot + science + 
sea + ship + sister + slave + snow + soldier + study + train + village + 
water + white + wife , data=x1.train)

plot(x1.tree); text(x1.tree)

#predict(x1.tree, x1.train)
pred <- predict(x1.tree, x1.train, type = "class")
table.train <- table(pred, x1.train$imdb)
#table.train
correct.train=sum(diag(table.train))/sum(table.train)
correct.train

#predict(x1.tree, x1.test)
pred <- predict(x1.tree, x1.test, type = "class")
table.test <- table(pred, x1.test$imdb)
#table.test
correct.test=sum(diag(table.test))/sum(table.test)
correct.test


#########################A-J RForest 類別版本

library(randomForest)
x1=read.csv("C:/Users/Student/Desktop/catetable.csv", sep = ",")
x1=na.exclude(x1)
np=ceiling(0.1*nrow(x1))
test.index=sample(1:nrow(x1),np)
x1.test=x1[test.index,]
x1.train=x1[-test.index,]
x1.tree=randomForest(imdb ~ animal + army + art + baseball + bear + beer + blue + boss + brother + buffalo + camera + castle + 
cave + cell + cigarette + city + cliff + college + colony + company + computer + desert + dorm + dragon + driver + duke + evil + family + father + fire + 
fish + friend + galaxy + gun + helicopter + horse + hotel + human + hunter + husband + iron + 
judge + jungle + keyword + kid + knife + laboratory + lie + love + magic + manager + mask + meeting + missal + money + 
monster + mother + mountain + music + news + ninja + office + paint + party + plane + planet + president + prince + princess + 
queen + raven + rain + resistance + restaurant + river + robot + science + 
sea + ship + sister + slave + snow + soldier + study + train + village + 
water + white + wife , data=x1.train ,importance= TRUE, proximity= TRUE)

print(x1.tree)
#round(importance(x1.tree),2)
#names(x1.tree)
table.rf <- x1.tree$confusion
sum(diag(table.rf))/sum(table.rf)

