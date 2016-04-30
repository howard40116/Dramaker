#############################################################################
#じ计-tree∕郸攫
#############################################################################
x=read.csv("C:/Users/howard/Desktop/summary.csv")
x=na.exclude(x)
np=ceiling(0.1*nrow(x))
test.index=sample(1:nrow(x),np)
x.test=x[test.index,]
x.train=x[-test.index,]
library(tree)
x.tree=tree(IMDBだ计 ~ animal + army + art + baseball + bear + beer + blue + boss + brother + buffalo + camera + castle + 
cave + cell + cigarette + city + cliff + college + colony + company + computer + desert + dorm + dragon + driver + duke + evil + family + father + fire + 
fish + friend + galaxy + gun + helicopter + horse + hotel + human + hunter + husband + iron + 
judge + jungle + keyword + kid + knife + laboratory + lie + love + magic + manager + mask + meeting + missal + money + 
monster + mother + mountain + music + news + ninja + office + paint + party + plane + planet + president + prince + princess + 
queen + raven + rain + resistance + restaurant + river + robot + science + 
sea + ship + sister + slave + snow + soldier + study + train + village + 
water + white + wife , data=x.train)

plot(x.tree); text(x.tree)
  

imdb.train=x$IMDBだ计[-test.index]
train.pred=predict(x.tree , newdata=x.train , type='vector')
train.MAPE=mean(abs(imdb.train-train.pred)/imdb.train)*100
train.MAPE


imdb.test=x$IMDBだ计[test.index]
test.pred=predict(x.tree,newdata=x.test,type='vector')
test.MAPE=mean(abs(imdb.test-test.pred)/imdb.test)*100
test.MAPE


#############################################################################
#じ计-CART∕郸攫
#############################################################################

x1=read.csv("C:/Users/howard/Desktop/summary.csv")
x1=na.exclude(x1)
np=ceiling(0.1*nrow(x1))
test.index=sample(1:nrow(x1),np)
x1.test=x1[test.index,]
x1.train=x1[-test.index,]
library(rpart)
x1.tree=rpart(IMDBだ计 ~ animal + army + art + baseball + bear + beer + blue + boss + brother + buffalo + camera + castle + 
cave + cell + cigarette + city + cliff + college + colony + company + computer + desert + dorm + dragon + driver + duke + evil + family + father + fire + 
fish + friend + galaxy + gun + helicopter + horse + hotel + human + hunter + husband + iron + 
judge + jungle + keyword + kid + knife + laboratory + lie + love + magic + manager + mask + meeting + missal + money + 
monster + mother + mountain + music + news + ninja + office + paint + party + plane + planet + president + prince + princess + 
queen + raven + rain + resistance + restaurant + river + robot + science + 
sea + ship + sister + slave + snow + soldier + study + train + village + 
water + white + wife , data=x1.train)

plot(x1.tree); text(x1.tree)

imdb.train=x1$IMDBだ计[-test.index]
train.pred=predict(x1.tree,newdata=x1.train,type="vector")
train.MAPE=mean(abs(imdb.train-train.pred)/imdb.train)
cat("MAPE(train)=",train.MAPE*100,"%\n")

imdb.test=x1$IMDBだ计[test.index]
test.pred=predict(x1.tree,newdata=x1.test,type='vector')
test.MAPE=mean(abs(imdb.test-test.pred)/imdb.test)
test.MAPE.output=test.MAPE*100
cat("MAPE(test)=",test.MAPE*100,"%\n")

#predict x1.test
x1.test.pred=predict(x1.tree,x1.test,type='vector')
#merge predict result and output
test=data.frame(movie=x1.test$Movie,IMDBpred=x1.test.pred,IMDBold=imdb.test,MAPE=test.MAPE.output)
write.table(test,"C:/Users/howard/Desktop/箇代挡狦1.csv",row.names=F,sep=",")


#############################################################################
#じκだゑ-CART∕郸攫
#############################################################################
x2=read.csv("C:/Users/howard/Desktop/summary2.csv")
x2=na.exclude(x2)
np=ceiling(0.1*nrow(x2))
test.index=sample(1:nrow(x2),np)
x2.test=x2[test.index,]
x2.train=x2[-test.index,]
library(rpart)
x2.tree=rpart(IMDBだ计 ~ animal + army + art + baseball + bear + beer + blue + boss + brother + buffalo + camera + castle + 
cave + cell + cigarette + city + cliff + college + colony + company + computer + desert + dorm + dragon + driver + duke + evil + family + father + fire + 
fish + friend + galaxy + gun + helicopter + horse + hotel + human + hunter + husband + iron + 
judge + jungle + keyword + kid + knife + laboratory + lie + love + magic + manager + mask + meeting + missal + money + 
monster + mother + mountain + music + news + ninja + office + paint + party + plane + planet + president + prince + princess + 
queen + raven + rain + resistance + restaurant + river + robot + science + 
sea + ship + sister + slave + snow + soldier + study + train + village + 
water + white + wife , data=x2.train)

plot(x2.tree); text(x2.tree)

imdb.train=x2$IMDBだ计[-test.index]
train.pred=predict(x2.tree,newdata=x2.train,type="vector")
train.MAPE=mean(abs(imdb.train-train.pred)/imdb.train)
cat("MAPE(train)=",train.MAPE*100,"%\n")

imdb.test=x2$IMDBだ计[test.index]
test.pred=predict(x2.tree,newdata=x2.test,type='vector')
test.MAPE=mean(abs(imdb.test-test.pred)/imdb.test)
test.MAPE.output=test.MAPE*100
cat("MAPE(test)=",test.MAPE*100,"%\n")

#predict x2.test
x2.test.pred=predict(x2.tree,x2.test,type='vector')
#merge predict result and output
test=data.frame(movie=x2.test$Movie,IMDBpred=x2.test.pred,IMDBold=imdb.test,MAPE=test.MAPE.output)
write.table(test,"C:/Users/howard/Desktop/箇代挡狦2.csv",row.names=F,sep=",")
