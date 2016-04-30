#############################################################################
#複迴歸
#############################################################################

x1=read.csv("C:/Users/howard/Desktop/summary.csv")
x1=na.exclude(x1)
np=ceiling(0.1*nrow(x1))
test.index=sample(1:nrow(x1),np)
x1.test=x1[test.index,]
x1.train=x1[-test.index,]
library(rpart)
x1.reg=lm(IMDB分數 ~ animal + army + art + baseball + bear + beer + blue + boss + brother + buffalo + camera + castle + 
cave + cell + cigarette + city + cliff + college + colony + company + computer + desert + dorm + dragon + driver + duke + evil + family + father + fire + 
fish + friend + galaxy + gun + helicopter + horse + hotel + human + hunter + husband + iron + 
judge + jungle + keyword + kid + knife + laboratory + lie + love + magic + manager + mask + meeting + missal + money + 
monster + mother + mountain + music + news + ninja + office + paint + party + plane + planet + president + prince + princess + 
queen + raven + rain + resistance + restaurant + river + robot + science + 
sea + ship + sister + slave + snow + soldier + study + train + village + 
water + white + wife , data=x1.train)
summary(x1.reg)


imdb.train=x1$IMDB分數[-test.index]
train.pred=?predict(x1.reg,newdata=x1.train,type="vector")
train.MAPE=mean(abs(imdb.train-train.pred)/imdb.train)
cat("MAPE(train)=",train.MAPE*100,"%\n")

imdb.test=x1$IMDB分數[test.index]
test.pred=predict(x1.tree,newdata=x1.test,type='vector')
test.MAPE=mean(abs(imdb.test-test.pred)/imdb.test)
test.MAPE.output=test.MAPE*100
cat("MAPE(test)=",test.MAPE*100,"%\n")


