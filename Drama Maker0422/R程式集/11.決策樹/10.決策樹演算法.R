#############################################################################
#じ计-CART∕郸攫   ネ逼舱ノ
#############################################################################

x1=read.csv("C:/Users/Student/Desktop/summary.csv")
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


##test data 箇代挡狦
x1.test.pred=predict(x1.tree,x1.test,type='vector')
#merge predict result and output
test=cbind(data.frame(x1.test$Movie,x1.test$IMDBだ计,pred=x1.test.pred))
write.table(test,"C:/Users/howard/Desktop/x1.test箇代挡狦.csv",row.names=F,sep=",")



##箇代穝戈
x8=read.csv("C:/Users/howard/Desktop/summaryt.csv")


imdb=numeric(nrow(x8))
for(i in 1:nrow(x8))
{
if (x8$water[i] > 0.5 && x8$mother[i] > 0.5 ) {imdb[i]=7.764706
}else if (x8$water[i] > 0.5 && x8$mother[i] < 0.5){imdb[i]=7.333
}else if (x8$water[i] < 0.5 && x8$wife[i] > 0.5){imdb[i]=7.885714
}else if (x8$water[i] < 0.5 && x8$wife[i] < 0.5 && x8$gun[i] > 0.5){imdb[i]=6.677778
}else if (x8$water[i] < 0.5 && x8$wife[i] < 0.5 && x8$gun[i] < 0.5 && x8$news[i] >= 0.5){imdb[i]=7.562500
}else if (x8$water[i] < 0.5 && x8$wife[i] < 0.5 && x8$gun[i] < 0.5 && x8$news[i] < 0.5 && x8$cell[i] >= 0.5){imdb[i]=7.557143
}else if (x8$water[i] < 0.5 && x8$wife[i] < 0.5 && x8$gun[i] < 0.5 && x8$news[i] < 0.5 && x8$cell[i] < 0.5 && x8$family[i]>=0.5){imdb[i]=7.410000
}else if (x8$water[i] < 0.5 && x8$wife[i] < 0.5 && x8$gun[i] < 0.5 && x8$news[i] < 0.5 && x8$cell[i]< 0.5 && x8$family[i]>=0.5 && x8$white[i]>=0.5){imdb[i]=7.325000
}else {imdb[i]=6.761111}
}


domatrix=cbind(x8,predimdb=imdb)
write.table(domatrix,"C:/Users/howard/Desktop/箇代挡狦.csv",row.names=F,sep=",")

#喷靡ノ#x8.pred=predict(x1.tree,x8,type='vector')
