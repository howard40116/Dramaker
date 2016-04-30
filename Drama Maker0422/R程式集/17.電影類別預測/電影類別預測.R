########################################################貝氏
library(e1071)

x1=read.csv("C:/Users/howard/Desktop/summary_class.csv")
for(i in 1:10)
{
x1=na.exclude(x1)
np=ceiling(0.1*nrow(x1))
test.index=sample(1:nrow(x1),np)
x1.test=x1[test.index,]
x1.train=x1[-test.index,]


x1.tree=naiveBayes(class ~ animal + army + art + baseball + bear + beer + blue + boss + brother + buffalo + camera + castle + 
cave + cell + cigarette + city + cliff + college + colony + company + computer + desert + dorm + dragon + driver + duke + evil + family + father + fire + 
fish + friend + galaxy + gun + helicopter + horse + hotel + human + hunter + husband + iron + 
judge + jungle + keyword + kid + knife + laboratory + lie + love + magic + manager + mask + meeting + missal + money + 
monster + mother + mountain + music + news + ninja + office + paint + party + plane + planet + president + prince + princess + 
queen + raven + rain + resistance + restaurant + river + robot + science + 
sea + ship + sister + slave + snow + soldier + study + train + village + 
water + white + wife , data=x1.train)

#predict(x1.tree, x1.train)
#predict(x1.tree, x1.train, type = "raw")
pred <- predict(x1.tree, x1.train)
table.train=table(pred, x1.train$class)
#table.train
correct.train=sum(diag(table.train))/sum(table.train)
print(correct.train)


#predict(x1.tree, x1.test)
#predict(x1.tree, x1.test, type = "raw")
pred <- predict(x1.tree, x1.test)
table.test=table(pred, x1.test$class)
#table.test
correct.test=sum(diag(table.test))/sum(table.test)
print(correct.test)
}

###################################################################決策
library(tree)
x1=read.csv("C:/Users/howard/Desktop/summary_class.csv")
for(i in 1:10)
{
x1=na.exclude(x1)
np=ceiling(0.1*nrow(x1))
test.index=sample(1:nrow(x1),np)
x1.test=x1[test.index,]
x1.train=x1[-test.index,]


x1.tree=tree(class ~ animal + army + art + baseball + bear + beer + blue + boss + brother + buffalo + camera + castle + 
cave + cell + cigarette + city + cliff + college + colony + company + computer + desert + dorm + dragon + driver + duke + evil + family + father + fire + 
fish + friend + galaxy + gun + helicopter + horse + hotel + human + hunter + husband + iron + 
judge + jungle + keyword + kid + knife + laboratory + lie + love + magic + manager + mask + meeting + missal + money + 
monster + mother + mountain + music + news + ninja + office + paint + party + plane + planet + president + prince + princess + 
queen + raven + rain + resistance + restaurant + river + robot + science + 
sea + ship + sister + slave + snow + soldier + study + train + village + 
water + white + wife , data=x1.train)


plot(x1.tree); text(x1.tree)
  

pred <- predict(x1.tree, x1.train)
table.train=table(pred, x1.train$class)
correct.train=sum(diag(table.train))/sum(table.train)
print(correct.train)


pred <- predict(x1.tree, x1.test)
table.test=table(pred, x1.test$class)
correct.test=sum(diag(table.test))/sum(table.test)
print(correct.test)
}
