#######################讀取關鍵字版EXCEL
my.read.csv=function(y)
{
read.csv(paste0("C:/Users/Student/Desktop/data/",y,".CSV"))
}
x1=my.read.csv("12 Years a Slave")
x2=my.read.csv("42")
x3=my.read.csv("127 Hours")
x4=my.read.csv("American Hustle")
x5=my.read.csv("Amityville Asylum")
x6=my.read.csv("Arbitrage")
x7=my.read.csv("August Osage County")
x8=my.read.csv("Authors Anonymous")
x9=my.read.csv("Bad Country")
x10=my.read.csv("Best Exotic Marigold Hotel")
x11=my.read.csv("Black Swan")
x12=my.read.csv("Bling Ring")
x13=my.read.csv("Candle to Water")
x14=my.read.csv("Celeste & Jesse Forever")
x15=my.read.csv("Christ Complex")
x16=my.read.csv("Chronicle")
x17=my.read.csv("Crazy, Stupid, Love")
x18=my.read.csv("Creation")
x19=my.read.csv("Croods")
x20=my.read.csv("Drive")
x21=my.read.csv("Flight")
x22=my.read.csv("Foxcatcher")
x23=my.read.csv("Fruitvale Station")
x24=my.read.csv("Gravity")
x25=my.read.csv("Horrible Bosses")
x26=my.read.csv("How to Train Your Dragon 2 ")
x27=my.read.csv("I Am Number Four")
x28=my.read.csv("Inception")
x29=my.read.csv("Les Miserables")
x30=my.read.csv("Lincoln")
x31=my.read.csv("Machine Gun Preacher")
x32=my.read.csv("Margin Call")
x33=my.read.csv("Men in Black 3")
x34=my.read.csv("Moonrise Kingdom")
x35=my.read.csv("Mud")
x36=my.read.csv("Oblivion")
x37=my.read.csv("Papadopoulos & Sons")
x38=my.read.csv("Rise of the Guardians")
x39=my.read.csv("Rise of the Planet of the Apes")
x40=my.read.csv("Rush")
x41=my.read.csv("Rust and Bone")
x42=my.read.csv("Saving Mr. Banks")
x43=my.read.csv("Scott Pilgrim vs the World")
x44=my.read.csv("Silver Linings Playbook")
x45=my.read.csv("Smashed")
x46=my.read.csv("Snow White and the Huntsman")
x47=my.read.csv("Super 8")
x48=my.read.csv("Take Shelter")
x49=my.read.csv("Wolf of Wall Street")
x50=my.read.csv("Zero Dark Thirty")

domatrix=do.call(rbind, lapply( paste0("x", 1:50) , get))
write.csv(domatrix,"C:/Users/Student/Desktop/tmp2.csv",row.names=FALSE)
####################接下來請用EXCEL樞紐表










