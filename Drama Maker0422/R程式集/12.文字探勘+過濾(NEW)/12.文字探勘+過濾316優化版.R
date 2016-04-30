########################################################################################################STEP1
library(tm)
library(dplyr)

#檔案路徑
path1 <- "C:/Users/howard/Desktop/script/dragon.txt"                                   ###檔案請存在C:/Users/howard/Desktop/script/
#目錄路徑
path2 <- "C:/Users/howard/Desktop/script"                                              ###第一次請改路徑
#讀txt檔看資料
tmp1 <- readLines(file(path1,encoding = "UTF-8"))
#讀目錄資訊
src <- DirSource(path2)
#Create volatile corpora 
ovid <- VCorpus(src)
#去除空白
ovid2 <- tm_map(ovid, stripWhitespace)
#轉小寫
ovid3 <- tm_map(ovid2,tolower)
#去除不重要的字元(EX介係詞)
ovid4 <- tm_map(ovid3,removeWords,stopwords("en"))
#去除標點符號
ovid5 <- tm_map(ovid4, removePunctuation)
#去除數字
ovid6 <- tm_map(ovid5, removeNumbers)
##(好像不重要)
##tdm <- TermDocumentMatrix(ovid2)
##inspect(tdm[2,])
#如何看資料
tmp3 <- as.character(ovid6[[1]])
#轉成data.frame格式
mores1 <- data.frame(x = as.character(ovid6[[1]]))
#去除列的NULL
mores2 <- filter(mores1,x != "")
#去除列的空白值
mores3 <- filter(mores2,x != " ")
#將資料前的空白去除
mores4 <- sapply(mores3,"[",1:lengths(mores3)[[1]])
#以字跟字的空白切割
mores5 <- strsplit(mores4," ")
#將個別list內資料抓出來
mores6 <-unlist(mores5)
mores7 <-data.frame(x = mores6) %>% filter(x != "")
mores8 <-table(mores7)
mores9 <-data.frame(mores8)
x1filtersort <- mores9



############################################################################################################STEP2
###請先處理好典影的字幕
#  格式請如下
#      mores7 Freq
#11 approaches   11
#16     arrows   11
#33     called   11
#44      claws   11
#55    crosses   11
#63       deep   11



library(dplyr)

my.read=function(x)
{
x.filter=filter(x, Freq > 10 )
x.filter.sort = x.filter[order(x.filter$Freq) , ]
x.filter.sort
}

x1filtersort=my.read(x1filtersort)



##SQL過濾出關鍵字
library(sqldf)

x1=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'water';row.names=TRUE")
if (nrow(x1)!=0) { x1$mores7="water"}
x2=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'sea';row.names=TRUE")
if (nrow(x2)!=0) { x2$mores7="sea"}
x3=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'river';row.names=TRUE")
if (nrow(x3)!=0) { x3$mores7="river"}
x4=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'rock';row.names=TRUE")
if (nrow(x4)!=0) { x4$mores7="rock"}
x5=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'armor';row.names=TRUE")
if (nrow(x5)!=0) { x5$mores7="armor"}
x6=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'weapon';row.names=TRUE")
if (nrow(x6)!=0) { x6$mores7="weapon"}
x7=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'camera';row.names=TRUE")
if (nrow(x7)!=0) { x7$mores7="camera"}
x8=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'blue';row.names=TRUE")
if (nrow(x8)!=0) { x8$mores7="blue"}
x9=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'raven';row.names=TRUE") 
if (nrow(x9)!=0) {x9$mores7="raven"}
x10=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'ice';row.names=TRUE") 
if (nrow(x10)!=0) {x10$mores7="ice"}
x11=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'love';row.names=TRUE") 
if (nrow(x11)!=0) {x11$mores7="love"}
x12=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'duke';row.names=TRUE") 
if (nrow(x12)!=0) {x12$mores7="duke"}
x13=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'castle';row.names=TRUE") 
if (nrow(x13)!=0) {x13$mores7="castle"}
x14=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'queen';row.names=TRUE") 
if (nrow(x14)!=0) {x14$mores7="queen"}
x15=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'mountain';row.names=TRUE") 
if (nrow(x15)!=0) {x15$mores7="mountain"}
x16=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'storm';row.names=TRUE") 
if (nrow(x16)!=0) {x16$mores7="storm"}
x17=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'horse';row.names=TRUE") 
if (nrow(x17)!=0) {x17$mores7="horse"}
x18=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'prince';row.names=TRUE") 
if (nrow(x18)!=0) {x18$mores7="prince"}
x19=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'princess';row.names=TRUE") 
if (nrow(x19)!=0) {x19$mores7="princess"}
x20=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'king';row.names=TRUE") 
if (nrow(x20)!=0) {x20$mores7="king"}
x21=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'white';row.names=TRUE") 
if (nrow(x21)!=0) {x21$mores7="white"}
x22=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'winter';row.names=TRUE") 
if (nrow(x22)!=0) {x22$mores7="winter"}
x23=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'magic';row.names=TRUE") 
if (nrow(x23)!=0) {x23$mores7="magic"}
x24=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'sister';row.names=TRUE") 
if (nrow(x24)!=0) {x24$mores7="sister"}
x25=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'brother';row.names=TRUE") 
if (nrow(x25)!=0) {x25$mores7="brother"}
x26=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'mother';row.names=TRUE") 
if (nrow(x26)!=0) {x26$mores7="mother"}
x27=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'father';row.names=TRUE") 
if (nrow(x27)!=0) {x27$mores7="father"}
x28=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'family';row.names=TRUE") 
if (nrow(x28)!=0) {x28$mores7="family"}
x29=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'snowman';row.names=TRUE") 
if (nrow(x29)!=0) {x29$mores7="snowman"}
x30=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'kingdom';row.names=TRUE") 
if (nrow(x30)!=0) {x30$mores7="kingdom"}
x31=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'turtle';row.names=TRUE") 
if (nrow(x31)!=0) {x31$mores7="turtle"}
x32=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'creature';row.names=TRUE") 
if (nrow(x32)!=0) {x32$mores7="animal"}
x33=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'city';row.names=TRUE") 
if (nrow(x33)!=0) {x33$mores7="city"}
x34=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'driver';row.names=TRUE") 
if (nrow(x34)!=0) {x34$mores7="driver"}
x35=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'jungle';row.names=TRUE") 
if (nrow(x35)!=0) {x35$mores7="jungle"}
x36=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'ninja';row.names=TRUE")
if (nrow(x36)!=0) {x36$mores7="ninja"}
x37=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'lair';row.names=TRUE") 
if (nrow(x37)!=0) {x37$mores7="lie"}
x38=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'monster';row.names=TRUE") 
if (nrow(x38)!=0) {x38$mores7="monster"}
x39=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'diner';row.names=TRUE") 
if (nrow(x39)!=0) {x39$mores7="restaurant"}
x40=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'cell';row.names=TRUE") 
if (nrow(x40)!=0) {x40$mores7="cell"}
x41=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'office';row.names=TRUE") 
if (nrow(x41)!=0) {x41$mores7="office"}
x42=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'master';row.names=TRUE") 
if (nrow(x42)!=0) {x42$mores7="slave"}
x43=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'music';row.names=TRUE") 
if (nrow(x43)!=0) {x43$mores7="music"}
x44=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'mask';row.names=TRUE") 
if (nrow(x44)!=0) {x44$mores7="mask"}
x45=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'ship';row.names=TRUE") 
if (nrow(x45)!=0) {x45$mores7="ship"}
x46=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'train';row.names=TRUE") 
if (nrow(x46)!=0) {x46$mores7="train"}
x47=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'space';row.names=TRUE") 
if (nrow(x47)!=0) {x47$mores7="galaxy"}
x48=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'fire';row.names=TRUE") 
if (nrow(x48)!=0) {x48$mores7="fire"}
x49=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'planet';row.names=TRUE") 
if (nrow(x49)!=0) {x49$mores7="planet"}
x50=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'metal';row.names=TRUE") 
if (nrow(x50)!=0) {x50$mores7="iron"}
x51=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'robot';row.names=TRUE") 
if (nrow(x51)!=0) {x51$mores7="robot"}
x52=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'penguin';row.names=TRUE") 
if (nrow(x52)!=0) {x52$mores7="penguin"}
x53=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'fish';row.names=TRUE") 
if (nrow(x53)!=0) {x53$mores7="fish"}
x54=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'colony';row.names=TRUE") 
if (nrow(x54)!=0) {x54$mores7="colony"}
x55=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'evil';row.names=TRUE") 
if (nrow(x55)!=0) {x55$mores7="evil"}
x56=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'lab';row.names=TRUE") 
if (nrow(x56)!=0) {x56$mores7="lab"}
x57=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'cancer';row.names=TRUE") 
if (nrow(x57)!=0) {x57$mores7="cancer"}
x58=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'cigarette';row.names=TRUE") 
if (nrow(x58)!=0) {x58$mores7="cigarette"}
x59=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'resistance';row.names=TRUE") 
if (nrow(x59)!=0) {x59$mores7="resistance"}
x60=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'airplane';row.names=TRUE") 
if (nrow(x60)!=0) {x60$mores7="plane"}
x61=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'spaceship';row.names=TRUE") 
if (nrow(x61)!=0) {x61$mores7="spaceship"}
x62=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'desert';row.names=TRUE") 
if (nrow(x62)!=0) {x62$mores7="desert"}
x63=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'computer';row.names=TRUE") 
if (nrow(x63)!=0) {x63$mores7="computer"}
x64=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'dorm';row.names=TRUE") 
if (nrow(x64)!=0) {x64$mores7="dorm"}
x65=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'friend';row.names=TRUE") 
if (nrow(x65)!=0) {x65$mores7="friend"}
x66=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'party';row.names=TRUE") 
if (nrow(x66)!=0) {x66$mores7="party"}
x67=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'company';row.names=TRUE") 
if (nrow(x67)!=0) {x67$mores7="company"}
x68=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'university';row.names=TRUE") 
if (nrow(x68)!=0) {x68$mores7="college"}
x69=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'money';row.names=TRUE") 
if (nrow(x69)!=0) {x69$mores7="money"}
x70=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'president';row.names=TRUE") 
if (nrow(x70)!=0) {x70$mores7="president"}
x71=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'boss';row.names=TRUE") 
if (nrow(x71)!=0) {x71$mores7="boss"}
x72=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'colonel';row.names=TRUE") 
if (nrow(x72)!=0) {x72$mores7="soldier"}
x73=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'news';row.names=TRUE") 
if (nrow(x73)!=0) {x73$mores7="news"}
x74=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'conference';row.names=TRUE") 
if (nrow(x74)!=0) {x74$mores7="meeting"}
x75=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'hotel';row.names=TRUE") 
if (nrow(x75)!=0) {x75$mores7="hotel"}
x76=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'missal';row.names=TRUE") 
if (nrow(x76)!=0) {x76$mores7="missal"}
x77=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'painting';row.names=TRUE") 
if (nrow(x77)!=0) {x77$mores7="painting"}
x78=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'art';row.names=TRUE") 
if (nrow(x78)!=0) {x78$mores7="art"}
x79=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'judge';row.names=TRUE") 
if (nrow(x79)!=0) {x79$mores7="judge"}
x80=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'child';row.names=TRUE") 
if (nrow(x80)!=0) {x80$mores7="kid"}
x81=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'Asian';row.names=TRUE") 
if (nrow(x81)!=0) {x81$mores7="Asian"}
x82=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'cave';row.names=TRUE") 
if (nrow(x82)!=0) {x82$mores7="cave"}
x83=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'bear';row.names=TRUE") 
if (nrow(x83)!=0) {x83$mores7="bear"}
x84=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'study';row.names=TRUE") 
if (nrow(x84)!=0) {x84$mores7="study"}
x85=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'helicopter';row.names=TRUE") 
if (nrow(x85)!=0) {x85$mores7="helicopter"}
x86=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'ai';row.names=TRUE") 
if (nrow(x86)!=0) {x86$mores7="ai"}
x87=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'beer';row.names=TRUE") 
if (nrow(x87)!=0) {x87$mores7="beer"}
x88=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'husband';row.names=TRUE") 
if (nrow(x88)!=0) {x88$mores7="husband"}
x89=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'wife';row.names=TRUE") 
if (nrow(x89)!=0) {x89$mores7="wife"}
x90=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'human';row.names=TRUE") 
if (nrow(x90)!=0) {x90$mores7="human"}
x91=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'dragon';row.names=TRUE") 
if (nrow(x91)!=0) {x91$mores7="dragon"}
x92=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'gun';row.names=TRUE") 
if (nrow(x92)!=0) {x92$mores7="gun"}
x93=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'hunter';row.names=TRUE") 
if (nrow(x93)!=0) {x93$mores7="hunter"}
x94=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'science';row.names=TRUE") 
if (nrow(x94)!=0) {x94$mores7="science"}
x95=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'warrier';row.names=TRUE") 
if (nrow(x95)!=0) {x95$mores7="warrier"}
x96=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'village';row.names=TRUE") 
if (nrow(x96)!=0) {x96$mores7="village"}
x97=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'buffalo';row.names=TRUE") 
if (nrow(x97)!=0) {x97$mores7="buffalo"}
x98=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'baseball';row.names=TRUE") 
if (nrow(x98)!=0) {x98$mores7="baseball"}
x99=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'manager';row.names=TRUE") 
if (nrow(x99)!=0) {x99$mores7="manager"}
x100=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'ocean';row.names=TRUE") 
if (nrow(x100)!=0) {x100$mores7="sea"}
x101=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'creek';row.names=TRUE") 
if (nrow(x101)!=0) {x101$mores7="river"}
x102=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'stone';row.names=TRUE") 
if (nrow(x102)!=0) {x102$mores7="cliff"}
x103=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'shield';row.names=TRUE") 
if (nrow(x103)!=0) {x103$mores7="armor"}
x104=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'sword';row.names=TRUE") 
if (nrow(x104)!=0) {x104$mores7="knife"}
x105=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'cctv';row.names=TRUE") 
if (nrow(x105)!=0) {x105$mores7="camera"}
x106=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'snow';row.names=TRUE") 
if (nrow(x106)!=0) {x106$mores7="snow"}
x107=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'affection';row.names=TRUE") 
if (nrow(x107)!=0) {x107$mores7="love"}
x108=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'knight';row.names=TRUE") 
if (nrow(x108)!=0) {x108$mores7="duke"}
x109=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'tower';row.names=TRUE") 
if (nrow(x109)!=0) {x109$mores7="castle"}
x110=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'bllizard';row.names=TRUE") 
if (nrow(x110)!=0) {x110$mores7="rain"}
x111=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'pony';row.names=TRUE") 
if (nrow(x111)!=0) {x111$mores7="horse"}
x112=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'emperor';row.names=TRUE") 
if (nrow(x112)!=0) {x112$mores7="king"}
x113=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'wizard';row.names=TRUE") 
if (nrow(x113)!=0) {x113$mores7="magic"}
x114=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'sibling';row.names=TRUE") 
if (nrow(x114)!=0) {x114$mores7="sister"}
x115=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'mom';row.names=TRUE") 
if (nrow(x115)!=0) {x115$mores7="mother"}
x116=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'dad';row.names=TRUE") 
if (nrow(x116)!=0) {x116$mores7="father"}
x117=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'animal';row.names=TRUE") 
if (nrow(x117)!=0) {x117$mores7="animal"}
x118=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'metropolis';row.names=TRUE") 
if (nrow(x118)!=0) {x118$mores7="city"}
x119=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'chofer';row.names=TRUE") 
if (nrow(x119)!=0) {x119$mores7="driver"}
x120=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'forest';row.names=TRUE") 
if (nrow(x120)!=0) {x120$mores7="jungle"}
x121=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'samurai';row.names=TRUE") 
if (nrow(x121)!=0) {x121$mores7="ninja"}
x122=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'lie';row.names=TRUE") 
if (nrow(x122)!=0) {x122$mores7="lie"}
x123=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'beast';row.names=TRUE") 
if (nrow(x123)!=0) {x123$mores7="monster"}
x124=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'restaurant';row.names=TRUE") 
if (nrow(x124)!=0) {x124$mores7="restaurant"}
x125=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'slave';row.names=TRUE") 
if (nrow(x125)!=0) {x125$mores7="slave"}
x126=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'musical';row.names=TRUE") 
if (nrow(x126)!=0) {x126$mores7="music"}
x127=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'boat';row.names=TRUE") 
if (nrow(x127)!=0) {x127$mores7="ship"}
x128=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'galaxy';row.names=TRUE") 
if (nrow(x128)!=0) {x128$mores7="galaxy"}
x129=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'flame';row.names=TRUE") 
if (nrow(x129)!=0) {x129$mores7="fire"}
x130=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'earth';row.names=TRUE") 
if (nrow(x130)!=0) {x130$mores7="planet"}
x131=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'iron';row.names=TRUE") 
if (nrow(x131)!=0) {x131$mores7="iron"}
x132=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'iron man';row.names=TRUE") 
if (nrow(x132)!=0) {x132$mores7="robot"}
x133=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'Antarctica';row.names=TRUE") 
if (nrow(x133)!=0) {x133$mores7="penguin"}
x134=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'sea';row.names=TRUE") 
if (nrow(x134)!=0) {x134$mores7="fish"}
x135=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'colonial';row.names=TRUE") 
if (nrow(x135)!=0) {x135$mores7="colony"}
x136=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'bad';row.names=TRUE") 
if (nrow(x136)!=0) {x136$mores7="evil"}
x137=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'laboratory';row.names=TRUE") 
if (nrow(x137)!=0) {x137$mores7="lab"}
x138=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'disease';row.names=TRUE") 
if (nrow(x138)!=0) {x138$mores7="cancer"}
x139=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'rabel';row.names=TRUE") 
if (nrow(x139)!=0) {x139$mores7="resistance"}
x140=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'plane';row.names=TRUE") 
if (nrow(x140)!=0) {x140$mores7="plane"}
x141=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'cockpit';row.names=TRUE") 
if (nrow(x141)!=0) {x141$mores7="spaceship"}
x142=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'internet';row.names=TRUE") 
if (nrow(x142)!=0) {x142$mores7="computer"}
x143=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'sorority';row.names=TRUE") 
if (nrow(x143)!=0) {x143$mores7="dorm"}
x144=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'pal';row.names=TRUE") 
if (nrow(x144)!=0) {x144$mores7="friend"}
x145=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'firm';row.names=TRUE") 
if (nrow(x145)!=0) {x145$mores7="company"}
x146=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'college';row.names=TRUE") 
if (nrow(x146)!=0) {x146$mores7="college"}
x147=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'wealth';row.names=TRUE") 
if (nrow(x147)!=0) {x147$mores7="money"}
x148=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'priminister';row.names=TRUE") 
if (nrow(x148)!=0) {x148$mores7="president"}
x149=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'president';row.names=TRUE") 
if (nrow(x149)!=0) {x149$mores7="boss"}
x150=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'military';row.names=TRUE")
if (nrow(x150)!=0) {x150$mores7="soldier"}
x151=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'journalist';row.names=TRUE") 
if (nrow(x151)!=0) {x151$mores7="news"}
x152=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'council';row.names=TRUE") 
if (nrow(x152)!=0) {x152$mores7="meeting"}
x153=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'hostel';row.names=TRUE") 
if (nrow(x153)!=0) {x153$mores7="hotel"}
x154=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'rocket';row.names=TRUE") 
if (nrow(x154)!=0) {x154$mores7="missal"}
x155=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'canvas';row.names=TRUE") 
if (nrow(x155)!=0) {x155$mores7="painting"}
x156=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'artist';row.names=TRUE") 
if (nrow(x156)!=0) {x156$mores7="art"}
x157=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'lawyer';row.names=TRUE") 
if (nrow(x157)!=0) {x157$mores7="judge"}
x158=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'children';row.names=TRUE") 
if (nrow(x158)!=0) {x158$mores7="kid"}
x159=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'Chinese';row.names=TRUE") 
if (nrow(x159)!=0) {x159$mores7="Asian"}
x160=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'rock';row.names=TRUE") 
if (nrow(x160)!=0) {x160$mores7="cave"}
x161=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'learn';row.names=TRUE") 
if (nrow(x161)!=0) {x161$mores7="study"}
x162=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'chopper';row.names=TRUE") 
if (nrow(x162)!=0) {x162$mores7="helicopter"}
x163=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'artificial inteligence';row.names=TRUE") 
if (nrow(x163)!=0) {x163$mores7="ai"}
x164=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'wine';row.names=TRUE") 
if (nrow(x164)!=0) {x164$mores7="beer"}
x165=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'mankind';row.names=TRUE") 
if (nrow(x165)!=0) {x165$mores7="human"}
x166=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'spiner';row.names=TRUE") 
if (nrow(x166)!=0) {x166$mores7="gun"}
x167=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'hound';row.names=TRUE") 
if (nrow(x167)!=0) {x167$mores7="hunter"}
x168=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'scientist';row.names=TRUE") 
if (nrow(x168)!=0) {x168$mores7="science"}
x169=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'figher';row.names=TRUE") 
if (nrow(x169)!=0) {x169$mores7="warrier"}
x170=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'town';row.names=TRUE") 
if (nrow(x170)!=0) {x170$mores7="village"}
x171=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'mine';row.names=TRUE") 
if (nrow(x171)!=0) {x171$mores7="cliff"}
x172=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'visor';row.names=TRUE") 
if (nrow(x172)!=0) {x172$mores7="armor"}
x173=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'knife';row.names=TRUE") 
if (nrow(x173)!=0) {x173$mores7="knife"}
x174=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'surveillance';row.names=TRUE") 
if (nrow(x174)!=0) {x174$mores7="camera"}
x175=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'breeze';row.names=TRUE") 
if (nrow(x175)!=0) {x175$mores7="snow"}
x176=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'lord';row.names=TRUE") 
if (nrow(x176)!=0) {x176$mores7="duke"}
x177=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'peak';row.names=TRUE") 
if (nrow(x177)!=0) {x177$mores7="mountain"}
x178=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'rain';row.names=TRUE") 
if (nrow(x178)!=0) {x178$mores7="rain"}
x179=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'donkey';row.names=TRUE") 
if (nrow(x179)!=0) {x179$mores7="horse"}
x180=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'voodoo';row.names=TRUE") 
if (nrow(x180)!=0) {x180$mores7="nagic"}
x181=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'mum';row.names=TRUE") 
if (nrow(x181)!=0) {x181$mores7="mother"}
x182=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'beast';row.names=TRUE") 
if (nrow(x182)!=0) {x182$mores7="animal"}
x183=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'town';row.names=TRUE") 
if (nrow(x183)!=0) {x183$mores7="city"}
x184=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'pilot';row.names=TRUE") 
if (nrow(x184)!=0) {x184$mores7="driver"}
x185=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'woods';row.names=TRUE") 
if (nrow(x185)!=0) {x185$mores7="jungle"}
x186=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'betray';row.names=TRUE") 
if (nrow(x186)!=0) {x186$mores7="lie"}
x187=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'creature';row.names=TRUE") 
if (nrow(x187)!=0) {x187$mores7="monster"}
x188=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'slavery';row.names=TRUE") 
if (nrow(x188)!=0) {x188$mores7="slave"}
x189=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'song';row.names=TRUE") 
if (nrow(x189)!=0) {x189$mores7="music"}
x190=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'oiler';row.names=TRUE") 
if (nrow(x190)!=0) {x190$mores7="ship"}
x191=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'universe';row.names=TRUE") 
if (nrow(x191)!=0) {x191$mores7="galaxy"}
x192=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'mars';row.names=TRUE") 
if (nrow(x192)!=0) {x192$mores7="planet"}
x193=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'steel';row.names=TRUE") 
if (nrow(x193)!=0) {x193$mores7="iron"}
x194=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'transformer';row.names=TRUE") 
if (nrow(x194)!=0) {x194$mores7="robot"}
x195=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'ocean';row.names=TRUE") 
if (nrow(x195)!=0) {x195$mores7="fish"}
x196=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'colonize';row.names=TRUE") 
if (nrow(x196)!=0) {x196$mores7="colony"}
x197=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'devil';row.names=TRUE") 
if (nrow(x197)!=0) {x197$mores7="evil"}
x198=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'experiment';row.names=TRUE") 
if (nrow(x198)!=0) {x198$mores7="lab"}
x199=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'cockpit';row.names=TRUE") 
if (nrow(x199)!=0) {x199$mores7="plane"}
x200=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'ai';row.names=TRUE") 
if (nrow(x200)!=0) {x200$mores7="computer"}
x201=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'buddy';row.names=TRUE") 
if (nrow(x201)!=0) {x201$mores7="friend"}
x202=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'business';row.names=TRUE") 
if (nrow(x202)!=0) {x202$mores7="company"}
x203=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'freshmen';row.names=TRUE") 
if (nrow(x203)!=0) {x203$mores7="college"}
x204=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'gold';row.names=TRUE") 
if (nrow(x204)!=0) {x204$mores7="money"}
x205=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'king';row.names=TRUE") 
if (nrow(x205)!=0) {x205$mores7="president"}
x206=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'chairman';row.names=TRUE") 
if (nrow(x206)!=0) {x206$mores7="boss"}
x207=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'general';row.names=TRUE") 
if (nrow(x207)!=0) {x207$mores7="soldier"}
x208=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'navy';row.names=TRUE") 
if (nrow(x208)!=0) {x208$mores7="army"}
x209=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'headline';row.names=TRUE") 
if (nrow(x209)!=0) {x209$mores7="news"}
x210=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'meeting';row.names=TRUE") 
if (nrow(x210)!=0) {x210$mores7="meeting"}
x211=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'inn';row.names=TRUE") 
if (nrow(x211)!=0) {x211$mores7="hotel"}
x212=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'cannon';row.names=TRUE") 
if (nrow(x212)!=0) {x212$mores7="missal"}
x213=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'paint';row.names=TRUE") 
if (nrow(x213)!=0) {x213$mores7="painting"}
x214=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'jury';row.names=TRUE") 
if (nrow(x214)!=0) {x214$mores7="judge"}
x215=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'baby';row.names=TRUE") 
if (nrow(x215)!=0) {x215$mores7="kid"}
x216=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'Japanese';row.names=TRUE") 
if (nrow(x216)!=0) {x216$mores7="Asian"}
x217=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'stone';row.names=TRUE") 
if (nrow(x217)!=0) {x217$mores7="cave"}
x218=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'school';row.names=TRUE") 
if (nrow(x218)!=0) {x218$mores7="study"}
x219=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'whiskey';row.names=TRUE") 
if (nrow(x219)!=0) {x219$mores7="beer"}
x220=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'homo sepiens';row.names=TRUE") 
if (nrow(x220)!=0) {x220$mores7="human"}
x221=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'riffle';row.names=TRUE") 
if (nrow(x221)!=0) {x221$mores7="gun"}
x222=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'technology';row.names=TRUE") 
if (nrow(x222)!=0) {x222$mores7="science"}
x223=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'country';row.names=TRUE")
if (nrow(x223)!=0) {x223$mores7="village"}
x224=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'cliff';row.names=TRUE") 
if (nrow(x224)!=0) {x224$mores7="cliff"}
x225=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'axe';row.names=TRUE") 
if (nrow(x225)!=0) {x225$mores7="knife"}
x226=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'bllizard';row.names=TRUE") 
if (nrow(x226)!=0) {x226$mores7="snow"}
x227=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'count';row.names=TRUE") 
if (nrow(x227)!=0) {x227$mores7="duke"}
x228=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'witch';row.names=TRUE") 
if (nrow(x228)!=0) {x228$mores7="magic"}
x229=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'black';row.names=TRUE") 
if (nrow(x229)!=0) {x229$mores7="slave"}
x230=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'sing';row.names=TRUE") 
if (nrow(x230)!=0) {x230$mores7="music"}
x231=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'tanker';row.names=TRUE") 
if (nrow(x231)!=0) {x231$mores7="ship"}
x232=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'moon';row.names=TRUE") 
if (nrow(x232)!=0) {x232$mores7="planet"}
x233=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'shark';row.names=TRUE") 
if (nrow(x233)!=0) {x233$mores7="fish"}
x234=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'test';row.names=TRUE") 
if (nrow(x234)!=0) {x234$mores7="test"}
x235=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'web';row.names=TRUE") 
if (nrow(x235)!=0) {x235$mores7="computer"}
x236=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'mate';row.names=TRUE") 
if (nrow(x236)!=0) {x236$mores7="friend"}
x237=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'billionaire';row.names=TRUE") 
if (nrow(x237)!=0) {x237$mores7="money"}
x238=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'chairman';row.names=TRUE") 
if (nrow(x238)!=0) {x238$mores7="president"}
x239=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'owner';row.names=TRUE") 
if (nrow(x239)!=0) {x239$mores7="boss"}
x240=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'soldier';row.names=TRUE") 
if (nrow(x240)!=0) {x240$mores7="soldier"}
x241=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'marine';row.names=TRUE") 
if (nrow(x241)!=0) {x241$mores7="army"}
x242=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'report';row.names=TRUE") 
if (nrow(x242)!=0) {x242$mores7="news"}
x243=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'seminar';row.names=TRUE") 
if (nrow(x243)!=0) {x243$mores7="meeting"}
x244=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'picture';row.names=TRUE") 
if (nrow(x244)!=0) {x244$mores7="painting"}
x245=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'court';row.names=TRUE") 
if (nrow(x245)!=0) {x245$mores7="judge"}
x246=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'infant';row.names=TRUE") 
if (nrow(x246)!=0) {x246$mores7="kid"}
x247=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'Thai';row.names=TRUE") 
if (nrow(x247)!=0) {x247$mores7="Asian"}
x248=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'test';row.names=TRUE") 
if (nrow(x248)!=0) {x248$mores7="study"}
x249=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'vodka';row.names=TRUE") 
if (nrow(x249)!=0) {x249$mores7="beer"}
x250=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'people';row.names=TRUE") 
if (nrow(x250)!=0) {x250$mores7="human"}
x251=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'pistol';row.names=TRUE") 
if (nrow(x251)!=0) {x251$mores7="gun"}
x252=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'canyon';row.names=TRUE") 
if (nrow(x252)!=0) {x252$mores7="cliff"}
x253=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'hammer';row.names=TRUE") 
if (nrow(x253)!=0) {x253$mores7="knife"}
x254=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'frost';row.names=TRUE") 
if (nrow(x254)!=0) {x254$mores7="snow"}
x255=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'sir';row.names=TRUE") 
if (nrow(x255)!=0) {x255$mores7="duke"}
x256=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'sorcerer';row.names=TRUE") 
if (nrow(x256)!=0) {x256$mores7="magic"}
x257=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'niggar';row.names=TRUE") 
if (nrow(x257)!=0) {x257$mores7="slave"}
x258=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'melody';row.names=TRUE") 
if (nrow(x258)!=0) {x258$mores7="music"}
x259=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'yacht';row.names=TRUE") 
if (nrow(x259)!=0) {x259$mores7="ship"}
x260=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'sun';row.names=TRUE") 
if (nrow(x260)!=0) {x260$mores7="planet"}
x261=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'dolphine';row.names=TRUE") 
if (nrow(x261)!=0) {x261$mores7="fish"}
x262=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'millionaire';row.names=TRUE") 
if (nrow(x262)!=0) {x262$mores7="money"}
x263=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'master';row.names=TRUE") 
if (nrow(x263)!=0) {x263$mores7="boss"}
x264=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'lieutenant';row.names=TRUE") 
if (nrow(x264)!=0) {x264$mores7="soldier"}
x265=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'airforce';row.names=TRUE") 
if (nrow(x265)!=0) {x265$mores7="army"}
x266=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'broadcast';row.names=TRUE") 
if (nrow(x266)!=0) {x266$mores7="news"}
x267=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'potrait';row.names=TRUE") 
if (nrow(x267)!=0) {x267$mores7="painting"}
x268=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'todler';row.names=TRUE") 
if (nrow(x268)!=0) {x268$mores7="kid"}
x269=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'Taiwanese';row.names=TRUE") 
if (nrow(x269)!=0) {x269$mores7="Asian"}
x270=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'canyon';row.names=TRUE") 
if (nrow(x270)!=0) {x270$mores7="cave"}
x271=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'exam';row.names=TRUE") 
if (nrow(x271)!=0) {x271$mores7="study"}
x272=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'gin';row.names=TRUE") 
if (nrow(x272)!=0) {x272$mores7="beer"}
x273=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'revolver';row.names=TRUE") 
if (nrow(x273)!=0) {x273$mores7="gun"}
x274=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'blade';row.names=TRUE") 
if (nrow(x274)!=0) {x274$mores7="knife"}
x275=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'aristocrat';row.names=TRUE") 
if (nrow(x275)!=0) {x275$mores7="duke"}
x276=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'private';row.names=TRUE") 
if (nrow(x276)!=0) {x276$mores7="soldier"}
x277=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'squad';row.names=TRUE") 
if (nrow(x277)!=0) {x277$mores7="army"}
x278=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'gallery';row.names=TRUE") 
if (nrow(x278)!=0) {x278$mores7="painting"}
x279=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'kid';row.names=TRUE") 
if (nrow(x279)!=0) {x279$mores7="kid"}
x280=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'Cantonese';row.names=TRUE") 
if (nrow(x280)!=0) {x280$mores7="Asian"}
x281=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'evaluation';row.names=TRUE") 
if (nrow(x281)!=0) {x281$mores7="study"}
x282=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'bourbon';row.names=TRUE") 
if (nrow(x282)!=0) {x282$mores7="beer"}
x283=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'arrow';row.names=TRUE") 
if (nrow(x283)!=0) {x283$mores7="knife"}
x284=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'major';row.names=TRUE") 
if (nrow(x284)!=0) {x284$mores7="soldier"}
x285=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'troops';row.names=TRUE") 
if (nrow(x285)!=0) {x285$mores7="army"}
x286=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'drawing';row.names=TRUE") 
if (nrow(x286)!=0) {x286$mores7="painting"}
x287=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'Korean';row.names=TRUE") 
if (nrow(x287)!=0) {x287$mores7="Asian"}
x288=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'brandey';row.names=TRUE") 
if (nrow(x288)!=0) {x288$mores7="beer"}
x289=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'bow';row.names=TRUE") 
if (nrow(x289)!=0) {x289$mores7="knife"}
x290=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'army';row.names=TRUE") 
if (nrow(x290)!=0) {x290$mores7="army"}
x291=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'champagne';row.names=TRUE") 
if (nrow(x291)!=0) {x291$mores7="beer"}
x292=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'drunk';row.names=TRUE") 
if (nrow(x292)!=0) {x292$mores7="beer"}
x293=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'hangover';row.names=TRUE") 
if (nrow(x293)!=0) {x293$mores7="beer"}
x294=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'tipsy';row.names=TRUE") 
if (nrow(x294)!=0) {x294$mores7="beer"}
 


domatrix=do.call(rbind, lapply( paste0("x", 1:294) , get) )



gogo=function(namess)
{
domatrix2 <- data.frame(movie=namess,keyword=domatrix$mores7,Freq=domatrix$Freq)
write.csv(domatrix2,paste0("C:/Users/howard/Desktop/",namess,".CSV") , row.names = FALSE)                                           ###第一次請改路徑
}


gogo("12 Years a Slave")                                                                                                             #<<<<<<<<<<<<<<<要改輸出名稱
