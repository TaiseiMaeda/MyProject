#データ読み込み三連複アペンドバージョン
Tanhuku2hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\2分前\\単複オッズ2分前.csv",header=TRUE)
TanNinki2hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\2分前\\単勝人気順2分前.csv",header=TRUE)
Umahuku2hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\2分前\\馬複オッズ2分前.csv",header=TRUE)
Umatan2hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\2分前\\馬単オッズ2分前.csv",header=TRUE)
Sanrenhuku2hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\2分前\\三連複オッズ2分前.csv",header=TRUE)
Sanrentan2hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\2分前\\三連単オッズ2分前.csv",header=TRUE)
RaceJohoKekka<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\レース情報結果.csv",header=TRUE)
UmaJoho<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\馬情報.csv",header=TRUE)
Shisu<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\指数.csv",header=TRUE)
#以下数のフィールド名変更
names(Tanhuku2hun)[5]<-"10以下数";names(Tanhuku2hun)[6]<-"20以下数";names(Tanhuku2hun)[7]<-"30以下数"
names(Tanhuku2hun)[8]<-"50以上数";names(Tanhuku2hun)[9]<-"70以上数";names(Tanhuku2hun)[10]<-"100以上数"
#馬情報のフィールド名変更
names(UmaJoho)[29]<-"3走前日付";names(UmaJoho)[32]<-"4走前日付"；names(UmaJoho)[35]<-"5走前日付"
names(Sanrenhuku2hun)[2]<-"三連複馬場";names(Umahuku2hun)[2]<-"馬複馬場";

ss<-1:30
ss<-ss*4 + 4

huninki<-1:30
huninki<-huninki * 3 +94
#馬場ごとに繰り返す
baba<-c("大井","船橋","川崎","浦和","名古屋","園田","笠松","門別","金沢","帯広","佐賀","高知")
baba2<-c("大井 ","船橋","川崎","浦和","名古屋","園田","笠松","門別","金沢","帯広","佐賀","高知")

for(j in 1:12){

Tanhuku2hunBaba <- subset(Tanhuku2hun,Tanhuku2hun["馬場"]==baba[j])
TanNinki2hunBaba <- subset(TanNinki2hun,TanNinki2hun["馬場"]==baba[j])
RaceJohoKekkaBaba <- subset(RaceJohoKekka,RaceJohoKekka["レース情報.馬場"]==baba[j])
Umahuku2hunBaba <- subset(Umahuku2hun,Umahuku2hun["馬複馬場"]==baba[j])　
Sanrenhuku2hunBaba <- subset(Sanrenhuku2hun,Sanrenhuku2hun["三連複馬場"]==baba[j])　
ShisuBaba<- subset(Shisu,Shisu["レース情報.馬場"]==baba[j])

#①　以下上数と単勝人気オッズ
merge1<-merge(Tanhuku2hunBaba[,1:10], TanNinki2hunBaba 
    , by.x=c("年月日","R")
    , by.y=c("年月日","R")
  )
#② ①と結果
x1<-cbind(RaceJohoKekkaBaba[,1:3],RaceJohoKekkaBaba[,27:68])
merge2<-merge(merge1, x1
    , by.y=c("レース情報.年月日","レース情報.R")
    , by.x=c("年月日","R")
    #,all.x=T
  )

#③ ②と馬複人気順
x2<-Umahuku2hunBaba [,1:3]
	x2<-cbind(x2,Umahuku2hunBaba [,5:94],Umahuku2hunBaba [,huninki])
merge3<-merge(merge2, x2
    , by.x=c("年月日","R")
    , by.y=c("年月日","R")
  )

#④
x3<-Sanrenhuku2hunBaba [,1:3]
	x3<-cbind(x3,Sanrenhuku2hunBaba [,ss])
x4<-merge(merge3, x3
    , by.x=c("年月日","R")
    , by.y=c("年月日","R")
  )

#⑤
UmahukuMat<-merge(x4, ShisuBaba
    , by.x=c("年月日","R")
    , by.y=c("レース情報.年月日","レース情報.R")
  )

if (j == 1){
	UmahukuMatApd<- UmahukuMat
}else{
	UmahukuMatApd<- rbind(UmahukuMatApd,UmahukuMat)
}
}#forの終わり

UmahukuMat <- UmahukuMatApd
length(UmahukuMat[,1] )
AllData <- UmahukuMatApd
UmahukuMat <- AllData


subumahuku<-subset(UmahukuMat,grepl("2017/06", UmahukuMat$"年月日")|
grepl("2017/05", UmahukuMat$"年月日")|
grepl("2017/04", UmahukuMat$"年月日"))


#subumahuku<-subset(UmahukuMat,(
# UmahukuMat["馬場.x"]=="船橋" |UmahukuMat["馬場.x"]=="大井" |UmahukuMat["馬場.x"]=="浦和" |UmahukuMat["馬場.x"]=="川崎" 
# UmahukuMat["馬場.x"]=="門別" |UmahukuMat["馬場.x"]=="園田" |UmahukuMat["馬場.x"]=="笠松" |UmahukuMat["馬場.x"]=="名古屋" 
#			)&UmahukuMat["馬場.x"]!="調整用の行")
#UmahukuMat <-subumahuku


#__________________________________________________________
#15分前データ読み込み
Tanhuku15hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\15分前\\単複オッズ15分前.csv",header=TRUE)
TanNinki15hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\15分前\\単勝人気順15分前.csv",header=TRUE)
Umahuku15hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\15分前\\馬複オッズ15分前.csv",header=TRUE)
Umatan15hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\15分前\\馬単オッズ15分前.csv",header=TRUE)
Sanrenhuku15hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\15分前\\三連複オッズ15分前.csv",header=TRUE)
Sanrentan15hun<-read.csv("C:\\KEIBA\\競馬自動購入ツール\\csv\\15分前\\三連単オッズ15分前.csv",header=TRUE)
#以下数のフィールド名変更
names(Tanhuku15hun)[5]<-"10以下数";names(Tanhuku15hun)[6]<-"20以下数";names(Tanhuku15hun)[7]<-"30以下数"
names(Tanhuku15hun)[8]<-"50以上数";names(Tanhuku15hun)[9]<-"70以上数";names(Tanhuku15hun)[10]<-"100以上数"
#馬情報のフィールド名変更
names(UmaJoho)[29]<-"3走前日付";names(UmaJoho)[32]<-"4走前日付"；names(UmaJoho)[35]<-"5走前日付"

#馬場ごとに繰り返す
baba<-c("大井","船橋","川崎","浦和","名古屋","園田","笠松","門別","金沢","帯広","佐賀","高知")

for(j in 1:12){

Tanhuku15hunBaba <- subset(Tanhuku15hun,Tanhuku15hun["馬場"]==baba[j])
 TanNinki15hunBaba <- subset(TanNinki15hun,TanNinki15hun["馬場"]==baba[j])
RaceJohoKekkaBaba <- subset(RaceJohoKekka,RaceJohoKekka["レース情報.馬場"]==baba[j])
Umahuku15hunBaba <- subset(Umahuku15hun,Umahuku15hun["馬場"]==baba[j])　

#①　以下上数と単勝人気オッズ
merge1<-merge(Tanhuku15hunBaba[,1:10], TanNinki15hunBaba 
    , by.x=c("年月日","R")
    , by.y=c("年月日","R")
  )
#② ①と結果
x1<-cbind(RaceJohoKekkaBaba[,1:3],RaceJohoKekkaBaba[,27:68])
merge2<-merge(merge1, x1
    , by.y=c("レース情報.年月日","レース情報.R")
    , by.x=c("年月日","R")
    #,all.x=T
  )

#③ ②と馬複人気順
x2<-Umahuku15hunBaba [,1:3]
	x2<-cbind(x2,Umahuku15hunBaba [,7:96])
UmahukuMat15hun<-merge(merge2, x2
    , by.x=c("年月日","R")
    , by.y=c("年月日","R")
  )
if (j == 1){
	UmahukuMat15hunApd<- UmahukuMat15hun
}else{
	UmahukuMat15hunApd<- rbind(UmahukuMat15hunApd,UmahukuMat15hun)
}
}#forの終わり
UmahukuMat15hun<- UmahukuMat15hunApd
length(UmahukuMat15hun[,1] )
AllData15hun <- UmahukuMat15hunApd
#UmahukuMat15hun<- AllData15hun

subumahuku15hun<-subset(UmahukuMat15hun,grepl("2017/04", UmahukuMat15hun$"年月日"))

#subumahuku15hun<-subset(UmahukuMat15hun,(
# UmahukuMat15hun["馬場.x"]=="船橋" |UmahukuMat15hun["馬場.x"]=="大井" |UmahukuMat15hun["馬場.x"]=="浦和" |UmahukuMat15hun["馬場.x"]=="川崎" 
 #UmahukuMat15hun["馬場.x"]=="門別" |UmahukuMat15hun["馬場.x"]=="園田" |UmahukuMat15hun["馬場.x"]=="笠松" |UmahukuMat15hun["馬場.x"]=="名古屋" 
#)&UmahukuMat15hun["馬場.x"]!="調整用の行")
#UmahukuMat15hun<-subumahuku15hun


#2分前、15分前差分析

mat <- matrix(0,length(UmahukuMat[,1]),17)
mat215 <- cbind(UmahukuMat[,1:3],mat) 
catched <- 1
#年月日、馬場、R、単勝オッズ・馬番と、必要なものだけの行列にする。
tanshoUmahuku2hun <- cbind(UmahukuMat[,1:3],UmahukuMat[,12:43])
tanshoUmahuku15hun <- cbind(UmahukuMat15hun[,1:3],UmahukuMat15hun[,12:43])

#二つのデータの行数は一致しないこともあるので、上から順に一致するものを探す。
for (All2Idx in 1:length(tanshoUmahuku2hun [,1])){
  findFlag <- FALSE
  start <- catched
  for (All15Idx in start:length(tanshoUmahuku15hun [,1])){
    #●年月日、馬場、Rが一致する行を探している。
    if(tanshoUmahuku15hun [All15Idx,1] == tanshoUmahuku2hun [All2Idx ,1] && 
       tanshoUmahuku15hun [All15Idx,2] == tanshoUmahuku2hun [All2Idx ,2] && 
       tanshoUmahuku15hun [All15Idx,3] == tanshoUmahuku2hun [All2Idx ,3] ){ 
      for(i in 1:16){
	  #人気順１から１６までで馬番が変わっているか判別する。
	  if(tanshoUmahuku15hun [All15Idx,2 + i*2] != tanshoUmahuku2hun [All2Idx,2 + i*2]){
	    for(j in 1:16){
		#15分前ではi番目だったのが、2分前にはj番目に変わっている。
	      if(tanshoUmahuku15hun [All15Idx,2 + i*2] == tanshoUmahuku2hun [All2Idx,2 + j*2]){
		  #その時の(変わった後のオッズ)/(変わる前の15分前オッズ)を、15分前時点の人気順の場所に格納する。
		　　mat215[All2Idx ,i+3] <- tanshoUmahuku2hun [All2Idx,3 + j*2] /tanshoUmahuku15hun [All15Idx,3 + i*2]
		　　break
		}	      
	    }
	  }
      }
      findFlag <- TRUE
      catched <- All15Idx 	
	break
    }
  }#15hun
  #見つかった場合は個数を計算して、見つからなかった場合は、個数に-1を入れる。
  if(findFlag){
    for(keisanIdx in 1:16){
      if(mat215 [All2Idx ,keisanIdx + 3] > 0){
        mat215 [All2Idx ,20] <- mat215 [All2Idx ,20] + 1
      }
    }
  }else{
    mat215 [All2Idx ,20] <- -1
  }
}#2hun

#mat215 <-subset(mat215 ,grepl("2017/06/1", UmahukuMat$"年月日"))


