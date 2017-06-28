#投票術
#2分までの時点でｎ番人気馬複を購入した時の期待値
#________________________________________
#●馬複
sosu <- length(UmahukuMat[,1])
UmahukuRiekiMat <- matrix(0,30,3)
for (i in 1:30){
  sub<-kekkaCheck(UmahukuMat ,i-1,i+1)
  B <- length(sub[,1])
  sumHaraimodoshi <- sum(sub[,65]) /B 
  UmahukuRiekiMat[i,1] <- B /sosu #勝った件数/全レース　で勝率を出している。
  UmahukuRiekiMat[i,2] <- sumHaraimodoshi #1勝利あたりの平均払い戻し額
  UmahukuRiekiMat[i,3] <- UmahukuRiekiMat[i,1] * UmahukuRiekiMat[i,2] #勝率*平均払い戻し額で期待値
}
UmahukuRiekiMat
sum(UmahukuRiekiMat[15:28,1])

#________________________________________
#●単勝
sosu <- length(UmahukuMat[,1])
UmahukuRiekiMatTansho <- matrix(0,16,3)
for (i in 1:16){
  sub<-kekkaCheckTansho(UmahukuMat ,i-1,i+1)
  T <- length(sub[,1])
  sumHaraimodoshi <- sum(sub[,48]) /T 
  UmahukuRiekiMatTansho [i,1] <- T /sosu 
  UmahukuRiekiMatTansho [i,2] <- sumHaraimodoshi 
  UmahukuRiekiMatTansho [i,3] <- UmahukuRiekiMatTansho [i,1] * UmahukuRiekiMatTansho [i,2]
}
UmahukuRiekiMatTansho 
sum(UmahukuRiekiMatTansho [5:9,1])

#________________________________________
#●ワイド
sosu <- length(UmahukuMat[,1])
UmahukuRiekiMatWide <- matrix(0,30,3)
for (i in 1:30){
  sub1<-kekkaCheckWide(UmahukuMat ,i-1,i+1,1)
  sub2<-kekkaCheckWide(UmahukuMat ,i-1,i+1,2)
  sub3<-kekkaCheckWide(UmahukuMat ,i-1,i+1,3)
  if (!is.null(sub1)){wide1Kingaku <- sum(sub1["ワイド1金額"]);wide1len<-length(sub1[,1])
}else{wide1Kingaku <- 0;wide1len<-0;}
  if (!is.null(sub2)){wide2Kingaku <- sum(sub2["ワイド2金額"]);wide2len<-length(sub2[,1])
}else{wide2Kingaku <- 0;wide2len<-0;}
  if (!is.null(sub3)){wide3Kingaku <- sum(sub3["ワイド3金額"]);wide3len<-length(sub3[,1])
}else{wide3Kingaku <- 0;wide3len<-0;}
  
W <- wide1len+wide2len+wide3len
  UmahukuRiekiMatWide[i,1] <- W /sosu #勝った件数/全レース　で勝率を出している。
  UmahukuRiekiMatWide[i,2] <- (wide1Kingaku+wide2Kingaku+wide3Kingaku)/W #1勝利あたりの平均払い戻し額
  UmahukuRiekiMatWide[i,3] <- UmahukuRiekiMatWide[i,1] * UmahukuRiekiMatWide[i,2] #勝率*平均払い戻し額で期待値
}
UmahukuRiekiMatWide
sum(UmahukuRiekiMatWide[9:12,1])
#________________________________________
#●三連複
sosu <- length(UmahukuMat3[,1])
SanrenhukuRiekiMat <- matrix(0,30,3)
for (i in 1:15){
  sub<-kekkaCheckSanrenhuku (UmahukuMat3,i-1,i+1)
  B <- length(sub[,1])
  sumHaraimodoshi <- sum(sub["三連複金額"]) /B 
  SanrenhukuRiekiMat[i,1] <- B /sosu #勝った件数/全レース　で勝率を出している。
  SanrenhukuRiekiMat[i,2] <- sumHaraimodoshi #1勝利あたりの平均払い戻し額
  SanrenhukuRiekiMat[i,3] <- SanrenhukuRiekiMat[i,1] * SanrenhukuRiekiMat[i,2] #勝率*平均払い戻し額で期待値
}
SanrenhukuRiekiMat
sum(SanrenhukuRiekiMat[15:28,1])


















#bk_____________________________________________________________
#勝が出るまでが一回の試行だとした時の期待値
#馬複1,2番人気　3.15倍　20%の仮定
konyuKin<-c(1,1,1,2,3,4,6,9,14,20,30,44)
ruikeiKin<-matrix(1,1,12)
shutsugenKakuritsu<-matrix(0.2,1,12)
for(i in 2:12){
ruikeiKin[i]<-konyuKin[i]+ruikeiKin[i-1]
shutsugenKakuritsu[i]<- 0.2 * 0.8^(i-1)
}
riekigaku<-konyuKin * 3.15 - ruikeiKin
rbind(konyuKin,ruikeiKin,shutsugenKakuritsu,riekigaku,riekigaku * shutsugenKakuritsu)
kitaichi<-sum(riekigaku[1:5] * shutsugenKakuritsu[1:5])
kitaichi * 100 
#一回の試行でkitaichi * 100 円儲かるということ。
#しかし、12回やっても勝たない確率7%でruikeiKin[12]円損する。
#______________________________________________________________



##_____________________________________________________________
#馬番ではなく、人気順で見ているのであまりよくない。
##結果取得用の関数
#kekkaCheckを使って馬番で確認する。

#総数
sosu <- length(UmahukuMat[,1])
sosuold <- length(kekkaIntoTansyoIkasu[,1])
sosuplus <- sosu + sosuold
#馬複21-30数
B2130 <- length(subset(UmahukuMat ,UmahukuMat [,66] > 20 & UmahukuMat [,66] < 31)[,1])
B2130old<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] > 20 & kekkaIntoTansyoIkasu[,30] < 31)[,1])
B2130plus <- B2130 + B2130old
B2130 /sosu 
B2130old/sosuold 
B2130plus/sosuplus 
sum(subset(UmahukuMat ,UmahukuMat [,66] > 20 & UmahukuMat [,66] < 31)[,65]) /B2130 

#馬複15-30数
B1530 <- length(subset(UmahukuMat ,UmahukuMat [,66] > 14 & UmahukuMat [,66] < 31)[,1])
B1530old<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] > 14 & kekkaIntoTansyoIkasu[,30] < 31)[,1])
B1530plus <- B1530 + B1530old
B1530 /sosu 
B1530old/sosuold 
B1530plus/sosuplus 
sum(subset(UmahukuMat ,UmahukuMat [,66] > 14 & UmahukuMat [,66] < 31)[,65]) /B1530

#馬複13-20数
B1320 <- length(subset(UmahukuMat ,UmahukuMat [,66] > 12 & UmahukuMat [,66] < 21)[,1])
B1320old<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] > 12 & kekkaIntoTansyoIkasu[,30] < 21)[,1])
B1320plus <- B1320 + B1320old
B1320 /sosu 
B1320old/sosuold 
B1320plus/sosuplus
sum(subset(UmahukuMat ,UmahukuMat [,66] > 12 & UmahukuMat [,66] < 21)[,65]) /B1320

#馬複8-14数
B814 <- length(subset(UmahukuMat ,UmahukuMat [,66] > 7 & UmahukuMat [,66] < 15)[,1])
B814old<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] > 7 & kekkaIntoTansyoIkasu[,30] < 15)[,1])
B814plus <- B814 + B814old
B814 /sosu 
B814old/sosuold 
B814plus/sosuplus 
sum(subset(UmahukuMat ,UmahukuMat [,66] > 7 & UmahukuMat [,66] < 15)[,65]) /B814 

#馬複5-10数
B510 <- length(subset(UmahukuMat ,UmahukuMat [,66] > 4 & UmahukuMat [,66] < 11)[,1])
B510old<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] > 4 & kekkaIntoTansyoIkasu[,30] < 11)[,1])
B510plus <- B510 + B510old
B510 /sosu 
B510old/sosuold 
B510plus/sosuplus 
sum(subset(UmahukuMat ,UmahukuMat [,66] > 4 & UmahukuMat [,66] < 11)[,65]) /B510 

#馬複3-6数
B36 <- length(subset(UmahukuMat ,UmahukuMat [,66] > 2 & UmahukuMat [,66] < 7)[,1])
B36old<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] > 2 & kekkaIntoTansyoIkasu[,30] < 7)[,1])
B36plus <- B36 + B36
B36 /sosu 
B36old/sosuold 
B36plus/sosuplus 
sum(subset(UmahukuMat ,UmahukuMat [,66] > 2 & UmahukuMat [,66] < 7)[,65]) /B36 

#馬複3-4数
B34 <- length(subset(UmahukuMat ,UmahukuMat [,66] > 2 & UmahukuMat [,66] < 5)[,1])
B34old<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] > 2 & kekkaIntoTansyoIkasu[,30] < 5)[,1])
B34plus <- B34 + B34
B34 /sosu 
B34old/sosuold 
B34plus/sosuplus 
sum(subset(UmahukuMat ,UmahukuMat [,66] > 2 & UmahukuMat [,66] < 5)[,65]) /B34 

#馬複2-3数
B23 <- length(subset(UmahukuMat ,UmahukuMat [,66] > 1 & UmahukuMat [,66] < 4)[,1])
B23old<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] > 1 & kekkaIntoTansyoIkasu[,30] < 4)[,1])
B23plus <- B23 + B23old
B23 /sosu 
B23old/sosuold 
B23plus/sosuplus 
sum(subset(UmahukuMat ,UmahukuMat [,66] > 1 & UmahukuMat [,66] < 4)[,65]) /B23 

#馬複1-1数
B11 <- length(subset(UmahukuMat ,UmahukuMat [,66] > 0 & UmahukuMat [,66] < 2)[,1])
B11old<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] > 0 & kekkaIntoTansyoIkasu[,30] < 2)[,1])
B11plus <- B11 + B11old
B11 /sosu 
B11old/sosuold 
B11plus/sosuplus
sum(subset(UmahukuMat ,UmahukuMat [,66] > 0 & UmahukuMat [,66] < 2)[,65]) /B11 

#馬複
UmahukuRiekiMat <- matrix(0,30,7)
for (i in 1:30){
B <- length(subset(UmahukuMat ,UmahukuMat [,66] == i)[,1])
Bold<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] == i)[,1])
Bplus <- B + Bold
sumHaraimodoshi <- sum(subset(UmahukuMat ,UmahukuMat [,66] == i)[,65]) /B 
sumHaraimodoshiold <- sum(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,30] == i)[,29]) /Bold
UmahukuRiekiMat[i,1] <- B /sosu 
UmahukuRiekiMat[i,2] <- Bold/sosuold 
UmahukuRiekiMat[i,3] <- Bplus/sosuplus 
UmahukuRiekiMat[i,4] <- sumHaraimodoshi 
UmahukuRiekiMat[i,5] <- sumHaraimodoshiold 
UmahukuRiekiMat[i,6] <- (sumHaraimodoshiold *Bold + sumHaraimodoshi * B)/(Bold + B)
UmahukuRiekiMat[i,7] <- UmahukuRiekiMat[i,3] * UmahukuRiekiMat[i,6]
}
UmahukuRiekiMat

sum(UmahukuRiekiMat[1:30,3])

#単勝
TanshoRiekiMat <- matrix(0,16,7)
for (i in 1:16){
T <- length(subset(UmahukuMat ,UmahukuMat [,49] == i)[,1])
Told<-length(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,13] == i)[,1])
Tplus <- T + Told
sumHaraimodoshi <- sum(subset(UmahukuMat ,UmahukuMat [,49] == i)[,48]) /T 
sumHaraimodoshiold <- sum(subset(kekkaIntoTansyoIkasu,kekkaIntoTansyoIkasu[,13] == i)[,12]) /Told
TanshoRiekiMat[i,1] <- T /sosu 
TanshoRiekiMat[i,2] <- Told/sosuold 
TanshoRiekiMat[i,3] <- Tplus/sosuplus 
TanshoRiekiMat[i,4] <- sumHaraimodoshi 
TanshoRiekiMat[i,5] <- sumHaraimodoshiold 
TanshoRiekiMat[i,6] <- (sumHaraimodoshiold *Told + sumHaraimodoshi * T)/(Told + T)
TanshoRiekiMat[i,7] <- TanshoRiekiMat[i,3] * TanshoRiekiMat[i,6]
}
TanshoRiekiMat

sum(TanshoRiekiMat[1:16,3])



##単勝5-9 1000回購入　均等に100円ずつ　　1000*100*5 = 500,000円
#5 54勝 70200円
#6 41勝 79130円
#7 28勝 72800円
#8 18勝 56100円
#9 13勝 55900円
#計334,130円　66.8%
##単勝5-9 454回購入　400,300,200,100,100円で分ける　　454*1100 = 499,400円
#5 454*0.054*1297*4 = 127189円
#6 454*0.041*1928*3 = 107663円
#7 454*0.028*2586*2 = 65746円
#8 454*0.018*3269*1 = 26714円
#9 454*0.013*4344*1 = 25638円
#計352,950円　70.6%