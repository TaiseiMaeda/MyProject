###############################################################
#傾向を調査する。 ★ワイド用
#sub2の代入式と何番人気狙いかには調べたい関数を入れる。
#________________________________________________________________
#  設定
#
xHanniStart<- 10 #表示の際のX軸の範囲始まり
xHanniEnd  <- 84 #範囲終わり
strNerai <- "馬複人気順10番"
#strNerai <- "10以下数"
#strNerai <-  "単勝人気順4番"

keikoData<- UmahukuMat
#keikoData<- subumahuku
BFKekkaNeraiStart <- 12 #結果何番人気を狙うか
BFKekkaNeraiEnd <- 21
#________________________________________________________________
bairitsu <- BFKekkaNeraiEnd - BFKekkaNeraiStart -1
#行をアペンドするためのダミー行。チェックが終わった後に消す。
Allsub<-0

x<- matrix(0,101,6)
y<- matrix(0,101,6)
for (i in 0:100) {   
#全体のデータに対して条件で絞り込む
sub2<-subset(keikoData,
		#UmahukuMat["30以下数"]< 9 &
			#UmahukuMat["馬複人気順9番"]< 33 &
			UmahukuMat["馬複人気順8番"]< 26 &
			#UmahukuMat["馬複人気順6番"]> 12.8 &
			#UmahukuMat["馬複人気順5番"]> 11.5 &
			#UmahukuMat["単勝人気順4番"]> 7 &
		keikoData[strNerai]>= i &
		keikoData[strNerai]<  i + 1 )
      x[i+1,1] <- i
  if (length(sub2[,1]) == 0){
      x[i+1,2] <- 0
      x[i+1,3] <- 0
      x[i+1,4] <- 0
	x[i+1,5] <- 0
	x[i+1,6] <- 0
  } else{
subwide1<- UmahukuMat[1,]
subwide2<- UmahukuMat[1,]
subwide3<- UmahukuMat[1,]

jIdx <- 1:length(sub2[,1])
kIdx <- (BFKekkaNeraiStart  + 1 ):(BFKekkaNeraiEnd  -1)
for (j in jIdx){
  for (k in kIdx){
    #人気順k番の馬番1,2と結果人気順の馬番1,2を比べている。
    if(!is.na(sub2[j,84 + k*3 + 1]) && !is.na(sub2[j,84 + k*3 + 2])){
      if(sub2[j,84 + k*3 + 1] == sub2[j, 75] && sub2[j,84 + k*3 + 2] == sub2[j, 76]){
        #一致していたら当たったということで行を取り出し、アペンドする。
        subwide1 <- rbind(subwide1,sub2[j,])
	  Allsub<-Allsub +1
      }
	if(sub2[j,84 + k*3 + 1] == sub2[j,79] && sub2[j,84 + k*3 + 2] == sub2[j,80]){
        #一致していたら当たったということで行を取り出し、アペンドする。
        subwide2 <- rbind(subwide2,sub2[j,])
	  Allsub<-Allsub +1
      }
	if(sub2[j,84 + k*3 + 1] == sub2[j,83] && sub2[j,84 + k*3 + 2] == sub2[j,84]){
        #一致していたら当たったということで行を取り出し、アペンドする。
        subwide3 <- rbind(subwide3,sub2[j,])
	  Allsub<-Allsub +1
      }
    }
  }
}
if (length(subwide1[,1]) > 1){
    subwide1 <- subwide1[2:length(subwide1[,1]),]
}else{
    subwide1 <- NULL
}
if (length(subwide2[,1]) > 1){
    subwide2 <- subwide2[2:length(subwide2[,1]),]
}else{
    subwide2 <- NULL
}
if (length(subwide3[,1]) > 1){
    subwide3 <- subwide3[2:length(subwide3[,1]),]
}else{
    subwide3 <- NULL
}
	#条件の中で狙い目が勝った数
      x[i+1,2] <- length(subwide1[,1])+length(subwide2[,1])+length(subwide3[,1])
	#条件に当てはまった数
      x[i+1,3] <- length(sub2[,1])
	#的中率（条件の中で勝った割合）
      x[i+1,4] <- (length(subwide1[,1])+length(subwide2[,1])+length(subwide3[,1]))/length(sub2[,1])

	wide1Kingaku <- 0
	wide2Kingaku <- 0
	wide3Kingaku <- 0
	#総損益額（条件の中で勝った金額 - 条件を全て購入した時に使ったお金）
	if (!is.null(subwide1)){
	  wide1Kingaku <- sum(subwide1["ワイド1金額"])
	}
	if(!is.null(subwide2)){
	  wide2Kingaku <- sum(subwide2["ワイド2金額"])
	}
	if(!is.null(subwide3)){
	  wide3Kingaku <- sum(subwide3["ワイド3金額"])
	}
	x[i+1,5] <-　 wide1Kingaku + wide2Kingaku + wide3Kingaku  - length(sub2[,1]) *bairitsu * 100

	#1Rの投票での回収率
	x[i+1,6] <- x[i+1,5] / x[i+1,3]

  }		              
 } 

if (length(Allsub[,1]) > 1){
    Allsub <- Allsub[2:length(Allsub[,1]),]
}else{
    Allsub <- NULL
}

##画面を横２つに分割して表示する
split.screen(c(1,2))
##グラフの表示
screen(1)
barplot(x[,2],xlim=c(xHanniStart,xHanniEnd),beside=T,axes=F, xlab="", ylab="")
axis(side=4)
par(new=T)
plot(x[,1],x[,5],xlim=c(xHanniStart,xHanniEnd),col="red")
# 左上にレジェンドを貼る。
legend(
	"topleft",
	legend="総利益"
)


screen(2)
plot(x[,1],x[,6],xlim=c(xHanniStart,xHanniEnd))
# 左上にレジェンドを貼る。
legend(
	"topleft",
	legend="総利益/購入回数"

)

x[xHanniStart:xHanniEnd,]
Allsub
sum(x[,5])

