#傾向調査ワイド関数版
################################################################
#傾向を調査する。 ★ワイド用
#sub2の代入式と何番人気狙いかには調べたい関数を入れる。
#________________________________________________________________
#●設定
#

BFKekkaNeraiStart <- 1 #結果何番人気を狙うか
BFKekkaNeraiEnd <- 3

fitted <- "KISOfitted"
#fitted <- ""
sink()#コンソールのみに戻す

url <- paste("R出力\\",format(Sys.Date(),"%Y%m%d"),"_","Wide",BFKekkaNeraiStart ,"_",BFKekkaNeraiEnd ,"_",fitted,".txt",sep="")
sink(file = url ,split = TRUE)
url 
#____________________________________________________________________
#●関数
#____________________________________________________________________
keikoChosa_Wide<- function(KEIKODATA,STRNERAI,KEKKASTART,KEKKAEND,XHANNISTART,XHANNIEND,HANNISETEI1,HANNISETEI2){

Allsub<-0
BAIRITSU <- KEKKAEND -KEKKASTART - 1
x<- matrix(0,101,6)
y<- matrix(0,101,6)
for (i in 0:(XHANNIEND+2)) {   
#全体のデータに対して条件で絞り込む
sub2<-subset(KEIKODATA,	KEIKODATA["10以下数"]< 5 &
			KEIKODATA["10以下数"]> 1 &
			KEIKODATA["20以下数"]> 2 &
			KEIKODATA["20以下数"]< 9 &
			KEIKODATA["30以下数"]< 10 &
			KEIKODATA["馬複人気順1番"]< 9.6 &
			KEIKODATA["馬複人気順2番"]< 9 &
			KEIKODATA["馬複人気順3番"]< 12 & 
			KEIKODATA["馬複人気順4番"]< 18 & 
			KEIKODATA["馬複人気順7番"]> 13.9 & 
			KEIKODATA["馬複人気順8番"]> 15.9 & 
			KEIKODATA["馬複人気順11番"]> 19.9 & 
			KEIKODATA["単勝人気順6番"]> 11.9 & 
			KEIKODATA["単勝人気順12番"]< 600 & 
			KEIKODATA["三連複人気順1番"]< 18 & 
			KEIKODATA["三連複人気順3番"]< 26 & 
			KEIKODATA["三連複人気順5番"]< 30 & 
			KEIKODATA["三連複人気順7番"]> 11.9 & 
			KEIKODATA["三連複人気順10番"]> 19.9 &
			KEIKODATA["三連複人気順13番"]> 27.9 & 
			KEIKODATA["馬複人気順28番"]< 1080 &
			KEIKODATA["馬複不人気順10番"]< 3000 &
		mat215[STRNERAI]>= HANNISETEI1 &
		mat215[STRNERAI]<  HANNISETEI2 )
      x[i+1,1] <- i
  if (length(sub2[,1]) == 0){
      x[i+1,2] <- 0;x[i+1,3] <- 0;x[i+1,4] <- 0;x[i+1,5] <- 0;x[i+1,6] <- 0
  } else{
subwide1<- KEIKODATA[1,];subwide2<- KEIKODATA[1,];subwide3<- KEIKODATA[1,]
jIdx <- 1:length(sub2[,1])
kIdx <- (KEKKASTART+ 1 ):(KEKKAEND -1)
for (j in jIdx){
  for (k in kIdx){
    if(!is.na(sub2[j,84 + k*3 + 1]) && !is.na(sub2[j,84 + k*3 + 2])){
      if(sub2[j,84 + k*3 + 1] == sub2[j, 75] && sub2[j,84 + k*3 + 2] == sub2[j, 76]){
        subwide1 <- rbind(subwide1,sub2[j,])
	  Allsub<-Allsub +1
      }
	if(sub2[j,84 + k*3 + 1] == sub2[j,79] && sub2[j,84 + k*3 + 2] == sub2[j,80]){
        subwide2 <- rbind(subwide2,sub2[j,])
	  Allsub<-Allsub +1
      }
	if(sub2[j,84 + k*3 + 1] == sub2[j,83] && sub2[j,84 + k*3 + 2] == sub2[j,84]){
        subwide3 <- rbind(subwide3,sub2[j,])
	  Allsub<-Allsub +1
      }
    }
  }
}
lenwide1　<-　0;lenwide2　<-　0;lenwide3　<-　0;
if (length(subwide1[,1]) > 1){subwide1 <- subwide1[2:length(subwide1[,1]),];lenwide1　<-length(subwide1[,1])
}else{subwide1 <- NULL}
if (length(subwide2[,1]) > 1){subwide2 <- subwide2[2:length(subwide2[,1]),];lenwide2　<-length(subwide2[,1])
}else{subwide2 <- NULL}
if (length(subwide3[,1]) > 1){subwide3 <- subwide3[2:length(subwide3[,1]),];lenwide3　<-length(subwide3[,1])
}else{subwide3 <- NULL}
      x[i+1,2] <- lenwide1 + lenwide2 + lenwide3
	#条件に当てはまった数
      x[i+1,3] <- length(sub2[,1])
	#的中率（条件の中で勝った割合）
      x[i+1,4] <- (lenwide1 + lenwide2 + lenwide3)/length(sub2[,1])
	wide1Kingaku <- 0
	wide2Kingaku <- 0
	wide3Kingaku <- 0
	#総損益額（条件の中で勝った金額 - 条件を全て購入した時に使ったお金）
	if (!is.null(subwide1)){wide1Kingaku <- sum(subwide1["ワイド1金額"])}
	if (!is.null(subwide2)){wide2Kingaku <- sum(subwide2["ワイド2金額"])}
	if (!is.null(subwide3)){wide3Kingaku <- sum(subwide3["ワイド3金額"])}
	x[i+1,5] <-　 wide1Kingaku + wide2Kingaku + wide3Kingaku  - length(sub2[,1]) * BAIRITSU * 100
	#1Rの投票での回収率
	x[i+1,6] <- x[i+1,5] / x[i+1,3]
  }		              
 } 
print(Allsub)
return(x)
}

#_______________________________________________________________
#●以下上数×6を一度に表示
ikajoMat<- c(4:19)
for (i in 1:16){
  x1<-keikoChosa_Wide(UmahukuMat,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,13,1 )
  print(paste("<-----------",ikajoMat[i],sep=""))
  print(cbind(x1,x2)[0:13,])
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}


