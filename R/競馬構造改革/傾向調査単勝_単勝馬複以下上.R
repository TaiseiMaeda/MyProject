#傾向調査馬複関数版

################################################################
#傾向を調査する。 ★単勝用
#sub2の代入式と何番人気狙いかには調べたい関数を入れる。
#________________________________________________________________
#●設定
#
BFKekkaNeraiStart <- 3 #結果何番人気を狙うか
BFKekkaNeraiEnd <- 7

fitted <- ""

sink()#コンソールのみに戻す

url <- paste("R出力\\",format(Sys.Date(),"%Y%m%d"),"_","TanshoIkajo",BFKekkaNeraiStart ,"_",BFKekkaNeraiEnd ,"_",fitted,".txt",sep="")
sink(file(url, encoding = "UTF-8") ,split = TRUE)
url 
#____________________________________________________________________
#●関数
#____________________________________________________________________
keikoChosa_IkajoTansho<- function(KEIKODATA,STRNERAI,KEKKASTART,KEKKAEND){

BAIRITSU <- KEKKAEND -KEKKASTART - 1
#行をアペンドするためのダミー行。チェックが終わった後に消す。
Allsub<- KEIKODATA[1,]

x<- matrix(0,31,6)
for (i in 0:30) {   
sub2<-subset(KEIKODATA,	

		as.numeric(umahukuIkasuMat [,STRNERAI])== i )
      x[i+1,1] <- i
  if (length(sub2[,1]) == 0){
      x[i+1,2] <- 0;x[i+1,3] <- 0;x[i+1,4] <- 0;x[i+1,5] <- 0;x[i+1,6] <- 0
  } else{
#行をアペンドするためのダミー行。チェックが終わった後に消す。
sub<- KEIKODATA[1,]
jIdx <- 1:length(sub2[,1])
kIdx <- (KEKKASTART+ 1 ):(KEKKAEND-1)
for (j in jIdx){
  for (k in kIdx){
    #（10+k*2）は単勝k番人気の馬番
    if(!is.na(sub2[j,10 + k*2])){
	#47列は単勝馬番
      if(sub2[j,10 + k*2] == sub2[j,47]){
        #一致していたら当たったということで行を取り出し、アペンドする。
        sub <- rbind(sub,sub2[j,])
        Allsub <- rbind(Allsub ,sub2[j,])
        break
      }
    }
  }
}
if (length(sub[,1]) > 1){sub <- sub[2:length(sub[,1]),]}
else{sub <- NULL}
	#条件の中で狙い目が勝った数
      x[i+1,2] <- length(sub[,1])
	#条件に当てはまった数
      x[i+1,3] <- length(sub2[,1])
	#的中率（条件の中で勝った割合）
      x[i+1,4] <- length(sub[,1])/length(sub2[,1])
	#総損益額（条件の中で勝った金額 - 条件を全て購入した時に使ったお金）
	if (is.null(sub)){
	      x[i+1,5] <-　- length(sub2[,1]) * BAIRITSU *100
	}else{
		x[i+1,5] <-　sum(sub["単勝金額"]) - length(sub2[,1]) *BAIRITSU * 100
	}
	#1Rの投票での回収率
	x[i+1,6] <- x[i+1,5] / x[i+1,3]
#print(x[i+1,])
  }		              
 } 
if (length(Allsub[,1]) > 1){Allsub <- Allsub[2:length(Allsub[,1]),]
}else{Allsub <- NULL}
#print(length(Allsub [,1]))
return(x)
}
                  
#_______________________________________________________________
#●以下上数×6を一度に表示
ikajoMat<- c("10以下","10以上20以下","20以上30以下","30以上50以下","50以上100以下","100以上200以下","200以上300以下","300以上","0","20以下","30以下","50以下","100以下","200以上","100以上")
for (i in 1:15){
  x1<-keikoChosa_IkajoTansho(UmahukuMat,i+3,BFKekkaNeraiStart,BFKekkaNeraiEnd)
  print(paste("<-----------配列",i+3,"番目",ikajoMat[i],sep=""))
  print(cbind(x1)[0:31,])
  print(sum(x1[,5]));
}



