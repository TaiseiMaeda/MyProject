#傾向調査馬複関数版

################################################################
#傾向を調査する。 ★馬複用
#sub2の代入式と何番人気狙いかには調べたい関数を入れる。
#________________________________________________________________
#●設定
#
BFKekkaNeraiStart <- 0 #結果何番人気を狙うか
BFKekkaNeraiEnd <- 3
#
fitted <- "fitted"
#fitted <- ""

#
sink()#コンソールのみに戻す
#
url <- paste("R出力\\",format(Sys.Date(),"%Y%m%d"),"_","Umahuku",BFKekkaNeraiStart ,"_",BFKekkaNeraiEnd ,"_",fitted,"mat215.txt",sep="")
#
sink(file = url ,split = TRUE)
#url 
#____________________________________________________________________
#●関数
#____________________________________________________________________
mat215Chosa_Umahuku2<- function(KEIKODATA,KEKKASTART,KEKKAEND,IDX){
BAIRITSU <- KEKKAEND -KEKKASTART - 1;Allsub<- KEIKODATA[1,];x<- matrix(0,40,6)
for (i in 0:39) {   
if(IDX == 20){hani1<-i;hani2<-(i+1);}
else if(i<20){hani1<-i * 0.05;hani2<-(i+1)*0.05;}
else if(i<30){hani1<-1+(i-20)*0.1;hani2<-1+(i-19)*0.1}
else{hani1<-2+(i-30)*0.25;hani2<-2+(i-29)*0.25;}
sub2<-subset(KEIKODATA,

  mat215[IDX]>= hani1 & mat215[IDX]< hani2);x[i+1,1] <- hani1
if (length(sub2[,1]) == 0){
      x[i+1,2] <- 0;x[i+1,3] <- 0;x[i+1,4] <- 0;x[i+1,5] <- 0;x[i+1,6] <- 0
  } else{
kIdx <- (KEKKASTART + 1 ):(KEKKAEND  -1)
mat<-matrix(0,1,BAIRITSU )
command <- ""
for (k in kIdx){　
  mat[k - KEKKASTART ] <- paste("(sub2[\"X", sprintf("%d",k), "番人気馬番1\"]  == sub2[\"馬複馬番1\"]", sep="")
  command <- paste(command,mat[k-KEKKASTART ]," & " ,sep="")
  mat[k - KEKKASTART ] <- paste("sub2[\"X", sprintf("%d",k), "番人気馬番2\"]  == sub2[\"馬複馬番2\"])", sep="")

  if(k < KEKKAEND  -1)command <- paste(command,mat[k-KEKKASTART ]," | " ,sep="")
  else command <- paste(command,mat[k-KEKKASTART ] ,sep="")
}
sub <- subset(sub2,
eval(parse(text=command))
)
	#条件の中で狙い目が勝った数
      x[i+1,2] <- length(sub[,1])
	#条件に当てはまった数
      x[i+1,3] <- length(sub2[,1])
	#的中率（条件の中で勝った割合）
      x[i+1,4] <- length(sub[,1])/length(sub2[,1])
	#総損益額（条件の中で勝った金額 - 条件を全て購入した時に使ったお金）
	if (length(sub[,1])==0){
	      x[i+1,5] <-　- length(sub2[,1]) * BAIRITSU *100
	}else{
		x[i+1,5] <-　sum(sub["馬複金額"]) - length(sub2[,1]) *BAIRITSU * 100
	}
	#1Rの投票での回収率
	x[i+1,6] <- x[i+1,5] / x[i+1,3]
  }		              
 } 
return(x)
}

#_____________________________________________________________________
#●mat215用調査
for (i in 1:17){
  x1<-mat215Chosa_Umahuku2(UmahukuMat,BFKekkaNeraiStart,BFKekkaNeraiEnd,i+3)
  print(paste("<-----------",i,sep=""))
  print(x1)
}

