#傾向調査馬複関数版

################################################################
#傾向を調査する。 ★馬複用
#sub2の代入式と何番人気狙いかには調べたい関数を入れる。
#________________________________________________________________
#●設定
#
BFKekkaNeraiStart <- 14 #結果何番人気を狙うか
BFKekkaNeraiEnd <- 29
#
fitted <- "KISOfitted"
#fitted <- "KISOfitted5bai"
#fitted <- "5bai"
#
fitted <- ""

sink()#コンソールのみに戻す

url <- paste("R出力\\",format(Sys.Date(),"%Y%m%d"),"_","Umahuku",BFKekkaNeraiStart ,"_",BFKekkaNeraiEnd ,"_",fitted,"OddsHI.txt",sep="")
sink(file(url, encoding = "UTF-8") ,split = TRUE)
url 
#____________________________________________________________________
#●関数
#____________________________________________________________________
keikoChosa_UmahukuOddsHI2<- function(KEIKODATA,STRNERAI1,STRNERAI2,KEKKASTART,KEKKAEND,XHANNISTART,XHANNIEND){

BAIRITSU <- KEKKAEND -KEKKASTART - 1
x<- matrix(0,51,6)
y<- matrix(0,51,6)
for (i in 1:(XHANNIEND)) {   
#全体のデータに対して条件で絞り込む
if(KEIKODATA[STRNERAI1] != 0 && KEIKODATA[STRNERAI2] != 0 ){
	sub2<-subset(KEIKODATA,

		KEIKODATA[STRNERAI1]/KEIKODATA[STRNERAI2] >= i * 0.02 &
		KEIKODATA[STRNERAI1]/KEIKODATA[STRNERAI2]< (i + 1) * 0.02 )
}else{
	sub2 <- matrix(0,0,1)
}
      x[i+1,1] <- i
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
#●馬複人気順×29を一度に表示
ikajoMat1<- c("馬複人気順1番","馬複人気順2番","馬複人気順3番","馬複人気順4番","馬複人気順5番","馬複人気順6番","馬複人気順7番","馬複人気順8番","馬複人気順9番","馬複人気順10番","馬複人気順11番","馬複人気順12番","馬複人気順13番","馬複人気順14番","馬複人気順15番","馬複人気順16番","馬複人気順17番","馬複人気順18番","馬複人気順19番","馬複人気順20番","馬複人気順21番","馬複人気順22番","馬複人気順23番","馬複人気順24番","馬複人気順25番","馬複人気順26番","馬複人気順27番","馬複人気順28番","馬複人気順29番")
ikajoMat2<- c("馬複人気順2番","馬複人気順3番","馬複人気順4番","馬複人気順5番","馬複人気順6番","馬複人気順7番","馬複人気順8番","馬複人気順9番","馬複人気順10番","馬複人気順11番","馬複人気順12番","馬複人気順13番","馬複人気順14番","馬複人気順15番","馬複人気順16番","馬複人気順17番","馬複人気順18番","馬複人気順19番","馬複人気順20番","馬複人気順21番","馬複人気順22番","馬複人気順23番","馬複人気順24番","馬複人気順25番","馬複人気順26番","馬複人気順27番","馬複人気順28番","馬複人気順29番","馬複人気順30番")
for (i in 1:29){
  x1<-keikoChosa_UmahukuOddsHI2(UmahukuMat,ikajoMat1[i],ikajoMat2[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50)
  x2<-keikoChosa_UmahukuOddsHI2(subumahuku,ikajoMat1[i],ikajoMat2[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50)
  print(paste("<-----------",ikajoMat1[i],"/",ikajoMat2[i],"   開始範囲:終了範囲=",0,":",100,sep=""))
  print(cbind(x1,x2))
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}
#_______________________________________________________________________
#●単勝人気順×13を一度に表示
ikajoMat1<- c("単勝人気順1番","単勝人気順2番","単勝人気順3番","単勝人気順4番","単勝人気順5番","単勝人気順6番","単勝人気順7番","単勝人気順8番","単勝人気順9番","単勝人気順10番","単勝人気順11番","単勝人気順12番","単勝人気順13番")
ikajoMat2<- c("単勝人気順2番","単勝人気順3番","単勝人気順4番","単勝人気順5番","単勝人気順6番","単勝人気順7番","単勝人気順8番","単勝人気順9番","単勝人気順10番","単勝人気順11番","単勝人気順12番","単勝人気順13番","単勝人気順14番")
for (i in 1:13){
  x1<-keikoChosa_UmahukuOddsHI2(UmahukuMat,ikajoMat1[i],ikajoMat2[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50)
  x2<-keikoChosa_UmahukuOddsHI2(subumahuku,ikajoMat1[i],ikajoMat2[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50)
  print(paste("<-----------",ikajoMat1[i],"/",ikajoMat2[i],"   開始範囲:終了範囲=",0,":",100,sep=""))
  print(cbind(x1,x2))
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}

#_______________________________________________________________________________
#●三連複人気順×15を一度に表示
ikajoMat1<- c("三連複人気順1番","三連複人気順2番","三連複人気順3番","三連複人気順4番","三連複人気順5番","三連複人気順6番","三連複人気順7番","三連複人気順8番","三連複人気順9番","三連複人気順10番","三連複人気順11番","三連複人気順12番","三連複人気順13番","三連複人気順14番")
ikajoMat2<- c("三連複人気順2番","三連複人気順3番","三連複人気順4番","三連複人気順5番","三連複人気順6番","三連複人気順7番","三連複人気順8番","三連複人気順9番","三連複人気順10番","三連複人気順11番","三連複人気順12番","三連複人気順13番","三連複人気順14番","三連複人気順15番")
for (i in 1:14){
  x1<-keikoChosa_UmahukuOddsHI2(UmahukuMat,ikajoMat1[i],ikajoMat2[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50)
  x2<-keikoChosa_UmahukuOddsHI2(subumahuku,ikajoMat1[i],ikajoMat2[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50)
  print(paste("<-----------",ikajoMat1[i],"/",ikajoMat2[i],"   開始範囲:終了範囲=",0,":",100,sep=""))
  print(cbind(x1,x2))
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}
#___________________________________________________________________
#●馬複不人気順×16-30を一度に表示
#ikajoMat1<- c("馬複不人気順1番","馬複不人気順2番","馬複不人気順3番","馬複不人気順4番","馬複不人気順5番","馬複不人気順6番","馬複不人気順7番","馬複不人気順8番","馬複不人気順9番","馬複不人気順10番","馬複不人気順11番","馬複不人気順12番","馬複不人気順13番","馬複不人気順14番","馬複不人気順15番","馬複不人気順16番","馬複不人気順17番","馬複不人気順18番","馬複不人気順19番","馬複不人気順20番","馬複不人気順21番","馬複不人気順22番","馬複不人気順23番","馬複不人気順24番","馬複不人気順25番","馬複不人気順26番","馬複不人気順27番","馬複不人気順28番","馬複不人気順29番")
#ikajoMat2<- c("馬複不人気順2番","馬複不人気順3番","馬複不人気順4番","馬複不人気順5番","馬複不人気順6番","馬複不人気順7番","馬複不人気順8番","馬複不人気順9番","馬複不人気順10番","馬複不人気順11番","馬複不人気順12番","馬複不人気順13番","馬複不人気順14番","馬複不人気順15番","馬複不人気順16番","馬複不人気順17番","馬複不人気順18番","馬複不人気順19番","馬複不人気順20番","馬複不人気順21番","馬複不人気順22番","馬複不人気順23番","馬複不人気順24番","馬複不人気順25番","馬複不人気順26番","馬複不人気順27番","馬複不人気順28番","馬複不人気順29番","馬複不人気順30番")
#for (i in 1:29){
#  x1<-keikoChosa_UmahukuOddsHI2(UmahukuMat,ikajoMat1[i],ikajoMat2[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50)
#  x2<-keikoChosa_UmahukuOddsHI2(subumahuku,ikajoMat1[i],ikajoMat2[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50)
##  print(paste("<-----------",ikajoMat1[i],"/",ikajoMat2[i],"   開始範囲:終了範囲=",0,":",100,sep=""))
#  print(cbind(x1,x2))
#  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
#}

