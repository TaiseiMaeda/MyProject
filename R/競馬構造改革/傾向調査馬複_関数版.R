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

url <- paste("R出力\\",format(Sys.Date(),"%Y%m%d"),"_","Umahuku",BFKekkaNeraiStart ,"_",BFKekkaNeraiEnd ,"_",fitted,".txt",sep="")
sink(file(url, encoding = "UTF-8") ,split = TRUE)
url 
#____________________________________________________________________
#●関数
#____________________________________________________________________
keikoChosa_Umahuku2<- function(KEIKODATA,STRNERAI,KEKKASTART,KEKKAEND,XHANNISTART,XHANNIEND,HANNISETEI){

if(STRNERAI== 10 |STRNERAI== 20 |STRNERAI== 30){STRNERAI= paste(STRNERAI,"以下数",sep="")
 }else if(STRNERAI== 50 |STRNERAI== 70 |STRNERAI== 100){STRNERAI= paste(STRNERAI,"以上数",sep="")}

BAIRITSU <- KEKKAEND -KEKKASTART - 1
x<- matrix(0,101,6)
y<- matrix(0,101,6)
for (i in 0:(XHANNIEND + 2)) {   
#全体のデータに対して条件で絞り込む
sub2<-subset(KEIKODATA,

			KEIKODATA[STRNERAI]>= i * as.numeric(HANNISETEI) &
		KEIKODATA[STRNERAI]< (i + 1) * as.numeric(HANNISETEI) )
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
                  
#_______________________________________________________________
#●以下上数×6を一度に表示
t<-proc.time()
ikajoMat<- c("10","20","30","50","70","100")
for (i in 1:6){
  x1<-keikoChosa_Umahuku2(UmahukuMat,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,13,1 )
  x2<-keikoChosa_Umahuku2(subumahuku,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,13,1 )
  print(paste("<-----------",ikajoMat[i],sep=""))
  print(cbind(x1,x2)[0:13,])
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}
proc.time()-t
wariaiBairitsu <- 1
#_____________________________________________________________________
#●馬複人気順×15を一度に表示
t<-proc.time()
ikajoMat<- c("馬複人気順1番","馬複人気順2番","馬複人気順3番","馬複人気順4番","馬複人気順5番","馬複人気順6番","馬複人気順7番","馬複人気順8番","馬複人気順9番","馬複人気順10番","馬複人気順11番","馬複人気順12番","馬複人気順13番","馬複人気順14番","馬複人気順15番")
haniStartMat<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
haniEndMat<- c(50,50,30,40,45,30,40,40,35,30,30,25,30,25,35)
wariaiMat<-c(1/5,1/2,1,1,1,2,2,2,4,4,4,8,8,10,12)
wariaiMat <- wariaiMat * wariaiBairitsu 
for (i in 1:15){
  x1<-keikoChosa_Umahuku2(UmahukuMat,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,haniStartMat[i],haniEndMat[i],wariaiMat[i])
  x2<-keikoChosa_Umahuku2(subumahuku,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,haniStartMat[i],haniEndMat[i],wariaiMat[i])
  print(paste("<-----------",ikajoMat[i],"   開始範囲:終了範囲=",haniStartMat[i],":",haniEndMat[i],"   倍率=",wariaiMat[i],"倍",sep=""))
  print(cbind(x1,x2)[haniStartMat[i]:haniEndMat[i],])
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}
proc.time()-t
#_______________________________________________________________________
#●単勝人気順×14を一度に表示
ikajoMat<- c("単勝人気順1番","単勝人気順2番","単勝人気順3番","単勝人気順4番","単勝人気順5番","単勝人気順6番","単勝人気順7番","単勝人気順8番","単勝人気順9番","単勝人気順10番","単勝人気順11番","単勝人気順12番","単勝人気順13番","単勝人気順14番")
haniStartMat<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
haniEndMat<- c(30,45,35,25,25,25,25,25,25,34,25,25,25,25)
wariaiMat<-c(1/5,1/2,1,2,6,8,15,20,30,30,40,40,40,40)
wariaiMat <- wariaiMat * wariaiBairitsu 
for (i in 1:14){
  x1<-keikoChosa_Umahuku2(UmahukuMat,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,haniStartMat[i],haniEndMat[i],wariaiMat[i])
  x2<-keikoChosa_Umahuku2(subumahuku,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,haniStartMat[i],haniEndMat[i],wariaiMat[i])
  print(paste("<-----------",ikajoMat[i],"   開始範囲:終了範囲=",haniStartMat[i],":",haniEndMat[i],"   倍率=",wariaiMat[i],"倍",sep=""))
  print(cbind(x1,x2)[haniStartMat[i]:haniEndMat[i],])
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}

#_______________________________________________________________________\\
#●馬複人気順16-30を一度に表示
ikajoMat<- c("馬複人気順16番","馬複人気順17番","馬複人気順18番","馬複人気順19番","馬複人気順20番","馬複人気順21番","馬複人気順22番","馬複人気順23番","馬複人気順24番","馬複人気順25番","馬複人気順26番","馬複人気順27番","馬複人気順28番","馬複人気順29番","馬複人気順30番")
haniStartMat<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
haniEndMat<- c(25,25,30,30,30,30,35,35,30,30,35,35,40,40,40)
wariaiMat<-c(20,20,20,30,30,35,35,40,50,50,60,60,60,60,60)
wariaiMat <- wariaiMat * wariaiBairitsu 
for (i in 1:15){
  x1<-keikoChosa_Umahuku2(UmahukuMat,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,haniStartMat[i],haniEndMat[i],wariaiMat[i])
  x2<-keikoChosa_Umahuku2(subumahuku,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,haniStartMat[i],haniEndMat[i],wariaiMat[i])
  print(paste("<-----------",ikajoMat[i],"   開始範囲:終了範囲=",haniStartMat[i],":",haniEndMat[i],"   倍率=",wariaiMat[i],"倍",sep=""))
  print(cbind(x1,x2)[haniStartMat[i]:haniEndMat[i],])
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}

#_______________________________________________________________________________
#●三連複人気順×15を一度に表示
ikajoMat<- c("三連複人気順1番","三連複人気順2番","三連複人気順3番","三連複人気順4番","三連複人気順5番","三連複人気順6番","三連複人気順7番","三連複人気順8番","三連複人気順9番","三連複人気順10番","三連複人気順11番","三連複人気順12番","三連複人気順13番","三連複人気順14番","三連複人気順15番")
haniStartMat<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
haniEndMat<- c(35,50,25,30,30,30,40,40,35,25,30,30,40,35,35)
wariaiMat<-c(1,1,2,2,2,2,2,2,2,4,4,4,4,6,6)
wariaiMat <- wariaiMat * wariaiBairitsu 
for (i in 1:15){
  x1<-keikoChosa_Umahuku2(UmahukuMat,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,haniStartMat[i],haniEndMat[i],wariaiMat[i])
  x2<-keikoChosa_Umahuku2(subumahuku,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,haniStartMat[i],haniEndMat[i],wariaiMat[i])
  print(paste("<-----------",ikajoMat[i],"   開始範囲:終了範囲=",haniStartMat[i],":",haniEndMat[i],"   倍率=",wariaiMat[i],"倍",sep=""))
  print(cbind(x1,x2)[haniStartMat[i]:haniEndMat[i],])
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}
#_________________________________________________________________
#●馬複不人気順×15を一度に表示
ikajoMat<- c("馬複不人気順1番","馬複不人気順2番","馬複不人気順3番","馬複不人気順4番","馬複不人気順5番","馬複不人気順6番","馬複不人気順7番","馬複不人気順8番","馬複不人気順9番","馬複不人気順10番","馬複不人気順11番","馬複不人気順12番","馬複不人気順13番","馬複不人気順14番","馬複不人気順15番")
wariai <- 200 * wariaiBairitsu 
for (i in 1:15){
  x1<-keikoChosa_Umahuku2(UmahukuMat,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50,wariai )
  x2<-keikoChosa_Umahuku2(subumahuku,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50,wariai )
  print(paste("<-----------",ikajoMat[i],"   開始範囲:終了範囲= 0 :倍率= ",wariai ,"倍",sep=""))
  print(cbind(x1,x2)[0:50,])
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}
#___________________________________________________________________
#●馬複不人気順×16-30を一度に表示
ikajoMat<- c("馬複不人気順16番","馬複不人気順17番","馬複不人気順18番","馬複不人気順19番","馬複不人気順20番","馬複不人気順21番","馬複不人気順22番","馬複不人気順23番","馬複不人気順24番","馬複不人気順25番","馬複不人気順26番","馬複不人気順27番","馬複不人気順28番","馬複不人気順29番","馬複不人気順30番")
wariai <- 80 * wariaiBairitsu 
for (i in 1:15){
  x1<-keikoChosa_Umahuku2(UmahukuMat,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50,wariai )
  x2<-keikoChosa_Umahuku2(subumahuku,ikajoMat[i],BFKekkaNeraiStart,BFKekkaNeraiEnd,0,50,wariai )
  print(paste("<-----------",ikajoMat[i],"   開始範囲:終了範囲= 0 :倍率= ",wariai ,"倍",sep=""))
  print(cbind(x1,x2)[0:50,])
  print(sum(x1[,5]));print(sum(x2[,5]))#利益額
}


