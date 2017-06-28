#傾向調査馬複関数版

################################################################
#傾向を調査する。 ★単勝用
#sub2の代入式と何番人気狙いかには調べたい関数を入れる。
#________________________________________________________________
#●設定
#
BFKekkaNeraiStart <- 3
BFKekkaNeraiEnd <- 7
fitted <- "KISOfitted"
#fitted <- ""

sink()#コンソールのみに戻す

url <- paste("R出力\\",format(Sys.Date(),"%Y%m%d"),"_","Tansho",BFKekkaNeraiStart ,"_",BFKekkaNeraiEnd ,"_",fitted,"mat215.txt",sep="")
sink(file(url, encoding = "UTF-8") ,split = TRUE)
url
#____________________________________________________________________
#●関数
#____________________________________________________________________
mat215Chosa_Tansho2<- function(KEIKODATA,KEKKASTART,KEKKAEND,IDX){
BAIRITSU <- KEKKAEND -KEKKASTART - 1;Allsub<- KEIKODATA[1,];x<- matrix(0,40,6)
for (i in 0:39) {   
if(IDX == 20){hani1<-i;hani2<-(i+1);}
else if(i<20){hani1<-i * 0.05;hani2<-(i+1)*0.05;}
else if(i<30){hani1<-1+(i-20)*0.1;hani2<-1+(i-19)*0.1;}
else{hani1<-2+(i-30)*0.25;hani2<-2+(i-29)*0.25;}
sub2<-subset(KEIKODATA,

mat215[IDX]>= hani1 & mat215[IDX]< hani2);x[i+1,1] <- hani1
if (length(sub2[,1]) == 0){x[i+1,2] <- 0;x[i+1,3] <- 0;x[i+1,4] <- 0;x[i+1,5] <- 0;x[i+1,6] <- 0
} else{
kIdx <- (KEKKASTART + 1 ):(KEKKAEND  -1)
mat<-matrix(0,1,BAIRITSU )
command <- ""
for (k in kIdx){　
  mat[k - KEKKASTART ] <- paste("sub2[\"単勝人気順", sprintf("%d",k), "番馬番\"]  == sub2[\"単勝馬番\"]", sep="")
  
  if(k < KEKKAEND  -1)command <- paste(command,mat[k-KEKKASTART ]," | " ,sep="")
  else command <- paste(command,mat[k-KEKKASTART ] ,sep="")
}
sub <- subset(sub2,
eval(parse(text=command))
)
x[i+1,2] <- length(sub[,1]);x[i+1,3] <- length(sub2[,1]);x[i+1,4] <- length(sub[,1])/length(sub2[,1])
if (length(sub[,1])==0){x[i+1,5] <-　- length(sub2[,1]) * BAIRITSU *100
}else{x[i+1,5] <-　sum(sub["単勝金額"]) - length(sub2[,1]) *BAIRITSU * 100	}
x[i+1,6] <- x[i+1,5] / x[i+1,3]}} 
if (length(Allsub[,1]) > 1){Allsub <- Allsub[2:length(Allsub[,1]),]}else{Allsub <- NULL}
return(x)}
#_____________________________________________________________________
#●mat215用調査

for (i in 1:1){
  x1<-mat215Chosa_Tansho2(UmahukuMat,BFKekkaNeraiStart,BFKekkaNeraiEnd,i+3)
  print(paste("<-----------",i,sep=""))
  print(x1)
}

