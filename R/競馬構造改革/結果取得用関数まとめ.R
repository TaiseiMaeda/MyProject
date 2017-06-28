#====================================================================
#結果取得用関数まとめ
#_____________________________________________________________
#●馬複結果取得用の関数
kekkaCheck2 <- function(x,y,z){
len <- z-y-1
kIdx <- (y + 1 ):(z -1)
mat<-matrix(0,1,len)
command <- ""
for (k in kIdx){　
  mat[k - y] <- paste("(x[\"X", sprintf("%d",k), "番人気馬番1\"]  == x[\"馬複馬番1\"]", sep="")
  command <- paste(command,mat[k-y]," & " ,sep="")
  mat[k - y] <- paste("x[\"X", sprintf("%d",k), "番人気馬番2\"]  == x[\"馬複馬番2\"])", sep="")

  if(k < z-1)command <- paste(command,mat[k-y]," | " ,sep="")
  else command <- paste(command,mat[k-y] ,sep="")
}
kekka <- subset(x,
eval(parse(text=command))
)
return(kekka)
}
#_____________________________________________________________
#●単勝結果取得用の関数
kekkaCheckTansho2 <- function(x,y,z){
len <- z-y-1
kIdx <- (y + 1 ):(z -1)
mat<-matrix(0,1,len)
command <- ""
for (k in kIdx){　
  mat[k - y] <- paste("x[\"単勝人気順", sprintf("%d",k), "番馬番\"]  == x[\"単勝馬番\"]", sep="")
  
  if(k < z-1)command <- paste(command,mat[k-y]," | " ,sep="")
  else command <- paste(command,mat[k-y] ,sep="")
}
kekka <- subset(x,
eval(parse(text=command))
)
return(kekka)
}
#____________________________________________________________

#●馬複結果取得用の関数（指数使用）
kekkaCheckShisu2 <- function(x,y,z,blnShisu){
len <- z-y-1
kIdx <- (y + 1 ):(z -1)
mat<-matrix(0,1,len)
command <- ""
sub<- x[1,]
cancelMat<-cbind(x[,1:3],matrix(0,length(x[,1]),1))
kachiKingaku <- 0
cancelKingaku <- 0
for (k in kIdx){　
  command <- paste("((blnShisu[1] & (x[\"X", sprintf("%d",k), "番人気馬番1\"]  == x[\"指数1\"] | x[\"X", sprintf("%d",k), "番人気馬番2\"]  == x[\"指数1\"]))|",  sep="")
  command <- paste(command ,"(blnShisu[2] & (x[\"X", sprintf("%d",k), "番人気馬番1\"]  == x[\"指数2\"] | x[\"X", sprintf("%d",k), "番人気馬番2\"]  == x[\"指数2\"]))|",  sep="")
  command <- paste(command ,"(blnShisu[3] & (x[\"X", sprintf("%d",k), "番人気馬番1\"]  == x[\"指数3\"] | x[\"X", sprintf("%d",k), "番人気馬番2\"]  == x[\"指数3\"]))|",  sep="")
  command <- paste(command ,"(blnShisu[4] & (x[\"X", sprintf("%d",k), "番人気馬番1\"]  == x[\"指数4\"] | x[\"X", sprintf("%d",k), "番人気馬番2\"]  == x[\"指数4\"]))|",  sep="")
  command <- paste(command ,"(blnShisu[5] & (x[\"X", sprintf("%d",k), "番人気馬番1\"]  == x[\"指数5\"] | x[\"X", sprintf("%d",k), "番人気馬番2\"]  == x[\"指数5\"])))",  sep="")
  command2<-command

  mat[k - y] <- paste("&(x[\"X", sprintf("%d",k), "番人気馬番1\"]  == x[\"馬複馬番1\"]", sep="")
  command <- paste(command,mat[k-y]," & " ,sep="")
  mat[k - y] <- paste("x[\"X", sprintf("%d",k), "番人気馬番2\"]  == x[\"馬複馬番2\"])", sep="")

  command <- paste(command,mat[k-y] ,sep="")

  kekka <- subset(x,
  eval(parse(text=command))
  )
  kekka2 <- subset(x,!
  eval(parse(text=command2))
  )
  if(length(kekka[,1])!=0) sub <- rbind(sub,kekka)
  if(length(kekka2[,1])!=0) cancelMat[j,4] <- cancelMat[j,4] + length(kekka2[,1])
}
if (length(sub[,1]) > 1){sub <- sub[2:length(sub[,1]),]
}else{sub <- NULL}
return(list(cancelMat,sub))

}


#●単勝結果取得用の関数（指数使用）
kekkaCheckShisu_tansho2 <- function(x,y,z,blnShisu){
len <- z-y-1
kIdx <- (y + 1 ):(z -1)
mat<-matrix(0,1,len)
command <- ""
sub<- x[1,]
cancelMat<-cbind(x[,1:3],matrix(0,length(x[,1]),1))
kachiKingaku <- 0
cancelKingaku <- 0
for (k in kIdx){　
  command <- paste("((blnShisu[1] & (x[\"単勝人気順", sprintf("%d",k), "番馬番\"]  == x[\"指数1\"]))|",  sep="")
  command <- paste(command,"(blnShisu[2] & (x[\"単勝人気順", sprintf("%d",k), "番馬番\"]  == x[\"指数2\"]))|",  sep="")
  command <- paste(command,"(blnShisu[3] & (x[\"単勝人気順", sprintf("%d",k), "番馬番\"]  == x[\"指数3\"]))|",  sep="")
  command <- paste(command,"(blnShisu[4] & (x[\"単勝人気順", sprintf("%d",k), "番馬番\"]  == x[\"指数4\"]))|",  sep="")
  command <- paste(command,"(blnShisu[5] & (x[\"単勝人気順", sprintf("%d",k), "番馬番\"]  == x[\"指数5\"])))",  sep="")
  command2<-command

  mat[k - y] <- paste("& x[\"単勝人気順", sprintf("%d",k), "番馬番\"]  == x[\"単勝馬番\"]", sep="")
  
  command <- paste(command,mat[k-y] ,sep="")

  kekka <- subset(x,
  eval(parse(text=command))
  )
  kekka2 <- subset(x,!
  eval(parse(text=command2))
  )
  if(length(kekka[,1])!=0) sub <- rbind(sub,kekka)
  if(length(kekka2[,1])!=0) cancelMat[j,4] <- cancelMat[j,4] + length(kekka2[,1])
}
if (length(sub[,1]) > 1){sub <- sub[2:length(sub[,1]),]
}else{sub <- NULL}
return(list(cancelMat,sub))

}

#_____________________________________________________________
#●ワイド結果取得用の関数 (馬番2分前人気順(y+1)から(z-1)の馬番をワイドで購入する)
kekkaCheckShisu_Wide <- function(x,y,z,w,blnShisu1,blnShisu2,blnShisu3,blnShisu4,blnShisu5){ #wはワイドw番（w;1:3）
cancelKai <- 0
cancelMat<-cbind(x[,1:3],matrix(0,length(x[,1]),1))
sub<- UmahukuMat[1,]
jIdx <- 1:length(x[,1])
kIdx <- (y + 1 ):(z -1)
for (j in jIdx){
  for (k in kIdx){　#人気順k番の馬番1,2と結果人気順の馬番1,2を比べている。
    if(!is.na(x[j,84 + k*3 + 1]) && !is.na(x[j,84 + k*3 + 2])){
	#指数を使ったキャンセルをしている。
	if(x[j,84 + k*3 + 1] != x[j,"指数1"] && x[j,84 + k*3 + 2] != x[j,"指数1"]
	  && x[j,84 + k*3 + 1] != x[j,"指数2"] && x[j,84 + k*3 + 2] != x[j,"指数2"]
	  && x[j,84 + k*3 + 1] != x[j,"指数3"] && x[j,84 + k*3 + 2] != x[j,"指数3"]){

		cancelKai <- cancelKai + 1 
	}else{
        if(x[j,84 + k*3 + 1] == x[j,4*(w - 1) + 75] && x[j,84 + k*3 + 2] == x[j,4*(w - 1) + 76]){
          #一致していたら当たったということで行を取り出し、アペンドする。
          sub <- rbind(sub,x[j,])
        }		
	}
    }
  }
  cancelMat[j,4] <- cancelKai 
  cancelKai <- 0
}

if (length(sub[,1]) > 1){sub <- sub[2:length(sub[,1]),]
}else{sub <- NULL}
return(list(cancelMat,sub))
}
