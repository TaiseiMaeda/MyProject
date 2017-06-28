ChosaMat <- subset(UmahukuMat,"単勝金額"> 0, c(a,"単勝人気順3番","馬複人気順11番","馬複人気順1番","単勝人気順4番馬番","単勝人気順5番馬番","単勝人気順6番馬番","単勝馬番","単勝金額","指数1","指数2","指数3","指数4","指数5"))

sakuseiHiduke <- "2017/05/01"
TanshoNin3_7<-subset(ChosaMat ,ChosaMat ["馬複人気順1番"] > 2.79 &
	ChosaMat["単勝人気順3番"] > 3.99)
BFKekkaNeraiStart <- 3
BFKekkaNeraiEnd   <- 7
checked <- kekkaCheckTansho2(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin3_7[,1]);length(checked [,1])/length(TanshoNin3_7[,1])
atehameMat <- TanshoNin3_7

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho2(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin3_7[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin3_7,checked_Shisu [[1]])


#"単勝人気順i番馬番"　という文字列を作成。
#nerai <- "単勝人気順"
#s <- paste(nerai , sprintf("%d",i), "番馬番", sep="")
#ss<-"ChosaMat [\"単勝人気順1番\"] < 120 "
#eval(parse(text=ss))

#最も高い利益率の番号を保持
hoji <- c(0,0,0,0)
riekihoji <-  c(0,0,0,0)
hojiShisu<-  c(0,0,0,0)
riekihojiShisu <-  c(0,0,0,0)
ChosaMat <- UmahukuMat

kekkaMatome <- matrix(1,1,20)
soeji <- 1
t<-proc.time()
repeat{
for(i in 1:length(joken)){
  shurui  <- strsplit(joken[i],"\"")[[1]][2]
  #ChosaMat <- subset(UmahukuMat,"単勝金額"> 0, c(shurui,"単勝人気順3番","馬複人気順1番","単勝人気順4番馬番","単勝人気順5番馬番","単勝人気順6番馬番","単勝馬番","単勝金額","指数1","指数2","指数3","指数4","指数5"))

  command <- ""
  if(soeji > 1){
  for(l in 1:(soeji - 1)){
    command <- paste(command,joken[kekkaMatome [l]] ," & ",  sep="")
  }
  }
  command <- paste(command,joken[i],  sep="")
  selected<-subset(ChosaMat ,
ChosaMat["単勝人気順10番"]/ChosaMat["単勝人気順11番"] < 0.96  & 
ChosaMat["馬複人気順11番"] < 36.00  & 
ChosaMat["馬複人気順9番"]/ChosaMat["馬複人気順10番"] > 0.81  & 
ChosaMat["馬複不人気順1番"] < 6000.00  & 
ChosaMat["三連複人気順11番"] < 40.00  & 
ChosaMat["単勝人気順10番"]/ChosaMat["単勝人気順11番"] > 0.31  & 
ChosaMat["馬複人気順6番"]/ChosaMat["馬複人気順7番"] > 0.75  & 
ChosaMat["馬複人気順4番"] < 14.00  & 
ChosaMat["単勝人気順2番"]/ChosaMat["単勝人気順3番"] > 0.39  & 
ChosaMat["三連複人気順7番"]/ChosaMat["三連複人気順8番"] < 1.00  & 
ChosaMat["単勝人気順4番"]/ChosaMat["単勝人気順5番"] > 0.39  & 
ChosaMat["馬複人気順25番"]/ChosaMat["馬複人気順26番"] < 1.00  &
 ChosaMat["単勝人気順3番"] > 3.99  &
 ChosaMat["馬複人気順25番"]/ChosaMat["馬複人気順26番"] > 0.73  &
 ChosaMat["三連複人気順9番"]/ChosaMat["三連複人気順10番"] > 0.7 &
    eval(parse(text=command ))  
  )

  BFKekkaNeraiStart <- 3
  BFKekkaNeraiEnd   <- 7
　　#通常
  checked <- kekkaCheckTansho2(selected,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
  riekiritsu<-sum(checked ["単勝金額"])/(length(selected[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
  blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
　 if(riekihoji[1] < riekiritsu){hoji[1]<-i;riekihoji[4]<-riekihoji[3];riekihoji[3]<-riekihoji[2];riekihoji[2]<-riekihoji[1];riekihoji[1]<-riekiritsu;
　 }else if(riekihoji[2] < riekiritsu){hoji[2]<-i;riekihoji[4]<-riekihoji[3];riekihoji[3]<-riekihoji[2];riekihoji[2]<-riekiritsu
  }else if(riekihoji[3] < riekiritsu){hoji[3]<-i;riekihoji[4]<-riekihoji[3];riekihoji[3]<-riekiritsu
  }else if(riekihoji[4] < riekiritsu){hoji[4]<-i;riekihoji[4]<-riekiritsu}


　　#指数
  checkedShisu<- kekkaCheckShisu_tansho2(selected,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
  riekiritsuShisu<- sum(checkedShisu [[2]]["単勝金額"])/(length(selected[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checkedShisu[[1]][4])* 100)
　 if(riekihojiShisu[1] < riekiritsuShisu){hojiShisu[1]<-i;riekihojiShisu[4]<-riekihojiShisu[3];riekihojiShisu[3]<-riekihojiShisu[2];riekihojiShisu[2]<-riekihojiShisu[1];riekihojiShisu[1]<-riekiritsuShisu
　 }else if(riekihojiShisu[2] < riekiritsuShisu){hojiShisu[2]<-i;riekihojiShisu[4]<-riekihojiShisu[3];riekihojiShisu[3]<-riekihojiShisu[2];riekihojiShisu[2]<-riekiritsuShisu
  }else if(riekihojiShisu[3] < riekiritsuShisu){hojiShisu[3]<-i;riekihojiShisu[4]<-riekihojiShisu[3];riekihojiShisu[3]<-riekiritsuShisu
  }else if(riekihojiShisu[4] < riekiritsuShisu){hojiShisu[4]<-i;riekihojiShisu[4]<-riekiritsuShisu}


  kekkaMatome[soeji] <- hojiShisu[1]
  for(idx in 1:4){
    if(hojiShisu[idx] == hoji[1] | hojiShisu[idx] == hoji[2] | hojiShisu[idx] == hoji[3] |hojiShisu[idx] == hoji[4] ){
	kekkaMatome[soeji] <- hojiShisu[idx]
    }
  }
  
}

  print(hoji)
  print(riekihoji)
  print(hojiShisu)
  print(riekihojiShisu)
  print(proc.time()-t)

soeji <- soeji + 1
if(soeji > 3)break;
}
proc.time()-t

