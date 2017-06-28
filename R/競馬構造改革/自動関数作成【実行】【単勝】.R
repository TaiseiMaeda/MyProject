sink()#コンソールのみに戻す
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 4
joken<-jokenT2_4
  blnShisu <- c(FALSE,TRUE,TRUE,FALSE,FALSE)
url <- paste("R出力\\",format(Sys.Date(),"%Y%m%d"),"_","Tansho",BFKekkaNeraiStart ,"_",BFKekkaNeraiEnd ,"_","自動生成.txt",sep="")
sink(file(url, encoding = "UTF-8") ,split = TRUE)
url 


#最も高い利益率の番号を保持
hoji <- c(0,0,0,0)
riekihoji <-  c(0,0,0,0)
riekihojiRelateShisu <-  c(0,0,0,0)

hojiShisu<-  c(0,0,0,0)
riekihojiShisu <-  c(0,0,0,0)
riekihojiShisuRelateTsujo <-  c(0,0,0,0)

kekkaMatome <- matrix(1,1,20)
soeji <- 1
t<-proc.time()
repeat{
for(i in 1:length(joken)){
  shurui  <- strsplit(joken[i],"\"")[[1]][2]

  command <- ""
  if(soeji > 1){
  for(l in 1:(soeji - 1)){
    command <- paste(command,joken[kekkaMatome [l]] ," & ",  sep="")
  }
  }
  command <- paste(command,joken[i],  sep="")
  selected<-subset(UmahukuMat ,
    eval(parse(text=command ))  
  )

  flag<-0
  riekiritsuShisu<-0
  checkedShisu<-matrix(1,0,1)
  if(length(selected[,1])>0){
　　#指数
  checkedShisu<- kekkaCheckShisu_tansho2(selected,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
  }
  if(length(checkedShisu[[2]][,1])>0){
  riekiritsuShisu<- sum(checkedShisu [[2]]["単勝金額"])/(length(selected[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checkedShisu[[1]][4])* 100)
  }
　 if(riekihojiShisu[1] < riekiritsuShisu){
   hojiShisu[1]<-i;riekihojiShisu[4]<-riekihojiShisu[3];riekihojiShisu[3]<-riekihojiShisu[2];riekihojiShisu[2]<-riekihojiShisu[1];riekihojiShisu[1]<-riekiritsuShisu
   riekihojiShisuRelateTsujo [4]<-riekihojiShisuRelateTsujo [3];riekihojiShisuRelateTsujo [3]<-riekihojiShisuRelateTsujo [2];riekihojiShisuRelateTsujo [2]<-riekihojiShisuRelateTsujo [1];flag<-1
　 }else if(riekihojiShisu[2] < riekiritsuShisu){
   hojiShisu[2]<-i;riekihojiShisu[4]<-riekihojiShisu[3];riekihojiShisu[3]<-riekihojiShisu[2];riekihojiShisu[2]<-riekiritsuShisu
   riekihojiShisuRelateTsujo [4]<-riekihojiShisuRelateTsujo [3];riekihojiShisuRelateTsujo [3]<-riekihojiShisuRelateTsujo [2];flag<-2
  }else if(riekihojiShisu[3] < riekiritsuShisu){
   hojiShisu[3]<-i;riekihojiShisu[4]<-riekihojiShisu[3];riekihojiShisu[3]<-riekiritsuShisu
   riekihojiShisuRelateTsujo [4]<-riekihojiShisuRelateTsujo [3];flag<-3
  }else if(riekihojiShisu[4] < riekiritsuShisu){
   hojiShisu[4]<-i;riekihojiShisu[4]<-riekiritsuShisu
   flag<-4
  }

　　#通常
  riekiritsu<-0
  checked<-matrix(1,0,1)
  if(length(selected[,1])>0){
  checked <- kekkaCheckTansho2(selected,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
  }
  if(length(checked [,1])>0){
  riekiritsu<-sum(checked ["単勝金額"])/(length(selected[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
  }

　 if(riekihoji[1] < riekiritsu){
   hoji[1]<-i;riekihoji[4]<-riekihoji[3];riekihoji[3]<-riekihoji[2];riekihoji[2]<-riekihoji[1];riekihoji[1]<-riekiritsu;
   riekihojiRelateShisu[4]<-riekihojiRelateShisu[3];riekihojiRelateShisu[3]<-riekihojiRelateShisu[2];riekihojiRelateShisu[2]<-riekihojiRelateShisu[1];riekihojiRelateShisu[1]<-riekiritsuShisu;
　 }else if( riekihoji[2] < riekiritsu){
   hoji[2]<-i;riekihoji[4]<-riekihoji[3];riekihoji[3]<-riekihoji[2];riekihoji[2]<-riekiritsu
   riekihojiRelateShisu[4]<-riekihojiRelateShisu[3];riekihojiRelateShisu[3]<-riekihojiRelateShisu[2];riekihojiRelateShisu[2]<-riekiritsuShisu;
  }else if(riekihoji[3] < riekiritsu){
   hoji[3]<-i;riekihoji[4]<-riekihoji[3];riekihoji[3]<-riekiritsu
   riekihojiRelateShisu[4]<-riekihojiRelateShisu[3];riekihojiRelateShisu[3]<-riekiritsuShisu;
  }else if( riekihoji[4] < riekiritsu){
   hoji[4]<-i;riekihoji[4]<-riekiritsu
   riekihojiRelateShisu[4]<-riekiritsuShisu
  }

  if(flag>0){
   riekihojiShisuRelateTsujo [flag]<-riekiritsu
  }

  kekkaMatome[soeji] <- hojiShisu[1]
　　sentaku <- 0
  #通常の方の1～4番目の中で最も指数の利益率が高いものを選ぶ
  #↓逆。指数の中で最も高い通常のものを選ぶ。
  for(idx in 1:4){
    if(sentaku < riekihojiShisuRelateTsujo [idx]){
      sentaku <- riekihojiShisuRelateTsujo [idx]
	kekkaMatome[soeji] <- hojiShisu[idx]
    }
  }
  
}

　　print("通常")
  print(hoji)
  print(riekihoji)
  print(riekihojiRelateShisu)
　　print("指数")
  print(hojiShisu)
  print(riekihojiShisuRelateTsujo)
  print(riekihojiShisu)
  print("選択↓")
  print(kekkaMatome[soeji])
  print(proc.time()-t)

soeji <- soeji + 1
if(soeji > 20)break;
}

command <- ""
for(l in 1:(soeji - 1)){
  command <- paste(command,joken[kekkaMatome [l]] ," & ",  sep="")
}
print(kekkaMatome )
print(command)
