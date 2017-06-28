TanshoNin3_7<-subset(UmahukuMat,
UmahukuMat["単勝人気順10番"]/UmahukuMat["単勝人気順11番"] < 0.96  & 
UmahukuMat["馬複人気順11番"] < 36.00  & 
UmahukuMat["馬複人気順9番"]/UmahukuMat["馬複人気順10番"] > 0.81  & 
UmahukuMat["馬複不人気順1番"] < 6000.00  & 
UmahukuMat["三連複人気順11番"] < 40.00  & 
UmahukuMat["単勝人気順10番"]/UmahukuMat["単勝人気順11番"] > 0.31  & 
UmahukuMat["馬複人気順6番"]/UmahukuMat["馬複人気順7番"] > 0.75  & 
UmahukuMat["馬複人気順4番"] < 14.00  & 
UmahukuMat["単勝人気順2番"]/UmahukuMat["単勝人気順3番"] > 0.39  & 
UmahukuMat["三連複人気順7番"]/UmahukuMat["三連複人気順8番"] < 1.00  & 
UmahukuMat["単勝人気順4番"]/UmahukuMat["単勝人気順5番"] > 0.39  & 
UmahukuMat["馬複人気順25番"]/UmahukuMat["馬複人気順26番"] < 1.00  &
 UmahukuMat["単勝人気順3番"] > 3.99  &
 UmahukuMat["馬複人気順25番"]/UmahukuMat["馬複人気順26番"] > 0.73  &
 UmahukuMat["三連複人気順9番"]/UmahukuMat["三連複人気順10番"] > 0.7 &
UmahukuMat["三連複人気順1番"] > 3.99  & 
UmahukuMat["三連複人気順12番"] < 44.00  & 
UmahukuMat["馬複人気順3番"]/UmahukuMat["馬複人気順4番"] > 0.53  
)
BFKekkaNeraiStart <- 3
BFKekkaNeraiEnd   <- 7
checked <- kekkaCheckTansho2(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin3_7[,1]);length(checked [,1])/length(TanshoNin3_7[,1])
atehameMat <- TanshoNin3_7

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin3_7[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin3_7,checked_Shisu [[1]])



TanshoNin3_7<-subset(UmahukuMat,
UmahukuMat["馬複人気順11番"] < 36.00  &
 UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"] > 0.47  & 
UmahukuMat["馬複人気順6番"]/UmahukuMat["馬複人気順7番"] > 0.75  & 
UmahukuMat["単勝人気順8番"]/UmahukuMat["単勝人気順9番"] > 0.23  & 
UmahukuMat["馬複不人気順4番"] < 3800.00  & 
UmahukuMat["馬複人気順4番"] < 14.00  & 
UmahukuMat["馬複人気順20番"]/UmahukuMat["馬複人気順21番"] < 1.00  & 
UmahukuMat["馬複人気順19番"]/UmahukuMat["馬複人気順20番"] > 0.77  & 
UmahukuMat["三連複人気順13番"] < 48.00  & 
UmahukuMat["単勝人気順7番"] < 95.00  & 
UmahukuMat["三連複人気順9番"]/UmahukuMat["三連複人気順10番"] > 0.77  & 
UmahukuMat["三連複人気順11番"]/UmahukuMat["三連複人気順12番"] < 1.00  & 
UmahukuMat["三連複人気順7番"]/UmahukuMat["三連複人気順8番"] < 1.00  & 
UmahukuMat["馬複人気順25番"]/UmahukuMat["馬複人気順26番"] < 1.00  & 
UmahukuMat["馬複人気順5番"] < 15.00  &
UmahukuMat["馬複人気順30番"] < 360.00  & 
UmahukuMat["単勝人気順9番"] < 135.00  & 
UmahukuMat["馬複人気順25番"]/UmahukuMat["馬複人気順26番"] > 0.73 )
BFKekkaNeraiStart <- 3
BFKekkaNeraiEnd   <- 7
checked <- kekkaCheckTansho2(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin3_7[,1]);length(checked [,1])/length(TanshoNin3_7[,1])
atehameMat <- TanshoNin3_7

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin3_7[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin3_7,checked_Shisu [[1]])



TanshoNin3_7<-subset(UmahukuMat,
UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"] > 0.39  &
 UmahukuMat["単勝人気順7番"] < 100.00  &
 UmahukuMat["三連複人気順4番"]/UmahukuMat["三連複人気順5番"] > 0.75  & 
UmahukuMat["馬複人気順9番"]/UmahukuMat["馬複人気順10番"] > 0.73  &
 UmahukuMat["馬複人気順3番"]/UmahukuMat["馬複人気順4番"] > 0.45  &
 UmahukuMat["馬複不人気順1番"] < 7600.00  &
 UmahukuMat["馬複不人気順1番"] < 7600.00  &
 UmahukuMat["単勝人気順10番"]/UmahukuMat["単勝人気順11番"] < 0.96  & 
UmahukuMat["馬複不人気順1番"] < 6800.00  & 
UmahukuMat["三連複人気順14番"] < 54.00  &
 UmahukuMat["三連複人気順11番"] < 40.00  &
 UmahukuMat["単勝人気順10番"]/UmahukuMat["単勝人気順11番"] > 0.31  &
 UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"] > 0.47  &
 UmahukuMat["馬複人気順9番"]/UmahukuMat["馬複人気順10番"] > 0.79   )
BFKekkaNeraiStart <- 3
BFKekkaNeraiEnd   <- 7
checked <- kekkaCheckTansho2(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin3_7[,1]);length(checked [,1])/length(TanshoNin3_7[,1])
atehameMat <- TanshoNin3_7

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin3_7[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin3_7,checked_Shisu [[1]])



TanshoNin4_10<-subset(UmahukuMat,
(mat215[5] < 0.001 || mat215[5] > 0.5999 )&
 mat215[6] < 3.25 &
 mat215[8] < 2 &
 mat215[10] < 2.75 &
 UmahukuMat["馬複人気順1番"] > 3.99  &
 UmahukuMat["馬複人気順2番"] > 4.49 &
UmahukuMat["馬複人気順7番"] < 26.00  &
UmahukuMat["馬複人気順9番"] < 32.00  &  
UmahukuMat["馬複人気順13番"] < 40.00  & 
UmahukuMat["馬複人気順18番"] < 80.00  & 
UmahukuMat["馬複人気順19番"] > 29.99  &
UmahukuMat["馬複人気順21番"] > 34.99  & 
UmahukuMat["馬複人気順22番"] < 105.00  & 
UmahukuMat["馬複人気順23番"] < 120.00  &
UmahukuMat["馬複人気順25番"] < 150.00  & 
UmahukuMat["馬複人気順27番"] < 180.00  &
UmahukuMat["馬複人気順28番"] > 59.99  & 
UmahukuMat["馬複人気順30番"] < 240.00  &
UmahukuMat["単勝人気順3番"] > 3.99  & 
UmahukuMat["単勝人気順4番"] < 30.00  & 
UmahukuMat["単勝人気順7番"] > 9.99  &
UmahukuMat["三連複人気順1番"] > 4.99 & 
UmahukuMat["三連複人気順12番"] < 48.00  & 
UmahukuMat["三連複人気順15番"] < 54.00  &
UmahukuMat["馬複不人気順20番"] > 19.99  &  
UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"] > 0.49  &
UmahukuMat["馬複人気順2番"]/UmahukuMat["馬複人気順3番"] > 0.37  & 
UmahukuMat["馬複人気順4番"]/UmahukuMat["馬複人気順5番"] > 0.59  &
 UmahukuMat["馬複人気順5番"]/UmahukuMat["馬複人気順6番"] > 0.61  &
 UmahukuMat["馬複人気順7番"]/UmahukuMat["馬複人気順8番"] > 0.63  &
 UmahukuMat["馬複人気順8番"]/UmahukuMat["馬複人気順9番"] > 0.71  & 
UmahukuMat["馬複人気順9番"]/UmahukuMat["馬複人気順10番"] > 0.69  & 
 UmahukuMat["馬複人気順11番"]/UmahukuMat["馬複人気順12番"] > 0.71  &
 UmahukuMat["馬複人気順13番"]/UmahukuMat["馬複人気順14番"] > 0.75  &
UmahukuMat["馬複人気順22番"]/UmahukuMat["馬複人気順23番"] < 1.00  & 
 UmahukuMat["馬複人気順25番"]/UmahukuMat["馬複人気順26番"] > 0.73  &
UmahukuMat["馬複人気順26番"]/UmahukuMat["馬複人気順27番"] > 0.67  & 
UmahukuMat["単勝人気順2番"]/UmahukuMat["単勝人気順3番"] > 0.35  &
 UmahukuMat["単勝人気順4番"]/UmahukuMat["単勝人気順5番"] > 0.35  & 
 UmahukuMat["単勝人気順7番"]/UmahukuMat["単勝人気順8番"] < 1.00  &
UmahukuMat["三連複人気順1番"]/UmahukuMat["三連複人気順2番"] > 0.31  & 
UmahukuMat["三連複人気順2番"]/UmahukuMat["三連複人気順3番"] > 0.57  &
 UmahukuMat["三連複人気順3番"]/UmahukuMat["三連複人気順4番"] > 0.65  &
 UmahukuMat["三連複人気順5番"]/UmahukuMat["三連複人気順6番"] > 0.69  &
UmahukuMat["三連複人気順8番"]/UmahukuMat["三連複人気順9番"] > 0.73 &
 UmahukuMat["三連複人気順9番"]/UmahukuMat["三連複人気順10番"] > 0.71  &
UmahukuMat["三連複人気順12番"]/UmahukuMat["三連複人気順13番"] > 0.77 )
BFKekkaNeraiStart <- 4
BFKekkaNeraiEnd   <- 10
checked <- kekkaCheckTansho2(TanshoNin4_10,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin4_10[,1]);length(checked [,1])/length(TanshoNin4_10[,1])
atehameMat <- TanshoNin4_10

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin4_10,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin4_10[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin4_10,checked_Shisu [[1]])


BahukuNin14_29<-subset(UmahukuMat,
UmahukuMat["馬複人気順18番"] < 80.00  )
BFKekkaNeraiStart <- 14
BFKekkaNeraiEnd   <- 29
checked <- kekkaCheck2(BahukuNin14_29,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin14_29[,1]);length(checked [,1])/length(BahukuNin14_29[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin14_29

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu2(BahukuNin14_29,BFKekkaNeraiStart,BFKekkaNeraiEnd   ,blnShisu )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin14_29[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin14_29,checked_Shisu [[1]])


TanshoNin4_10<-subset(UmahukuMat,
UmahukuMat["馬複人気順13番"] < 40.00  & 
UmahukuMat["馬複人気順18番"]/UmahukuMat["馬複人気順19番"] > 0.73  & 
UmahukuMat["馬複人気順13番"]/UmahukuMat["馬複人気順14番"] > 0.77  & 
UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"] > 0.43  &
 UmahukuMat["馬複人気順21番"] < 105.00  &
 UmahukuMat["馬複人気順4番"]/UmahukuMat["馬複人気順5番"] > 0.59  &
 UmahukuMat["単勝人気順2番"]/UmahukuMat["単勝人気順3番"] > 0.41  &
 UmahukuMat["三連複人気順4番"]/UmahukuMat["三連複人気順5番"] > 0.73  &
 UmahukuMat["単勝人気順4番"] < 16.00  & 
UmahukuMat["馬複不人気順20番"] > 19.99  &
 UmahukuMat["馬複人気順2番"]/UmahukuMat["馬複人気順3番"] > 0.45  &
 UmahukuMat["単勝人気順3番"] > 3.99  & 
UmahukuMat["三連複人気順12番"]/UmahukuMat["三連複人気順13番"] > 0.77  & 
UmahukuMat["馬複人気順5番"]/UmahukuMat["馬複人気順6番"] > 0.63  &
 UmahukuMat["馬複人気順8番"]/UmahukuMat["馬複人気順9番"] > 0.69  )
BFKekkaNeraiStart <- 4
BFKekkaNeraiEnd   <- 10
checked <- kekkaCheckTansho2(TanshoNin4_10,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin4_10[,1]);length(checked [,1])/length(TanshoNin4_10[,1])
atehameMat <- TanshoNin4_10

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin2_4,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin2_4[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin2_4[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin2_4,checked_Shisu [[1]])



TanshoNin2_4<-subset(UmahukuMat, 
 UmahukuMat["馬複人気順1番"] < 9.60 &
 UmahukuMat["馬複人気順10番"] < 32.00  &
 UmahukuMat["馬複人気順27番"] < 720.00  &
 UmahukuMat["単勝人気順2番"] < 9.00  &
 UmahukuMat["単勝人気順3番"] < 14.00  &
 UmahukuMat["単勝人気順8番"] < 170.00  &
 UmahukuMat["単勝人気順11番"] < 210.00  &
 UmahukuMat["三連複人気順13番"] > 27.99  &
 UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"] > 0.37  &
 UmahukuMat["馬複人気順5番"]/UmahukuMat["馬複人気順6番"] < 1.00  & 
 UmahukuMat["馬複人気順14番"]/UmahukuMat["馬複人気順15番"] > 0.71  & 
 UmahukuMat["馬複人気順14番"]/UmahukuMat["馬複人気順15番"] < 1.00  & 
 UmahukuMat["馬複人気順17番"]/UmahukuMat["馬複人気順18番"] > 0.69  &
 UmahukuMat["馬複人気順19番"]/UmahukuMat["馬複人気順20番"] < 1.00  &
 UmahukuMat["単勝人気順4番"]/UmahukuMat["単勝人気順5番"] > 0.37  &
 UmahukuMat["単勝人気順5番"]/UmahukuMat["単勝人気順6番"] < 0.98  & 
 UmahukuMat["単勝人気順6番"]/UmahukuMat["単勝人気順7番"] < 0.96  &
 UmahukuMat["三連複人気順10番"]/UmahukuMat["三連複人気順11番"] < 1.00 )
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 4
checked <- kekkaCheckTansho2(TanshoNin2_4,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin2_4[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin2_4[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin2_4[,1]);length(checked [,1])/length(TanshoNin2_4[,1])
atehameMat <- TanshoNin2_4

#指数利用
blnShisu <- c(FALSE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin2_4,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin2_4[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin2_4[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin2_4,checked_Shisu [[1]])


