
sakuseiHiduke <- "2017/05/01"
TanshoNin3_7<-subset(UmahukuMat,UmahukuMat["10以下数"] > 1.00 & 
UmahukuMat["馬複人気順1番"] > 2.39 &
	UmahukuMat["馬複人気順2番"] > 4.49 &
UmahukuMat["馬複人気順3番"] > 5.99 & 
	UmahukuMat["馬複人気順5番"] < 22 &
	UmahukuMat["馬複人気順6番"] < 24 &
	UmahukuMat["馬複人気順16番"] < 160 &
UmahukuMat["単勝人気順1番"] > 1.19 & 
	UmahukuMat["単勝人気順3番"] < 12 & 
	UmahukuMat["単勝人気順7番"] < 110 &
	UmahukuMat["単勝人気順8番"] < 210 &
	UmahukuMat["三連複人気順1番"] > 2.99 &
UmahukuMat["三連複人気順3番"] > 5.99 & 
	UmahukuMat["三連複人気順4番"] > 9.99 &
	UmahukuMat["三連複人気順9番"] > 17.99 &
	UmahukuMat["馬複不人気順5番"] > 200.00 & 
UmahukuMat["馬複不人気順1番"] < 6800 & 
UmahukuMat["馬複不人気順4番"]< 3800 & 
UmahukuMat["馬複不人気順20番"] < 860 & 
	UmahukuMat["馬複不人気順21番"] < 820 &
UmahukuMat["馬複不人気順27番"] < 580 & 
	UmahukuMat["馬複不人気順30番"] < 640 &
	(mat215[4] < 0.001 || mat215[4] > 1.049  ) &
	mat215[11] < 2.75 &
	mat215[9] < 2 &
	UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"] > 0.3199 &
	UmahukuMat["馬複人気順2番"]/UmahukuMat["馬複人気順3番"] > 0.3599 &
	UmahukuMat["馬複人気順3番"]/UmahukuMat["馬複人気順4番"] > 0.4199 &
	UmahukuMat["馬複人気順6番"]/UmahukuMat["馬複人気順7番"] > 0.6199 &
UmahukuMat["馬複人気順26番"]/UmahukuMat["馬複人気順27番"] > 0.6399 &
UmahukuMat["単勝人気順2番"]/UmahukuMat["単勝人気順3番"] < 0.97 &
UmahukuMat["単勝人気順8番"]/UmahukuMat["単勝人気順9番"] > 0.1599 &
UmahukuMat["単勝人気順10番"]/UmahukuMat["単勝人気順11番"] > 0.2999 &
UmahukuMat["三連複人気順5番"]/UmahukuMat["三連複人気順6番"] > 0.6399 &
UmahukuMat["三連複人気順7番"]/UmahukuMat["三連複人気順8番"] < 1 &
UmahukuMat["馬複人気順9番"]/UmahukuMat["馬複人気順10番"] > 0.7999 &
UmahukuMat["馬複人気順26番"]/UmahukuMat["馬複人気順27番"] < 1 )
BFKekkaNeraiStart <- 3
BFKekkaNeraiEnd   <- 7
checked <- kekkaCheckTansho(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin3_7[,1]);length(checked [,1])/length(TanshoNin3_7[,1])
atehameMat <- TanshoNin3_7

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin3_7[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin3_7,checked_Shisu [[1]])

