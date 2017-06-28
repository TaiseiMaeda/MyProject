#⑩
sakuseiHiduke <- "2017/03/13"
TanshoNin4_10<-subset(UmahukuMat,UmahukuMat["20以下数"]> 3 &
	UmahukuMat["30以下数"]> 4 & 
	#UmahukuMat["100以上数"]> 4&
	UmahukuMat["馬複人気順1番"]> 1.5 &
	UmahukuMat["馬複人気順2番"]> 3.4  &
	UmahukuMat["馬複人気順3番"]> 5.9 &
	UmahukuMat["馬複人気順6番"]> 7.9 &
	UmahukuMat["馬複人気順7番"]< 36 &
	UmahukuMat["馬複人気順8番"]< 42 &
	UmahukuMat["馬複人気順9番"]< 44 &
	UmahukuMat["馬複人気順10番"]< 52 &
	UmahukuMat["馬複人気順12番"]> 4 &
	UmahukuMat["馬複人気順16番"]> 19.9 &
	UmahukuMat["馬複人気順20番"]< 390 &
	UmahukuMat["馬複人気順22番"]< 380  &
	UmahukuMat["単勝人気順2番"]> 2.9 &
	UmahukuMat["単勝人気順3番"]> 3.9 &
	UmahukuMat["単勝人気順7番"]> 4.9 &
	UmahukuMat["単勝人気順8番"]< 200 &
	UmahukuMat["三連複人気順1番"]> 2.2 &
	UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"] > 0.2799 &
	UmahukuMat["馬複人気順6番"]/UmahukuMat["馬複人気順7番"] > 0.5799&
	UmahukuMat["馬複人気順8番"]/UmahukuMat["馬複人気順9番"] > 0.6999 &
	UmahukuMat["馬複人気順11番"]/UmahukuMat["馬複人気順12番"] < 1  &
	UmahukuMat["馬複人気順20番"]/UmahukuMat["馬複人気順21番"] > 0.6799 &
	UmahukuMat["馬複人気順26番"]/UmahukuMat["馬複人気順27番"]> 0.6399&
	UmahukuMat["単勝人気順1番"]/UmahukuMat["単勝人気順2番"] > 0.1199 &
	UmahukuMat["単勝人気順3番"]/UmahukuMat["単勝人気順4番"] < 1 &
	UmahukuMat["三連複人気順2番"]/UmahukuMat["三連複人気順3番"] > 0.4199 )
BFKekkaNeraiStart <- 4 
BFKekkaNeraiEnd   <- 10
checked <- kekkaCheckTansho(TanshoNin4_10,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin4_10[,1]);length(checked [,1])/length(TanshoNin4_10[,1])
atehameMat <- TanshoNin4_10

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin4_10,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu  )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin4_10[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin4_10,checked_Shisu [[1]])
