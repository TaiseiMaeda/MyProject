# 
sakuseiHiduke <- "2017/05/01"
TanshoNin3_7<-subset(UmahukuMat,UmahukuMat["30以下数"]> 4 &
			UmahukuMat["馬複人気順1番"]> 2.3  & 
			UmahukuMat["馬複人気順2番"]> 3.4 & 
			UmahukuMat["馬複人気順3番"]< 17  & 
			UmahukuMat["馬複人気順4番"]< 16 )
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
