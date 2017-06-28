#_馬複_____________________________________________________________
#(19) 17/2/27作成 @@@@@
sakuseiHiduke <- "2017/02/27"
BahukuNin6_11<-subset(UmahukuMat,UmahukuMat["単勝人気順4番"]< 6.2 &
			UmahukuMat["馬複人気順7番"]> 14 &
			UmahukuMat["馬複人気順7番"]< 16.3 )
BFKekkaNeraiStart <- 6
BFKekkaNeraiEnd   <- 11
checked <- kekkaCheck(BahukuNin6_11,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin6_11[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin6_11[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin6_11[,1]);length(checked [,1])/length(BahukuNin6_11[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin6_11[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin6_11

#指数利用
checked_Shisu <- kekkaCheckShisu(BahukuNin6_11,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin6_11[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin6_11[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin6_11,checked_Shisu [[1]])


#_単勝_______________________________________________________________________________
#20)　17/2/27作成 
sakuseiHiduke <- "2017/02/27"
TanshoNin3_7<-subset(UmahukuMat,UmahukuMat["30以下数"]> 6 &
			UmahukuMat["30以下数"]< 8 &
			UmahukuMat["馬複人気順1番"]> 2.8 &
			UmahukuMat["馬複人気順1番"]< 4  & 
			UmahukuMat["三連複人気順13番"]> 27.9 & 
			UmahukuMat["三連複人気順13番"]< 36 )
BFKekkaNeraiStart <- 3
BFKekkaNeraiEnd   <- 7
checked <- kekkaCheckTansho(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin3_7[,1]);length(checked [,1])/length(TanshoNin3_7[,1])
atehameMat <- TanshoNin3_7

#指数利用
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin3_7[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin3_7[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin3_7,checked_Shisu [[1]])

#_ワイド_______________________________________________________________________________
#⑬2017/1/23作成
sakuseiHiduke <- "2017/02/27"
BahukuNin6_10<-subset(UmahukuMat,
			UmahukuMat["馬複人気順6番"]> 13.9 &
			UmahukuMat["馬複人気順6番"]< 16 &
			UmahukuMat["単勝人気順9番"]> 74.9 &
			UmahukuMat["単勝人気順9番"]< 120 &
			UmahukuMat["馬複不人気順22番"]> 39.9 &
			UmahukuMat["馬複不人気順22番"]< 80 )
BFKekkaNeraiStart <- 6
BFKekkaNeraiEnd   <- 10
checked1 <- kekkaCheckWide(BahukuNin6_10,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin6_10,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin6_10,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]));(length(BahukuNin6_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100);(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))/(length(BahukuNin6_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100)
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin6_10[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin6_10[,1])
atehameMat <- BahukuNin6_10

checked1shisu <- kekkaCheckShisu_Wide(BahukuNin6_10,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2shisu <- kekkaCheckShisu_Wide(BahukuNin6_10,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3shisu <- kekkaCheckShisu_Wide(BahukuNin6_10,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
sum(checked1shisu  [[2]]["ワイド1金額"]) +sum(checked2shisu  [[2]]["ワイド2金額"]) + sum(checked3shisu  [[2]]["ワイド3金額"]);(length(BahukuNin6_10[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - (sum(checked1shisu[[1]][4])* 100);(sum(checked1shisu  [[2]]["ワイド1金額"]) +sum(checked2shisu  [[2]]["ワイド2金額"]) + sum(checked3shisu  [[2]]["ワイド3金額"]))/((length(BahukuNin6_10[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - (sum(checked1shisu[[1]][4]))* 100);
atehameMatShisu <- cbind(BahukuNin6_10,checked1shisu  [[1]])

#_単勝_______________________________________________________________________________
#24)　3/7
TanshoNin4_10<-subset(UmahukuMat,UmahukuMat["10以下数"]> 2 &
			UmahukuMat["20以下数"]> 5 &
			UmahukuMat["30以下数"]> 6 & #ここまでで82%
			UmahukuMat["馬複人気順3番"]> 5.9 & #ここまでで83.6%
			UmahukuMat["馬複人気順4番"]> 6.9 &
			UmahukuMat["馬複人気順8番"]< 32 &
			UmahukuMat["馬複人気順10番"]< 36 &
			UmahukuMat["馬複人気順11番"]< 40 & #ここまでで84.6%
			UmahukuMat["馬複人気順15番"]< 80 &
			UmahukuMat["単勝人気順3番"]> 3.9 & #ここまでで86%
			UmahukuMat["馬複人気順16番"]< 60 &
			UmahukuMat["馬複人気順19番"]> 29.9 &
			UmahukuMat["馬複人気順22番"]< 100 & #ここまでで87.3%
			UmahukuMat["馬複人気順24番"]< 110 &
			UmahukuMat["馬複人気順25番"]< 150 &
			UmahukuMat["馬複人気順26番"]> 59.9 & #ここまでで92%
			UmahukuMat["三連複人気順1番"]> 4.9 & #ここまでで93.3%
			UmahukuMat["三連複人気順2番"]> 6.9 &
			UmahukuMat["馬複不人気順1番"]> 399.9 &
			UmahukuMat["馬複不人気順3番"]< 3800 &
			UmahukuMat["馬複不人気順28番"]> 19.9 &
			UmahukuMat["馬複人気順1番"]> 3.1 &
			UmahukuMat["馬複人気順24番"]> 49.9 & 
			UmahukuMat["三連複人気順1番"]< 7 )
BFKekkaNeraiStart <- 4 
BFKekkaNeraiEnd   <- 10
checked <- kekkaCheckTansho(TanshoNin4_10,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin4_10[,1]);length(checked [,1])/length(TanshoNin4_10[,1])
atehameMat <- TanshoNin4_10

#指数利用
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin4_10,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin4_10[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin4_10,checked_Shisu [[1]])

