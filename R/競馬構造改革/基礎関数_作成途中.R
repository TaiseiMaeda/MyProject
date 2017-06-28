#_馬複_____________________________________________________________
#
sakuseiHiduke <- "2017/03/28"
BahukuNin3_7<-subset(UmahukuMat,mat215[20] < 8 &
		UmahukuMat["10以下数"]> 2 &
		UmahukuMat["10以下数"]< 6 &
		UmahukuMat["20以下数"]< 10 & 
		UmahukuMat["馬複人気順1番"]> 1.8 & 
		UmahukuMat["馬複人気順2番"]< 13.5 & 
		UmahukuMat["馬複人気順5番"]< 25 & 
		UmahukuMat["単勝人気順1番"]> 1.1 & 
		UmahukuMat["馬複人気順23番"]< 560 & 
		UmahukuMat["馬複不人気順1番"]> 400  )
BFKekkaNeraiStart <- 3
BFKekkaNeraiEnd   <- 7
checked <- kekkaCheck(BahukuNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin3_7[,1]);length(checked [,1])/length(BahukuNin3_7[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin3_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin3_7

#指数利用
checked_Shisu <- kekkaCheckShisu(BahukuNin3_7,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin3_7[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin3_7[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin3_7,checked_Shisu [[1]])


#_単勝_______________________________________________________________________________
#⑦ 2017/3/28
sakuseiHiduke <- "2017/03/28"
TanshoNin2_5<-subset(UmahukuMat,UmahukuMat["20以下数"] > 3 &
UmahukuMat["30以下数"] > 4 & 
		UmahukuMat["馬複人気順1番"]> 1.5& 
		UmahukuMat["馬複人気順10番"]< 56 & 
		UmahukuMat["単勝人気順11番"]< 900&
UmahukuMat["三連複人気順13番"]< 72)
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 5
checked <- kekkaCheckTansho(TanshoNin2_5,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin2_5[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin2_5[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin2_5[,1]);length(checked [,1])/length(TanshoNin2_5[,1])
atehameMat <- TanshoNin2_5

#指数利用
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin2_5,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin2_5[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin2_5[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin2_5,checked_Shisu [[1]])

#_ワイド_______________________________________________________________________________
#
sakuseiHiduke <- "2017/03/31"
BahukuNin20_31<-subset(UmahukuMat,UmahukuMat["20以下数"]> 4 &
		UmahukuMat["30以下数"]> 6 &
		UmahukuMat["馬複人気順2番"]> 4.9 &
		UmahukuMat["馬複人気順5番"]> 9.9 &
		UmahukuMat["馬複人気順7番"]< 26 &
		UmahukuMat["馬複人気順9番"]< 36 &
		UmahukuMat["馬複人気順10番"]< 38 &
		UmahukuMat["馬複人気順25番"]< 200 &
		UmahukuMat["馬複人気順29番"]< 240  & 
		UmahukuMat["三連複人気順1番"]> 3.9 & 
		UmahukuMat["三連複人気順2番"]> 7.9 & 
		UmahukuMat["三連複人気順8番"]> 17.9 & 
		UmahukuMat["三連複人気順11番"]> 23.9 )
BFKekkaNeraiStart <- 20
BFKekkaNeraiEnd   <- 31
checked1 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]));(length(BahukuNin20_31[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100);(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))/(length(BahukuNin20_31[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100)
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin20_31[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin20_31[,1])
atehameMat <- BahukuNin20_31

checked1shisu <- kekkaCheckShisu_Wide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2shisu <- kekkaCheckShisu_Wide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3shisu <- kekkaCheckShisu_Wide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
sum(checked1shisu  [[2]]["ワイド1金額"]) +sum(checked2shisu  [[2]]["ワイド2金額"]) + sum(checked3shisu  [[2]]["ワイド3金額"]);(length(BahukuNin20_31[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - (sum(checked1shisu[[1]][4])* 100);(sum(checked1shisu  [[2]]["ワイド1金額"]) +sum(checked2shisu  [[2]]["ワイド2金額"]) + sum(checked3shisu  [[2]]["ワイド3金額"]))/((length(BahukuNin20_31[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - (sum(checked1shisu[[1]][4]))* 100);
atehameMatShisu <- cbind(BahukuNin20_31,checked1shisu  [[1]])



#_ワイド_______________________________________________________________________________
#⑰ 17/1/15作成
sakuseiHiduke <- "2017/04/05"
BahukuNin1_3<-subset(UmahukuMat,UmahukuMat["10以下数"]< 5 &
			UmahukuMat["10以下数"]> 1 &
			UmahukuMat["20以下数"]> 2 &
			UmahukuMat["20以下数"]< 9 &
			UmahukuMat["30以下数"]< 10 &
			UmahukuMat["馬複人気順1番"]< 9.6 &
			UmahukuMat["馬複人気順2番"]< 9 &
			UmahukuMat["馬複人気順3番"]< 12 & 
			UmahukuMat["馬複人気順4番"]< 18 & 
			UmahukuMat["馬複人気順7番"]> 13.9 & 
			UmahukuMat["馬複人気順8番"]> 15.9 & 
			UmahukuMat["馬複人気順11番"]> 19.9 & 
			UmahukuMat["単勝人気順6番"]> 11.9 & 
			UmahukuMat["単勝人気順12番"]< 600 & 
			UmahukuMat["三連複人気順1番"]< 18 & 
			UmahukuMat["三連複人気順3番"]< 26 & 
			UmahukuMat["三連複人気順5番"]< 30 & 
			UmahukuMat["三連複人気順7番"]> 11.9 & 
			UmahukuMat["三連複人気順10番"]> 19.9 &
			UmahukuMat["三連複人気順13番"]> 27.9 & 
			UmahukuMat["馬複人気順28番"]< 1080 &
			UmahukuMat["馬複不人気順10番"]< 3000)
BFKekkaNeraiStart <- 1
BFKekkaNeraiEnd   <- 3
checked1 <- kekkaCheckWide(BahukuNin1_3,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin1_3,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin1_3,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]));(length(BahukuNin1_3[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100);(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))/(length(BahukuNin1_3[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100)
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin1_3[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin1_3[,1])
atehameMat <- BahukuNin1_3

checked1shisu <- kekkaCheckShisu_Wide(BahukuNin1_3,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2shisu <- kekkaCheckShisu_Wide(BahukuNin1_3,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3shisu <- kekkaCheckShisu_Wide(BahukuNin1_3,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
sum(checked1shisu  [[2]]["ワイド1金額"]) +sum(checked2shisu  [[2]]["ワイド2金額"]) + sum(checked3shisu  [[2]]["ワイド3金額"]);(length(BahukuNin1_3[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - (sum(checked1shisu[[1]][4])* 100);(sum(checked1shisu  [[2]]["ワイド1金額"]) +sum(checked2shisu  [[2]]["ワイド2金額"]) + sum(checked3shisu  [[2]]["ワイド3金額"]))/((length(BahukuNin1_3[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - (sum(checked1shisu[[1]][4]))* 100);
atehameMatShisu <- cbind(BahukuNin1_3,checked1shisu  [[1]])


