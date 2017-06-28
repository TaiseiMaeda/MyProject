#1 
sakuseiHiduke <- "2017/02/28"
BahukuNin14_29<-subset(UmahukuMat,UmahukuMat["10以下数"]> 1 &
	UmahukuMat["20以下数"]> 5 &
	UmahukuMat["30以下数"]> 6 &
	UmahukuMat["50以上数"]< 6 &
	UmahukuMat["馬複人気順1番"]> 3.9 &
	UmahukuMat["馬複人気順2番"]> 4.9 &
	UmahukuMat["馬複人気順3番"]> 6.9 &
	UmahukuMat["馬複人気順4番"]> 9.9 &
	UmahukuMat["馬複人気順5番"]< 22 &
	UmahukuMat["馬複人気順6番"]< 24 &
	UmahukuMat["馬複人気順8番"]< 28 &
	UmahukuMat["馬複人気順11番"]< 36 &
	UmahukuMat["馬複人気順13番"]< 48 &
	UmahukuMat["馬複人気順14番"]< 60 &
	UmahukuMat["馬複人気順15番"]< 60 &
	UmahukuMat["単勝人気順1番"]> 1.3 &
	UmahukuMat["単勝人気順2番"]> 3.5 &
	UmahukuMat["単勝人気順6番"]> 7.9 &
	UmahukuMat["単勝人気順6番"]< 48 &
	UmahukuMat["単勝人気順8番"]< 80 &
	UmahukuMat["単勝人気順9番"]< 120 &
	UmahukuMat["単勝人気順11番"]< 280 &
	UmahukuMat["馬複人気順16番"]> 19.9 &
	UmahukuMat["馬複人気順17番"]< 60 &
	UmahukuMat["馬複人気順18番"]< 80 &
	UmahukuMat["馬複人気順22番"]< 105 &
	UmahukuMat["馬複人気順29番"]> 59.9 &
	UmahukuMat["馬複人気順30番"]< 240 &
	UmahukuMat["三連複人気順1番"]< 19 &
	UmahukuMat["三連複人気順2番"]> 10.9 &
	UmahukuMat["三連複人気順3番"]> 12 &
	UmahukuMat["三連複人気順4番"]< 30 &
	UmahukuMat["三連複人気順6番"]> 15.9 &
	UmahukuMat["三連複人気順11番"]> 23.9 &
	UmahukuMat["三連複人気順15番"]< 66 &
	UmahukuMat["馬複不人気順7番"]> 200 &
	UmahukuMat["馬複不人気順6番"]< 5600 &
	UmahukuMat["馬複不人気順8番"]> 199.9 &
	UmahukuMat["馬複不人気順30番"]< 1200 )
BFKekkaNeraiStart <- 14
BFKekkaNeraiEnd   <- 29
checked <- kekkaCheck(BahukuNin14_29,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin14_29[,1]);length(checked [,1])/length(BahukuNin14_29[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin14_29

#指数利用
checked_Shisu <- kekkaCheckShisu(BahukuNin14_29,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin14_29[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin14_29,checked_Shisu [[1]])


#_馬複_________________________________________________________________________
# 2017/3/22作成
sakuseiHiduke <- "2017/03/22"
BahukuNin6_15<-subset(UmahukuMat,UmahukuMat["30以下数"]> 5 & 
			UmahukuMat["50以上数"]< 5 &
			UmahukuMat["70以上数"]> 0 &
			UmahukuMat["70以上数"]< 4 &
			UmahukuMat["馬複人気順1番"]> 1.5 &
			UmahukuMat["馬複人気順2番"]> 5.9 &
			UmahukuMat["馬複人気順2番"]< 14 &
			UmahukuMat["馬複人気順3番"]> 5.9 &
			UmahukuMat["馬複人気順4番"]> 6.9 &
			UmahukuMat["馬複人気順6番"]> 9.9 &
			UmahukuMat["馬複人気順6番"]< 22 &
			UmahukuMat["馬複人気順7番"]< 24 &
			UmahukuMat["馬複人気順8番"]< 44 &
			UmahukuMat["馬複人気順12番"]< 56 &
			UmahukuMat["馬複人気順14番"]< 70 &
			UmahukuMat["単勝人気順1番"]> 1.5 &
			UmahukuMat["単勝人気順2番"]> 2.4 &
			UmahukuMat["単勝人気順3番"]> 3.9 &
			UmahukuMat["単勝人気順3番"]< 13 &
			UmahukuMat["単勝人気順4番"]< 22 &
			UmahukuMat["単勝人気順6番"]< 48 &
			UmahukuMat["単勝人気順8番"]> 19.9 &
			UmahukuMat["単勝人気順13番"]< 200 &
			UmahukuMat["馬複人気順19番"]< 270 &
			UmahukuMat["馬複人気順26番"]> 59.9 &
			UmahukuMat["馬複人気順30番"]< 780 &	
			UmahukuMat["三連複人気順3番"]< 20 &	
			UmahukuMat["三連複人気順3番"]> 3.9 &
			UmahukuMat["馬複不人気順2番"]> 199.9 )
BFKekkaNeraiStart <- 6
BFKekkaNeraiEnd   <- 15
checked <- kekkaCheck(BahukuNin6_15,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin6_15[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin6_15[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin6_15[,1]);length(checked [,1])/length(BahukuNin6_15[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin6_15[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin6_15

#指数利用
checked_Shisu <- kekkaCheckShisu(BahukuNin6_15,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin6_15[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin6_15[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin6_15,checked_Shisu [[1]])


#_ワイド_______________________________________________________________________________
#⑫　17/2/27
sakuseiHiduke <- "2017/02/27"
BahukuNin12_19<-subset(UmahukuMat,UmahukuMat["10以下数"]> 1 &
			UmahukuMat["20以下数"]> 4 &
			UmahukuMat["30以下数"]> 5 &
			UmahukuMat["馬複人気順2番"]> 4.9 &
			UmahukuMat["馬複人気順3番"]> 6.9 &
			UmahukuMat["馬複人気順8番"]< 28 &
			UmahukuMat["単勝人気順2番"]> 2.9 &
			UmahukuMat["単勝人気順5番"]< 24 &
			UmahukuMat["単勝人気順7番"]< 40 &
			UmahukuMat["単勝人気順11番"]< 270 &
			UmahukuMat["馬複人気順18番"]< 80 &
			UmahukuMat["馬複人気順27番"]> 60 &	
			UmahukuMat["三連複人気順3番"]> 7.9 &	
			UmahukuMat["三連複人気順4番"]> 9.9 &	
			UmahukuMat["三連複人気順5番"]> 13.9 &	
			UmahukuMat["三連複人気順15番"]< 48 &
			UmahukuMat["馬複不人気順1番"]> 200 &
			UmahukuMat["馬複不人気順1番"]< 7600 &
			UmahukuMat["馬複不人気順4番"]> 400 &
			UmahukuMat["馬複不人気順8番"]> 200 )
BFKekkaNeraiStart <- 12
BFKekkaNeraiEnd   <- 19
checked1 <- kekkaCheckWide(BahukuNin12_19,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin12_19,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin12_19,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]));(length(BahukuNin12_19[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100);(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))/(length(BahukuNin12_19[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100)
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin12_19[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin12_19[,1])
atehameMat <- BahukuNin12_19

checked1shisu <- kekkaCheckShisu_Wide(BahukuNin12_19,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2shisu <- kekkaCheckShisu_Wide(BahukuNin12_19,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3shisu <- kekkaCheckShisu_Wide(BahukuNin12_19,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
sum(checked1shisu  [[2]]["ワイド1金額"]) +sum(checked2shisu  [[2]]["ワイド2金額"]) + sum(checked3shisu  [[2]]["ワイド3金額"]);(length(BahukuNin12_19[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - (sum(checked1shisu[[1]][4])* 100);(sum(checked1shisu  [[2]]["ワイド1金額"]) +sum(checked2shisu  [[2]]["ワイド2金額"]) + sum(checked3shisu  [[2]]["ワイド3金額"]))/((length(BahukuNin12_19[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - (sum(checked1shisu[[1]][4]))* 100);
atehameMatShisu <- cbind(BahukuNin12_19,checked1shisu  [[1]])

#_単勝_______________________________________________________________________________
#⑧ 17/2/18作成 
#きれいな相関
sakuseiHiduke <- "2017/02/28"
TanshoNin2_5<-subset(UmahukuMat,mat215[6] < 1 &
			mat215[5] < 1 &
			mat215[7] < 1 &
			UmahukuMat["10以下数"]> 2 &
			UmahukuMat["20以下数"]> 4 &
			UmahukuMat["馬複人気順1番"]> 2.3 &
			UmahukuMat["馬複人気順2番"]< 9 &
			UmahukuMat["馬複人気順2番"]> 3.4 &
			UmahukuMat["馬複人気順3番"]< 12 &
			UmahukuMat["馬複人気順9番"]< 32 &
			UmahukuMat["馬複人気順11番"]> 19.9 &
			UmahukuMat["馬複人気順11番"]< 40 &
			UmahukuMat["単勝人気順1番"]> 1.7 &
			UmahukuMat["単勝人気順11番"]< 360 &
			UmahukuMat["馬複人気順16番"]< 180 &
			UmahukuMat["三連複人気順8番"]> 15.9 &
			UmahukuMat["三連複人気順11番"]< 60 &
			UmahukuMat["三連複人気順14番"]< 66 )
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 4
checked <- kekkaCheckTansho(TanshoNin2_5,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin2_5[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin2_5[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin2_5[,1]);length(checked [,1])/length(TanshoNin2_5[,1])
atehameMat <- TanshoNin2_5

#指数利用
checked_Shisu <- kekkaCheckShisu_tansho(TanshoNin2_5,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin2_5[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin2_5[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin2_5,checked_Shisu [[1]])

#_単勝_______________________________________________________________________________
# 17/3/23作成 
sakuseiHiduke <- "2017/03/23"
TanshoNin3_7<-subset(UmahukuMat,mat215[4] < 1 &
			mat215[6] < 1 &
			mat215[5] < 1 &
			mat215[7] < 1 &
			mat215[20] < 11 & 
			UmahukuMat["10以下数"]> 1 & 
			UmahukuMat["20以下数"]> 2 & 
			UmahukuMat["30以下数"]> 4 & 
			UmahukuMat["50以上数"]< 6 &
			UmahukuMat["馬複人気順1番"]> 2.3 & 
			UmahukuMat["馬複人気順2番"]> 3.4 & 
			UmahukuMat["馬複人気順3番"]< 17 & 
			UmahukuMat["馬複人気順7番"]< 28 & 
			UmahukuMat["馬複人気順8番"]< 34 & 
			UmahukuMat["馬複人気順9番"]< 56 & 
			UmahukuMat["馬複人気順11番"]< 650 & 
			UmahukuMat["馬複人気順12番"]< 800 &
			UmahukuMat["単勝人気順1番"]> 1.1 &
			UmahukuMat["単勝人気順5番"]< 24 &
			UmahukuMat["馬複人気順18番"]< 280 & 
			UmahukuMat["三連複人気順2番"]> 3.9 & 
			UmahukuMat["三連複人気順4番"]> 7.9 & 
			UmahukuMat["三連複人気順9番"]> 17.9 &
			UmahukuMat["馬複不人気順25番"]< 800 &
			UmahukuMat["馬複不人気順29番"]< 680 )
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

TanshoNin3_7<-subset(TanshoNin3_7,TanshoNin3_7[,45]=='雨')

#単勝__________________________________________________________
#⑩　sakuseiHiduke <- "2017/03/13"
TanshoNin4_10<-subset(UmahukuMat,mat215[13] < 1 &
	mat215[14] < 1 &
	mat215[15] == 0 &
			UmahukuMat["10以下数"]> 2 &
			UmahukuMat["20以下数"]> 5 &
			UmahukuMat["30以下数"]> 6 & #ここまでで82%
			UmahukuMat["馬複人気順3番"]> 5.9 & #ここまでで83.6%
			UmahukuMat["馬複人気順4番"]> 6.9 &
			UmahukuMat["馬複人気順8番"]< 32 &
			UmahukuMat["馬複人気順10番"]< 36 &
			UmahukuMat["馬複人気順11番"]< 40 & #ここまでで84.6%
			UmahukuMat["馬複人気順15番"]< 80 &
			UmahukuMat["単勝人気順3番"]> 3.9 &
			UmahukuMat["単勝人気順11番"]< 900 & #ここまでで86%
			UmahukuMat["馬複人気順16番"]< 60 &
			UmahukuMat["馬複人気順19番"]> 29.9 &
			UmahukuMat["馬複人気順22番"]< 100 & #ここまでで87.3%
			UmahukuMat["馬複人気順24番"]> 49.9 &
			UmahukuMat["馬複人気順24番"]< 110 &
			UmahukuMat["馬複人気順25番"]< 150 &
			UmahukuMat["馬複人気順26番"]> 59.9 & #ここまでで92%
			UmahukuMat["三連複人気順1番"]> 4.9 & #ここまでで93.3%
			UmahukuMat["三連複人気順2番"]> 6.9 &
			UmahukuMat["馬複不人気順1番"]> 399.9 &
			UmahukuMat["馬複不人気順3番"]< 3800 &
			UmahukuMat["馬複不人気順28番"]> 19.9 )
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

TanshoNin4_10<-subset(TanshoNin4_10,TanshoNin4_10[,45]=='雨')
#_ワイド_______________________________________________________________________________
#⑰ 17/1/15作成
sakuseiHiduke <- "2017/04/05"
BahukuNin1_3<-subset(UmahukuMat,
			#mat215[16] < 1 &
			#mat215[17] < 1 &

			#mat215[6] < 1 &
			#mat215[7] < 1 &

			UmahukuMat["10以下数"]< 5 &
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


#_馬複_____________________________________________________________
#⑤ 17/4/09作成
sakuseiHiduke <- "2017/04/09"
BahukuNin2_6<-subset(UmahukuMat,UmahukuMat["天候"]== "曇" &
			mat215[17]< 1 &
			UmahukuMat["20以下数"]> 3 &
			UmahukuMat["30以下数"]> 4 &
			UmahukuMat["馬複人気順1番"]> 1.2 &
			UmahukuMat["馬複人気順2番"]< 13 &
			UmahukuMat["馬複人気順3番"]< 16 &
			UmahukuMat["馬複人気順5番"]< 18 &
			UmahukuMat["馬複人気順7番"]> 15.9 &
			UmahukuMat["馬複人気順7番"]< 50 &
			UmahukuMat["馬複人気順8番"]> 13.9 &
			UmahukuMat["馬複人気順9番"]< 64 &
			UmahukuMat["馬複人気順10番"]< 68 &
			UmahukuMat["馬複人気順11番"]> 23.9 &
			UmahukuMat["馬複人気順11番"]< 88 & 
			UmahukuMat["単勝人気順12番"]< 200 & 
			UmahukuMat["三連複人気順8番"]< 40 & 
			UmahukuMat["三連複人気順10番"]> 19.9 & 
			UmahukuMat["三連複人気順15番"]> 29.9 )
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 6
checked <- kekkaCheck(BahukuNin2_6,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin2_6[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin2_6[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin2_6[,1]);length(checked [,1])/length(BahukuNin2_6[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin2_6[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin2_6

#指数利用
checked_Shisu <- kekkaCheckShisu(BahukuNin2_6,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin2_6[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin2_6[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin2_6,checked_Shisu [[1]])


