#_単勝_______________________________________________________________________________
sakuseiHiduke <- "2017/05/07"
TanshoNin2_4<-subset(UmahukuMat,UmahukuMat["10以下数"]> 2 & 
	UmahukuMat["馬複人気順1番"]> 1.5 &
	UmahukuMat["馬複人気順4番"]< 14 &
	UmahukuMat["馬複人気順7番"]< 38 & 
	UmahukuMat["馬複人気順9番"]< 44 & 
	UmahukuMat["三連複人気順9番"]< 50 &
	UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"]> 0.2399 &
	UmahukuMat["馬複人気順17番"]/UmahukuMat["馬複人気順18番"]> 0.6999 &
	UmahukuMat["馬複人気順22番"]/UmahukuMat["馬複人気順23番"]> 0.6799 &
	UmahukuMat["馬複人気順27番"]/UmahukuMat["馬複人気順28番"]> 0.6799 &
	UmahukuMat["馬複人気順27番"]/UmahukuMat["馬複人気順28番"]< 1 &
	UmahukuMat["馬複人気順28番"]/UmahukuMat["馬複人気順29番"]> 0.7999  &
	UmahukuMat["単勝人気順4番"]/UmahukuMat["単勝人気順5番"]> 0.1799 &
	UmahukuMat["三連複人気順3番"]/UmahukuMat["三連複人気順4番"]> 0.3199 &
	UmahukuMat["三連複人気順5番"]/UmahukuMat["三連複人気順6番"]< 1 &
	UmahukuMat["三連複人気順9番"]/UmahukuMat["三連複人気順10番"]> 0.6199 &
	UmahukuMat["三連複人気順10番"]/UmahukuMat["三連複人気順11番"]< 1 )
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 4
checked <- kekkaCheckTansho2(TanshoNin2_4,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin2_4[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin2_4[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin2_4[,1]);length(checked [,1])/length(TanshoNin2_4[,1])
atehameMat <- TanshoNin2_4

#指数利用
blnShisu <- c(FALSE,FALSE,FALSE,TRUE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho2(TanshoNin2_4,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu   )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin2_4[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin2_4[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin2_4,checked_Shisu [[1]])

#_単勝_______________________________________________________________________________
# 
sakuseiHiduke <- "2017/05/01"
TanshoNin3_7<-subset(UmahukuMat,mat215[4] == 0 &
			mat215[9] < 2.5 &
			mat215[11] < 4 &
			mat215[13] < 1 &
			mat215[14] == 0 &
			mat215[20] < 11 & 
			(mat215[5] < 0.001 || mat215[5] > 0.5999 )&
			(mat215[7] < 0.001 || mat215[7] > 0.2999 )&
			UmahukuMat["馬複人気順3番"]/UmahukuMat["馬複人気順4番"] > 0.4199 &
			UmahukuMat["馬複人気順6番"]/UmahukuMat["馬複人気順7番"] > 0.5199 &
			UmahukuMat["馬複人気順16番"]/UmahukuMat["馬複人気順17番"] > 0.5799 &
			UmahukuMat["馬複人気順26番"]/UmahukuMat["馬複人気順27番"] > 0.6799 &
			UmahukuMat["10以下数"]> 1 & 
			UmahukuMat["20以下数"]> 2 & 
			UmahukuMat["30以下数"]> 4 & 
			UmahukuMat["30以下数"]< 10 & #要修正
			UmahukuMat["50以上数"]< 6 &
			UmahukuMat["馬複人気順1番"]> 2.3 & 
			UmahukuMat["馬複人気順2番"]> 3.4 & 
			UmahukuMat["馬複人気順2番"]< 10.5 & 
			UmahukuMat["馬複人気順4番"]< 16 & 
			UmahukuMat["馬複人気順3番"]< 17 & 
			UmahukuMat["馬複人気順6番"]< 24 & 
			UmahukuMat["馬複人気順7番"]< 28 & 
			UmahukuMat["馬複人気順8番"]< 34 & 
			UmahukuMat["馬複人気順9番"]< 56 & 
			UmahukuMat["馬複人気順11番"]< 650 & 
			UmahukuMat["馬複人気順12番"]< 96 &
			UmahukuMat["馬複人気順14番"]> 30 & #要修正
			UmahukuMat["単勝人気順1番"]> 1.1 &
			UmahukuMat["単勝人気順3番"]> 3.9 &
			UmahukuMat["単勝人気順5番"]< 24 &
			UmahukuMat["単勝人気順11番"]< 900 &
			UmahukuMat["馬複人気順18番"]< 280 & 
			UmahukuMat["三連複人気順2番"]> 3.9 & 
			UmahukuMat["三連複人気順4番"]> 7.9 & 
			UmahukuMat["三連複人気順9番"]> 17.9 &
			UmahukuMat["馬複不人気順25番"]< 800 &
			UmahukuMat["馬複不人気順29番"]< 680 )
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

#単勝__________________________________________________________
#⑩
sakuseiHiduke <- "2017/03/13"
TanshoNin4_10<-subset(UmahukuMat,mat215[13] < 1 &
			mat215[9] < 2.5 &
			mat215[10] < 3.25 &
			mat215[13] < 2.25 &
			mat215[14] < 1 &
			mat215[20] < 13 &
			UmahukuMat["馬複人気順5番"]/UmahukuMat["馬複人気順6番"] > 0.5599 &
			UmahukuMat["馬複人気順6番"]/UmahukuMat["馬複人気順7番"] > 0.6399 &
			UmahukuMat["馬複人気順11番"]/UmahukuMat["馬複人気順12番"] < 1 &
			UmahukuMat["馬複人気順19番"]/UmahukuMat["馬複人気順20番"] > 0.7599 &
			UmahukuMat["単勝人気順1番"]/UmahukuMat["単勝人気順2番"] > 0.1199 &
			UmahukuMat["単勝人気順3番"]/UmahukuMat["単勝人気順4番"] < 1 &
			UmahukuMat["10以下数"]> 2 &
			UmahukuMat["20以下数"]> 5 &
			UmahukuMat["30以下数"]> 6 & #ここまでで82%
			UmahukuMat["馬複人気順1番"]> 2.7 &
			UmahukuMat["馬複人気順8番"]< 32 &
			UmahukuMat["馬複人気順10番"]< 36 &
			UmahukuMat["馬複人気順11番"]< 40 & #ここまでで84.6%
			UmahukuMat["馬複人気順15番"]< 80 &
			UmahukuMat["単勝人気順3番"]> 3.9 &
			UmahukuMat["単勝人気順8番"]< 50 &
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
checked <- kekkaCheckTansho2(TanshoNin4_10,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["単勝金額"]);length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100;sum(checked ["単勝金額"])/(length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(TanshoNin4_10[,1]);length(checked [,1])/length(TanshoNin4_10[,1])
atehameMat <- TanshoNin4_10

#指数利用
blnShisu <- c(FALSE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu_tansho2(TanshoNin4_10,BFKekkaNeraiStart,BFKekkaNeraiEnd ,blnShisu  )
sum(checked_Shisu [[2]]["単勝金額"]);(length(TanshoNin4_10[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["単勝金額"])/(length(TanshoNin4_10[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(TanshoNin4_10,checked_Shisu [[1]])

#---------------------------------------------------------------------------------
#⑤ 
sakuseiHiduke <- "2017/05/03"
BahukuNin2_6<-subset(UmahukuMat,mat215[4]< 1 &
			mat215[17]< 1 &
			UmahukuMat["20以下数"]> 3 &
			UmahukuMat["30以下数"]> 4 &
			UmahukuMat["100以上数"]> 3 &
			UmahukuMat["馬複人気順5番"]< 18 &
			UmahukuMat["馬複人気順5番"]> 9.9 & #微妙
			UmahukuMat["馬複人気順10番"]< 68 &
			UmahukuMat["単勝人気順4番"]> 6.9 &
			UmahukuMat["三連複人気順15番"]> 29.9 &
			UmahukuMat["馬複不人気順7番"]> 1200 ) #要修正
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 6
checked <- kekkaCheck2(BahukuNin2_6,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin2_6[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin2_6[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin2_6[,1]);length(checked [,1])/length(BahukuNin2_6[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin2_6[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin2_6

#指数利用
blnShisu <- c(TRUE,FALSE,FALSE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu2(BahukuNin2_6,BFKekkaNeraiStart,BFKekkaNeraiEnd  ,blnShisu )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin2_6[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin2_6[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin2_6,checked_Shisu [[1]])


#---------------------------------------------------------------------------------
#⑥ 17/4/21作成
sakuseiHiduke <- "2017/05/17"
BahukuNin0_3<-subset(UmahukuMat,UmahukuMat["10以下数"]< 5 &
	UmahukuMat["30以下数"]< 9 &
	UmahukuMat["馬複人気順1番"]< 6 &
	UmahukuMat["馬複人気順2番"]< 12 &
	UmahukuMat["馬複人気順3番"]< 13 &
	UmahukuMat["馬複人気順6番"]> 11.9 &
	UmahukuMat["馬複人気順8番"]> 13.9 &
	UmahukuMat["馬複人気順9番"]> 15.9 &
	UmahukuMat["馬複人気順9番"]< 56 &
	UmahukuMat["馬複人気順11番"]> 27.9 &
	UmahukuMat["馬複人気順15番"]> 35.9  &
	UmahukuMat["単勝人気順2番"]> 1.9  &
	UmahukuMat["単勝人気順5番"]< 72   &
	UmahukuMat["単勝人気順13番"]< 280  &
	UmahukuMat["馬複人気順24番"]> 99.9  &
	UmahukuMat["馬複人気順24番"]< 350 &
	UmahukuMat["馬複人気順29番"]< 840 &
	UmahukuMat["三連複人気順2番"]< 15 &
	UmahukuMat["三連複人気順3番"]< 16&
	UmahukuMat["三連複人気順7番"]> 15.9&
	UmahukuMat["三連複人気順10番"]> 19.9 &
	UmahukuMat["三連複人気順14番"]< 90 &
	UmahukuMat["馬複不人気順1番"]> 199.9 &
	UmahukuMat["馬複不人気順1番"]< 9600 &
	UmahukuMat["馬複不人気順3番"]> 500 &
	UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"] > 0.1599 &
	UmahukuMat["馬複人気順3番"]/UmahukuMat["馬複人気順4番"] < 1 &
	UmahukuMat["馬複人気順7番"]/UmahukuMat["馬複人気順8番"] > 0.5199 &
	UmahukuMat["馬複人気順8番"]/UmahukuMat["馬複人気順9番"] > 0.5199 &
	UmahukuMat["馬複人気順10番"]/UmahukuMat["馬複人気順11番"] < 0.98 &
	UmahukuMat["馬複人気順10番"]/UmahukuMat["馬複人気順11番"] > 0.6399 &
	UmahukuMat["馬複人気順15番"]/UmahukuMat["馬複人気順16番"] > 0.7199 &
	UmahukuMat["馬複人気順15番"]/UmahukuMat["馬複人気順16番"] < 1 &
	UmahukuMat["馬複人気順16番"]/UmahukuMat["馬複人気順17番"] > 0.5799 &
	UmahukuMat["馬複人気順25番"]/UmahukuMat["馬複人気順26番"] > 0.6799 &
	UmahukuMat["馬複人気順29番"]/UmahukuMat["馬複人気順30番"] > 0.6999 &
	UmahukuMat["単勝人気順1番"]/UmahukuMat["単勝人気順2番"] < 1 &
	UmahukuMat["単勝人気順4番"]/UmahukuMat["単勝人気順5番"] > 0.2399 &
	UmahukuMat["三連複人気順4番"]/UmahukuMat["三連複人気順5番"] < 1  &
	UmahukuMat["三連複人気順6番"]/UmahukuMat["三連複人気順7番"] < 1  &
	UmahukuMat["三連複人気順13番"]/UmahukuMat["三連複人気順14番"] > 0.6999 &
	UmahukuMat["三連複人気順14番"]/UmahukuMat["三連複人気順15番"] < 1 &
	(mat215[6] < 0.001 | mat215[6] > 0.25 )&
	(mat215[10] < 0.001 | mat215[10] > 0.2 )&
	mat215[10] < 2.5 &
	mat215[11] < 2.75 &
	(mat215[12] < 0.1 | mat215[12] > 0.2499 )&
	mat215[13] < 2.5  &
	(mat215[14] < 0.001 | mat215[14] > 0.55 ) &
	mat215[15] < 1.6)
BFKekkaNeraiStart <- 0
BFKekkaNeraiEnd   <- 3
checked <- kekkaCheck(BahukuNin0_3,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin0_3[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin0_3[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin0_3[,1]);length(checked [,1])/length(BahukuNin0_3[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin0_3[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin0_3

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu2(BahukuNin0_3,BFKekkaNeraiStart,BFKekkaNeraiEnd   ,blnShisu )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin0_3[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin0_3[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin0_3,checked_Shisu [[1]])
#****************************************************************************************

#_馬複_______________________________________________________________________________
#23)
sakuseiHiduke <- "2017/05/14"
BahukuNin14_29<-subset(UmahukuMat,(mat215[20] < 2 | mat215[20] > 2 )&
	(mat215[6] < 0.001 | mat215[6] > 0.35 )&
	(mat215[7] < 0.1 | mat215[7] > 0.4 )&
	mat215[7] < 2.5 &
	mat215[9] < 2.25 &
	mat215[13] < 2.25 &
	mat215[14] < 2.25 &
	mat215[15] < 1 &
	UmahukuMat["馬複人気順1番"]/UmahukuMat["馬複人気順2番"] < 1 &
	UmahukuMat["馬複人気順4番"]/UmahukuMat["馬複人気順5番"] > 0.5199 &
	UmahukuMat["馬複人気順5番"]/UmahukuMat["馬複人気順6番"] > 0.5599 &
	UmahukuMat["馬複人気順6番"]/UmahukuMat["馬複人気順7番"] > 0.5599 &
	UmahukuMat["馬複人気順7番"]/UmahukuMat["馬複人気順8番"] > 0.5799 &
	UmahukuMat["馬複人気順9番"]/UmahukuMat["馬複人気順10番"] > 0.6599 &
	UmahukuMat["馬複人気順10番"]/UmahukuMat["馬複人気順11番"] > 0.6599 &
	UmahukuMat["馬複人気順12番"]/UmahukuMat["馬複人気順13番"] < 1 &
	UmahukuMat["馬複人気順13番"]/UmahukuMat["馬複人気順14番"] < 1 &
	UmahukuMat["馬複人気順13番"]/UmahukuMat["馬複人気順14番"] > 0.7599 &
	UmahukuMat["馬複人気順14番"]/UmahukuMat["馬複人気順15番"] > 0.7399 &
	UmahukuMat["馬複人気順17番"]/UmahukuMat["馬複人気順18番"] < 1 &
	UmahukuMat["馬複人気順17番"]/UmahukuMat["馬複人気順18番"] > 0.6599 &
	UmahukuMat["馬複人気順19番"]/UmahukuMat["馬複人気順20番"] > 0.7599 &
	UmahukuMat["馬複人気順20番"]/UmahukuMat["馬複人気順21番"] > 0.6599 &
	UmahukuMat["馬複人気順24番"]/UmahukuMat["馬複人気順25番"] < 1 &
	UmahukuMat["馬複人気順27番"]/UmahukuMat["馬複人気順28番"] > 0.6599 &
	UmahukuMat["馬複人気順27番"]/UmahukuMat["馬複人気順28番"] < 1 &
	UmahukuMat["馬複人気順29番"]/UmahukuMat["馬複人気順30番"] > 0.8399 &
	UmahukuMat["単勝人気順1番"]/UmahukuMat["単勝人気順2番"] > 0.1799 &
	UmahukuMat["単勝人気順7番"]/UmahukuMat["単勝人気順8番"] < 1 &
	UmahukuMat["単勝人気順8番"]/UmahukuMat["単勝人気順9番"] > 0.2199 &
	UmahukuMat["三連複人気順1番"]/UmahukuMat["三連複人気順2番"] > 0.3399 &
	UmahukuMat["三連複人気順3番"]/UmahukuMat["三連複人気順4番"] > 0.6599 &
	UmahukuMat["三連複人気順10番"]/UmahukuMat["三連複人気順11番"] < 1&
	UmahukuMat["三連複人気順9番"]/UmahukuMat["三連複人気順10番"] < 1 &
	UmahukuMat["三連複人気順13番"]/UmahukuMat["三連複人気順14番"] > 0.7399 &
	UmahukuMat["三連複人気順14番"]/UmahukuMat["三連複人気順15番"] > 0.8399 &
	UmahukuMat["10以下数"]> 2  &
	UmahukuMat["30以下数"]> 5 &
	UmahukuMat["20以下数"]> 4 &
	UmahukuMat["50以上数"]< 7 &
	UmahukuMat["馬複人気順1番"]> 2.8 &
	UmahukuMat["馬複人気順2番"]> 4.9 &
	UmahukuMat["馬複人気順3番"]> 5.4 &
	UmahukuMat["馬複人気順4番"]> 8.9 &
	UmahukuMat["馬複人気順5番"]> 9.9 &
	UmahukuMat["馬複人気順5番"]< 22 &
	UmahukuMat["馬複人気順6番"]> 11.9 &
	UmahukuMat["馬複人気順6番"]< 24 &
	UmahukuMat["馬複人気順7番"]< 26 &
	UmahukuMat["馬複人気順8番"]< 28 &
	UmahukuMat["馬複人気順10番"]< 36 &
	UmahukuMat["馬複人気順11番"]< 44 &
	UmahukuMat["馬複人気順14番"]< 60 &
	UmahukuMat["馬複人気順16番"]> 19.9 &
	UmahukuMat["馬複人気順16番"]< 80 &
	UmahukuMat["馬複人気順20番"]< 120 &
	UmahukuMat["馬複人気順21番"]< 105 &
	UmahukuMat["馬複人気順22番"]< 105 &
	UmahukuMat["馬複人気順24番"]> 49.9 &
	UmahukuMat["馬複人気順29番"]< 300 &
	UmahukuMat["馬複人気順29番"]> 59.9 &
	UmahukuMat["馬複人気順30番"]<　320 &
	UmahukuMat["単勝人気順1番"]> 1.3 &
	UmahukuMat["単勝人気順3番"]> 4.1 &
	UmahukuMat["単勝人気順6番"]< 48 &
	UmahukuMat["単勝人気順7番"]< 45 &
	UmahukuMat["単勝人気順8番"]< 100 &
	UmahukuMat["単勝人気順10番"]< 420  &
	UmahukuMat["単勝人気順11番"]< 640  &
	UmahukuMat["三連複人気順1番"]> 3.4 &
	UmahukuMat["三連複人気順2番"]> 5.9 &
	UmahukuMat["三連複人気順3番"]> 7.9 &
	UmahukuMat["三連複人気順6番"]> 15.9 &
	UmahukuMat["馬複不人気順7番"]> 199.9 &
	UmahukuMat["馬複不人気順8番"]> 199.9 &
	UmahukuMat["馬複不人気順13番"]< 2000)
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

#_馬複_________________________________________________________________________
#② 2017/1/21作成
sakuseiHiduke <- "2017/01/20"
BahukuNin6_15<-subset(UmahukuMat,UmahukuMat["単勝人気順1番"]> 1.4 &
	UmahukuMat["単勝人気順2番"]< 6 &
	UmahukuMat["単勝人気順6番"]> 7.9 &
	UmahukuMat["単勝人気順7番"]< 45 &
	UmahukuMat["単勝人気順9番"]< 180 &
	UmahukuMat["単勝人気順11番"]< 400 &
	UmahukuMat["単勝人気順13番"]< 240 &
	UmahukuMat["馬複人気順1番"]< 8 &
	UmahukuMat["馬複人気順1番"]> 2.5 &
	UmahukuMat["馬複人気順3番"]> 5.9 &
	UmahukuMat["馬複人気順4番"]> 8.9 &
	UmahukuMat["馬複人気順4番"]< 16 &
	UmahukuMat["馬複人気順6番"]> 13.9 &
	UmahukuMat["馬複人気順7番"]< 20 &
	UmahukuMat["馬複人気順9番"]< 28 &
	UmahukuMat["馬複人気順12番"]> 0 &
	UmahukuMat["馬複人気順13番"]< 56 &
	UmahukuMat["馬複人気順15番"]< 70 &
	UmahukuMat["馬複不人気順20番"]< 620 &
	UmahukuMat["10以下数"]> 2 &
	UmahukuMat["20以下数"]> 4 &
	UmahukuMat["30以下数"]> 5 &
	UmahukuMat["50以上数"]< 5 &
	UmahukuMat["70以上数"]> 0 &
	UmahukuMat["100以上数"]< 4 &
	UmahukuMat["100以上数"]> 0 &
	(mat215[6] < 0.15 | mat215[6] > 0.35 )&
	(mat215[9] < 0.001 | mat215[9] > 0.4 )&
	UmahukuMat["馬複人気順5番"]/UmahukuMat["馬複人気順6番"] < 1 &
	UmahukuMat["馬複人気順20番"]/UmahukuMat["馬複人気順21番"] > 0.6999 &
	UmahukuMat["馬複人気順21番"]/UmahukuMat["馬複人気順22番"] < 1 &
	UmahukuMat["馬複人気順25番"]/UmahukuMat["馬複人気順26番"] < 1 &
	UmahukuMat["単勝人気順2番"]/UmahukuMat["単勝人気順3番"] < 0.98 &
	UmahukuMat["単勝人気順10番"]/UmahukuMat["単勝人気順11番"] > 0.36 &
	UmahukuMat["三連複人気順5番"]/UmahukuMat["三連複人気順6番"] > 0.6599 )
BFKekkaNeraiStart <- 6
BFKekkaNeraiEnd   <- 15
checked <- kekkaCheck(BahukuNin6_15,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin6_15[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin6_15[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin6_15[,1]);length(checked [,1])/length(BahukuNin6_15[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin6_15[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin6_15

#指数利用
blnShisu <- c(FALSE,FALSE,FALSE,TRUE,FALSE)
checked_Shisu <- kekkaCheckShisu2(BahukuNin6_15,BFKekkaNeraiStart,BFKekkaNeraiEnd   ,blnShisu )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin6_15[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin6_15[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin6_15,checked_Shisu [[1]])

