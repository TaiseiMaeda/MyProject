#---------------------------------------------------------------------------------
#⑥ 17/4/21作成
sakuseiHiduke <- "2017/04/21"
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
blnShisu <- c(FALSE,FALSE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu(BahukuNin0_3,BFKekkaNeraiStart,BFKekkaNeraiEnd   ,blnShisu )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin0_3[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin0_3[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin0_3,checked_Shisu [[1]])
