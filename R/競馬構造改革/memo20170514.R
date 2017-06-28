
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
checked <- kekkaCheck(BahukuNin14_29,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin14_29[,1]);length(checked [,1])/length(BahukuNin14_29[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin14_29

#指数利用
blnShisu <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu(BahukuNin14_29,BFKekkaNeraiStart,BFKekkaNeraiEnd   ,blnShisu )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin14_29[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin14_29,checked_Shisu [[1]])
