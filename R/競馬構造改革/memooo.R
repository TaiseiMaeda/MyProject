#_馬複_______________________________________________________________________________
#23)
sakuseiHiduke <- "2017/03/06"
BahukuNin14_29<-subset(UmahukuMat,	
as.numeric(umahukuIkasuMat [,11])<1&
as.numeric(umahukuIkasuMat [,12])<1&
as.numeric(umahukuIkasuMat [,13])>5&
as.numeric(umahukuIkasuMat [,14])>8&
as.numeric(umahukuIkasuMat [,15])>11&
as.numeric(umahukuIkasuMat [,16])>17&
as.numeric(umahukuIkasuMat [,18])<8 &
mat215[4] == 0 &
	mat215[15] < 1 &
	mat215[16] < 1 &
	mat215[17] < 1 &
UmahukuMat["50以上数"]< 7  &
UmahukuMat["馬複人気順1番"]> 2.9 &
	UmahukuMat["馬複人気順2番"]> 4.9 &
	UmahukuMat["馬複人気順8番"]<28&
	UmahukuMat["馬複人気順11番"]< 36 &
	UmahukuMat["単勝人気順6番"]< 48  &
	UmahukuMat["単勝人気順11番"]< 340 &
	UmahukuMat["馬複人気順29番"]>60&
	UmahukuMat["三連複人気順1番"]<19&
	UmahukuMat["馬複不人気順7番"]> 150 &
	UmahukuMat["馬複不人気順30番"]<1200)
BahukuNin14_29<-subset(BahukuNin14_29,BahukuNin14_29["天候"]!='雨'||BahukuNin14_29["天候"]!='小雨')
BFKekkaNeraiStart <- 14
BFKekkaNeraiEnd   <- 29
checked <- kekkaCheck(BahukuNin14_29,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sum(checked ["馬複金額"]);(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);sum(checked ["馬複金額"])/(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
length(checked [,1]);length(BahukuNin14_29[,1]);length(checked [,1])/length(BahukuNin14_29[,1])
bfRiekiRitsu1 <- sum(checked ["馬複金額"])/(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
atehameMat <- BahukuNin14_29

#指数利用
blnShisu <- c(TRUE,FALSE,FALSE,FALSE,FALSE)
checked_Shisu <- kekkaCheckShisu(BahukuNin14_29,BFKekkaNeraiStart,BFKekkaNeraiEnd   ,blnShisu )
sum(checked_Shisu [[2]]["馬複金額"]);(length(BahukuNin14_29[,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;sum(checked_Shisu [[2]]["馬複金額"])/(length(BahukuNin14_29[,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
atehameMatShisu <- cbind(BahukuNin14_29,checked_Shisu [[1]])
