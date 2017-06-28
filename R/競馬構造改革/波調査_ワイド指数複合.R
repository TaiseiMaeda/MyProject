#length(table(UmahukuMat["年月日"]))
#table(UmahukuMat["年月日"])[63]

#●当てはめたデータの準備
#素の場合
#BFKekkaNeraiStart <- 14; BFKekkaNeraiEnd <- 29; checked1 <- kekkaCheckWide(UmahukuMat,BFKekkaNeraiStart,BFKekkaNeraiEnd,1); checked2 <- kekkaCheckWide(UmahukuMat,BFKekkaNeraiStart,BFKekkaNeraiEnd,2); checked3 <- kekkaCheckWide(UmahukuMat,BFKekkaNeraiStart,BFKekkaNeraiEnd,3);

taishoMat <- atehameMat
sortedMat <- taishoMat [order(taishoMat $"年月日"),]
atariMat1 <- checked1 [order(checked1 $"年月日"),]
atariMat2 <- checked2 [order(checked2 $"年月日"),]
atariMat3 <- checked3 [order(checked3 $"年月日"),]

kaime <- BFKekkaNeraiEnd - BFKekkaNeraiStart -1
#●UmahukuMatを使って、Rがあった日付を1行目に入れている。
UmahukuSortedMat <- UmahukuMat [order(UmahukuMat $"年月日"),]
strNengapiHoji <- as.character(UmahukuSortedMat [1,1])
lenHiduke <- length(table(UmahukuSortedMat ["年月日"]))
checkMat <- matrix(0,9,lenHiduke)

checkMat[1,1] <- strNengapiHoji 
checkIndex <- 1
checkBabaIndex <- 0
for (i in 1: length(UmahukuSortedMat [,1])){
	checkBabaIndex = checkBabaIndex + 1
	if(strNengapiHoji != UmahukuSortedMat [i,1]){
		checkIndex <- checkIndex  + 1
		strNengapiHoji <- as.character(UmahukuSortedMat [i,1])
		checkMat[1,checkIndex ] <- strNengapiHoji 

		#7: 馬場の数
		if(checkBabaIndex < 13){checkMat[7,checkIndex - 1] <- 1}
		else if(checkBabaIndex < 25){checkMat[7,checkIndex - 1] <- 2}
		else if(checkBabaIndex < 37){checkMat[7,checkIndex - 1] <- 3}
		checkBabaIndex <- 0
	}
}
#最後の1列分
if(checkBabaIndex < 37){checkMat[7,checkIndex] <- 3}
if(checkBabaIndex < 25){checkMat[7,checkIndex] <- 2}
if(checkBabaIndex < 13){checkMat[7,checkIndex] <- 1}

checkIndex <- 1
#購入したデータの繰り返し。
#日付ごとに回数と購入金額を保存する。
for (i in 1: length(sortedMat [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat[1,j] == sortedMat [i,1]){
		checkIndex <- j
		checkMat[2,checkIndex] <- as.numeric(checkMat[2,checkIndex])-1
		break
	}
  }
}
checkMat[3,] <- as.numeric(checkMat[2,]) * kaime * 100

checkIndex <- 1
#当たったデータの繰り返し。払戻金を指定の年月日に足し合わせる。
#ワイド1
for (i in 1: length(atariMat1 [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat[1,j] == atariMat1 [i,1]){
		checkIndex <- j
		#6:　あたり個数
		checkMat[6,checkIndex] <- as.numeric(checkMat[6,checkIndex]) + 1
		checkMat[4,checkIndex] <- atariMat1 [i,"ワイド1金額"] + as.numeric(checkMat[4,checkIndex])
		break
	}
  }
}
checkIndex <- 1
#ワイド2
for (i in 1: length(atariMat2 [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat[1,j] == atariMat2 [i,1]){
		checkIndex <- j
		#6:　あたり個数
		checkMat[6,checkIndex] <- as.numeric(checkMat[6,checkIndex]) + 1
		checkMat[4,checkIndex] <- atariMat2 [i,"ワイド2金額"] + as.numeric(checkMat[4,checkIndex])
		break
	}
  }
}
checkIndex <- 1
#ワイド3
for (i in 1: length(atariMat3 [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat[1,j] == atariMat3 [i,1]){
		checkIndex <- j
		#6:　あたり個数
		checkMat[6,checkIndex] <- as.numeric(checkMat[6,checkIndex]) + 1
		checkMat[4,checkIndex] <- atariMat3 [i,"ワイド3金額"] + as.numeric(checkMat[4,checkIndex])
		break
	}
  }
}
checkMat[5,] <- as.numeric(checkMat[4,]) + as.numeric(checkMat[3,]) 

#8:　1馬場当たりの出現率
checkMat[8,] <- as.numeric(checkMat[6,]) / as.numeric(checkMat[7,])

#9: 範囲以外の数（1馬場当たり）
checkMat[9,] <- -(as.numeric(checkMat[6,]) + as.numeric(checkMat[2,]) )/ as.numeric(checkMat[7,])

plot(as.numeric(checkMat[5,]))
axis(side=1, at=1:lenHiduke , labels=checkMat[1,])
par(new=T)
abline( h =0 ,col="red")
abline( h =mean(as.numeric(checkMat[5,])) ,lty="dashed")
sakuseiIndex <- which(checkMat[1,] ==sakuseiHiduke )
abline( v=sakuseiIndex ,col="red")
#length(table(UmahukuMat["年月日"]))
#table(UmahukuMat["年月日"])[63]

#●当てはめたデータの準備
#素の場合
#BFKekkaNeraiStart <- 14; BFKekkaNeraiEnd <- 29; checked1 <- kekkaCheckWide(UmahukuMat,BFKekkaNeraiStart,BFKekkaNeraiEnd,1); checked2 <- kekkaCheckWide(UmahukuMat,BFKekkaNeraiStart,BFKekkaNeraiEnd,2); checked3 <- kekkaCheckWide(UmahukuMat,BFKekkaNeraiStart,BFKekkaNeraiEnd,3);

taishoMat <- atehameMatShisu
sortedMat <- taishoMat [order(taishoMat $"年月日"),]
atariMat1 <- checked1shisu  [[2]][order(checked1shisu  [[2]]$"年月日"),]
atariMat2 <- checked2shisu  [[2]][order(checked2shisu  [[2]]$"年月日"),]
atariMat3 <- checked3shisu  [[2]][order(checked3shisu  [[2]]$"年月日"),]

kaime <- BFKekkaNeraiEnd - BFKekkaNeraiStart -1
#●UmahukuMatを使って、Rがあった日付を1行目に入れている。
UmahukuSortedMat <- UmahukuMat [order(UmahukuMat $"年月日"),]
strNengapiHoji <- as.character(UmahukuSortedMat [1,1])
lenHiduke <- length(table(UmahukuSortedMat ["年月日"]))
checkMat_Shisu <- matrix(0,10,lenHiduke)

checkMat_Shisu[1,1] <- strNengapiHoji 
checkIndex <- 1
checkBabaIndex <- 0
for (i in 1: length(UmahukuSortedMat [,1])){
	checkBabaIndex = checkBabaIndex + 1
	if(strNengapiHoji != UmahukuSortedMat [i,1]){
		checkIndex <- checkIndex  + 1
		strNengapiHoji <- as.character(UmahukuSortedMat [i,1])
		checkMat_Shisu[1,checkIndex ] <- strNengapiHoji 

		#7: 馬場の数
		if(checkBabaIndex < 13){checkMat_Shisu[7,checkIndex - 1] <- 1}
		else if(checkBabaIndex < 25){checkMat_Shisu[7,checkIndex - 1] <- 2}
		else if(checkBabaIndex < 37){checkMat_Shisu[7,checkIndex - 1] <- 3}
		checkBabaIndex <- 0
	}
}
#最後の1列分
if(checkBabaIndex < 37){checkMat_Shisu[7,checkIndex] <- 3}
if(checkBabaIndex < 25){checkMat_Shisu[7,checkIndex] <- 2}
if(checkBabaIndex < 13){checkMat_Shisu[7,checkIndex] <- 1}

checkIndex <- 1
#購入したデータの繰り返し。
#日付ごとに回数と購入金額を保存する。
for (i in 1: length(sortedMat [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat_Shisu[1,j] == sortedMat [i,1]){
		checkIndex <- j
		checkMat_Shisu[2,checkIndex] <- as.numeric(checkMat_Shisu[2,checkIndex])-1
		checkMat_Shisu[10,checkIndex] <- as.numeric(checkMat_Shisu[10,checkIndex]) + as.numeric(sortedMat [i,253]) 
		break
	}
  }
}
checkMat_Shisu[3,] <- as.numeric(checkMat_Shisu[2,]) * kaime * 100

checkIndex <- 1
#当たったデータの繰り返し。払戻金を指定の年月日に足し合わせる。
#ワイド1
for (i in 1: length(atariMat1 [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat_Shisu[1,j] == atariMat1 [i,1]){
		checkIndex <- j
		#6:　あたり個数
		checkMat_Shisu[6,checkIndex] <- as.numeric(checkMat_Shisu[6,checkIndex]) + 1
		checkMat_Shisu[4,checkIndex] <- atariMat1 [i,"ワイド1金額"] + as.numeric(checkMat_Shisu[4,checkIndex])
		break
	}
  }
}
checkIndex <- 1
#ワイド2
for (i in 1: length(atariMat2 [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat_Shisu[1,j] == atariMat2 [i,1]){
		checkIndex <- j
		#6:　あたり個数
		checkMat_Shisu[6,checkIndex] <- as.numeric(checkMat_Shisu[6,checkIndex]) + 1
		checkMat_Shisu[4,checkIndex] <- atariMat2 [i,"ワイド2金額"] + as.numeric(checkMat_Shisu[4,checkIndex])
		break
	}
  }
}
checkIndex <- 1
#ワイド3
for (i in 1: length(atariMat3 [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat_Shisu[1,j] == atariMat3 [i,1]){
		checkIndex <- j
		#6:　あたり個数
		checkMat_Shisu[6,checkIndex] <- as.numeric(checkMat_Shisu[6,checkIndex]) + 1
		checkMat_Shisu[4,checkIndex] <- atariMat3 [i,"ワイド3金額"] + as.numeric(checkMat_Shisu[4,checkIndex])
		break
	}
  }
}
checkMat_Shisu[5,] <- as.numeric(checkMat_Shisu[4,]) + as.numeric(checkMat_Shisu[3,]) + as.numeric(checkMat_Shisu[10,])*100

#8:　1馬場当たりの出現率
checkMat_Shisu[8,] <- as.numeric(checkMat_Shisu[6,]) / as.numeric(checkMat_Shisu[7,])

#9: 範囲以外の数（1馬場当たり）
checkMat_Shisu[9,] <- -(as.numeric(checkMat_Shisu[6,]) + as.numeric(checkMat_Shisu[2,]) )/ as.numeric(checkMat_Shisu[7,])

par(new=T)
lines(as.numeric(checkMat_Shisu[5,]),col="green")
axis(side=1, at=1:lenHiduke , labels=checkMat_Shisu[1,])
par(new=T)
abline( h =mean(as.numeric(checkMat_Shisu[5,])) ,lty="dashed",col="green")
sakuseiIndex <- which(checkMat_Shisu[1,] ==sakuseiHiduke )
abline( v=sakuseiIndex ,col="red")

hjw13<-as.numeric(checkMat_Shisu[5,])/kaime


 