# 通常
#
tanOrBahuku<- "単勝金額"
#tanOrBahuku<- "馬複金額"

taishoMat <- atehameMat
sortedMat <- taishoMat [order(taishoMat $"年月日"),]
atariMat <- checked [order(checked $"年月日"),]
kaime <- BFKekkaNeraiEnd - BFKekkaNeraiStart -1

#●UmahukuMatを使って、Rがあった日付を1行目に入れている。
UmahukuSortedMat <- UmahukuMat [order(UmahukuMat $"年月日"),]
lenHiduke <- length(table(UmahukuSortedMat ["年月日"]))
checkMat <- matrix(0,9,lenHiduke)
strNengapiHoji <- as.character(UmahukuSortedMat [1,1])
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
		#2:　購入した数
		checkMat[2,checkIndex] <- as.numeric(checkMat[2,checkIndex])-1
		break
	}
  }
}
checkMat[3,] <- as.numeric(checkMat[2,]) * kaime * 100

checkIndex <- 1
#当たったデータの繰り返し。払戻金を指定の年月日に足し合わせる。
for (i in 1: length(atariMat [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat[1,j] == atariMat [i,1]){
		checkIndex <- j
		#6:　あたり個数
		checkMat[6,checkIndex] <- as.numeric(checkMat[6,checkIndex]) + 1
		#4:　払戻金合計
		checkMat[4,checkIndex] <- atariMat [i,tanOrBahuku] + as.numeric(checkMat[4,checkIndex])
		break
	}
  }
}
#5:　払い戻し金 - 購入金額
checkMat[5,] <- as.numeric(checkMat[4,]) + as.numeric(checkMat[3,])

#8:　1馬場当たりの出現率
checkMat[8,] <- as.numeric(checkMat[6,]) / as.numeric(checkMat[7,])

#9: 範囲以外の数（1馬場当たり）
checkMat[9,] <- -(as.numeric(checkMat[6,]) + as.numeric(checkMat[2,]) )/ as.numeric(checkMat[7,])


plot(as.numeric(checkMat[5,]))
axis(side=1, at=1:lenHiduke , labels=checkMat[1,])
par(new=T)
abline( h =0 ,col="red")
abline( h =10000 ,col="red")
abline( h =mean(as.numeric(checkMat[5,])) ,lty="dashed")
sakuseiIndex <- which(checkMat[1,] ==sakuseiHiduke )
abline( v=sakuseiIndex ,col="red")

###指数
taishoMat <- atehameMatShisu 
sortedMat <- taishoMat [order(taishoMat $"年月日"),]
atariMat <- checked_Shisu [[2]] [order(checked_Shisu [[2]]$"年月日"),]
kaime <- BFKekkaNeraiEnd - BFKekkaNeraiStart -1

#●UmahukuMatを使って、Rがあった日付を1行目に入れている。
UmahukuSortedMat <- UmahukuMat [order(UmahukuMat $"年月日"),]
lenHiduke <- length(table(UmahukuSortedMat ["年月日"]))
checkMat_Shisu <- matrix(0,10,lenHiduke)
strNengapiHoji <- as.character(UmahukuSortedMat [1,1])
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
		#2:　購入した数
		checkMat_Shisu[2,checkIndex] <- as.numeric(checkMat_Shisu[2,checkIndex])-1
		#10:　キャンセルした数
		checkMat_Shisu[10,checkIndex] <- as.numeric(checkMat_Shisu[10,checkIndex]) + as.numeric(sortedMat [i,253]) 
		break
	}
  }
}
checkMat_Shisu[3,] <- as.numeric(checkMat_Shisu[2,]) * kaime * 100

checkIndex <- 1
#当たったデータの繰り返し。払戻金を指定の年月日に足し合わせる。
for (i in 1: length(atariMat [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat_Shisu[1,j] == atariMat [i,1]){
		checkIndex <- j
		#6:　あたり個数
		checkMat_Shisu[6,checkIndex] <- as.numeric(checkMat_Shisu[6,checkIndex]) + 1
		#4:　払戻金合計
		checkMat_Shisu[4,checkIndex] <- atariMat [i,tanOrBahuku] + as.numeric(checkMat_Shisu[4,checkIndex])
		break
	}
  }
}
#5:　払い戻し金 - (購入金額 - キャンセル金額)
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

#直近7日分の収支
as.numeric(checkMat_Shisu[5,(length(checkMat_Shisu[1,])-6):length(checkMat_Shisu[1,])])
sum(as.numeric(checkMat_Shisu[5,(length(checkMat_Shisu[1,])-6):length(checkMat_Shisu[1,])]))

#hjb26<-as.numeric(checkMat_Shisu[5,])/kaime
