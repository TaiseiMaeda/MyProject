
namiChosa<-function(idx,UmahukuMat ,tanOrBahuku,atehameMat,atehameMatShisu ){

iro <- c("green","blue","black","grey","gold")
taishoMat <- atehameMat
sortedMat <- taishoMat [order(taishoMat $"年月日"),]
atariMat <- checked [order(checked $"年月日"),]
kaime <- BFKekkaNeraiEnd - BFKekkaNeraiStart -1
UmahukuSortedMat <- UmahukuMat [order(UmahukuMat $"年月日"),]
lenHiduke <- length(table(UmahukuSortedMat ["年月日"]))
checkMat <- matrix(0,9,lenHiduke)
strNengapiHoji <- as.character(UmahukuSortedMat [1,1])
checkMat[1,1] <- strNengapiHoji ;checkIndex <- 1;checkBabaIndex <- 0
for (i in 1: length(UmahukuSortedMat [,1])){
	checkBabaIndex = checkBabaIndex + 1
	if(strNengapiHoji != UmahukuSortedMat [i,1]){
		checkIndex <- checkIndex  + 1
		strNengapiHoji <- as.character(UmahukuSortedMat [i,1])
		checkMat[1,checkIndex ] <- strNengapiHoji 
		if(checkBabaIndex < 13){checkMat[7,checkIndex - 1] <- 1}
		else if(checkBabaIndex < 25){checkMat[7,checkIndex - 1] <- 2}
		else if(checkBabaIndex < 37){checkMat[7,checkIndex - 1] <- 3}
		checkBabaIndex <- 0}}
if(checkBabaIndex < 37){checkMat[7,checkIndex] <- 3};if(checkBabaIndex < 25){checkMat[7,checkIndex] <- 2};if(checkBabaIndex < 13){checkMat[7,checkIndex] <- 1}
checkIndex <- 1
for (i in 1: length(sortedMat [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat[1,j] == sortedMat [i,1]){
		checkIndex <- j
		checkMat[2,checkIndex] <- as.numeric(checkMat[2,checkIndex])-1
		break	} }}
checkMat[3,] <- as.numeric(checkMat[2,]) * kaime * 100;checkIndex <- 1
for (i in 1: length(atariMat [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat[1,j] == atariMat [i,1]){
		checkIndex <- j
		checkMat[6,checkIndex] <- as.numeric(checkMat[6,checkIndex]) + 1
		checkMat[4,checkIndex] <- atariMat [i,tanOrBahuku] + as.numeric(checkMat[4,checkIndex])
		break	} }}
checkMat[5,] <- as.numeric(checkMat[4,]) + as.numeric(checkMat[3,])
checkMat[8,] <- as.numeric(checkMat[6,]) / as.numeric(checkMat[7,])
checkMat[9,] <- -(as.numeric(checkMat[6,]) + as.numeric(checkMat[2,]) )/ as.numeric(checkMat[7,])
if (idx ==1){
plot(as.numeric(checkMat[5,]));axis(side=1, at=1:lenHiduke , labels=checkMat[1,])
par(new=T);abline( h =0 ,col="red");abline( h =10000 ,col="red")
abline( h =mean(as.numeric(checkMat[5,])) ,lty="dashed")
sakuseiIndex <- which(checkMat[1,] ==sakuseiHiduke )
abline( v=sakuseiIndex ,col="red")}
taishoMat <- atehameMatShisu ;sortedMat <- taishoMat [order(taishoMat $"年月日"),]
atariMat <- checked_Shisu [[2]] [order(checked_Shisu [[2]]$"年月日"),]
kaime <- BFKekkaNeraiEnd - BFKekkaNeraiStart -1
UmahukuSortedMat <- UmahukuMat [order(UmahukuMat $"年月日"),]
lenHiduke <- length(table(UmahukuSortedMat ["年月日"]))
checkMat_Shisu <- matrix(0,10,lenHiduke)
strNengapiHoji <- as.character(UmahukuSortedMat [1,1])
checkMat_Shisu[1,1] <- strNengapiHoji ;checkIndex <- 1;checkBabaIndex <- 0
for (i in 1: length(UmahukuSortedMat [,1])){
	checkBabaIndex = checkBabaIndex + 1
	if(strNengapiHoji != UmahukuSortedMat [i,1]){
		checkIndex <- checkIndex  + 1
		strNengapiHoji <- as.character(UmahukuSortedMat [i,1])
		checkMat_Shisu[1,checkIndex ] <- strNengapiHoji 
		if(checkBabaIndex < 13){checkMat_Shisu[7,checkIndex - 1] <- 1}
		else if(checkBabaIndex < 25){checkMat_Shisu[7,checkIndex - 1] <- 2}
		else if(checkBabaIndex < 37){checkMat_Shisu[7,checkIndex - 1] <- 3}
		checkBabaIndex <- 0	}}
if(checkBabaIndex < 37){checkMat_Shisu[7,checkIndex] <- 3}
if(checkBabaIndex < 25){checkMat_Shisu[7,checkIndex] <- 2}
if(checkBabaIndex < 13){checkMat_Shisu[7,checkIndex] <- 1};checkIndex <- 1
for (i in 1: length(sortedMat [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat_Shisu[1,j] == sortedMat [i,1]){
		checkIndex <- j
		checkMat_Shisu[2,checkIndex] <- as.numeric(checkMat_Shisu[2,checkIndex])-1
		checkMat_Shisu[10,checkIndex] <- as.numeric(checkMat_Shisu[10,checkIndex]) + as.numeric(sortedMat [i,253]) 
		break	}  }}
checkMat_Shisu[3,] <- as.numeric(checkMat_Shisu[2,]) * kaime * 100
checkIndex <- 1
for (i in 1: length(atariMat [,1])){
  for(j in checkIndex :lenHiduke ){
	if(checkMat_Shisu[1,j] == atariMat [i,1]){
		checkIndex <- j
		checkMat_Shisu[6,checkIndex] <- as.numeric(checkMat_Shisu[6,checkIndex]) + 1
		checkMat_Shisu[4,checkIndex] <- atariMat [i,tanOrBahuku] + as.numeric(checkMat_Shisu[4,checkIndex])
		break
	}  }}
checkMat_Shisu[5,] <- as.numeric(checkMat_Shisu[4,]) + as.numeric(checkMat_Shisu[3,]) + as.numeric(checkMat_Shisu[10,])*100
checkMat_Shisu[8,] <- as.numeric(checkMat_Shisu[6,]) / as.numeric(checkMat_Shisu[7,])
checkMat_Shisu[9,] <- -(as.numeric(checkMat_Shisu[6,]) + as.numeric(checkMat_Shisu[2,]) )/ as.numeric(checkMat_Shisu[7,])
par(new=T)
lines(as.numeric(checkMat_Shisu[5,]),col=iro[idx])
axis(side=1, at=1:lenHiduke , labels=checkMat_Shisu[1,])
par(new=T)
abline( h =mean(as.numeric(checkMat_Shisu[5,])) ,lty="dashed",col="green")
sakuseiIndex <- which(checkMat_Shisu[1,] ==sakuseiHiduke )
abline( v=sakuseiIndex ,col="red")
}

