#bootstrap馬複
#繰り返しの数
iter<-300
sonekiGaku <- matrix(0,iter)
tekichuRitsu <- matrix(0,iter)

#UmahukuMatの長さ/10　個の乱数を作成する。範囲は0～UmahukuMatの長さ
lenUmat<-length(UmahukuMat[,1])
lenUmat10 <- 60

#繰り返しの開始
t<-proc.time()
for(idx in 1:iter){
UmatSample<-sample(1:lenUmat, lenUmat10, replace=TRUE)
subUmat<- UmahukuMat[1,]
for(i in 1:(lenUmat10)){
  subUmat <- rbind(subUmat,UmahukuMat[UmatSample[i],])
}
subUmat <- subUmat [2:length(subUmat [,1]),]
#______________________________________________________________
#関数をコピペ
AtehameUmat <-subset(subUmat ,subUmat["馬複人気順12番"]> 26 & 
			subUmat ["馬複人気順9番"]> 20 &
			subUmat ["馬複人気順2番"]> 5.9 & 
			subUmat ["馬複人気順1番"]> 3.4 & 
			subUmat ["馬複人気順1番"]< 7.5 & 
			subUmat ["単勝人気順4番"]> 6 &
			subUmat ["単勝人気順5番"]> 8.9 &
			subUmat ["100以上数"]< 4 )
BFKekkaNeraiStart <- 14
BFKekkaNeraiEnd   <- 29
#修正ここまで
#______________________________________________________________
if(length(AtehameUmat[,1]) == 0 ){
sonekiGaku[idx] <- 0;tekichuRitsu[idx]<-0
}else{
checkUmat <- kekkaCheck(AtehameUmat,BFKekkaNeraiStart,BFKekkaNeraiEnd   )
sonekiGaku[idx] <- sum(checkUmat ["馬複金額"]) - length(AtehameUmat [,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100
tekichuRitsu[idx]<-length(checkUmat [,1])/length(AtehameUmat [,1])
}
}
proc.time()-t #経過時間

cbind(sonekiGaku,tekichuRitsu)
summary(sonekiGaku)
sum(sonekiGaku > 0) #勝つ確率

