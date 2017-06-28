#指数

#指数2までが１着に来る確率
sosu <- length(UmahukuMat [,1])
atariKaisu <- 0
atariKingaku <- 0
for (i in 1:sosu ){
	if(UmahukuMat[i,"指数1"] == UmahukuMat[i,"複勝馬番1"]){
		atariKaisu <-atariKaisu + 1
		atariKingaku <- atariKingaku  + UmahukuMat[i,"複勝金額2"]
	}
}
atariKaisu 
sosu 
atariKaisu /sosu 

atariKingaku 
sosu * 100
atariKingaku /(sosu * 100)