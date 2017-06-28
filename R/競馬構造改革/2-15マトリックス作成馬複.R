#2分前、15分前差分析

mat <- matrix(0,length(UmahukuMat[,1]),31)
mat215Umahuku <- cbind(UmahukuMat[,1:3],mat) 
catched <- 1
#年月日、馬場、R、単勝オッズ・馬番と、必要なものだけの行列にする。
umahuku2hun <- cbind(UmahukuMat[,1:3],UmahukuMat[,88:177])
umahuku15hun <- cbind(UmahukuMat15hun[,1:3],UmahukuMat15hun[,88:177])

#二つのデータの行数は一致しないこともあるので、上から順に一致するものを探す。
for (All2Idx in 1:length(umahuku2hun  [,1])){
  findFlag <- FALSE
  start <- catched
  for (All15Idx in start:length(umahuku15hun  [,1])){
    #●年月日、馬場、Rが一致する行を探している。
    if(umahuku15hun  [All15Idx,1] == umahuku2hun  [All2Idx ,1] && 
       umahuku15hun  [All15Idx,2] == umahuku2hun  [All2Idx ,2] && 
       umahuku15hun  [All15Idx,3] == umahuku2hun  [All2Idx ,3] ){ 
      for(i in 1:30){
	  if(umahuku15hun  [All15Idx,1 + i*3] != umahuku2hun  [All2Idx,1 + i*3] ||
		umahuku15hun  [All15Idx,2 + i*3] != umahuku2hun  [All2Idx,2 + i*3]){
	    for(j in 1:30){
		#15分前ではi番目だったのが、2分前にはj番目に変わっている。
	      if(umahuku15hun  [All15Idx,1 + i*3] != umahuku2hun  [All2Idx,1 + j*3] ||
		    umahuku15hun  [All15Idx,2 + i*3] != umahuku2hun  [All2Idx,2 + j*3]){
		  #その時の(変わった後のオッズ)/(変わる前の15分前オッズ)を、15分前時点の人気順の場所に格納する。
		  if (umahuku15hun  [All15Idx,3 + i*3] ==0){
		    mat215Umahuku [All2Idx ,i+3] <- 0
		　　}else{
		    mat215Umahuku [All2Idx ,i+3] <- umahuku2hun  [All2Idx,3 + j*3] /umahuku15hun  [All15Idx,3 + i*3]
		　　}
		  break
		}	      
	    }
	  }
      }
      findFlag <- TRUE
      catched <- All15Idx 	
	break
    }
  }#15hun
  #見つかった場合は個数を計算して、見つからなかった場合は、個数に-1を入れる。
  if(findFlag){
    for(keisanIdx in 1:30){
      if(mat215Umahuku  [All2Idx ,keisanIdx + 3] > 0){
        mat215Umahuku  [All2Idx ,34] <- mat215Umahuku  [All2Idx ,34] + 1
      }
    }
  }else{
    mat215Umahuku  [All2Idx ,34] <- -1
  }
}#2hun

#__________________________________________________________________________
#単勝2-15調査_______________________________________________________________
BAIRITSU <- BFKekkaNeraiEnd - BFKekkaNeraiStart  - 1
x<-matrix(0,33,7)
for(i in 0:32){
    sub<-subset(UmahukuMat ,
		mat215Umahuku [34] > -1 &
		mat215Umahuku [i + 4] < 1 )
  if(length(sub[,1]) != 0){
    sub2<- UmahukuMat[1,];jIdx <- 1:length(sub[,1]);kIdx <- (BFKekkaNeraiStart + 1 ):(BFKekkaNeraiEnd-1)
    for (j in jIdx){
      for (k in kIdx){
        if(!is.na(sub[j,10 + k*2])){
          if(sub[j,10 + k*2] == sub[j,47]){
            sub2 <- rbind(sub2,sub[j,])
            break
          }
        }
      }
    }
  }
  if (length(sub2[,1]) > 1){sub2 <- sub2[2:length(sub2[,1]),]}else{sub2 <- NULL}
  #条件の中で狙い目が勝った数
  if(i < 16){
    x[i+1,1] <- i + 1 
  }else{
    x[i+1,1] <- i - 16   
  }
  x[i+1,2] <- length(sub2[,1])
  #条件に当てはまった数
  x[i+1,3] <- length(sub[,1])
  #的中率（条件の中で勝った割合）
  x[i+1,4] <- length(sub2[,1])/length(sub[,1])
  #的中金額
  if (is.null(sub2)){
    x[i+1,5] <-　0
  }else{
    x[i+1,5] <-　sum(sub2["単勝金額"])}
  #購入金額
  x[i+1,6] <- length(sub[,1]) * BAIRITSU * 100

  #利益率
  x[i+1,7] <- x[i+1,5] / x[i+1,6]
}
#_____________________________________________________________________


#__________________________________________________________________________
#馬複2-15調査_______________________________________________________________
BAIRITSU <- BFKekkaNeraiEnd - BFKekkaNeraiStart  - 1
x<-matrix(0,30,7)
for(i in 0:29){
    sub<-subset(UmahukuMat ,
		mat215Umahuku [34] > -1 &
		mat215Umahuku [i + 4] < 1 )
  if(length(sub[,1]) != 0){
    sub2<- UmahukuMat[1,];jIdx <- 1:length(sub[,1]);kIdx <- (BFKekkaNeraiStart + 1 ):(BFKekkaNeraiEnd-1)
    for (j in jIdx){
      for (k in kIdx){
        if(!is.na(sub[j,84 + k*3 + 1]) && !is.na(sub[j,84 + k*3 + 2])){
          if(sub[j,84 + k*3 + 1] == sub[j,63] && sub[j,84 + k*3 + 2] == sub[j,64]){
            sub2 <- rbind(sub2,sub[j,])
            break
          }
        }
      }
    }
  }
  if (length(sub2[,1]) > 1){sub2 <- sub2[2:length(sub2[,1]),]}else{sub2 <- NULL}
  #条件の中で狙い目が勝った数
  x[i+1,1] <- i + 1 
  x[i+1,2] <- length(sub2[,1])
  #条件に当てはまった数
  x[i+1,3] <- length(sub[,1])
  #的中率（条件の中で勝った割合）
  x[i+1,4] <- length(sub2[,1])/length(sub[,1])
  #的中金額
  if (is.null(sub2)){
    x[i+1,5] <-　0
  }else{
    x[i+1,5] <-　sum(sub2["馬複金額"])}
  #購入金額
  x[i+1,6] <- length(sub[,1]) * BAIRITSU * 100

  #利益率
  x[i+1,7] <- x[i+1,5] / x[i+1,6]
}
#_____________________________________________________________________






