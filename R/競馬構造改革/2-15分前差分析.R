#2分前、15分前差分析
#行をアペンドするためのダミー行。チェックが終わった後に消す。
sub<- UmahukuMat[1,]
mat <- matrix(0,1,3)
catched <- 1
#___________________________________________________
#条件
 One15Hani <- 1:30 #狙い目となる15分前の人気順 
#_________________________________________________
#二つのデータの行数は一致しないこともあるので、上から順に一致するものを探す。

for (All15Idx in 1:length(UmahukuMat15hun[,1])){ #15分前全レコードの繰り返し
  start <- catched	
  for (All2Idx in start:length(UmahukuMat[,1])){     #2分前全レコードの繰り返し
    #●年月日、馬場、Rが一致する行を探している。
    if(UmahukuMat15hun[All15Idx,1] == UmahukuMat[All2Idx ,1] && 
       UmahukuMat15hun[All15Idx,2] == UmahukuMat[All2Idx ,2] && 
       UmahukuMat15hun[All15Idx,3] == UmahukuMat[All2Idx ,3] ){
       catched <- All2Idx 
       for (One15Idx in One15Hani ){
         #馬複人気順1番オッズは88から始まる。
         umaban15_odds <- UmahukuMat15hun[All15Idx,85 + One15Idx *3 ]
         umaban15_1 <- UmahukuMat15hun[All15Idx,85 + One15Idx *3 + 1]
         umaban15_2 <- UmahukuMat15hun[All15Idx,85 + One15Idx *3 + 2]
         if (umaban15_1 == 0 || umaban15_2 == 0){
           next
         }
         for (One2Idx in 1:30){
           umaban2_odds <- UmahukuMat[All2Idx,85 + One2Idx *3 ]
           umaban2_1 <- UmahukuMat[All2Idx,85 + One2Idx *3 + 1]
           umaban2_2 <- UmahukuMat[All2Idx,85 + One2Idx *3 + 2]
           if (umaban2_1 == 0 || umaban2_2 == 0){
             next
           }
           if (umaban15_1 == umaban2_1 && umaban15_2 == umaban2_2){
             if(umaban15_odds * -0.90 <= umaban2_odds && umaban2_odds < umaban15_odds * 0.7 ){
               sub <- rbind(sub,UmahukuMat[All2Idx,])
               mat <- rbind(mat,c(umaban2_1,umaban2_2,One2Idx))
               next
             }
           }    
         }
       }
      next
    }
  }
}
if (length(sub[,1]) > 1){
    sub <- sub[2:length(sub[,1]),]
    mat <- mat[2:length(mat[,1]),]
}else{
    sub <- NULL
}


haraimodoshi <- 0
haraimodoshiMat<- matrix(0,30,6)
for (i in 1:length(mat[,1])){
  haraimodoshiMat[mat[i,3],1] <- haraimodoshiMat[mat[i,3],1] + 1
  if (mat[i,1] == sub[i,63] && mat[i,2] == sub[i,64]){
    haraimodoshi <- haraimodoshi  + sub[i,65]
    haraimodoshiMat[mat[i,3],2] <- haraimodoshiMat[mat[i,3],2] + 1
    haraimodoshiMat[mat[i,3],3] <- haraimodoshiMat[mat[i,3],3]+ sub[i,65]
  }
}
haraimodoshiMat[,4] <- haraimodoshiMat[,1] * 100
haraimodoshiMat[,5] <- haraimodoshiMat[,3] - haraimodoshiMat[,4] 

haraimodoshi  #払戻金合計
length(mat[,1])*100 #購入金額
haraimodoshi/length(mat[,1])

haraimodoshiMat