########################################
#新関数を一括読み込みしてからこのソースを読み込まなくては
#正しい値が入らないこともあるので注意。
#########################################
#--------------------------------------
#自作関数（Σ計算。）
sigma <- function(x){
  hoji <- 0
  if(x != 0){
    for(i in 1:x){
      hoji <- hoji + i
    }
  }
  return(hoji)
}
#--------------------------------------
#--------------------------------------
#自作関数（idxから二つの関数番号を返す。）
suji <- function(kansuSu,x){
  suji1 <- 0
  for(i in 1:(kansuSu-1)){
    if(x > choose(kansuSu,2) - sigma(i)){
      suji1<- kansuSu - i
	break
    }
  }
   return(c(suji1,kansuSu- (choose(kansuSu,2)-sigma(kansuSu-suji1-1)-x)))
}
#--------------------------------------
#############################################################################
#年月日、馬場、Rを検索して、同じ行か調べる。
#同じ行であれば列を取り出し、まとめる。これが2つの共通行列
#rownames関数で行ラベルを取り出せるので、
#intersect：積集合で二つの当てはめの結果の共通する行ラベルを取り出し
#UmahukuMatで行列を取得する。
#choose(kansuSu,2)は nCrの事
#############################################################################
kansuSu <- 7

kekkaHyoji<-matrix(0,choose(kansuSu,2)*2 ,12)
matmatome <- list()
kyotsuJoho1<-c("単勝金額","単勝金額","単勝金額","馬複金額","馬複金額","馬複金額","馬複金額")
kyotsuJoho2<- list()
kyotsuJoho3<- list()

#関数1
mat1<-TanshoNin2_4
matmatome[[1]]<-mat1
kyotsuJoho2[[1]] <- c(FALSE,FALSE,TRUE,FALSE,FALSE)
kyotsuJoho3[[1]] <- c(2,4)
#関数2
mat2<-TanshoNin3_7
matmatome[[2]]<-mat2
kyotsuJoho2[[2]] <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
kyotsuJoho3[[2]] <- c(3,7)
#関数3
mat3<-TanshoNin4_10
matmatome[[3]]<-mat3
kyotsuJoho2[[3]] <- c(FALSE,TRUE,TRUE,FALSE,FALSE)
kyotsuJoho3[[3]] <- c(4,10)
#関数4
mat4<-BahukuNin14_29
matmatome[[4]]<-mat4
kyotsuJoho2[[4]] <- c(TRUE,TRUE,FALSE,FALSE,FALSE)
kyotsuJoho3[[4]] <- c(14,29)
#関数5
mat5<-BahukuNin2_6
matmatome[[5]]<-mat5
kyotsuJoho2[[5]] <- c(TRUE,TRUE,TRUE,FALSE,FALSE)
kyotsuJoho3[[5]] <- c(2,6)
#関数6
mat6<-BahukuNin6_15
matmatome[[6]]<-mat6
kyotsuJoho2[[6]] <- c(TRUE,FALSE,FALSE,FALSE,FALSE)
kyotsuJoho3[[6]] <- c(6,15)
#関数7
mat7<-BahukuNin0_3
matmatome[[7]]<-mat7
kyotsuJoho2[[7]] <- c(FALSE,FALSE,TRUE,TRUE,FALSE)
kyotsuJoho3[[7]] <- c(0,3)

kyotsumatome <- list()
idx <- 1
for (i in 1:(kansuSu - 1) ){
  for(j in (i+1):kansuSu ){
    kyotsumatome[[idx]] <- UmahukuMat[intersect(rownames(matmatome[[i]]), 
				rownames(matmatome[[j]])),]
    idx <- idx + 1
  }
}

for (i in 1:choose(kansuSu,2)){
  kansubango<- suji(kansuSu,i)
  kekkaHyoji[2*(i-1)+1,1]<- kansubango[1]
  kekkaHyoji[2*(i-1)+1,2]<- kansubango[2]
  kekkaHyoji[2*(i-1)+1,3]<- kansubango[1]
  kekkaHyoji[2*i,1]<- kansubango[1]
  kekkaHyoji[2*i,2]<- kansubango[2]
  kekkaHyoji[2*i,3]<- kansubango[2]

  BFKekkaNeraiStart<-kyotsuJoho3[[kansubango[1]]][1]
  BFKekkaNeraiEnd<-kyotsuJoho3[[kansubango[1]]][2]
  if(length(kyotsumatome[[i]][,1])!=0){
    if(kyotsuJoho1[kansubango[1]] == "単勝金額"){
      checked <- kekkaCheckTansho(kyotsumatome[[i]],BFKekkaNeraiStart,BFKekkaNeraiEnd   )
      checked_Shisu <- kekkaCheckShisu_tansho(kyotsumatome[[i]],BFKekkaNeraiStart,BFKekkaNeraiEnd,kyotsuJoho2[[kansubango[1]]])
     }else{
       checked <- kekkaCheck(kyotsumatome[[i]],BFKekkaNeraiStart,BFKekkaNeraiEnd   )
       checked_Shisu <- kekkaCheckShisu(kyotsumatome[[i]],BFKekkaNeraiStart,BFKekkaNeraiEnd,kyotsuJoho2[[kansubango[1]]])
     }
    kekkaHyoji[2*(i-1)+1,4]<- sum(checked [kyotsuJoho1[kansubango[1]]]);
    kekkaHyoji[2*(i-1)+1,5]<- (length(kyotsumatome[[i]][,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);
    kekkaHyoji[2*(i-1)+1,6]<- sum(checked [kyotsuJoho1[kansubango[1]]])/(length(kyotsumatome[[i]][,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
    kekkaHyoji[2*(i-1)+1,7]<- length(checked [,1]);
    kekkaHyoji[2*(i-1)+1,8]<- length(kyotsumatome[[i]][,1]);
    kekkaHyoji[2*(i-1)+1,9]<- length(checked [,1])/length(kyotsumatome[[i]][,1])
    kekkaHyoji[2*(i-1)+1,10]<- sum(checked_Shisu [[2]][kyotsuJoho1[kansubango[1]]]);
    kekkaHyoji[2*(i-1)+1,11]<- (length(kyotsumatome[[i]][,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;
    kekkaHyoji[2*(i-1)+1,12]<- sum(checked_Shisu [[2]][kyotsuJoho1[kansubango[1]]])/(length(kyotsumatome[[i]][,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)

    BFKekkaNeraiStart<-kyotsuJoho3[[kansubango[2]]][1]
    BFKekkaNeraiEnd<-kyotsuJoho3[[kansubango[2]]][2]
    if(kyotsuJoho1[kansubango[2]] == "単勝金額"){
      checked <- kekkaCheckTansho(kyotsumatome[[i]],BFKekkaNeraiStart,BFKekkaNeraiEnd   )
      checked_Shisu <- kekkaCheckShisu_tansho(kyotsumatome[[i]],BFKekkaNeraiStart,BFKekkaNeraiEnd,kyotsuJoho2[[kansubango[2]]])
     }else{
       checked <- kekkaCheck(kyotsumatome[[i]],BFKekkaNeraiStart,BFKekkaNeraiEnd   )
       checked_Shisu <- kekkaCheckShisu(kyotsumatome[[i]],BFKekkaNeraiStart,BFKekkaNeraiEnd,kyotsuJoho2[[kansubango[2]]])
     }
    kekkaHyoji[2*i,4]<- sum(checked [kyotsuJoho1[kansubango[2]]]);
    kekkaHyoji[2*i,5]<- (length(kyotsumatome[[i]][,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100);
    kekkaHyoji[2*i,6]<- sum(checked [kyotsuJoho1[kansubango[2]]])/(length(kyotsumatome[[i]][,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
    kekkaHyoji[2*i,7]<- length(checked [,1]);
    kekkaHyoji[2*i,8]<- length(kyotsumatome[[i]][,1]);
    kekkaHyoji[2*i,9]<- length(checked [,1])/length(kyotsumatome[[i]][,1])
    kekkaHyoji[2*i,10]<- sum(checked_Shisu [[2]][kyotsuJoho1[kansubango[2]]]);
    kekkaHyoji[2*i,11]<- (length(kyotsumatome[[i]][,1]) * (BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100) - sum(checked_Shisu[[1]][4])* 100;
    kekkaHyoji[2*i,12]<- sum(checked_Shisu [[2]][kyotsuJoho1[kansubango[2]]])/(length(kyotsumatome[[i]][,1]) *(BFKekkaNeraiEnd-BFKekkaNeraiStart-1) * 100 - sum(checked_Shisu[[1]][4])* 100)
  }
}
url <- paste("R出力\\",format(Sys.Date(),"%Y%m%d"),"_","HukugoChosa",".txt",sep="")
sink(file = url ,split = TRUE)
print(kyotsuJoho1)
print(kyotsuJoho3)
print(kekkaHyoji)
sink()


