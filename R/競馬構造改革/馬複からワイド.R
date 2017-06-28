#馬複の予想→ワイド

#________________________________________________________
#結果取得用の関数
kekkaCheckWide <- function(x,y,z,w){
sub<- UmahukuMat[1,]
jIdx <- 1:length(x[,1])
kIdx <- (y + 1 ):(z -1)
for (j in jIdx){
  for (k in kIdx){
    #人気順k番の馬番1,2と結果人気順の馬番1,2を比べている。
    if(!is.na(x[j,85 + k*3 + 1]) && !is.na(x[j,85 + k*3 + 2])){
      if(x[j,85 + k*3 + 1] == x[j,4*(w - 1) + 75] && x[j,85 + k*3 + 2] == x[j,4*(w - 1) + 76]){
        #一致していたら当たったということで行を取り出し、アペンドする。
        sub <- rbind(sub,x[j,])
        break
      }
    }
  }
}
if (length(sub[,1]) > 1){
    sub <- sub[2:length(sub[,1]),]
}else{
    sub <- NULL
}
return(sub)
}#関数終了
#_________________________________________________________


#______________________________________________________________
#⑩
BahukuNin20_31<-subset(UmahukuMat,UmahukuMat["馬複人気順1番"]> 3.5 &
			UmahukuMat["馬複人気順13番"]> 32 &
			UmahukuMat["馬複人気順14番"]> 35 &
			UmahukuMat["馬複人気順15番"]< 53 &
			UmahukuMat["単勝人気順8番"]< 43 )
BFKekkaNeraiStart <- 20
BFKekkaNeraiEnd   <- 31
checked1 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))
length(BahukuNin20_31[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin20_31[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin20_31[,1])



#______________________________________________________________
#⑩
BahukuNin12_21<-subset(UmahukuMat,UmahukuMat["30以下数"]< 9 &
			UmahukuMat["馬複人気順9番"]< 33 &
			UmahukuMat["馬複人気順8番"]> 18.5 &
			UmahukuMat["馬複人気順6番"]> 12.8 &
			UmahukuMat["馬複人気順5番"]> 11.5 &
			UmahukuMat["単勝人気順4番"]> 7 )
BFKekkaNeraiStart <- 12
BFKekkaNeraiEnd   <- 21
checked1 <- kekkaCheckWide(BahukuNin12_21,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin12_21,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin12_21,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))
length(BahukuNin12_21[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin12_21[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin12_21[,1])





##_________________________________________________________
##旧関数
________________________________________________________________
######①
BahukuNin2_7<-subset(CancelBahukuNin2_7,CancelBahukuNin2_7["50以上数"]< 7 &
CancelBahukuNin2_7["70以上数"]< 5 &
CancelBahukuNin2_7["100以上数"]< 4 &
CancelBahukuNin2_7["20以下数"]< 8 &
CancelBahukuNin2_7["30以下数"]< 9 &
CancelBahukuNin2_7["馬複人気順15番"]< 51 &
CancelBahukuNin2_7["馬複人気順11番"]> 24.5 &
CancelBahukuNin2_7["馬複人気順13番"]< 77 &
CancelBahukuNin2_7["馬複人気順11番"]< 45 &
CancelBahukuNin2_7["馬複人気順9番"]> 20 &
CancelBahukuNin2_7["馬複人気順9番"]< 31 &
CancelBahukuNin2_7["馬複人気順8番"]> 19 &
CancelBahukuNin2_7["馬複人気順7番"]> 16 &
CancelBahukuNin2_7["馬複人気順4番"]< 16 &
CancelBahukuNin2_7["馬複人気順4番"]> 10 &
CancelBahukuNin2_7["馬複人気順3番"]< 13 &
CancelBahukuNin2_7["馬複人気順1番"]> 2.8 &
CancelBahukuNin2_7["馬複人気順1番"]< 5.5 &
CancelBahukuNin2_7["単勝人気順1番"]> 1.9 &
CancelBahukuNin2_7["単勝人気順2番"]< 6 &
CancelBahukuNin2_7["単勝人気順3番"]< 10 &
CancelBahukuNin2_7["単勝人気順5番"]< 34 )
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 7
checked1 <- kekkaCheckWide(BahukuNin2_7,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin2_7,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin2_7,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))
length(BahukuNin2_7[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin2_7[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin2_7[,1])


#____________________________________________________________________
######②
BahukuNin0_2<-subset(CancelBahukuNin0_2,CancelBahukuNin0_2["70以上数"]> 2 &
CancelBahukuNin0_2["馬複人気順2番"]< 12.5 &
CancelBahukuNin0_2["馬複人気順4番"]< 18 &
CancelBahukuNin0_2["馬複人気順5番"]> 14 &
CancelBahukuNin0_2["馬複人気順7番"]> 17 &
CancelBahukuNin0_2["馬複人気順8番"]< 28 &
CancelBahukuNin0_2["馬複人気順11番"]< 70 &
CancelBahukuNin0_2["馬複人気順13番"]> 41 &
CancelBahukuNin0_2["単勝人気順4番"]> 5.8 )
sub<-subset(BahukuNin0_2,BahukuNin0_2["馬複人気順"]>0 &BahukuNin0_2["馬複人気順"]<2)
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 7
checked1 <- kekkaCheckWide(BahukuNin0_2,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin0_2,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin0_2,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))
length(BahukuNin0_2[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin0_2[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin0_2[,1])

#____________________________________________________________________
######③
BahukuNin20_31<-subset(CancelBahukuNin20_31,CancelBahukuNin20_31["馬複人気順15番"]> 28 &
CancelBahukuNin20_31["馬複人気順14番"]> 27 &
CancelBahukuNin20_31["馬複人気順12番"]> 24 &
CancelBahukuNin20_31["馬複人気順11番"]< 38 & 
CancelBahukuNin20_31["馬複人気順10番"]> 19 &
CancelBahukuNin20_31["馬複人気順6番"]> 13 & 
CancelBahukuNin20_31["馬複人気順1番"]> 5.5 &
CancelBahukuNin20_31["単勝人気順5番"]> 10.3 &
CancelBahukuNin20_31["30以下数"]< 10 &
CancelBahukuNin20_31["50以上数"]> 1 )
BFKekkaNeraiStart <- 20
BFKekkaNeraiEnd   <- 31
checked1 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))
length(BahukuNin20_31[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin20_31[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin20_31[,1])

#____________________________________________________________________
######④
TanshoNin2_6<-subset(CancelTanshoNin2_6,CancelTanshoNin2_6["馬複人気順10番"]> 23 &
CancelTanshoNin2_6["馬複人気順8番"]< 30 &
CancelTanshoNin2_6["馬複人気順8番"]> 17 &
CancelTanshoNin2_6["馬複人気順7番"]> 15 &
CancelTanshoNin2_6["馬複人気順6番"]< 18 &
CancelTanshoNin2_6["馬複人気順6番"]> 12 &
CancelTanshoNin2_6["馬複人気順2番"]< 11 &
CancelTanshoNin2_6["馬複人気順1番"]> 3 &
CancelTanshoNin2_6["馬複人気順1番"]< 8 &
CancelTanshoNin2_6["単勝人気順1番"]> 2.1 &
CancelTanshoNin2_6["単勝人気順1番"]< 3.4 &
CancelTanshoNin2_6["単勝人気順2番"]< 5.2 &
CancelTanshoNin2_6["単勝人気順2番"]> 2.7 &
CancelTanshoNin2_6["単勝人気順3番"]< 8.8 &
CancelTanshoNin2_6["単勝人気順4番"]> 5.4 &
CancelTanshoNin2_6["単勝人気順4番"]< 9.7 &
CancelTanshoNin2_6["単勝人気順5番"]> 10 &
CancelTanshoNin2_6["30以下数"]< 10 )
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 6
checked1 <- kekkaCheckWide(TanshoNin2_6,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(TanshoNin2_6,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(TanshoNin2_6,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))
length(TanshoNin2_6[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(TanshoNin2_6[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(TanshoNin2_6[,1])

#____________________________________________________________________
######⑤
TanshoNin1_3<-subset(CancelTanshoNin1_3,CancelTanshoNin1_3["単勝人気順1番"]> 1.6 &
CancelTanshoNin1_3["単勝人気順1番"]< 3.7 &
CancelTanshoNin1_3["単勝人気順2番"]< 4 &
CancelTanshoNin1_3["単勝人気順3番"]< 10 &
CancelTanshoNin1_3["単勝人気順4番"]> 8 &
CancelTanshoNin1_3["単勝人気順5番"]> 10 &
CancelTanshoNin1_3["馬複人気順1番"]< 9 &
CancelTanshoNin1_3["馬複人気順2番"]> 4 &
CancelTanshoNin1_3["馬複人気順4番"]< 18 &
CancelTanshoNin1_3["馬複人気順5番"]< 20 &
CancelTanshoNin1_3["馬複人気順7番"]< 28 &
CancelTanshoNin1_3["馬複人気順9番"]< 34 &
CancelTanshoNin1_3["馬複人気順9番"]> 19 &
CancelTanshoNin1_3["馬複人気順11番"]> 26 &
CancelTanshoNin1_3["馬複人気順12番"]< 40 &
CancelTanshoNin1_3["馬複人気順15番"]> 34 &
CancelTanshoNin1_3["馬複人気順15番"]< 53 &
CancelTanshoNin1_3["20以下数"]< 8 &
CancelTanshoNin1_3["50以上数"]< 7 )
BFKekkaNeraiStart <- 1
BFKekkaNeraiEnd   <- 3
checked1 <- kekkaCheckWide(TanshoNin1_3,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(TanshoNin1_3,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(TanshoNin1_3,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))
length(TanshoNin1_3[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(TanshoNin1_3[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(TanshoNin1_3[,1])
#____________________________________________________________________
######⑥
BahukuNin1_4<-subset(CancelBahukuNin1_4,CancelBahukuNin1_4["馬複人気順15番"]> 41 &
CancelBahukuNin1_4["馬複人気順11番"]> 24 &
CancelBahukuNin1_4["馬複人気順10番"]> 21 &
CancelBahukuNin1_4["馬複人気順5番"]> 13.3 &
CancelBahukuNin1_4["馬複人気順4番"]> 8.8 &
CancelBahukuNin1_4["馬複人気順3番"]< 13 &
CancelBahukuNin1_4["馬複人気順1番"]> 2 &
CancelBahukuNin1_4["単勝人気順1番"]< 3 &
CancelBahukuNin1_4["単勝人気順2番"]> 4.5 &
CancelBahukuNin1_4["単勝人気順2番"]< 6.4 &
CancelBahukuNin1_4["単勝人気順3番"]> 5.1 &
CancelBahukuNin1_4["単勝人気順3番"]< 10 &
CancelBahukuNin1_4["単勝人気順4番"]< 18 &
CancelBahukuNin1_4["単勝人気順5番"]< 28 &
CancelBahukuNin1_4["20以下数"]> 3 )
sub<-subset(BahukuNin1_4,BahukuNin1_4["馬複人気順"]>1& BahukuNin1_4["馬複人気順"]<4)
sum(sub["馬複金額"]);sum(length(BahukuNin1_4[,1])) * 200
length(sub[,1]);length(BahukuNin1_4[,1]);length(sub[,1])/length(BahukuNin1_4[,1])

#____________________________________________________________________
######未対応
BahukuNin3_7<-subset(BahukuNin4_6,BahukuNin4_6["場所"]!= "大井" &
BahukuNin4_6["場所"]!="浦和" &
BahukuNin4_6["70以上数"]< 3 &
BahukuNin4_6["馬複人気順8番"]< 21 & 
BahukuNin4_6["馬複人気順3番"]< 12.5 & 
BahukuNin4_6["馬複人気順3番"]> 10.5 )

sub<-subset(sub2,sub2["馬複人気順"]>3& sub2["馬複人気順"]<7)
sum(sub["馬複金額"]);length(sub2[,1]) * 300
length(sub[,1]);length(sub2[,1]);length(sub[,1])/length(sub2[,1])
konyuTosu <- 3

#____________________________________________________________________
######⑦
BahukuNin2_5<-subset(CancelBahukuNin2_5,CancelBahukuNin2_5["馬複人気順2番"]< 7 &
CancelBahukuNin2_5["50以上数"]> 1 & 
CancelBahukuNin2_5["馬複人気順7番"]< 15.5 )

BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 5
checked1 <- kekkaCheckWide(BahukuNin2_5,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin2_5,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin2_5,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))
length(BahukuNin2_5[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin2_5[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin2_5[,1])


#____________________________________________________________________
######⑧
BahukuNin20_31<-subset(CancelBahukuNin20_31,CancelBahukuNin20_31["馬複人気順9番"]> 24.5 & 
CancelBahukuNin20_31["馬複人気順1番"]> 5.5 &
CancelBahukuNin20_31["50以上数"]< 4 )
BFKekkaNeraiStart <- 20
BFKekkaNeraiEnd   <- 31
checked1 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(BahukuNin20_31,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))
length(BahukuNin20_31[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(BahukuNin20_31[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(BahukuNin20_31[,1])


#____________________________________________________________________
######⑨無条件##################################
#単勝3,4,5番人気。キャンセル使わない。
TanshoNin2_6<-subset(UmahukuMat,UmahukuMat["100以上数"]> 6 )
BFKekkaNeraiStart <- 2
BFKekkaNeraiEnd   <- 6
checked1 <- kekkaCheckWide(TanshoNin2_6,BFKekkaNeraiStart,BFKekkaNeraiEnd,1)
checked2 <- kekkaCheckWide(TanshoNin2_6,BFKekkaNeraiStart,BFKekkaNeraiEnd,2)
checked3 <- kekkaCheckWide(TanshoNin2_6,BFKekkaNeraiStart,BFKekkaNeraiEnd,3)
(sum(checked1 ["ワイド1金額"]) + sum(checked2 ["ワイド2金額"]) + sum(checked3 ["ワイド3金額"]))
length(TanshoNin2_6[,1]) * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1) *100
(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]));length(TanshoNin2_6[,1]);(length(checked1 [,1])+length(checked2 [,1])+length(checked3 [,1]))/length(TanshoNin2_6[,1])
