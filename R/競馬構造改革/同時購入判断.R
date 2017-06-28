#同時購入判断馬複

TF<-c(TRUE,FALSE)
dojiKonyuMat<-UmahukuMat
KekkaKeepMat<-matrix("",64,5)
idx<-1

for (tf1 in 1:2){
  FlagBF1 <- TF[tf1]
  mat1<-BFKansu1(FlagBF1 ,UmahukuMat)

  for (tf2 in 1:2){
    FlagBF2 <- TF[tf2]
    mat2<-BFKansu2(FlagBF2 ,mat1)

    for (tf3 in 1:2){
      FlagBF3 <- TF[tf3]
      mat3<-BFKansu3(FlagBF3 ,mat2)

      for (tf4 in 1:2){
        FlagBF4 <- TF[tf4]
        mat4<-BFKansu4(FlagBF4 ,mat3)

        for (tf5 in 1:2){
          FlagBF5 <- TF[tf5]
          mat5<-BFKansu5(FlagBF5 ,mat4)

          for (tf6 in 1:2){
            FlagBF6 <- TF[tf6]
            mat6<-BFKansu6(FlagBF6 ,mat5)

		#処理開始
		keepRiekiritsu <- 1
            KekkaKeepMat[idx,1] <- paste(FlagBF1,FlagBF2,FlagBF3,FlagBF4,FlagBF5,FlagBF6,length(mat6[,1]))
		if(length(mat6[,1]) > 0){

		if(FlagBF1){
		  kekkaMat <- kekkaCheck(mat6,14,29) #関数①の範囲
		  if(is.null(kekkaMat)){kekkaKingaku <- 0
		  }else{kekkaKingaku  <- sum(kekkaMat["馬複金額"])}
		  Riekiritsu <- kekkaKingaku  /(length(mat6[,1]) * 1400)
		  if(keepRiekiritsu  < Riekiritsu & bfRiekiRitsu1 < Riekiritsu){
		    keepRiekiritsu  <- Riekiritsu 
		    KekkaKeepMat[idx,2] <- "1"
		    KekkaKeepMat[idx,3] <- kekkaKingaku  
    		    KekkaKeepMat[idx,4] <- (length(mat6[,1]) * 1400)
		    KekkaKeepMat[idx,5] <- Riekiritsu 
		  }
		}
		if(FlagBF2){
		  kekkaMat <- kekkaCheck(mat6,6,15)
		  if(is.null(kekkaMat)){kekkaKingaku <- 0
		  }else{kekkaKingaku  <- sum(kekkaMat["馬複金額"])}
		  Riekiritsu <- kekkaKingaku  /(length(mat6[,1]) * 800)
		  if(keepRiekiritsu  < Riekiritsu & bfRiekiRitsu2 < Riekiritsu){
		    keepRiekiritsu  <- Riekiritsu 
		    KekkaKeepMat[idx,2] <- "2"
		    KekkaKeepMat[idx,3] <- kekkaKingaku
    		    KekkaKeepMat[idx,4] <- (length(mat6[,1]) * 800)
		    KekkaKeepMat[idx,5] <- Riekiritsu 
		  }
		}
		if(FlagBF3){
		  kekkaMat <- kekkaCheck(mat6,6,15)
		  if(is.null(kekkaMat)){kekkaKingaku <- 0
		  }else{kekkaKingaku  <- sum(kekkaMat["馬複金額"])}
		  Riekiritsu <- kekkaKingaku/(length(mat6[,1]) * 800)
		  if(keepRiekiritsu  < Riekiritsu & bfRiekiRitsu3 < Riekiritsu){
		    keepRiekiritsu  <- Riekiritsu 
		    KekkaKeepMat[idx,2] <- "3"
		    KekkaKeepMat[idx,3] <- kekkaKingaku
    		    KekkaKeepMat[idx,4] <- (length(mat6[,1]) * 800)
		    KekkaKeepMat[idx,5] <- Riekiritsu 
		  }
		}
		if(FlagBF4){
		  kekkaMat <- kekkaCheck(mat6,0,3)
		  if(is.null(kekkaMat)){kekkaKingaku <- 0
		  }else{kekkaKingaku  <- sum(kekkaMat["馬複金額"])}
		  Riekiritsu <- kekkaKingaku/(length(mat6[,1]) * 200)
		  if(keepRiekiritsu  < Riekiritsu & bfRiekiRitsu4 < Riekiritsu){
		    keepRiekiritsu  <- Riekiritsu 
		    KekkaKeepMat[idx,2] <- "4"
		    KekkaKeepMat[idx,3] <- kekkaKingaku
    		    KekkaKeepMat[idx,4] <- (length(mat6[,1]) * 200)
		    KekkaKeepMat[idx,5] <- Riekiritsu 
		  }
		}
		if(FlagBF5){
		  kekkaMat <- kekkaCheck(mat6,2,7)
		  if(is.null(kekkaMat)){kekkaKingaku <- 0
		  }else{kekkaKingaku  <- sum(kekkaMat["馬複金額"])}
		  Riekiritsu <- kekkaKingaku/(length(mat6[,1]) * 400)
		  if(keepRiekiritsu  < Riekiritsu & bfRiekiRitsu5 < Riekiritsu){
		    keepRiekiritsu  <- Riekiritsu 
		    KekkaKeepMat[idx,2] <- "5"
		    KekkaKeepMat[idx,3] <- kekkaKingaku
    		    KekkaKeepMat[idx,4] <- (length(mat6[,1]) * 400)
		    KekkaKeepMat[idx,5] <- Riekiritsu 
		  }
		}
		if(FlagBF6){
		  kekkaMat <- kekkaCheck(mat6,2,7)
		  if(is.null(kekkaMat)){kekkaKingaku <- 0
		  }else{kekkaKingaku  <- sum(kekkaMat["馬複金額"])}
		  Riekiritsu <- kekkaKingaku/(length(mat6[,1]) * 400)
		  if(keepRiekiritsu  < Riekiritsu & bfRiekiRitsu6 < Riekiritsu){
		    keepRiekiritsu  <- Riekiritsu 
		    KekkaKeepMat[idx,2] <- "6"
		    KekkaKeepMat[idx,3] <- kekkaKingaku
    		    KekkaKeepMat[idx,4] <- (length(mat6[,1]) * 400)
		    KekkaKeepMat[idx,5] <- Riekiritsu 
		  }
		}
		
		}
		idx <- idx + 1
          }
        }
      }
    }
  }
}
KekkaKeepMat #複合効果が見られたもののみ表示している

#_____________________________________________________________________
#関数①のチェック
BFKansu1<-function(flag1,mat){
  if(flag1 & length(mat[,1]) > 0){
    BahukuNin14_29<-subset(mat,mat["馬複人気順12番"]> 26 & 
			mat["馬複人気順9番"]> 20 &
			mat["馬複人気順2番"]> 5.9 & 
			mat["馬複人気順1番"]> 3.4 & 
			mat["馬複人気順1番"]< 7.5 & 
			mat["単勝人気順4番"]> 6 &
			mat["単勝人気順5番"]> 8.9 &
			mat["100以上数"]< 4 )
    mat<-BahukuNin14_29
  }
  return(mat)
}
#関数②のチェック
BFKansu2<-function(flag2,mat){
  if(flag2& length(mat[,1]) > 0){
    mat<-subset( mat,mat["単勝人気順11番"]> 56 &
			mat["単勝人気順2番"]> 3 &
			mat["単勝人気順1番"]<2.7 &
			mat["馬複人気順6番"]> 14.9 &
			mat["馬複人気順12番"]> 28 & 
			mat["馬複人気順1番"]> 3.6 & 
			mat["馬複人気順1番"]< 6.8 )
  }
  return(mat)
}
#関数③のチェック
BFKansu3<-function(flag3,mat){
  if(flag3& length(mat[,1]) > 0){
    mat<-subset( mat,mat["単勝人気順12番"]< 1 &
			mat["単勝人気順2番"]> 3.1 &
			mat["馬複人気順15番"]< 80 &
			mat["馬複人気順6番"]> 15 &
			mat["馬複人気順6番"]< 19 &
			mat["馬複人気順4番"]> 8 &
			mat["馬複人気順3番"]> 6 & 
			mat["馬複人気順1番"]> 2.6 ) 
  }
  return(mat)
}
#関数④のチェック
BFKansu4<-function(flag4,mat){
  if(flag4& length(mat[,1]) > 0){
    mat<-subset( mat,mat["単勝人気順1番"]> 1.4 &
			mat["単勝人気順2番"]< 5.5 &
			mat["単勝人気順11番"]< 1 &
			mat["馬複人気順15番"]> 42 &
			mat["馬複人気順14番"]> 39 &
			mat["馬複人気順7番"]> 13 &
			mat["馬複人気順5番"]> 11 &	
			mat["馬複人気順1番"]> 2.7 )
  }
  return(mat)
}
#関数⑤のチェック
BFKansu5<-function(flag5,mat){
  if(flag5& length(mat[,1]) > 0){
    mat<-subset( mat,mat["馬複人気順1番"]> 1.8 &
			mat["単勝人気順1番"]> 1.1 &
			mat["単勝人気順4番"]< 19 &
			mat["単勝人気順5番"]> 7.9 &
			mat["単勝人気順6番"]< 48 &
			mat["単勝人気順9番"]> 36 &
			mat["単勝人気順10番"]< 1 )
  }
  return(mat)
}
#関数⑥のチェック
BFKansu6<-function(flag6,mat){
  if(flag6& length(mat[,1]) > 0){
    mat<-subset( mat,mat["30以下数"]> 4 &
			mat["20以下数"]< 7 &
			mat["馬複人気順1番"]> 2.2 &
			mat["馬複人気順6番"]< 16.3 &
			mat["馬複人気順13番"]> 29 &
			mat["単勝人気順5番"]> 7.9 &
			mat["単勝人気順9番"]> 36 &
			mat["単勝人気順10番"]< 300 )
  }
  return(mat)
}

