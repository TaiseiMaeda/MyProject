len<-length(UmahukuMat[,1])
umahukuIkasuMat <- matrix(0,len,18)
for (i in 1:len){
　　umahukuIkasuMat[i,1] <- as.character(UmahukuMat[i,1])
　 umahukuIkasuMat[i,2] <- UmahukuMat[i,2]
　　umahukuIkasuMat[i,3] <- as.character(UmahukuMat[i,3])
  #30番人気まで以下上を確認し、Matに格納する。
  for(j in 1:30){
	if(UmahukuMat[i,87 + 3*j] < 0.1){
	  umahukuIkasuMat [i,12] <- as.numeric(umahukuIkasuMat [i,12]) + 1 
	}else if(UmahukuMat[i,87 + 3*j] < 10){
	  umahukuIkasuMat [i,4] <- as.numeric(umahukuIkasuMat [i,4]) + 1 
	}else if(UmahukuMat[i,87 + 3*j] < 20){
	  umahukuIkasuMat [i,5] <- as.numeric(umahukuIkasuMat [i,5]) + 1 
	}else if(UmahukuMat[i,87 + 3*j] < 30){
	  umahukuIkasuMat [i,6] <- as.numeric(umahukuIkasuMat [i,6]) + 1 
	}else if(UmahukuMat[i,87 + 3*j] < 50){
	  umahukuIkasuMat [i,7] <- as.numeric(umahukuIkasuMat [i,7]) + 1 
	}else if(UmahukuMat[i,87 + 3*j] < 100){
	  umahukuIkasuMat [i,8] <- as.numeric(umahukuIkasuMat [i,8]) + 1 
	}else if(UmahukuMat[i,87 + 3*j] < 200){
	  umahukuIkasuMat [i,9] <- as.numeric(umahukuIkasuMat [i,9]) + 1 
	}else if(UmahukuMat[i,87 + 3*j] < 300){
	  umahukuIkasuMat [i,10] <- as.numeric(umahukuIkasuMat [i,10]) + 1 
	}else if(UmahukuMat[i,87 + 3*j] > 299.9){
	  umahukuIkasuMat [i,11] <- as.numeric(umahukuIkasuMat [i,11]) + 1 
	}
  }
}
umahukuIkasuMat[,13] <- as.numeric(umahukuIkasuMat [,4]) + as.numeric(umahukuIkasuMat [,5])
umahukuIkasuMat[,14] <- as.numeric(umahukuIkasuMat [,13]) + as.numeric(umahukuIkasuMat [,6])
umahukuIkasuMat[,15] <- as.numeric(umahukuIkasuMat [,14]) + as.numeric(umahukuIkasuMat [,7])
umahukuIkasuMat[,16] <- as.numeric(umahukuIkasuMat [,15]) + as.numeric(umahukuIkasuMat [,8])
umahukuIkasuMat[,17] <- as.numeric(umahukuIkasuMat [,11]) + as.numeric(umahukuIkasuMat [,10])
umahukuIkasuMat[,18] <- as.numeric(umahukuIkasuMat [,17]) + as.numeric(umahukuIkasuMat [,9])







