hoji<-as.numeric(checkMat_Shisu[5,])*20
#hjt13 + hjt37 + hjt410 + hjb1429
i<-12
j<-15
k<-14
l<-6
s<-6
hojiSum <- i * hjt37 + j * hjt410 + k * hjb1429+ l * hjt24 + s*hjb26
table(hojiSum > 0)

plot(hojiSum,type="l",col="red")
abline( h =0 ,col="red")
abline( h =10000 ,col="red")
abline( h =5000 ,col="red",lty="dotted")
abline( h =-5000 ,col="red",lty="dotted")
abline( h =-10000 ,col="red")
axis(side=1, at=1:lenHiduke , labels=checkMat_Shisu[1,])

#hojiSum <- hjt24 * 10
#sum(subset(hojiSum ,hojiSum <0))
par(new=T)
lines(i * hjt37,col="green")
lines(j * hjt410,col="blue")
lines(k * hjb1429,col="yellow")
lines(l * hjt24,col="black")
lines(s * hjb26,col="orange")

idx<-c(4,6,8,10,12,14,16,18,20,22,24,26,28)
for( i in idx){
  for(j in idx){
      for(l in idx){
	hojiSum <- i * hjt37 + 20 * hjt410 + 14 * hjb1429 + l * hjt24
	if(table(hojiSum < -5000)[[1]]!=103 ){
	if(table(hojiSum < -5000)[[2]]<3 ){
	print(table(hojiSum > 0))
	print(sum(hojiSum))
	print("i,j,l")
	print(i)
	print(j)
	print(l)
	}
	}
	}
    }
}
