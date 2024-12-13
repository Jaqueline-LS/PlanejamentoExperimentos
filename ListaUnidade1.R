library("ExpDes")
material<-data.frame(y=c(110, 1, 880, 495,
  7, 194, 157, 2, 
  1256, 4, 5276, 7040,
  5, 5307, 29, 178, 
  18, 4355, 10050, 2), trat=rep(c(1,2,3,4,5),c(4,4,4,4,4)))
modelo<-crd(material$trat, material$y)

m.hat<-mean(material$y)

mean(material$y[material$trat==1])
mean(material$y[material$trat==2])
mean(material$y[material$trat==3])
mean(material$y[material$trat==4])
mean(material$y[material$trat==5])
