setwd("f:/votes/")
x=read.delim2("expc.txt",stringsAsFactors = F) # c=7 , b =6
colnames(x)[2:3]=c("cityID","PollingStationID");
cityPoll=read.table("cityPoll.txt",stringsAsFactors = F);
xx=t(apply(x[,-(1:7)],1,function(x){return(log(1+x/sum(x)))}))
x=merge(x,cityPoll,by=c("cityID","PollingStationID"),all.x=TRUE,sort=F)
cenNum=20;
km=kmeans(xx,cenNum)
farDist=km$centers[km$cluster,];
farDD=rep(0,nrow(xx));
for (i in (1:nrow(xx))){
  farDD[i]=dist(rbind(xx[i,],farDist[i,]))
}
weird=order(farDD,decreasing = T)[1:20];
png(filename = "weird.png",width = 2500,height = 2500)
par(mfrow=c(cenNum/2,4))
for (i in (weird)){
  others=xx[which(x$PollingStationName==x$PollingStationName[i] & x$cityID==x$cityID[i] & x$PollingStationID!=x$PollingStationID[i]),];
  if (!is.null(nrow(others)) && nrow(others)>1){
    othersMean=colMeans(others);
  }else{
    othersMean=others;
    if (sum(others)==0){
      othersMean=xx[i,];
    }
  }
  pie(othersMean,col=(rainbow(ncol(xx))),main=paste(x[i, 34],"Others",nrow(others),sep="-"))
  print(nrow(others))
  pie(xx[i,],col=(rainbow(ncol(xx))),main=paste(x[i, 2],x[i, 3],sep="-"))
}
dev.off();




#plot
prec=round(table(km$cluster)/sum((km$cluster)),4);
cent=km$centers;
cent=cent[order(prec),];
prec=sort(prec);
png(filename = "test.png",width = 2500,height = 2500)
par(mfrow=c(cenNum/4,4))
for (i in (1:nrow(km$centers))){
  pie(cent[i,],col=(rainbow(ncol(cent))),main=prec[i])
}
dev.off();
