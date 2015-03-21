
getAllcits <- function(){
  citys=read.delim2("expb.txt") # c=7 , b =6
  citys=unique(citys[,2]);
  a=lapply(citys,getCityKalp);
  return(do.call(rbind.data.frame,a));
}

getCityKalp <- function(cityID){
  html.raw<-htmlTreeParse(
    paste0("http://votes20.gov.il/ballotresults?cityID=",cityID),
    useInternalNodes=T
  )
  html.parse<-xpathApply(html.raw, "//option", xmlValue)
  toParse=html.parse[[2]][1];
  html.parse=html.parse[-(1:1198)]
  res=t(sapply(html.parse,parseKalp));
  colnames(res)=c("PollingStationName","PollingStationID");
  return(cbind(cityID=cityID,res));
}

parseKalp <- function(x){
  words=unlist(strsplit(x," "));
  num=as.numeric(tail(words,1));
  name=paste(words[-((length(words)-2):length(words))],collapse = " ");
  return(append(name,num));
}
