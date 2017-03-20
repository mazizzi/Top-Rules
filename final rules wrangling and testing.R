
library(stringr)

abc<-matrix(data=unique(sapply(finalrules.coc,"[[",2)),ncol=1)

aaa<-matrix(taxmap[which(taxmap$checked.list.redux.idnum2 %in% abc[,1] ),2],ncol=1)


for(i in 1:36){
  if(abc[i,1] %in% taxmap$checked.list.redux.idnum2){
    abc[which(!(abc[,1] %in% taxmap$checked.list.redux.idnum2)),]
    print(i)
  }
  
  
}


abc<-matrix(data=unique(sapply(finalrules.lib,"[[",2)),ncol=1)
abc

toprule.taxmap<-read.csv("west coc lib taxmatch.csv")
toprule.taxmap<-toprule.taxmap[1:56,]
toprule.taxmap<-as.character(toprule.taxmap)

bracketremove<-function(element){
  element<-as.character(element)
  if(length(grep(']',element))!=0){
    dummy<-strsplit(element,  split="\\]")
    dummy2<-dummy[[1]][2]
    dummy2<-gsub(" ","",dummy2)
    return(dummy2)
  }
  else{return (element)}
}


for(i in 1:56){
  for (j in 1:3){
    if(is.na(toprule.taxmap[i,j])){next}
    else(toprule.taxmap[i,j]<-as.character(toprule.taxmap[i,j]))
  }
}


new.topruletaxmap<-matrix(nrow=56,ncol=3)
for(i in 1:56){
  for (j in 1:3){
    if(is.na(toprule.taxmap[i,j])){next}
    else(new.topruletaxmap[i,j]<-bracketremove(toprule.taxmap[i,j]))
  }
}

new.topruletaxmap
final.toptaxmap<-new.topruletaxmap
final.toptaxmap<-as.data.frame(final.toptaxmap)
save(final.toptaxmap,file="final_toptaxmap.RData")








