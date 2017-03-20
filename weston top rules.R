## weston top rules ##

#Author Mike Zizzi
load("C:/Users/mzizzi/Documents/mlearn jaggaer/vanilla_westonex.RData")

west.toprules<-read.csv(file="weston_toprules.csv")

west.toprulesID<-na.omit(west.toprules[,1])
west.ruleindex<-which(westrules$Rule.ID %in% west.toprulesID)

westrules<-read.csv("~/mlearn/weston/westonrules.csv")

west.toprules<-westrules[c(west.ruleindex),]
west.toprules<-west.toprules[,c(1,2,9,10,11,12,13,14)]


rules.parsedwest<-vector('list',length=200)
for (i in 1:nrow(west.toprules)){
  
  if(length(grep("Supplier Parent Name Contains ",apply(west.toprules[i,],1, as.character)))!=0){
    rules.parsedwest[[i]][[1]]<-west.toprules[i,1]
    
    rules.parsedwest[[i]][[2]]<-gsub("-","",as.character(vanilla.westonex$Category.Code[match(west.toprules[i,1],vanilla.westonex$Rule.ID)]))
    
    rules.parsedwest[[i]][[3]]<-west.toprules[i,2]
    
    rules.parsedwest[[i]][[4]]<-gsub("Supplier Parent Name Contains ","",west.toprules[i,(grep("Supplier Parent Name Contains ",value=FALSE,apply(west.toprules[i,],1, as.character)))]) #will break if more than one supplier parent rule
    for (j in 1:6){
      if((west.toprules[i,j+2] != c("")) && (!grepl("Supplier Parent Name Contains ", west.toprules[i,j+2]))){
        rules.parsedwest[[i]][[j+4]]<-as.character(west.toprules[i,j+2])
      }
    }
    
  }
  
}

rules.parsedwest<-rules.parsedwest[!sapply(rules.parsedwest, is.null)]

x<-sapply(rules.parsedwest,"[[",4)
x<-x[which(x!="NA")]

z1west<-which(x %in% (coc.exmod$Supplier.Parent.Name))
z2west<-which(x %in% (lib.vanilla$Supplier.Parent.Name))
zzzwest<-unique(append(z1west,z2west))

finalrules.west<-rules.parsedwest[c(zzzwest)]
