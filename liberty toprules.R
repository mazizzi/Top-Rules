## Liberty Top Rules Matching##

#Author: Mike Zizzi

setwd("~/mlearn jaggaer/Jaggaer_machine-learning")
#lib.vanilla<-read.csv(file="Liberty_Export.csv")
#save(lib.vanilla,file="lib_vanilla.RData")
load("lib_vanilla.RData")

librules<-read.csv(file="Liberty_Rules_Export.csv")
lib.toprules<-read.csv(file="Liberty_Top_Rules.csv")

lib.toprulesID<-na.omit(lib.toprules[,1])
lib.ruleindex<-which(librules$Rule.ID %in% lib.toprulesID)

lib.toprules<-librules[c(lib.ruleindex),]
lib.toprules<-lib.toprules[,c(1,2,9,10,11,12,13,14)]

rules.parsedlib<-vector('list',length=200)
for (i in 1:nrow(lib.toprules)){
 
  if(length(grep("Supplier Parent Name Contains ",apply(lib.toprules[i,],1, as.character)))!=0){
    rules.parsedlib[[i]][[1]]<-lib.toprules[i,1]
    
    rules.parsedlib[[i]][[2]]<-gsub("-","",as.character(lib.vanilla$Category.Code[match(lib.toprules[i,1],lib.vanilla$Rule.ID)]))
    
    rules.parsedlib[[i]][[3]]<-lib.toprules[i,2]
    
    rules.parsedlib[[i]][[4]]<-gsub("Supplier Parent Name Contains ","",lib.toprules[i,(grep("Supplier Parent Name Contains ",value=FALSE,apply(lib.toprules[i,],1, as.character)))]) #will break if more than one supplier parent rule
    for (j in 1:6){
      if((lib.toprules[i,j+2] != c("")) && (!grepl("Supplier Parent Name Contains ", lib.toprules[i,j+2]))){
        rules.parsedlib[[i]][[j+4]]<-as.character(lib.toprules[i,j+2])
      }
    }
    
  }
  
}

rules.parsedlib<-rules.parsedlib[!sapply(rules.parsedlib, is.null)]
load("C:/Users/mzizzi/Documents/mlearn jaggaer/vanilla_westonex.RData")

x<-sapply(rules.parsedlib,"[[",4)
x<-x[which(x!="NA")]

z1lib<-which(x %in% (coc.exmod$Supplier.Parent.Name))
z2lib<-which(x %in% (vanilla.westonex$Supplier.Parent.Name))
zzzlib<-unique(append(z1,z2))

finalrules.lib<-rules.parsedlib[c(zzzlib)]



