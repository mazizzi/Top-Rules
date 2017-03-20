## Charleston Top Rules



setwd("~/mlearn jaggaer/Jaggaer_machine-learning")
#coc.vanilla<-read.csv(file="Charleston_Export.csv")
#save(coc.vanilla,file="coc_vanilla.RData")
load("coc_ex_vanilla.RData")

cocrules<-read.csv(file="Charleston_Rules_Export.csv")
coc.toprules<-read.csv("Charleston_toprules.csv")


coc.toprulesID<-na.omit(coc.toprules[,1])
coc.ruleindex<-which(cocrules$Rule.ID %in% coc.toprulesID)

coc.toprules<-cocrules[c(coc.ruleindex),]
coc.toprules<-coc.toprules[,c(1,2,9,10,11,12,13,14)]

rules.parsedcoc<-vector('list',length=200)
for (i in 1:nrow(coc.toprules)){
  
  
  if(length(grep("Supplier Parent Name Contains ",apply(coc.toprules[i,],1, as.character)))!=0){
    rules.parsedcoc[[i]][[1]]<-coc.toprules[i,1]
    
    rules.parsedcoc[[i]][[2]]<-gsub("-","",as.character(coc.ex.vanilla$Category.Code[match(coc.toprules[i,1],coc.ex.vanilla$Rule.ID)]))
    
    rules.parsedcoc[[i]][[3]]<-coc.toprules[i,2]
    rules.parsedcoc[[i]][[4]]<-gsub("Supplier Parent Name Contains ","",coc.toprules[i,(grep("Supplier Parent Name Contains ",value=FALSE,apply(coc.toprules[i,],1, as.character)))]) #will break if more than one supplier parent rule
    for (j in 1:6){
      if((coc.toprules[i,j+2] != c("")) && (!grepl("Supplier Parent Name Contains ", coc.toprules[i,j+2]))){
        rules.parsedcoc[[i]][[j+4]]<-as.character(coc.toprules[i,j+2])
      }
    }
    
  }
  
}
rules.parsedcoc<-rules.parsedcoc[!sapply(rules.parsedcoc, is.null)]

load("C:/Users/mzizzi/Documents/mlearn jaggaer/vanilla_westonex.RData")

x<-sapply(rules.parsedcoc,"[[",4)
x<-x[which(x!="NA")]

z1coc<-which(x %in% (lib.vanilla$Supplier.Parent.Name))
z2coc<-which(x %in% (vanilla.westonex$Supplier.Parent.Name))
zzzcoc<-unique(append(z1coc,z2coc))

finalrules.coc<-rules.parsedcoc[c(zzzcoc)]

finalrules.all<-append(finalrules.coc,finalrules.lib)
finalrules.all<-append(finalrules.all,finalrules.west)
#save(finalrules.all,file="final_rules_all.RData")
finalrules.all.copy<-finalrules.all

#Clean final rules
for (i in 1:length(finalrules.all)){
  for (j in 1:length(finalrules.all[[i]])){
    if(length(grep('Is Blank|Is Not Blank',value=FALSE,finalrules.all[[i]][j+4]))!=0){
      finalrules.all[[i]]<-finalrules.all[[i]][-(j+4)]
    }
    else{}
  }
  
}
## check head to make sure entries were fixed, may need run twice?
#head(finalrules.all)

##remove contains
for (i in 1:length(finalrules.all)){
  for (j in 1:length(finalrules.all[[i]])){
    if(length(grep('Contains',value=FALSE,finalrules.all[[i]][j+4]))!=0){
      dummy<-strsplit(finalrules.all[[i]][j+4],"Contains ")
      finalrules.all[[i]][j+4]<-dummy[[1]][2]
    }
    else{}
  }
  
}


##remove missing
for (i in 1:length(finalrules.all)){
  for (j in 1:length(finalrules.all[[i]][-c(1,2,3)])){
    if(is.na(finalrules.all[[i]][j+3])==TRUE){
      finalrules.all[[i]]<-finalrules.all[[i]][-(j+3)]
    }
    else{finalrules.all[[i]]<-finalrules.all[[i]]}
  }
  
}

#save(finalrules.all,file="final_rules_all.RData")
finalrules.coc<-finalrules.all[c(1:48)]
finalrules.lib<-finalrules.all[c(49:68)]
finalrules.west<-finalrules.all[c(69:76)]

