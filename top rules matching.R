#### Top 200 Rules from Weston to CoC #####
#"That's not a spend, this is a spend"#

#Author: Mike Zizzi

load("coc_ex_vanilla.RData")
load("taxmap.RData")
load("C:/Users/mzizzi/Documents/mlearn/keywords_beta.RData")
#Pull top rules by spend and omit na rows
toprules<-read.csv("weston_toprules.csv")
toprules<-na.omit(toprules)

#pre filter by taxes already known to match 
xvec<-sapply(toprules$tax, function(x) x%in%taxmap$checked.list.redux.idnum1 )
rulevec<-toprules[c(xvec),1]

#select rows from westons rules
westonrules<-read.csv("C:/Users/mzizzi/Documents/mlearn/weston/westonrules.csv")
yvec<-which(westonrules$Rule.ID %in% rulevec)

#create table to run through rule parser function
westonrules.redux<-westonrules[c(yvec),c(1,9,10,11,12,13,14)]

ruleparser<-function(inputrow){
  rules.parsed<-list()
  rules.parsed[1]<-inputrow[1,1]
  
  rules.parsed[2]<-toprules$tax[which(toprules$Rule.ID %in% inputrow[1,1])]
  
  if(grepl("Supplier Parent Name Contains ",inputrow[1,2])){
    rules.parsed[3]<-gsub("Supplier Parent Name Contains ","",inputrow[1,2])
    for (j in 1:6){
      if(inputrow[1,j+2] != c("")){
        rules.parsed[j+3]<-as.character(inputrow[1,j+2])
      }
      else{break}
    }
    
  }
  else{
    rules.parsed[3]<-"NA"
    for (k in 2:6){
      if(length(inputrow[1,k+1]) !=0){
        rules.parsed[k+2]<-as.character(inputrow[1,k+2])
      }
      else{break}
    }
  }
  return(rules.parsed)
}


t<-proc.time()
i=1
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

westontopkeywords<-foreach(i=1:195) %dopar%{
  ruleparser(westonrules.redux[i,])
}

t-proc.time()
stopCluster(cl)


westrulecopy<-westontopkeywords
westontopkeywords<-westrulecopy
## Cleaning ##

for (i in 1:length(westontopkeywords)){
  for (j in 1:length(westontopkeywords[[i]])){
    if(grepl('Is Blank|Is Not Blank',westontopkeywords[[i]][j+3])){
      westontopkeywords[[i]]<-westontopkeywords[[i]][-(j+3)]
    }
    else{}
  }
}

for (i in 1:length(westontopkeywords)){
  for (j in 1:length(westontopkeywords[[i]])){
    if(grepl('Contains',westontopkeywords[[i]][j+3])){
      dummy<-strsplit(as.character(westontopkeywords[[i]][j+3]),"Contains ")
      westontopkeywords[[i]][j+3]<-dummy[[1]][2]
    }
    else{}
  }
  
}


rules.parsed<-vector('list',length=195)
for (i in 1:nrow(westonrules.redux)){
  rules.parsed[[i]][[1]]<-westonrules.redux[i,1]
  
  rules.parsed[[i]][2]<-toprules$tax[which(toprules$Rule.ID %in% westonrules.redux[i,1])]
  
  if(length(grep("Supplier Parent Name Contains ",value=FALSE,westonrules.redux[i,2]))!=0){
    rules.parsed[[i]][[3]]<-gsub("Supplier Parent Name Contains ","",westonrules.redux[i,2])
    for (j in 1:6){
      if(westonrules.redux[i,j+2] != c("")){
        rules.parsed[[i]][[j+3]]<-as.character(westonrules.redux[i,j+2])
      }
      else{break}
    }
    
  }
  else{
    rules.parsed[[i]][[3]]<-"NA"
    for (k in 1:6){
      if(length(westonrules.redux[i,k+1]) !=0){
        rules.parsed[[i]][[k+3]]<-as.character(westonrules.redux[i,k+1])
      }
      else{break}
    }
  }
}

rules.parsedwest<-rules.parsed

