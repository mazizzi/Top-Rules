
x<-sapply(rules.parsedwest,"[[",3)
x<-x[which(x!="NA")]

z1west<-which(x %in% coc.exmod$Supplier.Parent.Name)
z2west<-which(x %in% lib.vanilla$Supplier.Parent.Name)
zzzwest<-unique(append(z1west,z2west))
finalrules.west<-rules.parsedwest[c(zzzwest)]

z<-c("VWR INTERNATIONAL INC", "SUN LIFE ASSOCIATION","ANIXTER INC")
zindex<-c(3,50,92)

xyz<-rules.parsed.test[zindex]








reducematch<-function(sampleset,i){
  
  #pastes all text from a row of the data file
  dummy.paste<-paste(sampleset[1,,]$Supplier.Parent.Name,sampleset[1,,]$Detail.Description,sampleset[1,,]$ORGN.Title,sampleset[1,,]$Account.Title,sampleset[1,,]$Fund.Title,sampleset[1,,]$Invoice.Description,sampleset[1,,]$Commodity.Description ,sep=" ")
  # j  for #entries in keywords lists
  metric.matrix<-matrix(nrow=length(xyz),ncol = 8)
  
  for(j in(1:length(xyz))){
    
    if(xyz[[j]][[3]]!=c("NA")){
      #metric.matrix<-matrix(nrow=length(xyz),ncol = 4)
      
      if(grepl(xyz[[j]][[3]], ignore.case = TRUE ,as.character(sampleset$Supplier.Parent.Name))){
        
        metric.matrix[j,1]<-c(1)
        metric.matrix[j,4]<-xyz[[j]][[2]]
        metric.matrix[j,5]<-taxmap$checked.list.redux.idnum2[which(taxmap$checked.list.redux.idnum1 %in% metric.matrix[j,4])]
        metric.matrix[j,6]<-as.character(coc.exmod$Category.Code[i])
        metric.matrix[j,7]<-j
        metric.matrix[j,8]<-i
        # k for length of each entry in keywords list
        if(length(xyz[[j]][-c(1,2,3,4)])!=0){
          for(k in (1:length(xyz[[j]][-c(1,2,3,4)]))){
            if (agrepl("Does Not Contain",xyz[[j]][k+3],ignore.case = TRUE )){
              ##use strsplit to gain keyword part of does not contain, grep phrase for part and if match, disqualify
              #print("anti")
              antiphrase.split<-strsplit(xyz[[j]][k+3],"Contain ")
              antiphrase<-antiphrase.split[[1]][2]
              #disqualify if antiphrase matches
              if(agrepl(antiphrase,ignore.case = TRUE, dummy.paste)){
                break
              }
            }
            #check that keyword[j,k] is not empty
            else if((xyz[[j]][[k+3]])==c("")){
              #print("book")
              next}
            
            else if(grepl(xyz[[j]][[k+3]],dummy.paste,fixed=TRUE)){
              
              metric.matrix[j,2]<-c(1)
              # metric.matrix[j,4]<-xyz[[j]][[2]]
            }
            else if(agrepl(xyz[[j]][[k+3]],dummy.paste,ignore.case = TRUE)){
              
              metric.matrix[j,3]<-c(1)
              #metric.matrix[j,6]<-xyz[[j]][[2]]
            }  
            
            
            
            
          } #end k loop 
          
        } #end first run grep parent
      }    
    }
  }
  if(all(is.na(metric.matrix))){
  }
  else{
    return(metric.matrix[!sapply(metric.matrix,is.na)])
  }
}


t<-proc.time()
i=1
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

coc.matches<-foreach(i=1:51863) %dopar%{
  reducematch(coc.exmod[i,],i)
}

t-proc.time()
stopCluster(cl)

coc.matches[!sapply(coc.matches,is.null)]


