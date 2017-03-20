### Run rules against individual Weston, CoC, Liberty ###

#Author: Mike Zizzi
library(doParallel)
library(foreach)
library(iterators)
library(data.table)

#create subset of data where the rules will actually  (hopefully) apply#
sub.west1<-vanilla.westonex[which(vanilla.westonex$Supplier.Parent.Name %in% sapply(finalrules.coc,"[[",4) ),]
sub.west2<-vanilla.westonex[which(vanilla.westonex$Supplier.Parent.Name %in% sapply(finalrules.lib,"[[",4) ),]
sub.west<-rbind(sub.west1,sub.west2)

sub.coc1<-coc.ex.vanilla[which(coc.ex.vanilla$Supplier.Parent.Name %in% sapply(finalrules.west,"[[",4) ),]
sub.coc2<-coc.ex.vanilla[which(coc.ex.vanilla$Supplier.Parent.Name %in% sapply(finalrules.lib,"[[",4) ),]
sub.coc <-rbind(sub.coc1,sub.coc2)

sub.lib1<-lib.vanilla[which(lib.vanilla$Supplier.Parent.Name %in% sapply(finalrules.west,"[[",4) ),]
sub.lib2<-lib.vanilla[which(lib.vanilla$Supplier.Parent.Name %in% sapply(finalrules.coc,"[[",4) ),]
sub.lib<-rbind(sub.lib1,sub.lib2)

rm(sub.lib1,sub.lib2,sub.coc1,sub.coc2,sub.west1,sub.west2)



toprules_sampler<-function(sampleset,client,i){
  if(client=="west")    {dummy.paste<-paste(sampleset[1,,]$Account.Description,sampleset[1,,]$Account.Code,
                                            sampleset[1,,]$PO.Line.Description,sampleset[1,,]$Commodity.Code.Description,
                                            sampleset[1,,]$Description,sampleset[1,,]$MCC.Description,
                                            sampleset[1,,]$Faculty.or.Unit.ID,sampleset[1,,]$Expense.Type,
                                            sampleset[1,,]$Invoice.Line.Description,sampleset[1,,]$Supplier.Parent.Name ,sep=" ")

   clientnum=1
   clientsum=sum(vanilla.westonex$Spend.Amount,na.rm = TRUE)
   rulelist<-append(finalrules.coc,finalrules.lib)
   } #end west
  else if(client=="coc"){dummy.paste<-paste(sampleset[1,,]$Supplier.Parent.Name,sampleset[1,,]$Detail.Description,
                                            sampleset[1,,]$ORGN.Title,sampleset[1,,]$Account.Title,sampleset[1,,]$Fund.Title,
                                            sampleset[1,,]$Invoice.Description,sampleset[1,,]$Commodity.Description ,sep=" ")
  clientnum=2
  clientsum=sum(coc.ex.vanilla$Spend.Amount,na.rm = TRUE)
  rulelist<-append(finalrules.west,finalrules.lib)
  } #end coc 
  
  else if(client=="lib"){dummy.paste <-paste(sampleset[1,,]$Supplier.Parent.Name,sampleset[1,,]$Account.Description,
                                            sampleset[1,,]$Description,sampleset[1,,]$Invoice.Line.Description,
                                            sampleset[1,,]$Transaction.Description,sampleset[1,,]$Commodity.Description,
                                            sampleset[1,,]$Account.Title)
  clientnum=3 
  clientsum=sum(lib.vanilla$Spend.Amount,na.rm = TRUE)
  rulelist<-append(finalrules.west,finalrules.coc)
  } #end lib
  
  dummymatrix<-matrix(nrow=length(rulelist),ncol=9)
  for(j in 1:length(rulelist)){
    hit.counter<-0
    parent.counter<-c(0)
    if(agrepl(rulelist[[j]][4], as.character(sampleset[1,,]$Supplier.Parent.Name))){
      parent.counter<-parent.counter+1
      if(length(rulelist[[j]])>4){
        for(k in 1:length(rulelist[[j]][-c(1:4)])){
            if (length(agrep("Does Not Contain",rulelist[[j]][k+4],ignore.case = TRUE ))!=0){
              antiphrase.split<-strsplit(rulelist[[j]][k+4],"Contain ")
              antiphrase<-antiphrase.split[[1]][2]
              #disqualify if antiphrase matches
              if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
                break
              }
            }
            #check that keyword[j,k] is not empty
            else if((rulelist[[j]][[k+4]])==c("")){
              #print("book")
              next}
            
            else if(length(grep(rulelist[[j]][[k+4]],dummy.paste,fixed=TRUE))!=0){
              hit.counter<-hit.counter+1
              
            }
            else if(length(agrep(rulelist[[j]][[k+4]],dummy.paste,ignore.case = TRUE))!=0){
              hit.counter<-hit.counter+1
            }  
          
        } #end k loop
        
          
      } #end if rulelist > 4
    } # end if agrep parent
    if((parent.counter==0) && (hit.counter == 0)){next} #dont enter non matches
     dummymatrix[j,1]=client
     dummymatrix[j,2]<-i
     dummymatrix[j,3]<-parent.counter
     dummymatrix[j,4]<-hit.counter
     dummymatrix[j,5]=rulelist[[j]][2]
     dummymatrix[j,6]=gsub("-","",as.character(sampleset$Category.Code))
     dummymatrix[j,7]=sampleset[1,,]$Spend.Amount/clientsum
     dummymatrix[j,9]=rulelist[[j]][1]
     if(client=="west"){
       if(j<49){dummymatrix[j,8]="coc"}
       else(dummymatrix[j,8]="lib")
     }
     else if(client=="coc"){
       if(j<9){
         dummymatrix[j,8]="west"
       }
     else(dummymatrix[j,8]="lib")
     }
     else if(client=="lib"){
       if(j<9){
         dummymatrix[j,8]="west"
       }
       else(dummymatrix[j,8]="coc")
     }
     
  } # end j loop
  if(sum(as.numeric(dummymatrix[,3]),na.rm = TRUE)>0){
  return(dummymatrix)}
  
} # end function  

### WEST ### hour
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
t<-proc.time()
west.test<-foreach(i= 1:nrow(sub.west)) %dopar%{
  toprules_sampler(sub.west[i,],"west",i)
}
proc.time()-t
stopCluster(cl)

### COC ### fast
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
t<-proc.time()
coc.test<-foreach(i= 1:nrow(sub.coc)) %dopar%{
  toprules_sampler(sub.coc[i,],"coc",i)
}
proc.time()-t
stopCluster(cl)

### LIBERTY ### 33 min
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
t<-proc.time()
lib.test<-foreach(i= 1:nrow(sub.lib)) %dopar%{
  toprules_sampler(sub.lib[i,],"lib",i)
}
proc.time()-t
stopCluster(cl)

####### COMBINE AND CONQUER #########

x<-lapply(west.test, function(x) na.omit(x))
west.final<-do.call(rbind,x)

sum(as.numeric(west.final[,7]))
#0.05056167

y<-lapply(coc.test, function(x) na.omit(x))
coc.final<-do.call(rbind,y)

sum(as.numeric(coc.final[,7]))
#0.03473489

z<-lapply(lib.test, function(x) na.omit(x))
lib.final<-do.call(rbind,z)

sum(as.numeric(lib.final[,7]))
# 0.07802922


###############  ACCURACY  ###############

### WEST ###
west.finalredux<-matrix(nrow = nrow(west.final),ncol=ncol(west.final))
runsum<-c(0)
final.toptaxmap<-as.data.frame(final.toptaxmap)
for(i in 1:nrow(west.finalredux)){
  if(west.final[i,8]=="coc"){
    if(west.final[i,6]==as.character(final.toptaxmap$V1[which(final.toptaxmap$V2 %in% west.final[i,5])])){
       runsum<-runsum+as.numeric(west.final[i,7])}
  }
  else(
    if(west.final[i,6]==as.character(final.toptaxmap$V1[which(final.toptaxmap$V3 %in% west.final[i,5])])){
    runsum<-runsum+as.numeric(west.final[i,7])})
}
runsum #0.005196361

### COC ###
runsum<-c(0)
for(i in 1:nrow(coc.final)){
  if(coc.final[i,6]==""){next}
  if(coc.final[i,8]=="west"){
    if(length(as.character(final.toptaxmap$V2[which(final.toptaxmap$V1 %in% coc.final[i,5])]))==0){next}
    if(coc.final[i,6]==as.character(final.toptaxmap$V2[which(final.toptaxmap$V1 %in% coc.final[i,5])][1])){
      runsum<-runsum+as.numeric(coc.final[i,7])}
  }
  else(
    if(coc.final[i,6]==as.character(final.toptaxmap$V2[which(final.toptaxmap$V3 %in% coc.final[i,5])][1])){
      runsum<-runsum+as.numeric(coc.final[i,7])})
}
runsum #0.01662671


### liberty ###
runsum<-c(0)
for(i in 1:nrow(lib.final)){
  if(lib.final[i,6]==""){next}
  if(lib.final[i,8]=="west"){
    if(length(as.character(final.toptaxmap$V3[which(final.toptaxmap$V1 %in% lib.final[i,5])]))==0){next}
    if(lib.final[i,6]==as.character(final.toptaxmap$V3[which(final.toptaxmap$V1 %in% lib.final[i,5])])){
      runsum<-runsum+as.numeric(lib.final[i,7])}
  }
  else(
    if(lib.final[i,6]==as.character(final.toptaxmap$V3[which(final.toptaxmap$V2 %in% lib.final[i,5])])[1]){
      runsum<-runsum+as.numeric(lib.final[i,7])})
}
runsum #0.01622154


####################
mismatch.coc<-matrix(nrow = nrow(coc.final),ncol=ncol(coc.final))
runsum<-c(0)
for(i in 1:nrow(coc.final)){
  if(coc.final[i,6]==""){next}
  if(coc.final[i,8]=="west"){
    if(length(as.character(final.toptaxmap$V2[which(final.toptaxmap$V1 %in% coc.final[i,5])]))==0){next}
    else if(coc.final[i,6] %in% as.character(final.toptaxmap$V2[which(final.toptaxmap$V1 %in% coc.final[i,5])])){
      runsum<-runsum+as.numeric(coc.final[i,7])}
    else(mismatch.coc[i,]<-coc.final[i,])
  }
  else(
    if(length(as.character(final.toptaxmap$V2[which(final.toptaxmap$V2 %in% coc.final[i,5])]))==0){next}
    else if(coc.final[i,6] %in% as.character(final.toptaxmap$V2[which(final.toptaxmap$V3 %in% coc.final[i,5])])){
      runsum<-runsum+as.numeric(coc.final[i,7])}
    else(mismatch.coc[i,]<-coc.final[i,]))
}
runsum #0.01622671
head(mismatch.coc)
sum(as.numeric(mismatch.coc[,7]), na.rm = TRUE)
mismatch.coc<-na.omit(mismatch.coc)
head(mismatch.coc)











