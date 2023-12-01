
#aggregate final tables

#Per algorithm

for (algo in c("M1","S2","S3","S5","S6")) {
  assign(paste0("D4_proportion_per_algorithm_pregnancy_",algo) ,fread(paste0(direxp,"/D4_proportion_per_algorithm_pregnancy_",algo,".csv")))
  fwrite(get(paste0("D4_proportion_per_algorithm_pregnancy_",algo)), file=paste0(toupload,"D4_proportion_per_algorithm_pregnancy_",algo,".csv"))
}

y_end<-"2018"

#Algo S1
D4_proportion_per_algorithm_pregnancy_S1 <- fread(paste0(direxp,"/D4_proportion_per_algorithm_pregnancy_S1.csv"))
D4_proportion_per_algorithm_pregnancy_S1<-D4_proportion_per_algorithm_pregnancy_S1[Year_at_pregnancy_start==paste0("2006-",y_end),]
fwrite(D4_proportion_per_algorithm_pregnancy_S1, file=paste0(toupload,"D4_proportion_per_algorithm_pregnancy_S1.csv"))

#Algo S4
D4_proportion_per_algorithm_pregnancy_S4 <- fread(paste0(direxp,"/D4_proportion_per_algorithm_pregnancy_S4.csv"))
D4_proportion_per_algorithm_pregnancy_S4<-D4_proportion_per_algorithm_pregnancy_S4[Year_at_pregnancy_start==paste0("2006-",y_end),]
fwrite(D4_proportion_per_algorithm_pregnancy_S4, file=paste0(toupload,"D4_proportion_per_algorithm_pregnancy_S4.csv"))

#Algo V1
D4_proportion_per_algorithm_pregnancy_V1 <- fread(paste0(direxp,"/D4_proportion_per_algorithm_pregnancy_V1.csv"))
D4_proportion_per_algorithm_pregnancy_V1<-D4_proportion_per_algorithm_pregnancy_V1[medication=="any gabapentinoids",]
fwrite(D4_proportion_per_algorithm_pregnancy_V1, file=paste0(toupload,"D4_proportion_per_algorithm_pregnancy_V1.csv"))

#Algo V2
D4_proportion_per_algorithm_pregnancy_V2 <- fread(paste0(direxp,"/D4_proportion_per_algorithm_pregnancy_V2.csv"))
D4_proportion_per_algorithm_pregnancy_V2<-D4_proportion_per_algorithm_pregnancy_V2[medication=="any gabapentinoids",]
fwrite(D4_proportion_per_algorithm_pregnancy_V2, file=paste0(toupload,"D4_proportion_per_algorithm_pregnancy_V2.csv"))


#Per component

for (comp in c("S3","S4","S5","S6")) {
  assign(paste0("D4_proportion_per_component_pregnancy_",comp) ,fread(paste0(direxp,"/D4_proportion_per_component_pregnancy_",comp,".csv")))
  fwrite(get(paste0("D4_proportion_per_component_pregnancy_",comp)), file=paste0(toupload,"D4_proportion_per_component_pregnancy_",comp,".csv"))
}


for (comp in c("M1","S1","S2")) {
  assign(paste0("D4_proportion_per_component_pregnancy_",comp) ,fread(paste0(direxp,"/D4_proportion_per_component_pregnancy_",comp,".csv")))
  tmp1<-get(paste0("D4_proportion_per_component_pregnancy_",comp))[component!="E" & component!="H",]
  tmp2<-get(paste0("D4_proportion_per_component_pregnancy_",comp))[component=="E" & medication=="any gabapentinoids",]
  tmp3<-get(paste0("D4_proportion_per_component_pregnancy_",comp))[component=="H" & medication=="any gabapentinoids",]
  tmp<-rbind(tmp1,tmp2,tmp3)
  
  fwrite(tmp, file=paste0(toupload,"D4_proportion_per_component_pregnancy_",comp,".csv"))
}
