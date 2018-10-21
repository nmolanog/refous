rm(list=ls())
library(tidyverse)

fromx_to_10_v2<-function(bse,x){
  if(x%>%str_detect("\\.")){
    x%>%as.character()%>%strsplit("\\.")%>%unlist->break_x
    break_x%>%map(~as.character(.)%>%strsplit("")%>%unlist%>%as.numeric())->sep_digits_radix
    sep_digits_radix%>%unlist()->sep_digits

    base_exp_1<-(length(sep_digits_radix[[1]])-1):0
    base_exp_2<-1:(length(sep_digits_radix[[2]]))
    base_exp<-c(base_exp_1,-base_exp_2)
  }else {
    x%>%as.character()%>%strsplit("")%>%unlist%>%as.numeric()->sep_digits
    if(any(sep_digits>=bse)){stop(paste("error: ", sep_digits[sep_digits>=bse], "are not digits for the given base"))}
    base_exp<-(length(sep_digits)-1):0
  }
  return(sum((bse^base_exp)*(sep_digits)))
  }
fromx_to_10_v2(3,112)
fromx_to_10_v2(3,112.12121121212)

from10_to_x_v2<-function(bse,x,dgts=15){
  x0<-x
  dec_part<-NULL
  if(x%>%str_detect("\\.")){
    x%>%as.character()%>%strsplit("\\.")%>%unlist->break_x
    break_x[2]%>%{paste0(".",.)}%>%as.numeric()->temp_dec
    for(i in 1:dgts){
      (temp_dec*bse)%>%as.character()%>%strsplit("\\.")%>%unlist->temp_split
      dec_part<-c(dec_part,temp_split[1])
      temp_dec<-temp_split[2]%>%{paste0(".",.)}%>%as.numeric()
    }
    x0<-as.numeric(break_x[1])
  }
  newnumber<-NULL
  res_vect<-NULL
  flag<-T
  while(flag){
    temp<-div_int(bse,x0)
    newnumber<-c(newnumber,temp[2])
    x0<-temp[1]
    res_vect<-c(res_vect,x0)
    if(x0<bse){flag<-F}
  }
  newnumber<-c(newnumber,res_vect[length(res_vect)])
  newnumber%>%rev %>%c(".",dec_part)->newnumber
  return(newnumber%>%paste(collapse="")%>%as.numeric())
}

from10_to_x_v2(5,54.124)%>%print(digits=20)

