rm(list=ls())
library(tidyverse)
fromx_to_10<-function(bse,x){
  x%>%as.character()%>%strsplit("")%>%unlist%>%as.numeric()->sep_digits
  if(any(sep_digits>=bse)){stop(paste("error: ", sep_digits[sep_digits>=bse], "are not digits for the given base"))}
  base_exp<-(length(sep_digits)-1):0
  return(sum((bse^base_exp)*(sep_digits)))
}

fromx_to_10(3,23)

div_int<-function(bse,x){
  div_int<-x%/%bse
  res<-x-div_int*bse
  return(c(div_int,res))  
}

from10_to_x<-function(bse,x){
  newnumber<-NULL
  res_vect<-NULL
  flag<-T
  x0<-x
  while(flag){
    temp<-div_int(bse,x0)
    newnumber<-c(newnumber,temp[2])
    x0<-temp[1]
    res_vect<-c(res_vect,x0)
    if(x0<bse){flag<-F}
  }
  newnumber<-c(newnumber,res_vect[length(res_vect)])
  return(rev(newnumber)%>%paste(collapse="")%>%as.numeric())
}

from10_to_x(3,11)

fromx_to_10(5,1403)+fromx_to_10(5,424)
from10_to_x(5,342)

sum_b<-function(bse,x,y){
  fromx_to_10(bse,x)+fromx_to_10(bse,y)->sum_N
  return(from10_to_x(bse,sum_N))
}

prod_b<-function(bse,x,y){
  fromx_to_10(bse,x)*fromx_to_10(bse,y)->sum_N
  return(from10_to_x(bse,sum_N))
}

sum_b(5,1403,424)

prod_b(5,123,14)

prod_b(5,31,3)
