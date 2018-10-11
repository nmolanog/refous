rm(list=ls())
list.of.packages <- c("pacman")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(pacman)
p_load(tidyverse)
p_load(conf.design)
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

div_b<-function(bse,x,y){
  fromx_to_10(bse,x)/fromx_to_10(bse,y)->sum_N
  return(from10_to_x(bse,sum_N))
}

####function to generate random exercises
random_bin<-function(bse,dgts,op){
  a<-sample(0:(bse-1),dgts,replace = T)
  b<-sample(0:(bse-1),dgts,replace = T)
  achr<-paste0(a,collapse = "")
  bchr<-paste0(b,collapse = "")
  if(op=="sum"){
    opchr<-"+"
    res2<-sum_b(bse,a,b)
  }else
    {
    opchr<-"*"
    res2<-prod_b(bse,a,b)  
    }
  res1<-paste0(achr,opchr,bchr)
  return(list(op=res1,res=res2))
}

library("conf.design")

factorize_bse<-function(bse,x){
  x_10<-fromx_to_10(bse,x)
  fprime<-conf.design::factorize(x_10)
  return(purrr::map_dbl(fprime,~from10_to_x(bse,.)))
}

get_all_div_b<-function(bse,x){
  comb_list<-list()
  f_prime<-factorize_bse(bse,x)
  for(i in seq_along(f_prime)[-c(1,length(f_prime))]){
    comb_list[[i-1]]<-t(combn(f_prime,i))
  }
  res<-comb_list%>%map(~apply(.,1,function(x){reduce(x,function(z,y){prod_b(5,z,y)})}))%>%unlist%>%unique
  res<-sort(unique(c(res,f_prime)))
  return(res)
}

###examples
random_bin(5,2,"*")
random_bin(5,2,"sum")

prod_b(5,2,31)
div_b(5,121,3)

sum_b(5,1403,424)
prod_b(5,42,12)
prod_b(5,12,14)

factorize_bse(5,124)

get_all_div_b(5,314)
get_all_div_b(5,220)


