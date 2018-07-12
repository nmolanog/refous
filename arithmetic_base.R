library(tidyverse)
x<-123
bse<-5
x%>%as.character()%>%strsplit("")%>%unlist%>%as.numeric()->sep_digits
sep_digits>=bse
base_exp<-(length(sep_digits)-1):0
sum((bse^base_exp)*(sep_digits))

fromx_to_10<-function(bse,x){
  x%>%as.character()%>%strsplit("")%>%unlist%>%as.numeric()->sep_digits
  if(any(sep_digits>=bse)){stop(paste("error: ", sep_digits[sep_digits>=bse], "are not digits for the given base"))}
  base_exp<-(length(sep_digits)-1):0
  return(sum((bse^base_exp)*(sep_digits)))
}

fromx_to_10(3,23)

x<-13
bse<-2

div_int<-function(bse,x){
  div_int<-x%/%bse
  res<-x-div_int*bse
return(c(div_int,res))  
}


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

rev(newnumber)
