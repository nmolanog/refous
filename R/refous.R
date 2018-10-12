#' pipe operator
`%>%` <- purrr::`%>%`

#' fromx_to_10 Function
#'
#' this functions converts integers from a given base to base 10
#' @param bse integer from 2 to 9. Original base of the number
#' @param x number in base bse
#' @return integer in base 10
#' @keywords base
#' @export
#' @examples
#' fromx_to_10(2,1101)
#' fromx_to_10(5,43)

fromx_to_10<-function(bse,x){
  x%>%as.character()%>%strsplit("")%>%unlist%>%as.numeric()->sep_digits
  if(any(sep_digits>=bse)){stop(paste("error: ", sep_digits[sep_digits>=bse], "are not digits for the given base"))}
  base_exp<-(length(sep_digits)-1):0
  return(sum((bse^base_exp)*(sep_digits)))
}

#' div_int Function
#'
#' this function performs integer division in a given base. Used by from10_to_x function. not intended for user use.
#' @param bse integer
#' @param x integer
#' @return integer
#' @keywords base
div_int<-function(bse,x){
  div_int<-x%/%bse
  res<-x-div_int*bse
  return(c(div_int,res))
}

#' from10_to_x Function
#'
#' this functions converts integers in base 10 to a given base.
#' @param bse integer from 2 to 9. New base of the number.
#' @param x number in base 10
#' @return integer in base bse
#' @keywords base
#' @export
#' @examples
#' from10_to_x(2,7)
#' from10_to_x(5,26)

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

#' sum_b Function
#'
#' this functions does sum in a given base.
#' @param bse integer from 2 to 9. base of the numbers.
#' @param x number in base bse
#' @param y number in base bse
#' @return sum in base bse
#' @keywords base
#' @export
#' @examples
#' sum_b(2,10,101)
#' sum_b(5,24,23)

sum_b<-function(bse,x,y){
  fromx_to_10(bse,x)+fromx_to_10(bse,y)->sum_N
  return(from10_to_x(bse,sum_N))
}

#' prod_b Function
#'
#' this functions does product in a given base.
#' @param bse integer from 2 to 9. base of the numbers.
#' @param x number in base bse
#' @param y number in base bse
#' @return product in base bse
#' @keywords base
#' @export
#' @examples
#' prod_b(2,10,101)
#' prod_b(5,24,23)
prod_b<-function(bse,x,y){
  fromx_to_10(bse,x)*fromx_to_10(bse,y)->sum_N
  return(from10_to_x(bse,sum_N))
}

#' div_b Function
#'
#' this functions does integer division in a given base. if division is not integer, the result may be inexact.
#' @param bse integer from 2 to 9. base of the numbers.
#' @param x number in base bse
#' @param y number in base bse
#' @return division in base bse
#' @keywords base
#' @export
#' @examples
#' div_b(5,30,10)
#' div_b(5,20,2)
div_b<-function(bse,x,y){
  fromx_to_10(bse,x)/fromx_to_10(bse,y)->sum_N
  return(from10_to_x(bse,sum_N))
}

#' random_bin Function
#'
#' this functions generates random exercises for prod and sum
#' @param bse integer from 2 to 9. base of the numbers.
#' @param dgts number of digits of the numbers to be generated
#' @param op character either "sum" or "prod"
#' @return division in base bse
#' @keywords base
#' @export
#' @examples
#' div_b(5,30,10)
#' div_b(5,20,2)
random_bin<-function(bse,dgts,op){
  a<-sample(0:(bse-1),dgts,replace = T)
  b<-sample(0:(bse-1),dgts,replace = T)
  achr<-paste0(a,collapse = "")
  bchr<-paste0(b,collapse = "")
  if(op=="sum"){
    opchr<-"+"
    res2<-sum_b(bse,a,b)
  }else if(op=="prod"){
    opchr<-"*"
    res2<-prod_b(bse,a,b)
    }
  res1<-paste0(achr,opchr,bchr)
  return(list(op=res1,res=res2))
}

#' factorize_bse Function
#'
#' this functions generates the prime decomposition of a number in a given base
#' @param bse integer from 2 to 9. base of the numbers.
#' @param x number in base bse to be decomposed
#' @return vector with primes
#' @keywords base
#' @export
#' @examples
#' factorize_bse(5,30)
#' factorize_bse(5,20)
factorize_bse<-function(bse,x){
  x_10<-fromx_to_10(bse,x)
  fprime<-conf.design::factorize(x_10)
  return(purrr::map_dbl(fprime,~from10_to_x(bse,.)))
}

#' get_all_div_b Function
#'
#' this functions generates the set of al the divisors of a number in a given base. 1 an the number are not in this set.
#' @param bse integer from 2 to 9. base of the numbers.
#' @param x number in base bse.
#' @return vector with divisors
#' @keywords base
#' @export
#' @examples
#' get_all_div_b(5,30)
#' get_all_div_b(5,20)
get_all_div_b<-function(bse,x){
  comb_list<-list()
  f_prime<-factorize_bse(bse,x)
  for(i in seq_along(f_prime)[-c(1,length(f_prime))]){
    comb_list[[i-1]]<-t(combn(f_prime,i))
  }
  res<-comb_list%>%purrr::map(~apply(.,1,function(x){purrr::reduce(x,function(z,y){prod_b(5,z,y)})}))%>%unlist%>%unique
  res<-sort(unique(c(res,f_prime)))
  return(res)
}
