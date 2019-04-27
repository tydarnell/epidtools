#' 2x2 Epi Table Function
#'
#' This function creates a 2x2 Disease Exposure Table
#' @param exposure exposure column
#' @param disease disease column
#' @keywords etable
#' @export
#' @examples
#' etable_function()

etable=function(exposure,disease){
  t=table(exposure,disease)[2:1,2:1]
  rownames(t)=c('E+','E-')
  colnames(t)=c('D+','D-')
  t
}

#' Risk Ratio function
#'
#' This function calculates a risk ratio
#' @param a first entry
#' @param b second entry
#' @param c third entry
#' @param d fourth entry
#' @keywords risk
#' @export
#' @examples
#' riskratio_function()

riskratio=function(a,b,c,d){
  e=a/(a+b)
  f=c/(c+d)
  c(e,f,e/f)
}

#' Risk Ratio and CI function
#'
#' This function calculates a risk ratio and the CI
#' @param ecases exposure cases
#' @param enoncases exposure noncases
#' @param uecases non exposed cases
#' @param uenoncases non exposed non cases
#' @keywords ciriskr
#' @export
#' @examples
#' ciriskr_function()

ciriskr=function(ecases,enoncases,uecases,uenoncases){
  a=ecases
  b=enoncases
  c=uecases
  d=uenoncases
  e=a/(a+b)
  f=c/(c+d)
  rr=e/f
  z=c(-1.96,1.96)
  se=sqrt(1/a-1/(a+b)+
            1/c-1/(c+d))
  ci=exp(log(rr)+z*se)
  c("Risk Ratio"=rr,"95 CI"=ci)
}

#' Rate Ratio
#'
#' This function calculates a rate ratio
#' @param cases cases
#' @param noncases noncases
#' @param pyrcases person years for cases
#' @param pyrnoncases person years for noncases
#' @keywords rateratio
#' @export
#' @examples
#' rateratio_function()

rateratio=function(cases,noncases,pyrcases,pyrnoncases){
  a=cases
  b=noncases
  p=pyrcases
  q=pyrnoncases
  (a/p)/(b/q)
}

#' Rate Ratio and CI function
#'
#' This function calculates a Rate ratio and the CI
#' @param cases cases
#' @param noncases noncases
#' @param pyrcases person years for cases
#' @param pyrnoncases person years for noncases
#' @keywords cirater
#' @export
#' @examples
#' cirater_function()

cirater=function(cases,noncases,pyrcases,pyrnoncases){
  a=cases
  b=noncases
  p=pyrcases
  q=pyrnoncases
  rr=(a/p)/(b/q)
  se=sqrt(1/a+1/b)
  z=c(-1.96,1.96)
  ci=exp(log(rr)+z*se)
  c("Rate Ratio"=rr,"95 CI"=ci)
}


#' Odds Ratio and CI function
#'
#' This function calculates an adds ratio and the CI
#' @param ecases exposure cases
#' @param enoncases exposure noncases
#' @param uecases non exposed cases
#' @param uenoncases non exposed non cases
#' @keywords cioddsr
#' @export
#' @examples
#' cioddsr_function()

cioddsr=function(ecases,enoncases,uecases,uenoncases){
  a=ecases
  b=enoncases
  c=uecases
  d=uenoncases
  or=(a/c)*(d/b)
  se=sqrt(1/a+1/b+1/c+1/d)
  z=c(-1.96,1.96)
  ci=exp(log(or)+z*se)
  c("Odds Ratio"=or,"95 CI"=ci)
}


#' 2x2 Epi Table Function with margins
#'
#' This function creates a 2x2 Disease Exposure Table with margins
#' @param exposure exposure column
#' @param disease disease column
#' @keywords etable2
#' @export
#' @examples
#' etable2_function()

etable2=function(exposure,disease){
  t=table(exposure,disease)[2:1,2:1]
  rownames(t)=c('E+','E-')
  colnames(t)=c('D+','D-')
  addmargins(t)
}
