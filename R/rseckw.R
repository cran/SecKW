#' Generates random deviates from a SecantKumaraswamyWeibull probability distribution.
#' @export
#' @importFrom fdrtool dhalfnorm
#'
#' @param n Number of observations to be generated.
#' @param a A parameter.
#' @param b B parameter.
#' @param c C parameter.
#' @param lambda Lambda parameter.
#' @return A vector with n observations of the SecantKumaraswamyWeibull distribution.
#' @examples
#' rseckw(1000, 1, 1, 1, 1)
#' rseckw(1000, 2, 2, 1, 1)

rseckw<-function(n,a,b,c,lambda){

  accept = c()
  count = 0

  while (length(accept) < n){

    U <- fdrtool::rhalfnorm(1)
    x <- fdrtool::rhalfnorm(1)

    if(U <= dseckw(x,a,b,c,lambda)/(sqrt(pi)*dhalfnorm(x)/sqrt(2))) {
      accept[count] = x
      count = count + 1
    }
  }
  return(accept)
}
