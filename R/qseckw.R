#' The cumulative function of the SecantKumaraswamyWeibull probability distribution.
#' @export
#' @importFrom pracma asec
#'
#' @param p Vector of probabilities.
#' @param a A parameter.
#' @param b B parameter.
#' @param c C parameter.
#' @param lambda Lambda parameter.
#' @param lower logical; if TRUE (default), probabilities are \eqn{P[X \leq x]} otherwise, \eqn{P[X > x]}.
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @return A vector with n observations of the Secant Kumaraswamy Weibull distribution.
#' @examples
#' qseckw(0.5, 1, 1, 1, 1, TRUE, FALSE)
#' qseckw(0.5, 3, 0.5, 2, 2, TRUE, FALSE)

qseckw<-function(p,a,b,c,lambda,lower = TRUE,log.p = FALSE){

  if (log.p == TRUE) {
    if (lower == TRUE){
      y=asec(2-p)
      log((-1/lambda)*(log(1-(1-(1-((3/pi)*y))^(1/b))^(1/a)))^(1/c))
    }else{
      y=asec(p)
      log((-1/lambda)*(log(1-(1-(1-((3/pi)*y))^(1/b))^(1/a)))^(1/c))
    }
  } else {
    if (lower == TRUE){
      y=asec(2-p)
      (-1/lambda)*(log(1-(1-(1-((3/pi)*y))^(1/b))^(1/a)))^(1/c)
    }else{
      y=asec(p)
      (-1/lambda)*(log(1-(1-(1-((3/pi)*y))^(1/b))^(1/a)))^(1/c)
    }
  }

}
