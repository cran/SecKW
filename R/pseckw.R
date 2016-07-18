#' The cumulative function of the Secant Kumaraswamy Weibull probability distribution.
#' @export
#' @importFrom pracma sec
#'
#' @param q Vector of quantiles.
#' @param a A parameter.
#' @param b B parameter.
#' @param c C parameter.
#' @param lambda Lambda parameter.
#' @param lower logical; if TRUE (default), probabilities are \eqn{P[X \leq x]} otherwise, \eqn{P[X > x]}.
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @return A vector with n observations of the Secant Kumaraswamy Weibull distribution.
#' @examples
#' pseckw(0.5, 1, 1, 1, 1,TRUE,FALSE)
#' pseckw(0.5, 3, 0.5, 2, 2,TRUE,FALSE)

pseckw<-function(q,a,b,c,lambda,lower = TRUE,log.p = FALSE){

  if (log.p == TRUE) {
    if (lower == TRUE) {
      log((sec((pi/3)*(1-(1-(1-exp(-(lambda*q)^(c))^(a))^(b))))-1))
    }else{
      log((2-sec((pi/3)*(1-(1-(1-exp(-(lambda*q)^(c))^(a))^(b))))))
    }
  } else {
    if (lower == TRUE) {
      (sec((pi/3)*(1-(1-(1-exp(-(lambda*q)^(c))^(a))^(b))))-1)
    }else{
      (2-sec((pi/3)*(1-(1-(1-exp(-(lambda*q)^(c))^(a))^(b)))))
    }
  }

}
