#' The hazard rate function of the Secant Kumaraswamy Weibull probability distribution.
#' @export
#' @importFrom pracma sec
#'
#' @param x Vector of quantiles.
#' @param a A parameter.
#' @param b B parameter.
#' @param c C parameter.
#' @param lambda Lambda parameter.
#' @return A vector with n observations of the SecantKumaraswamyWeibull distribution.
#' @examples
#' hseckw(1, 1, 1, 1, 1)
#' hseckw(1, 3, 0.5, 2, 2)

hseckw<-function(x,a,b,c,lambda){

  ((pi/3)*(a*b*c*lambda^c*x^(c-1))*exp(-(lambda*x)^c)*(1-exp(-(lambda*x)^c))^(a-1)*(1-(1-exp(-(lambda*x)^c))^a)
  ^(b-1))*sec((pi/3)*(1-(1-(1-exp(-(lambda*x)^c))^a)^b))*
    tan((pi/3)*(1-(1-(1-exp(-(lambda*x)^c))^a)^b))/(2-sec((pi/3)*(1-(1-(1-exp(-(lambda*x)^c))^a)^b)))
}
