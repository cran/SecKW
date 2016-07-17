#' The density function of the Secant Kumaraswamy Weibull probability distribution.
#' @export
#'
#' @param x Vector of quantiles.
#' @param a A parameter.
#' @param b B parameter.
#' @param c C parameter.
#' @param lambda Lambda parameter.
#' @return A vector with n observations of the Secant Kumaraswamy Weibull distribution.
#' @examples
#' dseckw(1, 1, 1, 1, 1)
#' dseckw(1, 2, 2, 1, 1)

dseckw<-function(x,a,b,c,lambda){
  (pi/3)*(a*b*c*lambda^c*x^(c-1)*exp(-(lambda*x)^c)*(1-exp(-lambda*x)^c)^(a-1)*(1-(1-exp(-lambda*x)^c)^a)^(b-1))*pracma::sec((pi/3)*(1-(1-(1-exp(-(lambda*x)^c))^a)^b))*tan((pi/3)*(1-(1-(1-exp(-(lambda*x)^c))^a)^b))
}
