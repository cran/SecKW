#' The survival function of the Secant Kumaraswamy Weibull probability distribution.
#' @export
#' @importFrom pracma sec
#'
#' @param x Vector of quantiles.
#' @param a A parameter.
#' @param b B parameter.
#' @param c C parameter.
#' @param lambda Lambda parameter.
#' @return A vector with n observations of the Secant Kumaraswamy Weibull distribution.
#' @examples
#' sseckw(1, 1, 1, 1, 1)
#' sseckw(1, 3, 0.5, 2, 2)

sseckw <- function(x,a,b,c,lambda){
  (2-sec((pi/3)*(1-(1-(1-exp(-(lambda*x)^(c))^(a))^(b)))))
}
