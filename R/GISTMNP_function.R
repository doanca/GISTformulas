#' GISTMNP Calculation Function
#'
#' The purpose of this function is to calculate GISTMNP predictions given user-specified input
#'
#' @param num_dim Number of dimensions defining each stimulus object
#' @param num_obj Number of objects belonging to categorical stimulus
#' @param manifold Structural Manifold values specified in 0-1 interval
#' @return The GISTMNP prediction given what was specified for the three parameters
#' @examples
#' family_341 <- GISTMNP(3,4,c(1,1,0))
#' @export
#'
GISTMNP <- function(num_dim, num_obj, manifold) {
  ### Error Checking the input variables ###
  if(!is.numeric(num_dim)) {stop("num_dim must be a number")}
  if(!is.numeric(num_obj)) {stop("num_obj must be a number")}
  if(length(manifold) != num_dim) {stop("manifold must have length num_dim")}

  D <- num_dim
  P <- num_obj
  sq_prop <- numeric(length(manifold))
  for (i in 1:num_dim){
    sk_int <- manifold[i]*num_obj
    sq_prop[i] <- (sk_int/num_obj)^2
  }

  GISTM_NP <- P*exp(-((2/D)*(sqrt(sum(sq_prop))^2)))
  return(GISTM_NP)
}
