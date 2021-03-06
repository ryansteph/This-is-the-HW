#' Calculate Mean, Variane, SD
#'
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func1(rnorm(10))
func1 <- function(x){
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' Calculate Mean, Variane, SD
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func2(rnorm(10))
func2 <- function(x){
  stopifnot(is.numeric(x))
  stopifnot(length(x)!=0)
  stopifnot(is.finite(x))
 
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' MLE of gamma distribution
#'
#' @param x vector
#'
#' @return scalar
#' @export
#' @examples
#' func3(rnorm(10))
func3 <- function(x){
  alpha <- pi
  log <- function(alpha)
    sum(dgamma(x, shape = alpha, log = TRUE))
  interval <- mean(x) + c(-1,1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)
  
  oout<- optimize(log, maximum = TRUE, interval)
  return (oout$maximum)
}

#' Weighted mean, var, sd
#'
#' Computes the weighted mean, var, sd
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' data(d)
#' func4(d)
func4 <- function(d){
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Weighted mean, var, sd with user checkes
#'
#' Computes the weighted mean, var, sd with user checks
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
#' func5(d)
func5 <- function(d){
  
  stopifnot(is.numeric(d$x))
  stopifnot(is.numeric(d$p))
  
  stopifnot(length(d$x)!=0)
  stopifnot(length(d$p)!=0)
  
  stopifnot(is.finite(d$x))
  stopifnot(is.finite(d$p))
  
  stopifnot(!is.na(d$x))
  stopifnot(!is.na(d$p))
  
  stopifnot(!is.nan(d$x))
  stopifnot(!is.nan(d$p))
  
  stopifnot(all.equal(sum(d$p),1))
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Highlevel check function
#'
#' Checks and throws error if not numeric, finit, zero lenth, NA, NAN
#'
#' @param x object
#'
#' @return object
#' @export
#' @examples
#' func6(NA)

func6 <- function(x){
  
  tryCatch(stopifnot(is.numeric(x)), error=function(e){print("not numeric")})
  tryCatch(stopifnot(is.finite(x)), error=function(e){print("not finite")})
  tryCatch(stopifnot(length(x)!=0), error=function(e){print("has 0 length")})
  tryCatch(stopifnot(!is.nan(x)), error=function(e){print("NA or NAN")})
  tryCatch(stopifnot(!is.na(x)), error=function(e){print("NA or NAN")})
  
}

#' MLE 
#'
#' Computes the liklihood of a given distribution for data x
#'
#' @param x vector
#' @param func function, e.g., `function(theta, x) dgamma(x, shape = theta, log = TRUE)
#' @param interval vector, i.e., interval for optimize function
#'
#' @return scalar
#' @export
#' @examples
#' func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
#' result7_gamma <- func7(x1,func1,c(0,3))
#' result7_gamma
#' 
func7 <- function(x, func, interval){
  
  f7 <- function(theta, x)
  {sum(func(theta, x))}
  
  oout<- optimize(f7, maximum = TRUE, interval, x=x)
  return(oout$maximum)
} 

#'  x^T* (A^(− 1))* x
#'
#' given a numeric matrix A and a numeric vector x, calculates x^T* (A^(− 1))* x
#'
#' @param d2 data.frame
#' @param 
#' @param 
#'
#' @return scalar
#' @export
#' @examples
#' data(d2)
#' func8(a,x)
#' 
#' 
func8 <- function(a, x) {
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(x))
  stopifnot(nrow(a) == ncol(a))
  stopifnot(nrow(a) == length(x))
  b <- solve(a, x)
  return(sum(x * b))
  }
#'  x^T* (A^(− 1))* x binary
#'
#' given a numeric matrix A and a numeric vector x, calculates x^T* (A^(− 1))* x
#'
#' @param d2 data.frame
#' @param 
#' @param 
#'
#' @return scalar
#' @export
#' @examples
#' data(d2)
#' "%func9%"(a,x)
#' 
#' 
  func9<- function(a, x) {
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(x))
  stopifnot(nrow(a) == ncol(a))
  stopifnot(nrow(a) == length(x))
  b <- solve(a, x)
  return(sum(x * b))
}


#'  standardizes
#'
#' takes a numeric matrix and "standardizes" its columns
#'
#' @param d3 data.frame
#' @param 
#' @param 
#'
#' @return scalar
#' @export
#' @examples
#' data(d3)
#' func10"(a)
#' 

fun10 <- function(a) {
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  for (i in 1:ncol(a)) {
    foo <- a[ , i]
    a[ , i] <- (foo - mean(foo)) / sd(foo)
  }
  return(a) }
#'  standardizes
#'
#' takes a numeric matrix and "standardizes" its columns
#'
#' @param d3 data.frame
#' @param 
#' @param 
#'
#' @return scalar
#' @export
#' @examples
#' fred <- matrix(1:6, nrow = 3, ncol = 2)
#' func11(fred, 1, mean)
#' 

func11 <- function(X, MARGIN, FUN, ...) {
  FUN <- match.fun(FUN)
  stopifnot(length(MARGIN) == 1)
  stopifnot(MARGIN %in% 1:2)
  if (MARGIN == 1) {
    foo <- X[1, ]
    bar <- FUN(foo, ...)
    result <- matrix(NA, nrow = nrow(X), ncol = length(bar))
    for (i in 1:nrow(X)) {
      foo <- X[i, ]
      bar <- FUN(foo, ...)
      result[i, ] <- bar
    }
  } else {
    foo <- X[ , 1]
    bar <- FUN(foo, ...)
    result <- matrix(NA, ncol = ncol(X), nrow = length(bar))
    for (i in 1:ncol(X)) {
      foo <- X[ , i]
      bar <- FUN(foo, ...)
      result[ , i] <- bar
    } }
  return(result)
}