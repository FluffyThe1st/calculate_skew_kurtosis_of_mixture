calc_all <- function(mean1, mean2, sd1, sd2, PI1, PI2) {
  
  densityX <- function(x) {
    (PI1 * dnorm(x, mean1, sd1) + PI2 * dnorm(x, mean2, sd2))
  }
  id <- function(x) {x}
  g <- function(x) {id(x)*densityX(x)}
  
  mom2 <- function(x) {
    mu_int <- integrate(g, lower = -Inf, upper = Inf)$value
    (id(x)-mu_int)^2 * densityX(x)
  }
  mom3 <- function(x) {
    mu_int <- integrate(g, lower = -Inf, upper = Inf)$value
    (id(x)-mu_int)^3 * densityX(x)
  }
  mom4 <- function(x) {
    mu_int <- integrate(g, lower = -Inf, upper = Inf)$value
    (id(x)-mu_int)^4 * densityX(x)
  }
  
  y <- seq(-6, 6, .1)
  den <- densityX(y)
  mu <- PI1*mean1 + PI2*mean2 
  # mu <- integrate(g, lower = -Inf, upper = Inf)$value
  sd_2 <- PI1*(mean1^2+sd1^2)-mu^2 + PI2*(mean2^2+sd2^2)-mu^2
  skew <- (integrate(mom3, lower = -Inf, upper = Inf))$value / (sd_2)^(3/2)
  kurt <- (integrate(mom4, lower = -Inf, upper = Inf))$value / (sd_2)^2 -3
  return(list(mu, sd_2, skew, kurt, y, den))
}

show <- function(mu2) {
  par(mfrow=c(3,1))
  sd1 <- .7; sd2 <- 1.65
  listing1 <- calc_all(0, mu2, sd1, sd2, .7, .3)
  listing2 <- calc_all(0, mu2, sd1, sd2, .8, .2)
  listing3 <- calc_all(0, mu2, sd1, sd2, .9, .1)
  
  m <- array(dim=c(3,4))
  m[1, 1:4] <- c(listing1[[1]], listing1[[2]], listing1[[3]], listing1[[4]])
  m[2, 1:4] <- c(listing2[[1]], listing2[[2]], listing2[[3]], listing2[[4]])
  m[3, 1:4] <- c(listing3[[1]], listing3[[2]], listing3[[3]], listing3[[4]])
  colnames(m) <- c("mu", "sd²", "skew", "e_kurt")
  
  plot(as.vector(listing1[[5]]), as.vector(listing1[[6]]), type = "l", main = "pi2 = .3", lwd = 2)
  lines(listing1[[5]], .7*dnorm(listing1[[5]], 0, sd1), col = "orange", lty = 2)
  lines(listing1[[5]], .3*dnorm(listing1[[5]], mu2, sd2), col = "pink", lty = 3)
  plot(as.vector(listing2[[5]]), as.vector(listing2[[6]]), type = "l", main = "pi2 = .2", lwd = 2)
  lines(listing2[[5]], .8*dnorm(listing2[[5]], 0, sd1), col = "orange", lty = 2)
  lines(listing1[[5]], .2*dnorm(listing1[[5]], mu2, sd2), col = "pink", lty = 3)
  plot(as.vector(listing3[[5]]), as.vector(listing3[[6]]), type = "l", main = "pi2 = .1", lwd = 2)
  lines(listing3[[5]], .9*dnorm(listing3[[5]], 0, sd1), col = "orange", lty = 2)
  lines(listing1[[5]], .1*dnorm(listing1[[5]], mu2, sd2), col = "pink", lty = 3)
  return(m)
}
