library(MASS)
library(mixtools)
library(matrixcalc)
library(car)
inc <- function(x) {
  eval.parent(substitute(x <- x + 1))
}

u0 <- function(data) {
  # sample covariance matrix
  S <- cov(data)
  # number of variables
  p <- ncol(data)
  # number of observations
  n <- nrow(data)
  
  u <-p^p * det(S) / (sum(diag(S)))^p
  return(-n* log(u))
}

u1 <- function(data) {
  # sample covariance matrix
  S <- cov(data)
  # number of variables
  p <- ncol(data)
  # u and statistic u'
  u <-p^p * det(S) / (sum(diag(S)))^p
  # number of observations
  n <- nrow(data)
  
  u1 <- -(n-1 - ((2* p^2 +p +2)/( 6* p))) * log(u)
  return(u1)
}

u0_distr_n <- list()
u0_alpha_n <- list()

u1_distr_n <- list()
u1_alpha_n <- list()
k = 1

num_of_obs <- seq(from=11,to=21,by=2)


for(N in num_of_obs) {
  nboot <- 10000 # Number of bootstraps
  alpha <- 0.05 # Significance level
  P <- 10 # Number of variables
  degfreed <- 1/2 * P * (P+1) -1 #degrees of freedom
  
  # Parameters for multivariate normal distribution, under H0.
  mu <- rep(0,P) # Mean vector
  sigma <- diag(P) # Covariance matrix
  
  u0_distr <- rep(0,nboot)
  u1_distr <- rep(0,nboot)
  
  for (i in 1:nboot) {
    mvn <- mvrnorm(N, mu = mu, Sigma = sigma ) # from MASS package
    u0_distr[i] <- u0(mvn)
    u1_distr[i] <- u1(mvn)
    
  }
  
  u0_distr_n[[k]] <-  u0_distr
  u0_alpha_n[[k]] <- (length(which(u0_distr_n[[k]] > qchisq(1-alpha,degfreed)))) / length(u0_distr_n[[k]])
  u1_distr_n[[k]] <-  u1_distr
  u1_alpha_n[[k]] <- (length(which(u1_distr_n[[k]] > qchisq(1-alpha,degfreed)))) / length(u1_distr_n[[k]])
  
  inc(k)
}
u0_distance <- list()
for (i in 1:length(u0_distr_n)){
  u0_distance[[i]] <- sum(abs(quantile(density(u0_distr_n[[i]])$x,seq(0,0.99,0.01))-qchisq(seq(0,0.99,0.01),degfreed)))/100
}
u1_distance <- list()
for (i in 1:length(u1_distr_n)){
  u1_distance[[i]] <- sum(abs(quantile(density(u1_distr_n[[i]])$x,seq(0,0.99,0.01))-qchisq(seq(0,0.99,0.01),degfreed)))/100
}
x.grid <- seq(0,120,length = 10000)
chisqcurve <- dchisq(x.grid, df = degfreed)

par(cex.axis=2.5,cex.lab =3)
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))
par(mar=c(6.9,8,1.8,1))
par(mgp=c(5.8,2,0))
#c(bottom, left, top, right)

plot(x.grid,chisqcurve, type = 'l', main = "",lwd = 4, xlab="U", ylab="density",ylim = c(0,0.04), xlim = c(20,110))
for (i in 1:length(u0_distr_n)) {
  lines(density(u0_distr_n[[i]]),col = i+1,lwd = 4)
  #legend("topleft", col = 1:7, legend = c("chi-sq","n=11","n=13","n=15","n=17","n=19","n=21"),
         #lty=1,lwd=3,cex=1.3,pt.cex =1,y.intersp = 0.8,x.intersp=0.7)
}

plot(1,1)
legend("topleft", col = 1:7, legend = c("chi-sq","n=11","n=13","n=15","n=17","n=19","n=21"),
       lty=1,lwd=3,cex=1.3,pt.cex =1,y.intersp = 0.85,x.intersp=0.7)

plot(x.grid,chisqcurve, type = 'l', main = "", lwd = 4, xlab="U'", ylab="density",ylim = c(0,0.04), xlim = c(20,110))
for (i in 1:length(u1_distr_n)) {
  lines(density(u1_distr_n[[i]]),col = i+1, lwd = 4)
  #legend("topright", col = 1:7, legend = c("chi-sq","n=11","n=13","n=15","n=17","n=19","n=21"),lty=1,lwd =2)
}

plot(num_of_obs,u1_distance,type = "o",main = "", xlab="n", ylab="distance",ylim=c(0,150),lwd =4)
lines(num_of_obs,u0_distance,type = "o",col = 'red',lwd =4);abline(h=0.05,lty=2)
legend("topright", col = 1:2, legend = c("U'","U"),lty=1,lwd =4,cex=2)

plot(num_of_obs,u1_alpha_n,type = "o",ylim=c(0,1),main = "", xlab="n", ylab="alpha",lwd =4)
lines(num_of_obs,u0_alpha_n,type = "o",col = 'red',lwd =4);abline(h=0.05,lty=2)
legend("topright", col = 1:2, legend = c("U'","U"),lty=1,lwd =4,cex=2)


par(mfrow=c(6,2))
par(oma=c(0,0,0,0))
par(mar=c(5,5,2,1))
par(mgp=c(3.2,1,0))
#c(bottom, left, top, right)
par(cex.axis=1.3,cex.lab =1.7,cex.main=2)

for (i in 1:6) {
  hist(u0_distr_n[[i]], prob = TRUE, col = "white",
       ylim = c(0, 0.05), xlim=c(10,110),
       breaks = 50, ylab = "Density", main = paste("n =", 11 + 2*(i-1)), xlab = "U")
  lines(x.grid, chisqcurve, col = 2, lwd = 2)
  hist(u1_distr_n[[i]], prob = TRUE, col = "white",
       ylim = c(0, 0.05), xlim=c(10,110),
       breaks = 50, ylab = "Density", main = paste("n =", 11 + 2*(i-1)), xlab = "U'")
  lines(x.grid, chisqcurve, col = 2, lwd = 2)
}
#mtext(paste('n =',rev(num_of_obs)),at=seq(0.1,1,1/6),side=2,outer=T,line = 1,cex = 2)

save(, file = "data2.RData")



