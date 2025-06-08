load("data2.RData")

num_of_obs <- seq(from=11, to=100, by=10)
values_of_rho <- seq(from=-0.1, to=0.25, by=0.35/20)

par(cex.axis=2,cex.lab =2.5)
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))
par(mar=c(6.9,8,1.8,1))
par(mgp=c(5.8,2,0))
#c(bottom, left, top, right)

plot(values_of_rho,u0_power_rho_n[[1]], type = "l", xlim = c(-0.1,0.1),ylim = c(0,1),
     main = "", xlab="rho", ylab="power",lwd=3)
for(i in 2:length(u0_power_rho_n)-1){
  lines(values_of_rho,u0_power_rho_n[[i]], type = "l",col = i,,lwd=3);abline(h = 0.05,lty=2)
  #legend("bottomleft", col = 1:(length(u0_power_rho_n)-1), legend = c("n=11","n=21","n=31","n=41","n=51","n=61","n=71","n=81"),lty=1,cex = 0.8)
}

plot(values_of_rho,u1_power_rho_n[[1]], type = "l", xlim = c(-0.1,0.1),ylim = c(0,1),main = "", xlab="rho", ylab="power",lwd=3)
for(i in 2:length(u0_power_rho_n)-1){
  lines(values_of_rho,u1_power_rho_n[[i]], type = "l",col = i,lwd=3);abline(h = 0.05,lty=2)
  #legend("topright", col = 1:(length(u1_power_rho_n)-1), legend = c("n=11","n=21","n=31","n=41","n=51","n=61","n=71","n=81"),lty=1,cex = 0.6)
}

plot(1,1)
legend("topright", col = 1:(length(u1_power_rho_n)-1), legend = c("n=11","n=21","n=31","n=41","n=51","n=61","n=71","n=81"),lty=1,lwd=3,cex=1.5)


par(mfrow=c(4,2))
par(oma=c(0,2,2,2))
par(mar=c(6,5,2,2))
par(mgp=c(3.5,1,0))
#c(bottom, left, top, right)
par(cex.axis=1.5,cex.lab =2,cex.main=2)

for (i in 1:8) {
  plot(values_of_rho,u1_power_rho_n[[i]], type = "l", xlim = c(-0.1,0.1),ylim = c(0,1),xlab= "rho", ylab = "power",lwd=2, main = paste("n =", 10 *i +1))
  lines(values_of_rho,u0_power_rho_n[[i]], type = "l",col = 2,lwd=2);abline(h = 0.05, lty=2)
}
legend("bottomleft", col = 1:2, legend = c("U'","U"),lty=1,lwd=3,cex= 1.5)

#mtext(paste(c('u')),at=c(0.5),side=3,outer=T,line = 1)
#mtext(paste('n =',rev(num_of_obs[1:8])),at=seq(0.05,0.95,1/8),side=2,outer=T,line = 1,cex = 2)
