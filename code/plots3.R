load("data3.RData")
num_of_obs <- seq(from=11, to=100, by=20)
values_of_rho <- seq(from=0, to=0.4, by=0.4/15)

par(mfrow=c(5,2))
par(oma=c(0,0,0,0))
par(mar=c(5,5,2,2))
par(mgp=c(3,1,0))
#c(bottom, left, top, right)
par(cex.axis=1.3,cex.lab =1.7,cex.main=2)

for (q in 1:5) {
  plot(values_of_rho,u0_power_rho_n_pca[[1]][[q]], type = "l", xlim = c(0,0.3), ylim = c(0,1), xlab = "rho", ylab = "power",lwd=1.7,main = paste("U, ", "n =", -9 + 20 * q))
  for(i in 2:length(u0_power_rho_n_pca)){
    lines(values_of_rho,u0_power_rho_n_pca[[i]][[q]], type = "l",col = i,lwd=1.7);abline(h=0.05,lty=2)
  }
  plot(values_of_rho,u1_power_rho_n_pca[[1]][[q]], type = "l", xlim = c(0,0.3), ylim = c(0,1), xlab = "rho", ylab = "power",lwd=1.7,main = paste("U', ", "n =", -9 + 20 * q))
  for(i in 2:length(u1_power_rho_n_pca)){
    lines(values_of_rho,u1_power_rho_n_pca[[i]][[q]], type = "l",col = i,lwd=1.7);abline(h=0.05,lty=2)
  }
}


#mtext(paste(c('U', "U'")),at=c(0.27,0.77),side=3,outer=T,line = 1,cex=2.2)
#mtext(paste('n =',rev(num_of_obs)),at=seq(0.1,0.9,1/5),side=2,outer=T,line = 1)


plot(1,1)
legend("topright", col = 1:(length(u0_power_rho_n_pca)), legend = c("10 PCs retained","9 PCs retained","8 PCs retained","7 PCs retained"),lty=1,lwd=2.5,cex=1)