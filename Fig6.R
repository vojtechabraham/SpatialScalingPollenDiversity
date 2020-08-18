library(reshape)
setwd("C:/Users/vojta/ownCloud/documents/HoVNoPro2/CALC/")
load( file="ready_data/01Jaccard.RData")
ttb <- jac[[1]]
load("ready_data/BDtotal_jacc_diff_dist.RData")
bkm <- bet[[1]] 


rg <- unique(ttb$V4)
ds <- unique(ttb$fixdist)
arsq <- numeric()
pval <- numeric()  

for(i in 1:NROW(ds)){
  linr <- lm(ttb[ttb$fixdist==ds[i],"beta_paly"]~ttb[ttb$fixdist==ds[i],"beta_flori"])
  arsq[i] <- summary(linr)$adj.r.squared
  pval[i] <- cor.test( ttb[ttb$fixdist==ds[i],"beta_paly"],ttb[ttb$fixdist==ds[i],"beta_flori"])$p.value # p hodota pro slope
  #rsd[3:nrowd(rsd) ,i+1]<- rstandard(linr)
}

ttt <- ttb[ttb$fixdist==150,]
ttt[,5] <- c("BMHf", "BMHm", "BMH", "WCMf", "WCMm", "WCM")

tiff("Fig6.tiff", compression = "lzw", width = 600, height = 320,  res=300, units = "mm",pointsize = 38)
par(mfrow=c(1,2), oma=c(0,0,2,1), mar=c(4.5,4.5,0,1))
plot(arsq~ds, type="l", xlab="distance (m)", ylab=expression("adjusted R "^2), lwd=3)
points(ds[pval<0.05], arsq[pval<0.05], col="red", pch=16, cex=0.6)
abline(h=0, col="gray", lwd=3, lty=2)
plot(ttt$beta_paly~ttt$beta_flori, type="p", xlab="floristic BDtotal at 150 m", ylab="pollen BDtotal", col="transparent",lwd=3, xlim=c(0.28,0.39))
abline(lm(ttt$beta_paly~ttt$beta_flori), col="blue", lwd=3)
text(ttt$beta_flori, ttt$beta_paly, labels = ttt$V5,col="black")
points(bkm[4,2],ttt[ttt$V4=="BKm",2], )
mtext("(b)", outer=T, line=0.5, cex=1.5)
mtext("(a)", outer=T, line=0.5, adj = 0, cex=1.5)
dev.off()


