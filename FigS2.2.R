library(reshape)
load( file="ready_data/01Jaccard.RData")


ttb <- jac[[2]]   
arsq <- numeric()
pval <- numeric()  
e=4
i=10
ds

head(rsd)

for(e in 1:NROW(rg)){
  for(i in 1:NROW(ds)){
    
    linr <- lm(ttb[ttb$dist==ds[i]&ttb$V5==rg[e],"pyl_lc"]~ttb[ttb$dist==ds[i]&ttb$V5==rg[e],"veg_lc"])
    arsq[i] <- summary(linr)$adj.r.squared
    pval[i] <- cor.test( ttb[ttb$dist==ds[i]&ttb$V5==rg[e],"veg_lc"],ttb[ttb$dist==ds[i]&ttb$V5==rg[e],"pyl_lc"])$p.value # p hodota pro slope
    #rsd[3:nrowd(rsd) ,i+1]<- rstandard(linr)
  }
  if(e ==1 ){ 
    lcbd_fit <- data.frame(rg=rg[e], ds, arsq,pval)
  }else{
    lcbd_fit <- rbind(lcbd_fit,data.frame(rg=rg[e], ds, arsq,pval))
  }
}

i=1


tiff("analyza_hlubsi_beta/adjusted_R2_lcbd_fit.tiff", compression = "lzw", width = 600, height = 320,  res=300, units = "mm",pointsize = 35)
par(mfrow=c(1,2), oma=c(0,2,0,0), mar=c(4.5,2.5,3,2.5))

for(i in c(1,4)){
  yf <- lcbd_fit[lcbd_fit$rg==rg[i  ],3]
  ym <- lcbd_fit[lcbd_fit$rg==rg[i+1],3]
  ya <- lcbd_fit[lcbd_fit$rg==rg[i+2],3]
  pf <- lcbd_fit[lcbd_fit$rg==rg[i  ],4]
  pm <- lcbd_fit[lcbd_fit$rg==rg[i+1],4]
  pa <- lcbd_fit[lcbd_fit$rg==rg[i+2],4]
  if(i==1){
    plot(ds, ya, type="l", col="black", ylim=c(-0.15,0.4), ylab="",xlab="distance (m)", main="VR region", lwd=3, xpd=T)
    text(x=c(1050,1050,1050),y=c(ya[NROW(ya)], yf[NROW(yf)],ym[NROW(ym)])  , c("VR", "VRf", "VRm"), col=c("black", "darkgreen", "orange"), xpd=T, adj=0, font = 2)
  }else{
    plot(ds, ya, type="l", col="black", ylim=c(-0.15,0.4),  ylab="",xlab="distance (m)", main="BK region", lwd=3, xpd=F)  
    text(x=c(1050,1050,1050),y=c(ya[NROW(ya)], yf[NROW(yf)],ym[NROW(ym)])  , c("BK", "BKf", "BKm"), col=c("black", "darkgreen", "orange"), xpd=T, adj=0, font = 2)
  }
  abline(h=0, col="gray", lwd=3, lty=2)
  lines(ds, ym,col="orange", lwd=3)
  lines(ds, yf,col="darkgreen", lwd=3)
  
  points(ds[which(pa<0.05)],ya[which(pa<0.05)], col="red", pch=16, cex=0.6)
  points(ds[which(pm<0.05)],ym[which(pm<0.05)], col="red", pch=16, cex=0.6)
  points(ds[which(pf<0.05)],yf[which(pf<0.05)], col="red", pch=16, cex=0.6)
  
  
}

mtext(outer = T, side = 2, expression("adjusted R"^2), cex = 1, line=0.5)
dev.off()

head(lcbd_fit)
i=2
ds = unique(ttb$dist)
ber=c(10, 14, 20, 10, 10, 10)
subn = c("forest", "meadow", "all" ,"forest", "meadow", "all")
tiff("FigS2.2.tiff", compression = "lzw",res = 300, width = 1000, height = 2000)
par(mfcol=c(3,2), oma=c(3,3,3,1), mar=c(4.5,3,3,0))

for(i in 1:6){
d <- ber[i]
ttt <- ttb[ttb$dist==ds[d]&ttb$V5==rg[i],]
plot(ttt[,"pyl_lc"]~ttt[,"veg_lc"], xlab=paste("at" ,ds[d],"m"), ylab="", col="transparent")
abline(lm(ttt$pyl_lc~ttt$veg_lc), col="blue", lwd=1.5)
text(ttt$veg_lc, ttt$pyl_lc, labels = ttt$sites,col="darkred", cex = 0.6, xpd=T)
title(line=0.5, subn[i], cex.main=1.5)

if(i %in% c(1)){  mtext("BMH region", line = 3, cex=1.2)}
if(i %in% c(4)){  mtext("WCM region", line = 3, cex=1.2)}
}
mtext(outer = T, side = 1, line=1, "floristic LCBD", cex = 1)
mtext(outer = T, side = 2, line=1,"pollen LCBD", cex = 1)

dev.off()

