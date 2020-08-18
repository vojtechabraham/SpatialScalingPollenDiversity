pho <- read.table("ready_data/Fig4_R2_data.txt")
dis2 <- as.numeric(unique(pho[pho$X2 %in% c("all",   "fst","mdw"),1]))
dis <- as.numeric(unique(pho[!(pho$X2 %in% c("all",   "fst","mdw")),1]))

vv <- c("vr",   "vr forest","vr meadow")
bb <- c("bk","bk forested" ,"bk open")
ss <- c("all",   "fst","mdw")

ll <- list(vv,bb,ss)
i=1
jmn <- c("BMH region", "WCM region")#, "simulation")
disy <- list(dis,dis)#, dis2)
xlimax <- c(1000,1000)#, 2500)
abl <- c(550, 250, 250 )
pho[pho$value.y==0, 4] <- NA
#pdf("adjusted_Rsquared.pdf", width = 10)

load( file="ready_data/01Jaccard.RData")
ttb <- jac[[2]]   
head(jac[[2]])

rg <- unique(ttb$V5)
ds <- unique(ttb$dist)

arsq <- numeric()
pval <- numeric()  
e=4
i=10
ds
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



tiff("Fig4.tiff", compression = "lzw", width = 600, height = 600,  res=150, units = "mm",pointsize = 43)
par(mfrow=c(2,2), oma=c(2,2,0,0), mar=c(2.5,2.5,3,3.5))

for(i in 1:2){
  yb <- pho[pho$X2==ll[[i]][1],4]
  yo <- pho[pho$X2==ll[[i]][3],4]
  yg <- pho[pho$X2==ll[[i]][2],4]
  if(i==1){
    plot( yb, type="l", col="black", ylim=c(-0.1,0.55), ylab="",xlab="distance (m)", main="BMH region", lwd=3, xpd=T, xaxt="n")
    axis(1,  at=1:length(yb), labels=as.numeric(disy[[i]]))
    text(x=c(27.5,27.5,27.5),y=c(yo[NROW(yo)], yg[NROW(yg)],yb[NROW(yb)])  , c("BMHm", "BMHf", "BMH"), col=c("orange", "darkgreen", "black"), xpd=T, adj=0, font = 2)
    mtext(side = 3, "(a)", cex=1.7, line=1, font=2, adj=-0.3,  xpd=T)
   }else{
    plot( yb, type="l", col="black", ylim=c(-0.1,0.55), ylab="",xlab="distance (m)", main="WCM region", lwd=3, xpd=F, xaxt="n")  
    axis(1,  at=1:length(yb), labels=as.numeric(disy[[i]]))
    text(x=c(27.5,27.5,27.5),y=c(yo[NROW(yo)], yg[NROW(yg)],yb[NROW(yb)])  , c("WCMm", "WCMf", "WCM"), col=c("orange", "darkgreen", "black"), xpd=T, adj=0, font = 2)
  }
  abline(v=c(5,8), col="gray", lty=2, lwd=3)
  abline(h=0, col="gray", lwd=3, lty=2)
  lines( yo,col="orange", lwd=3)
  lines( yg,col="darkgreen", lwd=3)
  #vrphons <- pho[pho$X2 %in% ll[[i]] , ]
  #points(vrphons$X1, vrphons$value.y, col="white", pch=16, cex=0.6)
  vrphosig <- pho[pho$X2 %in% ll[[i]] & !is.na(pho$value.x), ]
  points(match( vrphosig$X1, disy[[i]]), vrphosig$value.y, col="red", pch=16, cex=0.6)
  
  
}


for(i in c(1,4)){
  yf <- c(NA,NA, NA,NA,lcbd_fit[lcbd_fit$rg==rg[i  ],3])
  ym <- c(NA,NA, NA,NA,lcbd_fit[lcbd_fit$rg==rg[i+1],3])
  ya <- c(NA,NA, NA,NA,lcbd_fit[lcbd_fit$rg==rg[i+2],3])
  pf <- c(NA,NA, NA,NA,lcbd_fit[lcbd_fit$rg==rg[i  ],4])
  pm <- c(NA,NA, NA,NA,lcbd_fit[lcbd_fit$rg==rg[i+1],4])
  pa <- c(NA,NA, NA,NA,lcbd_fit[lcbd_fit$rg==rg[i+2],4])
  if(i==1){
    plot(ya, type="l", col="black", ylim=c(-0.15,0.4), ylab="",xlab="distance (m)", main="BMH region",xaxt="n", lwd=3, xpd=T)
    axis(1,  at=1:length(ya), labels=dis)
    abline(v=c(5,8), col="gray", lty=2, lwd=3)
    text(x=c(27.5,27.5,27.5),y=c(ya[NROW(ya)], yf[NROW(yf)],ym[NROW(ym)])  , c("BMH", "BMHf", "BMHm"), col=c("black", "darkgreen", "orange"), xpd=T, adj=0, font = 2)
    mtext(side = 3, "(b)", cex=1.7, line=1, font=2, adj=-0.3,  xpd=T)
    }else{
    plot(ya, type="l", col="black", ylim=c(-0.15,0.4),  ylab="",xlab="distance (m)", main="WCM region", lwd=3, xaxt="n", xpd=F)  
      axis(1,  at=1:length(ya), labels=dis)
      abline(v=c(5,8), col="gray", lty=2, lwd=3)
   text(x=c(27.5,27.5,27.5),y=c(ya[NROW(ya)], yf[NROW(yf)],ym[NROW(ym)])  , c("WCM", "WCMf", "WCMm"), col=c("black", "darkgreen", "orange"), xpd=T, adj=0, font = 2)
  }
  abline(h=0, col="gray", lwd=3, lty=2)
  lines( ym,col="orange", lwd=3)
  lines( yf,col="darkgreen", lwd=3)
  
  points((1:NROW(ya))[which(pa<0.05)],ya[which(pa<0.05)], col="red", pch=16, cex=0.6)
  points((1:NROW(ym))[which(pm<0.05)],ym[which(pm<0.05)], col="red", pch=16, cex=0.6)
  points((1:NROW(yf))[which(pf<0.05)], yf[which(pf<0.05)], col="red", pch=16, cex=0.6)
  
  
}

mtext(outer = T, side = 2, expression("adjusted R"^2), cex = 1, line=0.5, adj = 0.75)
mtext(outer = T, side = 2, expression("adjusted R"^2), cex = 1, line=0.5, adj = 0.25)
mtext(outer = T, side = 1, "distance (m)", cex = 1, line=0.5)


dev.off()
