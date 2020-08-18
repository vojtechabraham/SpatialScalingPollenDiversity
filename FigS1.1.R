
pho <- read.table("ready_data/FigS1.1_R2_data_350gr_ppe.txt")
ph <- read.table("ready_data/FigS1.1_R2_data_350gr.txt")

dis2 <- as.numeric(unique(pho[pho$X2 %in% c("all",   "fst","mdw"),1]))
dis <- as.numeric(unique(pho[!(pho$X2 %in% c("all",   "fst","mdw")),1]))

pho <- merge(pho, ph, by=1:2)
pho <- pho[order(pho$X1),]


vv <- c("vr",   "vr forest","vr meadow")
bb <- c("bk","bk forested" ,"bk open")
ss <- c("all",   "fst","mdw")

ll <- list(vv,bb,ss)
ll <- c(vv,bb)
i=1
jmn <- c("BMH region", "WCM region")#, "simulation")
disy <- list(dis,dis)#, dis2)
xlimax <- c(1000,1000)#, 2500)
abl <- c(550, 250, 250 )
pho[pho$value.y==0, 4] <- NA
#pdf("adjusted_Rsquared.pdf", width = 10)

jm <-  c("BMH all", "BMH forest", "BMH meadow", "WCM all", "WCM forest", "WCM meadow")

tiff("FigS1.1.tiff", compression = "lzw", width = 600, height = 1000,  res=300, units = "mm",pointsize = 55)
par(mfcol=c(3,2), oma=c(2,2.5,0,0), mar=c(4.5,2.5,3,2.5))
i=1
for(i in 1:6){
  ynorm <- pho[pho$X2==ll[[i]][1],6]
  yppe  <- pho[pho$X2==ll[[i]][1],4]
    plot( ynorm, type="l", col="black", ylim=c(-0.1,0.55), ylab="",xlab="", main=jm[i], lwd=3, xpd=T, xaxt="n")
    axis(1,  at=1:length(yb), labels=dis)
    text(x=c(27.5,27.5),y=c(ynorm[NROW(ynorm)], yppe[NROW(yppe)])  , c("raw", "ppe"), col=c("black", "blue"), xpd=T, adj=0, font = 2)
  
  abline(v=c(5,8), col="gray", lty=2, lwd=3)
  abline(h=0, col="gray", lwd=3, lty=2)
  lines( yppe,col="blue", lwd=3)
  vrphosig <- pho[pho$X2 %in% ll[[i]] & !is.na(pho$value.x.x), ]
  points(match( vrphosig$X1, dis), vrphosig$value.y.x, col="red", pch=16, cex=0.6)
  vrphosign <- pho[pho$X2 %in% ll[[i]] & !is.na(pho$value.x.y), ]
  points(match( vrphosign$X1, dis), vrphosign$value.y.y, col="red", pch=16, cex=0.6)
  
  
}
mtext(outer = T, side = 1, "distance (m)", cex = 1, line=0.5)
mtext(outer = T, side = 2, expression("adjusted R"^2), cex = 1, line=0.5)
dev.off()

