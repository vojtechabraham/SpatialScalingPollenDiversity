ff<-read.table("ready_data/FigS2.2richness_all.txt", sep=";", header = T, check.names = F)
xlimi <- list(c(100,290), c(50,290), c(80,700) )
ylimi <- c(24,60)
subs <- c( "forest", "meadow", "all","forest", "meadow", "all") 
jmn <- c("BMH region",NA,NA, "WCM region")
dts <-list("vrf", "vrm",c("vrf","vrm"),"bkf","bkm",c("bkf","bkm"))
dis <- c(70,550,550, 300,1.5,250)

tiff("FigS2.1.tiff", compression = "lzw",res = 300, width = 1000, height = 2000)#, pointsize=65,units = "mm")
par(mfcol=c(3,2), oma=c(3,3,3,1), mar=c(4.5,3,3,0))

for(f in 1:NROW(dts)){
  fl <-  as.integer(ff[as.character(ff$V4) %in% dts[[f]],as.character(dis[f])])
  pyl <- as.integer(ff[as.character(ff$V4) %in% dts[[f]],"pr"])
  # jmn <- names()
  
  
  plot( pyl~ fl, pch=16, main= "", xlab=paste("at",dis[f], "m"), ylab = "", col="transparent")
  abline(lm( pyl~fl), col="blue")
  text(  fl, pyl,labels = ff[as.character(ff$V4) %in% dts[[f]],"id"], cex = 0.5, xpd=T, col="darkred")
  
  title(line=0.5, subs[f], cex.main=1.5, font=2)
  if(f %in% c(1,4)){mtext(jmn[f], line = 3, cex=1.2)}
}

mtext(outer = T, side = 1,line=1, "floristic richness", cex = 1.2)
mtext(outer = T, side = 2, line=1,"pollen richness", cex = 1.2)

dev.off()


