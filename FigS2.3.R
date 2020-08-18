
library(reshape)
prih <- read.table("orig_data/meta.txt", sep="\t")
prih <- prih[!(prih$V4 %in% c("pond", "quarry")),c(1:2,4)]
coo <- unique(prih[,c(1,3)])
coo <- coo[rev(order(coo$V1)),]

k <- read.table("orig_data/prostor_ciste_klastry_relev_bez_koncu_sousedu.txt")
vp <-k[order(k$reg, k$id, k$ringend),]
vps <- aggregate(vp$propplgring, by=list( vp$reg, vp$id, vp$ringend, vp$biot), FUN=sum )

ncst <-    cast(vps , formula = Group.1+Group.2+Group.4~Group.3)
ncst[is.na(ncst)] <- 0

jd <- unique(prih$V4)
bar <- read.table("orig_data/colours_legend.txt", sep = ",", header=T, stringsAsFactors = F)
e=1
tit <- c("forest", "meadow","forest", "meadow")

tiff("FigS2.3.tiff", compression = "lzw",res = 300, width = 700, height = 500, units = "mm", pointsize = 50)
par(mfcol=c(2,2), oma=c(4,0,0,0))
for(e in 1:4){
  pp <- prih[prih$V1==coo[e,"V1"] & prih$V4==coo[e,"V4"],-3]
  pcnc  <-  merge(ncst, pp, by=c(1,2))
  
  zgrh <- aggregate(pcnc[,4:ncol(pcnc)], list(pcnc$Group.1, pcnc$Group.4), FUN=mean)

  zgg <-  t(zgrh[match(bar$rd,as.character(zgrh$Group.2)),3:ncol(zgrh)])
  fnu <- as.character(zgrh[match(bar$rd,as.character(zgrh$Group.2)),2])
  zgg <- zgg[,!is.na(fnu)]
  colnames(zgg) <- fnu[!is.na(fnu)]
  zgg <- zgg/rowSums(zgg)
  if(e %in% c(1,2)){
    par( mar=c(1,6,4,3))
  }else{
    par( mar=c(1,1,4,8))
  }
  barplot(as.matrix(t(zgg)), col= bar[bar$rd %in% colnames(zgg),2], las=2)  
  title(main=tit[e], line=0.8, cex.main=0.8)
  if(e==3){ title(main=expression(bold("WCM region"^"")), line=2.5)  

        xx <- round(max(rowSums(zgg)))
    points(c( rep(30, 7)),rev(seq(0,xx, xx/6)), pch=15, col=bar[16:10,2], cex=2, xpd=T)
    text(c( rep(32, 7)),rev(seq(0,xx, xx/6)), labels=bar[16:10,3], adj=0, xpd=T,  cex=0.8)    
  }
  if(e==4){
    xx <- round(max(rowSums(zgg)))
    points(c( rep(30, 8)),rev(seq(0,xx, xx/7)), pch=15, col=bar[9:2,2], cex=2, xpd=T)
    text(c( rep(32, 8)),rev(seq(0,xx, xx/7)), labels=bar[9:2,3], adj=0, xpd=T, cex=0.8)    
  }
  if(e==1){ title(main=expression(bold("BMH region"^"")), line=2.5)  
    
    }
}

mtext(side = 1, outer = T, text="distance (m)", line = 2, font = 2)
mtext(side = 2, outer = T, text="mean proportional area", line = -2, font = 2)  

dev.off()  

