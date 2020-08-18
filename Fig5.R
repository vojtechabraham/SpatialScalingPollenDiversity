library(reshape)

prih <- read.table("orig_data/meta.txt", sep="\t")
prih <- prih[!(prih$V4 %in% c("pond", "quarry")),c(1:2,4)]
coo <- unique(prih[,c(1,3)])
coo <- coo[rev(order(coo$V1)),]

k <- read.table("orig_data/exclude.txt")
v <- read.table("orig_data/floristic_data.txt")
pomv <- aggregate(v$relev, by=list(v$Group.1,v$Group.2, v$Group.3, v$x), min) # Jurickovy duplicity, kdy je jeden druh ve vice polygonech na stejne vzdalenosti
v <- merge(pomv, v, by=1:5)
vk <- v[!(v$Group.3 %in% k[,1]),] # o ca 90 radku min vypadnou vruha urceni
vk <- vk[!(vk$Group.1=="VR"&vk$Group.2>30),]
radi <- sort(unique(vk$Group.4))
vv <- aggregate(vk$Group.4, list(vk$Group.1, vk$Group.2, vk$Group.4, vk$biot), FUN=length)
ncst <-    cast(vv, formula = Group.1+Group.2+Group.4~Group.3)
ncst[is.na(ncst)] <- 0

jd <- unique(prih$V4)

bar <- read.table("orig_data/colours_legend.txt", sep = ",", header=T, stringsAsFactors = F)
e=1
tit <- c("forest", "meadow","forest", "meadow")

tiff("Fig5.tiff", compression = "lzw",res = 150, width = 700, height = 500, units = "mm", pointsize = 50)
par(mfcol=c(2,2), oma=c(4,0,0,0))

for(e in 1:4){
  pp <- prih[prih$V1==coo[e,"V1"] & prih$V4==coo[e,"V4"],-3]
  pcnc  <-  merge(ncst, pp, by=c(1,2))
  
  zgrh <- aggregate(pcnc[,4:ncol(pcnc)], list(pcnc$Group.1,pcnc[,3]), FUN=mean)

  zgg <- zgrh
   
  zgg <-  t(zgrh[match(bar$rd,as.character(zgrh$Group.2)),3:28])
  fnu <- as.character(zgrh[match(bar$rd,as.character(zgrh$Group.2)),2])
  zgg <- zgg[,!is.na(fnu)]
  colnames(zgg) <- fnu[!is.na(fnu)]
  if(e %in% c(1,2)){
    par( mar=c(1,8,4,1))
  }else{
    par( mar=c(1,1,4,8))
  }
  barplot(as.matrix(t(zgg)), col= bar[bar$rd %in% colnames(zgg),2], las=2)  
  title(main=tit[e], line=0.8, cex.main=0.8)
  if(e==3){ title(main=expression(bold("WCM region"^"")), line=2.5)  

        xx <- round(max(rowSums(zgg)))
    points(c( rep(35, 7)),rev(seq(1,xx, xx/7)), pch=15, col=bar[16:10,2], cex=2, xpd=T)
    text(c( rep(37, 7)),rev(seq(1,xx, xx/7)), labels=bar[16:10,3], adj=0, xpd=T,  cex=0.8)    
  }
  if(e==4){
    xx <- round(max(rowSums(zgg)))
    points(c( rep(35, 8)),rev(seq(1,xx, xx/8)), pch=15, col=bar[9:2,2], cex=2, xpd=T)
    text(c( rep(37, 8)),rev(seq(1,xx, xx/8)), labels=bar[9:2,3], adj=0, xpd=T, cex=0.8)    
  }
  if(e==1){ title(main=expression(bold("BMH region"^"")), line=2.5)  
    
    }
}
mtext(side = 1, outer = T, text="distance (m)", line = 2, font = 2)
mtext(side = 2, outer = T, text="number of species", line = -5, font = 2)  

dev.off()  

