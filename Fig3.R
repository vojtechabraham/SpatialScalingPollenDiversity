library(reshape)

prih <- read.table("orig_data/meta.txt", sep="\t")
prih <- prih[!(prih$V4 %in% c("pond", "quarry")),c(1:2,4)]
ppa <- rbind(prih[prih$V1=="VR",], prih[prih$V1=="BK",])
ppa[,3] <- "all"
prih <- rbind(prih,ppa)
coo <- unique(prih[,c(1,3)])
coo <- coo[order(coo$V1),]

k <- read.table("orig_data/exclude.txt")
v <- read.table("orig_data/floristic_data.txt")
pomv <- aggregate(v$relev, by=list(v$Group.1,v$Group.2, v$Group.3, v$x), min) # Jurickovy duplicity, kdy je jeden druh ve vice polygonech na stejne vzdalenosti
v <- merge(pomv, v, by=1:5)
vk <- v[!(v$Group.3 %in% k[,1]),] # o ca 90 radku min vypadnou vruha urceni
unique(vk$m)
radi <- sort(unique(vk$Group.4))
vk <- vk[,c(1:4,14) ]
vk <- vk[!(vk$Group.1=="VR"&vk$Group.2>30),]

vk$m <- 1
vkm <- aggregate(vk$m, by=list( vk$Group.1, vk$Group.2, vk$Group.4), sum)
vkk <- cast(vkm, formula=Group.1+Group.2~Group.3)
vkk[is.na(vkk)]<-0

for(i in 3:(ncol(vkk)-1)) {
  vkk[,i+1] <- vkk[,i]+vkk[,i+1]
}

vkk <- merge(prih[prih$V4!="all",1:3], vkk,by=1:2 )

vlin <- aggregate(vkk[,4:29], by=list(vkk$V1, vkk$V4), FUN=mean)

load("ready_data/BDtotal_jacc_diff_dist.RData")
i=4
jmn <- c(NA,NA, "White Carpathians", NA,NA, "B.M. Highland")
disy <- list(NA,NA, radi, NA,NA,radi[5:26])
dis <- list(c(),c(),c(),c(NA,NA,NA, NA),c(NA,NA,NA, NA),c(NA,NA,NA, NA))



#tiff("Fig3.tiff", compression = "lzw",res = 300, width = 700, height = 650, units = "mm", pointsize = 50)
tiff("Fig3.tiff", compression = "lzw",res = 150, width = 700, height = 650, units = "mm", pointsize = 50)

par(mfrow=c(2,2), oma=c(2,3,3,1), mar=c(3,2,0,3))

yx <- vlin[4,3:28]
yx[1:4] <- NA
plot(x=1:length(yx),y=as.numeric(yx), type="l", col="transparent", ylim=c(0,315),ylab="",xlab="", xaxt="n",main="", lwd=4, xpd=F)
      axis(1,  at=1:length(yx), labels=as.numeric(names(yx)))
      abline(v=c(5,8), col="gray", lty=2, lwd=3)
      lines(x=1:length(yx),y=as.numeric(yx),col="darkgreen", lwd=4)
yx <- vlin[3,3:28]
yx[1:4] <- NA
      lines(x=1:length(yx),y=as.numeric(yx),col="orange", lwd=4)
      text(x=c(27.5,27.5),y= vlin[3:4,28], c("BMHm", "BMHf"), col=c("orange", "darkgreen"), xpd=T, adj=0, font = 2)
      #mtext("B.M. Highland", line = 1.5, cex=1.2)
      mtext(side = 2,"floristic richness", line = 3, cex=1.2)
      mtext(side=3,expression(bold("BMH region"^"")), line=0.8, cex = 1.2,  adj=0.38)
      
      yx <- vlin[1,3:28]

plot(x=1:length(yx),y=as.numeric(yx), type="l", col="transparent", ylim=c(0,315),ylab="",xlab="",xaxt="n", main="", lwd=4, xpd=F)
axis(1,  at=1:length(yx), labels=as.numeric(names(yx)))
abline(v=c(5,8), col="gray", lty=2, lwd=3)
lines(x=1:length(yx),y=as.numeric(yx),col="darkgreen", lwd=4)
yx <- vlin[2,3:28]
lines(x=1:length(yx) ,y=as.numeric(yx),col="orange", lwd=4)
text(x=c(27.5,27.5),y= vlin[1:2,28], c("WCMf", "WCMm"), col=c( "darkgreen", "orange"), xpd=T, adj=0, font = 2)

#mtext("White Carpathians", line = 1.5, cex=1.2)
mtext(side=3, expression(bold("WCM region"^"")), line=0.8,  cex = 1.2, adj=0.38)



for(i in c(6, 3)){
  plot(1:26, c(dis[[i]],as.numeric(bet[[i]][,"BDtotal"])), type="l", col="transparent", ylim=c(0.28,0.48),ylab="",xlab="", main="", lwd=4, xpd=F, xaxt="n")
  axis(1,  at=1:26, labels=radi)
  abline(v=c(5,8), col="gray", lty=2, lwd=3)
  if(i==6){
    mtext(side = 2,"BDtotal", line = 3, cex=1.2)
    text(x=c(27.5,27.5,27.5),y= c(0.31, 0.295, 0.28),c("BMH", "BMHf", "BMHm"), col=c("black", "darkgreen", "orange"), xpd=T, adj=0, font = 2)
    lines(1:26, c(dis[[i]],as.numeric(bet[[i]][,"BDtotal"])),col="black", lwd=4 )
    lines(1:26, c(dis[[i]],as.numeric(bet[[i-2]][,"BDtotal"])),col="orange", lwd=4)
    lines(1:26, c(dis[[i]],as.numeric(bet[[i-1]][,"BDtotal"])),col="darkgreen", lwd=4)
    
  }else {
    text(x=c(27.5,27.5,27.5),y= c(0.31,  0.28, .295),c("WCM", "WCMf", "WCMm"), col=c("black", "darkgreen", "orange"), xpd=T, adj=0, font = 2)
    lines(1:26, c(dis[[i]],as.numeric(bet[[i]][,"BDtotal"])),col="black", lwd=4 )
    lines(1:26, c(dis[[i]],as.numeric(bet[[i-2]][,"BDtotal"])),col="orange", lwd=4)
    lines(1:26, c(dis[[i]],as.numeric(bet[[i-1]][,"BDtotal"])),col="darkgreen", lwd=4)
  }
  
}

mtext(outer = T, side = 1, "distance (m)", line=0.8, cex = 1.2)
dev.off()

