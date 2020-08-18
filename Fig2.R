
vr <- read.table("orig_data/vr_resamp943gr.txt", check.names = F, row.names = 1)
bk <- read.table("orig_data/bk_resamp943gr.txt", check.names = F, row.names = 1)
rownames(vr) <- vr$taxa
rownames(bk) <- bk$taxa
prih <- read.table("orig_data/meta.txt", sep="\t")
bk[bk>0] <- 1
vr[vr>0] <- 1

vrm <- vr[,as.character(prih[prih$V1=="VR" & prih$V4=="meadow", 2])]
vrf <- vr[,as.character(prih[prih$V1=="VR" & prih$V4=="poor forest", 2])]
bkf <- bk[,as.character(prih[prih$V1=="BK" & prih$V4=="forest", 2])]
bkm <- bk[,as.character(prih[prih$V1=="BK" & prih$V4=="meadow", 2])]
vrw <- vr[,as.character(prih[prih$V1=="VR" & prih$V4 %in% c("pond", "quarry"), 2])]
vr <- vr[,as.character(prih[prih$V1=="VR" & !(prih$V4 %in% c("pond", "quarry")), 2])] 
bk <- cbind(bkf, bkm)



palydata <- list(colSums(vrf), colSums(vrm), colSums(bkf), colSums(bkm),  colSums(vr), colSums(vrw), colSums(bk))
load(file="ready_data/BDtotal_p_jaccard.RData") 
fin <- numeric()
for(i in 1:7) {fin[i] <- bet_p[[i]]["BDtotal"]}
tabl <- tabl[-6]
fin  <- fin[-6]
palydata <- list(colSums(vrf), colSums(vrm), colSums(bkf), colSums(bkm),  colSums(vr),  colSums(bk))
popi <- c("BMHf", "BMHm" ,"WCMf" ,"WCMm" ,"BMH"  ,"WCM") 
kolor <- c("darkgreen", "orange","darkgreen", "orange", gray(0.3), gray(0.3))
tiff("Fig2.tiff", pointsize = 33, compression = "lzw", height = 200, width = 200, res=300, units = "mm")
par(mar=c(4,4,1,1))
boxplot(palydata,      at=fin, xlim=c(0.20,0.26),  pars = list(boxwex = 0.005, staplewex = 0.5, outwex = 0.5),
        xlab="BDtotal", ylab="pollen richness", xaxt="n", las=2, col=kolor)
axis(1, at=c(0.20,0.21,0.22,.23, 0.24, .25), labels = c(0.20,0.21,0.22,.23, 0.24,  .25), las=2)
text(x=fin-c(0.001,0.001,0.001,0.001,-0.002,-0.002),y=sapply(palydata, median)+c(1,4.5,5,5,-2.5,-2.5), labels = popi,  srt=90,  pos=c(2,2,2,2,4,4) ,font = 2, cex=0.8, col=kolor)#, pos=c(1,3,1,3,3,3),
dev.off()

