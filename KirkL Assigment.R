get_heights <- function(number){
  heights <- rnorm(number)
  mean(heights)
  }
get_heights(100)
heights=get_heights(100)
heights
mean(heights)
for (1000){get_heights}
mean_heights_100 <- rep(NA,1000)
for (ii in 1:1000){
  mean_heights_100[ii]<- get_heights(100)
  }
mean_heights_1000 <- rep(NA,1000)
for (ii in 1:1000){
  mean_heights_1000[ii]<- get_heights(1000)
}
pdf(file="homework hist.pdf", width=4,height=7)
?hist
bins<-seq(-10,10,by=1)
hist(get_heights_100,breaks=bins)$breaks
hist(mean_heights_100,breaks=bins)$breaks
hist(mean_heights_1000,breaks=bins)$breaks
counts_mean_heights_100<-hist(mean_heights_100,breaks=bins)$counts
counts_mean_heights_1000<-hist(mean_heights_1000,breaks=bins)$counts
pdf(file="yay.pdf",width=6,height=6)
par(mfrow=c(1,1), mar=c(4,4,3,2))
barplot(rbind(counts_mean_heights_100,counts_mean_heights_1000),col=c(2,4),beside=T,names.arg=seq(-2,2,length.out = 20),xlab="average height inches",ylab="count")
legend(6,350,c("n=100","n=1000"), lwd=4,col=c(2,4),cex=1.5)
dev.off()
