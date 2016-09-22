getwd()
setwd("/Users/iboucher/bootcamp-2016")
read.table("hapmap_CEU_r23a_chr2_ld-2.txt", header = T)

globalEnvironment <- read.table('hapmap_CEU_r23a_chr2_ld-2',head =T)
compute_chisquare=function(x){
  freq=sum(x,na.rm=T)/(2.0*sum(!is.na(x))) #minor allele freq
  cnt0=sum(x==0,na.rm=T) #types of genotypes
  cnt1=sum(x==1,na.rm=T)
  cnt2=sum(x==2,na.rm=T)
  obscnts=c(cnt0,cnt1,cnt2) #observed counts
  #print(obscnts)
  n=sum(obscnts)
  #built in chi sqr distribution
  exp_probs=c((1-freq)^2,2*freq*(1-freq),freq^2)
  chisq<-chisq.test(obscnts,p=exp_probs, correct = FALSE)$statistic
  return(chisq)
}
SNP = as.matrix(globalEnvironment); #SNP
chisqs=apply(SNP, 1, compute_chisquare); #SNP computation
pvals=pchisq(chisqs,1,lower.tail=FALSE) #lower end to p-value
#Part B
Threshold.05 <- 0.05;
sum(pvals < Threshold.05);
length(pvals);
sum((pvals < Threshold.05)/length(pvals));
#proportion = 0.0451

Threshold.01 <- 0.01;
sum((pvals < Threshold.01) / length(pvals));
#proportion = 0.0102;

Threshold.001 <- 0.001;
sum((pvals < Threshold.001) / length(pvals));
#proportion = 0.0012;

#Part C
num_pvals <- length(pvals);
#4014 SNPs

#Part D
exp_pvals <- ((seq(1, num_pvals, by = 1))/num_pvals);

#Part E
sort_pvals <- sort(pvals, decreasing = F);

#Part F
log_sort_pvals <- (-log10(sort_pvals));
log_exp_pvals <- (-log10(exp_pvals));

#Part G
plot(log_exp_pvals, log_sort_pvals, xlab = "-log(Expected P-Value)", ylab = "-log10(observed P-Value)", pch = 16);

#Part H
abline(0, 1, h = 5, col = 2, lty = 2)