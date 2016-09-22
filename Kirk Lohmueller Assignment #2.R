#Kirk 2.2
read.table("pheno.sim.2014.txt", header=T)
getwd()
read.table("pheno.sim.2014-2.txt", header=TRUE)
pheno= read.table("pheno.sim.2014-2.txt", header=T)
quantile(pheno$glucose_mmolperL,0.25)
#C
quantile(pheno$glucose_mmolperL,0.25)
#D Density Plot
hist(pheno$glucose_mmolperL,xlab="glucose levels (mm/L)", main="Density Glucose Level Phenotypes")
abline(v=quantile(pheno$glucose_mmolperL,0.25), col=5)
abline(v=quantile(pheno$glucose_mmolperL, 0.75), col=8)