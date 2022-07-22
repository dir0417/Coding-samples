#Load libraries
library("vegan")
library("car")
library("cluster")
library("gplots")
library("rgl")

library(grid)

# xsnorm -in pca_otufile.cts -out pca_otufile -alg 1 ;
# xsnorm -in pca_otufile.cts -out pca_otufile -alg 2 ;
# xsnorm -in pca_otufile.cts -out pca_otufile -alg 3 ;
# xsnorm -in pca_otufile.cts -out pca_otufile -alg 4 ;
# xsnorm -in pca_otufile.cts -out pca_otufile -alg 5 ;

#Import files into R

#These files transform raw sequence count data into various format
# per = percentage (relative abundance), bin = binary (OTU presence/absence)
# clt = center-log transformed (converts abundance data to linear scale
# log = log transformed abundances.  unit = unit vector transformation
# I now have R functions to perform most of these transformations, so one
# can simply import the .cts file
otu_cts = read.table("pca_otufile.cts",header=TRUE, na.strings = "na")
otu_per = read.table("pca_otufile.per",header=TRUE, na.strings = "na")
otu_bin = read.table("pca_otufile.bin",header=TRUE, na.strings = "na")
otu_clt = read.table("pca_otufile.clt",header=TRUE, na.strings = "na")
otu_log = read.table("pca_otufile.log",header=TRUE, na.strings = "na")
otu_unit = read.table("pca_otufile.unit",header=TRUE, na.strings = "na")

# Read in metadata
meta = read.table("PCA_MetadataFile.txt",header=TRUE, na.strings = "na")

#Store OTU names in list "otu_nms"
otu_nms = otu_cts[,1]

#Remove column of OTU names, transpose data so subjects are rows, OTUs are columns
# since vegan and other packages analyze data in this format.  Finally, convert
# data to a matrix.
otu_cts = as.matrix(t(otu_cts[,-1]))
otu_per = as.matrix(t(otu_per[,-1]))
otu_bin = as.matrix(t(otu_bin[,-1]))
otu_clt = as.matrix(t(otu_clt[,-1]))
otu_log = as.matrix(t(otu_log[,-1]))
otu_unit = as.matrix(t(otu_unit[,-1]))


#colnames(genus_per) = genus_nms
#rownames(meta2) = rownames(genus_per)


#******************  Principle Components Analysis

# R has two functions for PCA.  Originally I used "princomp", but now use "prcomp"
# princomp is limited in that the number of variables must be less than the number of subjects
# Results are stored in objects XXX.pccor, which are plotted below
otu_cltCOR.pccor = princomp(otu_clt[,1:30],cor=TRUE)
otu_ctsCOR.pccor = princomp(otu_cts[,1:30],cor=TRUE)
otu_perCOR.pccor = princomp(otu_per[,1:30],cor=TRUE)
otu_cltCOV.pccor = princomp(otu_clt[,1:30],cor=FALSE)
otu_ctsCOV.pccor = princomp(otu_cts[,1:30],cor=FALSE)
otu_perCOR.pccor = princomp(otu_per[,1:30],cor=FALSE)

otu_cltCOR.prcomp = prcomp(otu_clt[,1:30], scale=TRUE)  #prcomp doesn't require variables < subject.  
otu_cltCOV.prcomp = prcomp(otu_clt[,1:30], scale=FALSE)  # Use 30 to be consistent with princomp

totu_cltCOV.prcomp = prcomp(t(otu_clt[,1:30]), scale=FALSE)

# 2-D plots of axes 1-3 from princomp function, labeled by anatomy metadata
par(mfrow=c(2,2),pty="s")
plot(otu_cltCOR.pccor)
plot(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,2], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,3], xlab = "PC1", ylab="PC3", type="n", lwd=2)
text(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,3], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_cltCOR.pccor$scores[,2], otu_cltCOR.pccor$scores[,3], xlab = "PC2", ylab="PC3", type="n", lwd=2)
text(otu_cltCOR.pccor$scores[,2], otu_cltCOR.pccor$scores[,3], cex = 0.5, label = substr(meta$Anat, 1,1))

# 3-D plot (not sure if this works on newer versions of R
plot3d(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,2], otu_cltCOR.pccor$scores[,3], xlab = "PC1", ylab="PC2", zlab="PC3", type="s", size = 2,  col=meta$Anat)
# Write PDF
rgl.postscript("otu_cltCOR.pdf","pdf")

# 2-D plots of results from prcomp, labeled by anatomy metadata
par(mfrow=c(2,2),pty="s")
plot(otu_cltCOR.prcomp)
plot(otu_cltCOR.prcomp$x[,1], otu_cltCOR.prcomp$x[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_cltCOR.prcomp$x[,1], otu_cltCOR.prcomp$x[,2], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_cltCOR.prcomp$x[,1], otu_cltCOR.prcomp$x[,3], xlab = "PC1", ylab="PC3", type="n", lwd=2)
text(otu_cltCOR.prcomp$x[,1], otu_cltCOR.prcomp$x[,3], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_cltCOR.prcomp$x[,2], otu_cltCOR.prcomp$x[,3], xlab = "PC2", ylab="PC3", type="n", lwd=2)
text(otu_cltCOR.prcomp$x[,2], otu_cltCOR.prcomp$x[,3], cex = 0.5, label = substr(meta$Anat, 1,1))

plot3d(otu_cltCOR.prcomp$x[,1], otu_cltCOR.prcomp$x[,2], otu_cltCOR.prcomp$x[,3], xlab = "PC1", ylab="PC2", zlab="PC3", type="s", size = 2,  col=meta$Anat)
rgl.postscript("otu_cltCOR_prcomp.pdf","pdf")


# "par(mfrow=c(2,4)) instructs R to plot a 2x4 matrix of panels, to compare methods
par(mfrow=c(2,4),pty="s")
plot(otu_cltCOR.pccor)
plot(otu_cltCOV.pccor)
plot(otu_cltCOR.prcomp)
plot(otu_cltCOV.prcomp)
plot(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,2], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_cltCOV.pccor$scores[,1], otu_cltCOV.pccor$scores[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_cltCOV.pccor$scores[,1], otu_cltCOV.pccor$scores[,2], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_cltCOR.prcomp$x[,1], otu_cltCOR.prcomp$x[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_cltCOR.prcomp$x[,1], otu_cltCOR.prcomp$x[,2], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_cltCOV.prcomp$x[,1], otu_cltCOV.prcomp$x[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_cltCOV.prcomp$x[,1], otu_cltCOV.prcomp$x[,2], cex = 0.5, label = substr(meta$Anat, 1,1))

par(mfrow=c(2,2),pty="s")
biplot(otu_cltCOR.pccor)
biplot(otu_cltCOV.pccor)
biplot(otu_cltCOR.prcomp)
biplot(otu_cltCOV.prcomp)

par(mfrow=c(1,2),pty="s")
plot(otu_cltCOR.pccor)
plot(otu_cltCOR.prcomp)

par(mfrow=c(1,2),pty="s")
biplot(otu_cltCOR.pccor)
biplot(otu_cltCOR.prcomp)

par(mfrow=c(1,1),pty="s")
biplot(otu_cltCOR.prcomp)

par(mfrow=c(2,2),pty="s")
plot(otu_perCOR.pccor)
plot(otu_perCOR.pccor$scores[,1], otu_perCOR.pccor$scores[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_perCOR.pccor$scores[,1], otu_perCOR.pccor$scores[,2], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_perCOR.pccor$scores[,1], otu_perCOR.pccor$scores[,3], xlab = "PC1", ylab="PC3", type="n", lwd=2)
text(otu_perCOR.pccor$scores[,1], otu_perCOR.pccor$scores[,3], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_perCOR.pccor$scores[,2], otu_perCOR.pccor$scores[,3], xlab = "PC2", ylab="PC3", type="n", lwd=2)
text(otu_perCOR.pccor$scores[,2], otu_perCOR.pccor$scores[,3], cex = 0.5, label = substr(meta$Anat, 1,1))

par(mfrow=c(1,1),pty="s")
biplot(otu_perCOR.pccor)

plot3d(otu_perCOR.pccor$scores[,1], otu_perCOR.pccor$scores[,2], otu_perCOR.pccor$scores[,3], xlab = "PC1", ylab="PC2", zlab="PC3", type="s", size = 2,  col=meta$Anat)
rgl.postscript("otu_perCOR.pdf","pdf")

par(mfrow=c(2,2),pty="s")
plot(otu_cltCOR.pccor)
plot(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,2], cex = 0.5, label = substr(meta$Microenvironment, 1,1))
plot(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,3], xlab = "PC1", ylab="PC3", type="n", lwd=2)
text(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,3], cex = 0.5, label = substr(meta$Microenvironment, 1,1))
plot(otu_cltCOR.pccor$scores[,2], otu_cltCOR.pccor$scores[,3], xlab = "PC2", ylab="PC3", type="n", lwd=2)
text(otu_cltCOR.pccor$scores[,2], otu_cltCOR.pccor$scores[,3], cex = 0.5, label = substr(meta$Microenvironment, 1,1))

par(mfrow=c(1,1),pty="s")
biplot(otu_perCOR.pccor)

plot3d(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,2], otu_cltCOR.pccor$scores[,3], xlab = "PC1", ylab="PC2", zlab="PC3", type="s", size = 2,  col=meta$Microenvironment)
rgl.postscript("otu_perCOR.pdf","pdf")
plot3d(otu_perCOR.pccor$scores[,1], otu_perCOR.pccor$scores[,2], otu_perCOR.pccor$scores[,3], xlab = "PC1", ylab="PC2", zlab="PC3", type="s", size = 2,  col=meta$Microenvironment)

#Compare CLT to PER
par(mfrow=c(2,2),pty="s")
plot(otu_cltCOR.pccor)
plot(otu_perCOR.pccor)
plot(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_cltCOR.pccor$scores[,1], otu_cltCOR.pccor$scores[,2], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_perCOR.pccor$scores[,1], otu_perCOR.pccor$scores[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_perCOR.pccor$scores[,1], otu_perCOR.pccor$scores[,2], cex = 0.5, label = substr(meta$Anat, 1,1))


#write.table(genus_allCOV.pccor$scores, "genus_allCOV.txt")
#write.table(genus_allCOR.pccor$scores, "genus_allCOR.txt")

par(mfrow=c(2,2),pty="s")
plot(otu_ctsCOR.pccor)
plot(otu_ctsCOR.pccor$scores[,1], otu_ctsCOR.pccor$scores[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(otu_ctsCOR.pccor$scores[,1], otu_ctsCOR.pccor$scores[,2], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_ctsCOR.pccor$scores[,1], otu_ctsCOR.pccor$scores[,3], xlab = "PC1", ylab="PC3", type="n", lwd=2)
text(otu_ctsCOR.pccor$scores[,1], otu_ctsCOR.pccor$scores[,3], cex = 0.5, label = substr(meta$Anat, 1,1))
plot(otu_ctsCOR.pccor$scores[,2], otu_ctsCOR.pccor$scores[,3], xlab = "PC2", ylab="PC3", type="n", lwd=2)
text(otu_ctsCOR.pccor$scores[,2], otu_ctsCOR.pccor$scores[,3], cex = 0.5, label = substr(meta$Anat, 1,1))

plot3d(otu_ctsCOR.pccor$scores[,1], otu_ctsCOR.pccor$scores[,2], otu_ctsCOR.pccor$scores[,3], xlab = "PC1", ylab="PC2", zlab="PC3", type="s", size = 2,  col=meta$Anat)
rgl.postscript("otu_ctsCOR.pdf","pdf")

par(mfrow=c(2,2),pty="s")
plot(totu_cltCOV.prcomp)
plot(totu_cltCOV.prcomp$x[,1], totu_cltCOV.prcomp$x[,2], xlab = "PC1", ylab="PC2", type="n", lwd=2)
text(totu_cltCOV.prcomp$x[,1], totu_cltCOV.prcomp$x[,2], cex = 0.5)
plot(totu_cltCOV.prcomp$x[,1], totu_cltCOV.prcomp$x[,3], xlab = "PC1", ylab="PC3", type="n", lwd=2)
text(totu_cltCOV.prcomp$x[,1], totu_cltCOV.prcomp$x[,3], cex = 0.5)
plot(totu_cltCOV.prcomp$x[,2], totu_cltCOV.prcomp$x[,3], xlab = "PC2", ylab="PC3", type="n", lwd=2)
text(totu_cltCOV.prcomp$x[,2], totu_cltCOV.prcomp$x[,3], cex = 0.5)

plot3d(totu_cltCOV.prcomp$x[,1], totu_cltCOV.prcomp$x[,2], totu_cltCOV.prcomp$x[,3], xlab = "PC1", ylab="PC2", zlab="PC3", type="s", size = 2)

