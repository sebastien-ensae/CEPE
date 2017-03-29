library(data.table)
library(dbscan)
library(fpc)
library(stats)
library(deldir)


setwd("F:/Ensae/4.Data scientist - M3")



d.small <- read.table('data/donclassif.txt', sep = ";", header = TRUE)
d.big <- read.table('data/donclassif2.txt', sep = ";", header = TRUE)
plot(d.big$V1, d.big$V2)


nrow(d.big)
nrow(d.small)

nclust = 4
res <- kmeans(d.big, nclust, 100)
plot(d.big, col=res$cluster)
points(res$centers, cex=2, pch=16, col=1:nclust)

nclust <- nrow(d.small)
many_clust<- kmeans(d.big, nclust, iter.max = 10^5)
plot(many_clust$centers)
tmp.var <- as.data.frame((many_clust$cluster))
View(tmp.var)
as.vector(many_clust$cluster)
d.big.clust <- d.big
d.big.clust$kcenter <- as.vector(many_clust$cluster)

hclust.centers.ward <-hclust(dist(many_clust$centers), method = 'ward')
plot(hclust.centers.ward)
#summary(hclust.centers.single)
#plot(sort(hclust.centers.ward$height, dec=T)[1:100], type = 'h')
cut.ward.k4 <- cutree(hclust.centers.ward, k = 4)
plot(many_clust$centers, col=cut.ward.k4)


hclust.centers.single <-hclust(dist(many_clust$centers), method = 'single')
plot(hclust.centers.single)
#summary(hclust.centers.single)
plot(sort(hclust.centers.single$height, dec=T)[1:100], type = 'h')

cut.single.h03 <- cutree(hclust.centers.single, h = 0.3)
plot(many_clust$centers, col=cut.single.h03)

cut.single.h25 <- cutree(hclust.centers.single, h = 0.25)
plot(many_clust$centers, col=cut.single.h25)

cut.single <- cut.single.h03
tmp.var <- as.data.frame(cut.single)
View(tmp.var)
length(levels(as.factor(cut.single)))

hclust.id <- as.data.frame(cut.single)
hclust.id$hcut_lab<- rownames(hclust.id)
#d.big.clust$hcut <- hclust.id[d.big.clust$kcenter]

#KO d.big.clust[d.big.clust$kcenter == hclust.id$hcut_lab, hcut <- hclust.id$cut.single]




d.big.clust[1,]$kcenter
hclust.id[6]



