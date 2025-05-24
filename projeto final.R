################################################################################
# EVELBAUER Beatriz 
# FLEURY Diego 
# STA203
# Projet Final
################################################################################
setwd("C:/Users/mevel/OneDrive/Documentos/ENSTA/2A/STA3") 

# libreries
library(ggplot2) # plots :)
library(MASS) # 
library(dplyr) # manipulation de données
library(pROC)
library(ROCR) # courbe roc 
library(corrplot)
library(leaps)
library(forecast) # cross validation
library(zoo) # also cross validation
library(cowplot) # multiple plots

library(GGally)
library(reshape2)
library(car)

library(FactoMineR) # PCA
library(alluvial) # alluvial plot
library(factoextra)
library(tibble)
library(cluster)
library(glmnet) # lasso et ridge

library(readxl)
# xls files


df <- data.frame(read_excel("Raisin.xlsx"))
head(df)
summary(df)

# variables reduites sans la cologne de classification des 
df.red <- data.frame(apply(df[,-8],2,function(x){
  x = (x - mean(x))/sd(x)
}))

################################################################################
### PARTIE I
################################################################################

########################
# question 1
########################
p1 <- ggplot(df.red) + geom_boxplot(aes(y = Area, fill = 'red', alpha = 0.7)) + 
  theme(legend.position="none")

p2 <- ggplot(df.red) + geom_boxplot(aes(y = MajorAxisLength, fill = 'red', alpha = 0.7)) + 
  theme(legend.position="none")

p3 <- ggplot(df.red) + geom_boxplot(aes(y = MinorAxisLength, fill = 'red', alpha = 0.7)) + 
  theme(legend.position="none")

p4 <- ggplot(df.red) + geom_boxplot(aes(y = Eccentricity, fill = 'red', alpha = 0.7)) + 
  theme(legend.position="none")

p5 <- ggplot(df.red) + geom_boxplot(aes(y = ConvexArea, fill = 'red', alpha = 0.7)) + 
  theme(legend.position="none")

p6 <- ggplot(df.red) + geom_boxplot(aes(y = Extent, fill = 'red', alpha = 0.7)) + 
  theme(legend.position="none")

p7 <- ggplot(df.red) + geom_boxplot(aes(y = Perimeter, fill = 'red', alpha = 0.7)) + 
  theme(legend.position="none")

plot_grid(p1,p2,p3,p4,p5,p6,p7, nrow = 3, ncol = 3)


p1 <- ggplot(df.red) + geom_boxplot(aes(y = Area, fill = as.factor(df$Class))) + 
  scale_fill_discrete(name = "Classification")

p2 <- ggplot(df.red) + geom_boxplot(aes(y = MajorAxisLength, fill = as.factor(df$Class))) + 
  scale_fill_discrete(name = "Classification")

p3 <- ggplot(df.red) + geom_boxplot(aes(y = MinorAxisLength, fill = as.factor(df$Class))) + 
  scale_fill_discrete(name = "Classification")

p4 <- ggplot(df.red) + geom_boxplot(aes(y = Eccentricity, fill = as.factor(df$Class))) + 
  scale_fill_discrete(name = "Classification")

p5 <- ggplot(df.red) + geom_boxplot(aes(y = ConvexArea, fill = as.factor(df$Class))) + 
  scale_fill_discrete(name = "Classification")

p6 <- ggplot(df.red) + geom_boxplot(aes(y = Extent, fill = as.factor(df$Class))) + 
  scale_fill_discrete(name = "Classification")

p7 <- ggplot(df.red) + geom_boxplot(aes(y = Perimeter, fill = as.factor(df$Class))) + 
  scale_fill_discrete(name = "Classification")

plot_grid(p1,p2,p3,p4,p5,p6,p7, nrow = 3, ncol = 3)

####### analyse bivarie - correlation plot 
ggpairs(df.red, aes(color = as.factor(df$Class), alpha = 0.5))

corrplot(cor(df.red),method = "square", diag = FALSE, type = "upper",
         tl.col = "black")

# un peu de clustering directement 

corrplot(cor(df.red),method = "circle", order = "hclust", addrect = 2,
         tl.col = "black")
corrplot(cor(df.red),method = "circle", order = "hclust", addrect = 3,
         tl.col = "black")
corrplot(cor(df.red),method = "circle", order = "hclust", addrect = 4,
         tl.col = "black")
corrplot(cor(df.red),method = "circle", order = "hclust", addrect = 5,
         tl.col = "black")
corrplot(cor(df.red),method = "circle", order = "hclust", addrect = 6,
         tl.col = "black")
corrplot(cor(df.red),method = "circle", order = "hclust", addrect = 7,
         tl.col = "black")


#############################
### question 2 
#############################
C = cor(df.red)         # matrice de corrélation 
cor(df[,-8])    # idem (pas obligé ce centrer réduire pour utiliser cor)
round(C,2)

eig = eigen(C)                 # diagonalisation
valp = eig$values              # valeurs propres
sum(valp)                      # somme des vp = 7 = nbre variables (ACP normée)
p_inertie = valp/sum(valp)*100 # pourcentage d'inertie
# [1] 69.03269444 20.75983485  8.97784954  0.81177875  0.31154669  0.09185987  0.01443585

barplot(p_inertie,col = "slateblue", names.arg = 1:7)        
abline(h=100/7, lwd = 2) # ligne de la moyenne d'inertie
title(main= "Pourcentage d'inertie par composante (ACP)",
      xlab = "Valeurs Propres",
      ylab = "Inertie (%)")

df_inertie <- data.frame(
  Composante = paste0("PC", seq_along(p_inertie)),
  Inertie = p_inertie
)

ggplot(df_inertie, aes(x = Composante, y = Inertie)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.75) +
  geom_hline(yintercept = 100/7, linetype = "dashed", color = "black", linewidth = 1) + 
  ylab("Inertie (%)") +
  xlab("Composante principale") +
  ggtitle("Pourcentage d'inertie par composante (ACP)") +
  theme_minimal()


# Calcul des coordonnées des individus sur les axes principaux
vectp = eig$vectors                  # axes principaux
apply(vectp^2,2,sum)         

Fp = as.matrix(df.red)%*%vectp      

# reconstruction de la matrice sum_j Fj * t(uj)
Xr = matrix(0,nrow=length(df.red$Area),ncol=length(df.red))    
for (j in 1:7) Xr = Xr + Fp[,j]%*%t(vectp[,j])
sum(abs(as.matrix(df.red) - Xr))                  # différence des coeff de l'ordr de la précision machine

sum(abs(as.matrix(df.red) - Fp %*% t(vectp)))

sum(abs(as.matrix(df.red) - as.matrix(df.red)%*%vectp %*% t(vectp))) # reconstruction du nuage: F%*%t(vectp)

# Calcul des coordonnées des variables sur les axes principaux
# pas besoin de normer les composantes principales, c'est la corrélation qui s'en charge
G1 = cor(as.matrix(df.red),Fp[,1])
G2 = cor(as.matrix(df.red),Fp[,2])

# Tracé des individus dans le premier plan principal
xlab = paste("Dim 1:", round(p_inertie[1],2),"%")
ylab = paste("Dim 2:", round(p_inertie[2],2),"%")


ggplot() + aes(x=Fp[,1],y=Fp[,2]) + geom_point()+
  geom_hline(yintercept=0, lty=2) + geom_vline(xintercept=0,lty=2) +
  geom_text(aes(label=val_ind),nudge_y = 0.15,size=3 )+
  theme(legend.position = "none") +
  ggtitle("nuage des individus") +
  xlab(xlab) +   ylab(ylab)


circle <- annotate("path",
                   x=0+1*cos(seq(0,2*pi,length.out=100)),
                   y=0+1*sin(seq(0,2*pi,length.out=100)))
ggplot() + aes(x=G1,y=G2)+
  coord_fixed(ratio = 1) + 
  geom_line(aes(x=x, y=y), data=data.frame(x=-1:1,y=0),lty=2) + 
  geom_line(aes(x=0, y=-1:1),lty=2) + 
  theme_light() +
  circle +
  geom_segment(aes(x=0,y=0,xend=G1,yend=G2),arrow=arrow(),linewidth=1)+
  ggrepel::geom_label_repel(aes(label=val_var),nudge_y = 0.05, alpha=0.5) +
  ggtitle("Cercle des corrélations") +
  xlab(xlab) +  ylab(ylab)


#####################################
## en utilisant FactoMineR
res.pca <- PCA(df.red)

ind_coords <- as.data.frame(res.pca$ind$coord)
ind_coords$ID <- rownames(ind_coords)

ggplot(ind_coords, aes(x = Dim.1, y = Dim.2, label = ID)) +
  geom_text(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Nuage des individus",
    x = paste0("Dim 1: ", round(res.pca$eig[1, 2], 2), " %"),
    y = paste0("Dim 2: ", round(res.pca$eig[2, 2], 2), " %")
  ) 


V = res.pca$var
plot(res.pca,choix="var")  # ajouter l'argument graph.type = "classic" pour ne pas passer par ggplot

#qualité de représentation
V$cos2
V$cor^2

# corrélation
V$cor
cbind(G1,G2)

#contribution à l'axe
V$contrib
V$cos2[,1]/sum(V$cos2[,1])

#visualisation
plt1 = plot(res.pca,axes = c(1,2), choix = "var")
plt2 = plot(res.pca,axes = c(2,3), choix = "var")
plt3 = plot(res.pca,axes = c(3,4), choix = "var")
plt4 = plot(res.pca,axes = c(4,5), choix = "var")
cowplot::plot_grid(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2)



##########################
# question 3
#########################
# CAH

res.cah = hclust(dist(df.red),method = "complete")
res.cah2 = hclust(dist(df.red),method="ward.D2")

################# methode complete
plot(res.cah)
abline(h=8)
abline(h=6)
barplot(rev(res.cah$height)[1:50],main="diagramme des hauteurs")
abline(h=8)
abline(h=6)

# prettier plots :)
# dendograme
fviz_dend(res.cah, k = 3, 
          rect = TRUE, rect_fill = FALSE, show_labels = FALSE,
          lwd = 0.7, main = "Cluster Dendogram")
fviz_dend(res.cah, k = 3, 
          rect = FALSE, rect_fill = FALSE, show_labels = FALSE,
          lwd = 0.7, main = "Cluster Dendogram")

# coude plot
height_data = tibble(num_clusters = length(res.cah$height):1,
                     height = rev(res.cah$height))

ggplot(height_data, aes(x = num_clusters, y = height)) + 
  geom_line(color = "#0073C2FF") +
  geom_point(color = "#0073C2FF") +
  labs(title = "Elbow Method: Merge Height vs Number of Clusters",
       x = "Number of Clusters",
       y = "Height") 

# silhouette plot 
f1 <- fviz_silhouette(silhouette(cutree(res.cah,k=2),dist(df.red))) + ggtitle("Silhouette Plot (k=2)")
f2 <- fviz_silhouette(silhouette(cutree(res.cah,k=3),dist(df.red))) + ggtitle("Silhouette Plot (k=3)")
f3 <- fviz_silhouette(silhouette(cutree(res.cah,k=4),dist(df.red))) + ggtitle("Silhouette Plot (k=4)")
f4 <- fviz_silhouette(silhouette(cutree(res.cah,k=5),dist(df.red))) + ggtitle("Silhouette Plot (k=5)")
f5 <- fviz_silhouette(silhouette(cutree(res.cah,k=6),dist(df.red))) + ggtitle("Silhouette Plot (k=6)")
f6 <- fviz_silhouette(silhouette(cutree(res.cah,k=7),dist(df.red))) + ggtitle("Silhouette Plot (k=7)")
plot_grid(f1,f2,f3,f4,f5,f6, nrow = 3, ncol = 2)

# gap stat
gap_stat <- clusGap(df.red, FUN = hcut, K.max = 10, B = 100)
fviz_gap_stat(gap_stat) + ggtitle("Statistique de Gap")


################# methode ward
plot(res.cah2)
barplot(rev(res.cah2$height)[1:50],main="diagramme des hauteurs")

# prettier plots :)
# dendograme
fviz_dend(res.cah2, k = 3, 
          rect = TRUE, rect_fill = FALSE, show_labels = FALSE,
          lwd = 0.7, main = "Cluster Dendogram")
fviz_dend(res.cah2, k = NULL, 
          rect = FALSE, rect_fill = FALSE, show_labels = FALSE,
          lwd = 0.7, main = "Cluster Dendogram")

# coude plot
height_data = tibble(num_clusters = length(res.cah2$height):1,
                     height = rev(res.cah2$height))

ggplot(height_data, aes(x = num_clusters, y = height)) + 
  geom_line(color = "#0073C2FF") +
  geom_point(color = "#0073C2FF") +
  labs(title = "Elbow Method: Merge Height vs Number of Clusters",
       x = "Number of Clusters",
       y = "Height") 

# silhouette plot 
f1 <- fviz_silhouette(silhouette(cutree(res.cah2,k=2),dist(df.red))) + ggtitle("Silhouette Plot (k=2)")
f2 <- fviz_silhouette(silhouette(cutree(res.cah2,k=3),dist(df.red))) + ggtitle("Silhouette Plot (k=3)")
f3 <- fviz_silhouette(silhouette(cutree(res.cah2,k=4),dist(df.red))) + ggtitle("Silhouette Plot (k=4)")
f4 <- fviz_silhouette(silhouette(cutree(res.cah2,k=5),dist(df.red))) + ggtitle("Silhouette Plot (k=5)")
f5 <- fviz_silhouette(silhouette(cutree(res.cah2,k=6),dist(df.red))) + ggtitle("Silhouette Plot (k=6)")
f6 <- fviz_silhouette(silhouette(cutree(res.cah2,k=7),dist(df.red))) + ggtitle("Silhouette Plot (k=7)")
plot_grid(f1,f2,f3,f4,f5,f6, nrow = 3, ncol = 2)

# alluvial plot 
alluvial(data.frame(complete=cutree(res.cah,k=3),
                    wardD2=cutree(res.cah2,k=3)), 
         freq = rep(1,length(df.red$Area)), 
         col = c("red", "blue","orange")[cutree(res.cah,k=3)],
         border = NA)

#############################
# retenons la classification a 2 groupes 
cah2 <- cutree(res.cah,k=2)
cah2.w <- cutree(res.cah2,k=2)

num.cat <- rep(0,length(df.red$Area))
num.cat[df[,8] == "Besni"] <- 2
num.cat[df[,8] == "Kecimen"] <- 1
table(num.cat)
table(df[,8]) #OK 
err.complete <- abs(num.cat - cah2)
sum(err.complete)/900

num.cat <- rep(0,length(df.red$Area))
num.cat[df[,8] == "Besni"] <- 1
num.cat[df[,8] == "Kecimen"] <- 2
err.ward <- abs(num.cat - cah2.w)
sum(err.ward)/900

##########################################
##### CAH non normalisé
res.cah = hclust(dist(df[,-8]),method = "complete")
res.cah2 = hclust(dist(df[,-8]),method="ward.D2")

cah2 <- cutree(res.cah,k=2)
cah2.w <- cutree(res.cah2,k=2)

num.cat <- rep(0,length(df.red$Area))
num.cat[df[,8] == "Besni"] <- 2
num.cat[df[,8] == "Kecimen"] <- 1
table(num.cat)
table(df[,8]) #OK 
err.complete <- abs(num.cat - cah2)
sum(err.complete)/900

num.cat <- rep(0,length(df.red$Area))
num.cat[df[,8] == "Besni"] <- 2
num.cat[df[,8] == "Kecimen"] <- 1
err.ward <- abs(num.cat - cah2.w)
sum(err.ward)/900

# la normalisation reduit l'erreur

################################################################################
### Partie II
################################################################################
set.seed(1)
n = length(df$Class)
train = sample(c(TRUE,FALSE),n,rep=TRUE,prob=c(2/3,1/3))




################################################################################
### Partie III
################################################################################