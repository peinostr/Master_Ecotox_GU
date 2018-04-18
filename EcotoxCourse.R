rm(list=ls())
dev.off()
setwd("//home.gu.gu.se/home-XI$/xinope/Documents/R/Master_Ecotox")

library(readxl)
library(vegan)

########################################################################################################
#################################################################################################### PCA

cuek<-read_excel("cuek.xlsx", col_types = c("text","numeric","numeric","numeric","numeric"))

#structure of data file
str(cuek)

#summary of data file
summary(cuek)

#log transformation
log.cuek<-log(cuek[,-1])
site.cuek<-cuek[,1]

#running PCA
my.pca <- rda(log.cuek)

# summary method
summary(my.pca)

#In this simple case with only 4 PCs, first two PCs explain most of the variability in the data (94%).

biplot(my.pca,
       display = c("sites", 
                   "species"),
       type = c("text",
                "points"))
#group sites
ordihull(my.pca,
         group = site.cuek$sites,
         col = c(1,2,3))

#add legend
legend("bottomright",
       col = c(1,2,3), 
       lty = 1,
       cex = 0.75,
       legend = c("Site 1", "Site 2", "Site 3"))

########################################################################################################
################################################################################################ Cluster

bla<-read_excel("blablabla.xlsx", col_types = c("text","numeric","numeric","numeric","numeric","numeric",
                                             "numeric","numeric","numeric","numeric","numeric",
                                             "numeric","numeric","numeric","numeric","numeric",
                                             "numeric"))
bla_1<-bla[,-c(1:2)]

dis<-vegdist(bla_1)
clus<-hclust(dis, "complete") #complete linkage
plot(clus, labels = bla$Site)

########################################################################################################
#################################################################################################### nMDS

plop<-read_excel("plop.xlsx", col_types = c("text","numeric","numeric","numeric","numeric","numeric",
                                            "numeric","numeric","numeric","numeric","numeric",
                                            "numeric","numeric"))

plop_name<-plop[,1]
plop_1<-plop[,-1]

#structure of data file
str(plop_1)

#running nMDS
#k=2, means the number of reduced dimensions
example_nMDS=metaMDS(plop_1, k=2)

# summary method
example_nMDS

#Distance= Bray-Curtis
#stress= 0.006826, stress values <0.05 indicates a good plot with excellent representation of the data

# Shepard plot
stressplot(example_nMDS) #very goof fit and dissimilarities are well preserved.

#plot nMDS
plot(example_nMDS, display = "species", xlim=c(0,3))

orditorp(example_nMDS, type= "t", cex=0.75, display="species",air=0.1)

#diversity indexes
diversity(plop_nMDS,index = "shannon")
diversity(plop_nMDS,index = "simpson")

########################################################################################################
#################################################################################################### RDA

#bla 14 periphyton pigments and 6 observations
#env 12 env variables and 6 observations

bla<-read_excel("blablabla_RDA.xlsx", col_types = c("text","numeric","numeric","numeric","numeric","numeric",
                                                "numeric","numeric"))
env<-read_excel("env.xlsx", col_types = c("text", "numeric","numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric") )

bla_1<-bla[,-1]
env_1<-env[,-c(1:2)]
log_env<-log(env_1)
sites<-c(19,19,15,15,17,17)

#nomalization using -log(x+1)
peri.rda<-rda(log(bla_1+1) ~ w_velocity + pH + conductivity + w_temp + DO + O + DOC + light +
                alkalinity + NH4+ PO4 + NO3, log_env)
peri.rda

#Focus in on the proportion of constrained and unconstrained variance. RDA is a "constrained" ordination model,
#so the proportion of constrained variance is the amount of variance explained by our explanatory variables,
#depth and zone. We can see that this amounts to about 99,99% of the variance.


plot(peri.rda, scaling = 1, type = "n")
text(peri.rda, "sp", cex=.75)
points(peri.rda, display = "sites", pch = sites,
       col= c("red2", "red2", "green4", "green4", "mediumblue", "mediumblue"), cex = 1.5)
text(peri.rda, display = "cn", cex = .8, col = "blue")
