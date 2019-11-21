###################PROJET TUTO##################
library("deldir")
library("rgl")
library("ggplot2")
library("hyperSpec")
library(readxl)
library("ggplot2")



First <- read_excel("C:/Users/doums/Desktop/cours Vannes/Lardjane/projet_tutore/colon2.xlsx")
View(First) 

dff=First[,-1]


#CREATION DU SPECTRE POUR L'ENSEMBLE DES PATIENTS
Sp_ALL <- new("hyperSpec", spc =dff )

#Representation du spectre pour l'ensemble des patients
x11
par(bg="gray")
plot(Sp_ALL,col="blue")

#construction du spectre pour l'intervalle de 3200-2800/1800-928 (Troncature)#

x<-Sp_ALL[,, 3200 ~ 2800]
y<-Sp_ALL[,, 1800 ~ 928]
#x<-col[,2800:3200]
#y<-col[,1800:928]

par(mfrow=c(1,2))
plot(x,col="yellow")
plot(y,col=4)

spec1=cbind(x,y)
par(mfrow=c(1,2))
plot(x,col="blue")
plot(y,col="cyan")

dff<-cbind(x,y)
plot (dff,col = "green")

#representaion du spectre initiale et des deux spectre ici de la troncature

par(mfrow=c(1,3))
plot(Sp_ALL,col="blue")
plot(x,col="cyan")
plot(y,col="yellow")


#Choix du meilleur spectre
#Premier patient:

p1=dff[1:3,]

m1=mean(p1)
m1
x1=m1-p1[1,]
x1
x2=m1-p1[2,]
x2
x3=m1-p1[3,]
x3
#leur representation graphique
par(mfrow=c(4,1))
plot(x1)
plot(x2)
plot(x3)
plot(m1)
reg=as.matrice(lm(m1~x1+x2+x3,data=p1))

#Nous retenons ARNOJ1.3

#patient 2
p2=dff[10:12,]
p2
m2=mean(p2)
m2
x4=m2-p2[1,]
x4
x5=m2-p2[2,]
x5
x6=m2-p2[3,]
x6
par(mfrow=c(4,1))
plot(x4)
plot(x5)
plot(x6)
plot(m2)

#patient3,premiere visite
p3=dff[19:21,]
p3
m3=mean(p3)
m2
x7=m3-p2[1,]
x7
x8=m3-p2[2,]
x8
x9=m3-p2[3,]
x9
par(mfrow=c(4,1))
plot(x7)
plot(x8)
plot(x9)
plot(m3)

#patient4,premiere visite
p4=dff[28:30,]
m4=mean(p4)
#patient5,premiere visite
p5=dff[37:39,]
m5=mean(p5)
#patient6,premiere visite
p6=dff[46:48,]
m6=mean(p6)
#patient7,premiere visite
p7=dff[55:57,]
m7=mean(p7)
#patient8,premiere visite
p8=dff[64:66,]
m8=mean(p8)
#patient9,premiere visite
p9=dff[73:75,]
m9=mean(p9)
#patient10,premiere visite
p10=dff[82:84,]
m10=mean(p10)
#patient11,premiere visite
p11=dff[91:93,]
m11=mean(p11)
#patient12,premiere visite
p12=dff[101:103,]
m12=mean(p12)
#patient13,premiere visite
p13=dff[110:112,]
m13=mean(p13)
#patient14,premiere visite
p14=dff[119:121,]
m14=mean(p14)
#patient15,premiere visite
p15=dff[128:130,]
m15=mean(p15)
#patient16,premiere visite
p16=dff[137:139,]
m16=mean(p16)
#patient17,premiere visite
p17=dff[146:148,]
m17=mean(p17)
#patient18,premiere visite
p18=dff[155:157,]
m18=mean(p18)
#patient19,premiere visite
p19=dff[166:169,]
m19=mean(p19)
#patient20,premiere visite
p20=dff[176:178,]
m20=mean(p20)
#patient21,premiere visite
p21=dff[185:187,]
m21=mean(p21)
#patient22,premiere visite
p22=dff[194:196,]
m22=mean(p22)
#patient23,premiere visite
p23=dff[203:205,]
m23=mean(p23)

newb=c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19
,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32,m33,m34,m35,m36,m37,m38,m39,m40,m41,m42
,m43,m44,m45,m46,m47,m48,m49,m50,m51,m52,m53,m54,m55,m56,m57,m58,m59,m60,m61,m62,m63,m64,m65,m66
,m67,m68,m69,m70,m71,m72,m73,m74,m75,m76,m77,m78,m79,m80,m81,m82,m83,m84,m85,m86,m87,m88,m89,m90,m91
,m92,m93,m94,m95,m96,m97,m98,m99,m100,m101,m102,m103,m104,m105,m106,m107,m108,m109,m110)

#Nous avons retenu la moyenne spectrales:



moy<-read_excel("C:/Users/doums/Desktop/cours Vannes/Lardjane/projet_tutore/moy_patien.xlsx")
moy1=moy[,-1]
Spec_moy <- new("hyperSpec", spc =moy1 )

plot(Spec_moy)
w<-Spec_moy[,, 3200 ~ 2800]
z<-Spec_moy[,, 1800 ~ 928]

par(mfrow=c(1,2))
plot(w,col="yellow")
plot(z,col=4)

spec_moy1=cbind(w,z)
par(mfrow=c(1,2))
plot(w,col="blue")
plot(z,col="cyan")



ggplot(moy,aes(kmean,y=moy$PATIENTS))+geom_-point()


library(stats)
tr1.cr <- scale(spec_moy1,center=T,scale=T)
d.f <- dist(tr1.cr)
cah.ward <- hclust(d.f)
plot(cah.ward)
#Le dendogramme nous suggere un decoupage de trois groupes de patients

#Classification ascendante hierarchique
rect.hclust(cah.ward,k=3)
groupes.cah <- cutree(cah.ward,k=3)
print(sort(groupes.cah))

#Methode kmeans

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
data <- tr1.cr
wss <- sapply(1:k.max, 
              function(k){kmeans(tr1.cr, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")




#groupes.kmeans <-kmeans(tr1.cr,centers=3,nstart=5)
#print(groupes.kmeans)
#print(table(groupes.cah,groupes.kmeans$cluster))
##Proportion d’inertie expliquée par la partition : 55%
#Certe si nous prenons un k=4 la proportion d'inertie expliquee par la partition augmente,mais cela ne veut rien dire car le deuxieme groupe sera compose que de deux patients

kmean=kmeans(spec_moy1,center=4)
#kmean=kmeans(deriv,center=4)
kmean$centers
kmean$cluster
kmean$size


 #1er groupe=13 patients;2er groupe=42 et 3emG=58#



library(MASS)
mod = lda(kmean$cluster~.,data=moy1)
par(bg="gray")
plot(predict(mod,dimen=2)$x,col=kmean$cluster)
mod
predict(mod,dimen=2)


library(cluster)
tree<-(moy1,diss=FALSE,stand=FALSE)
pltree(tree)
clu<-pam(spec_moy1,k=1,diss=FALSE,stand=FALSE)
mydata=(data.frame(spec_moy1,kmean$cluster))
plotcluster(mydata,kmean$cluster)


##########
faire les derivee 2nd,
traitement spectre brute traitement spectre derivee.
methode MSC(mutiple scattering corection)


library(prospectr)
library(ChemoSpec)
deriv1 <- savitzkyGolay(spec_moy1$spc[1, ], p =3, w = 11, m =1)
deriv2<- savitzkyGolay(spec_moy1$spc, p = 3, w = 11, m = 2)
plot(deriv1,type="l",col="green")
plot(deriv2,type="l",col="green")
lines(sg)
plot(A)



SpcDf <- 1/112^spec_moy1$spc#conversion en réflectance
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,1),mar=c(4,4,2,2))
matplot(as.numeric(colnames(SpcDf)),t(SpcDf[1:112,]),type='l',xlab='',ylab='la réflectance')
mtext('Lignes spectra')

DV2 <- savitzkyGolay(X = SpcDf,2,3,11,delta.wav=2)#Dérivée seconde.

matplot(as.numeric(colnames(DV2)),t(DV2[1:112,]),type='l',xlab='Wavelength /nm',ylab='Dérivée seconde')
mtext( 'dérivée seconde du  spectra')


##SERUM pour chaque patients##

variance inter et intra groupe
deleter les aberrant 
et la methode utiliser pour optimiser les groupe
https:\\eric.univ-lyon2.fr/~rico/cours
refaire le meme travail sans les aberant et faire la normalisation vectorielle.


###NORMALISATION###
#Methode1

library(ppls)
norm1<-normalize.vector(DV2)

#Methode2
norm2<- DV2 / rowMeans (DV2)
plot (DV2, "spcprctl5")

##Classification

tr1.cr <- scale(norm2,center=T,scale=T)
d.f <- dist(tr1.cr)
cah.ward <- hclust(d.f)
plot(cah.ward)

rect.hclust(cah.ward,k=3)
groupes.cah <- cutree(cah.ward,k=3)
print(sort(groupes.cah))

norm2$clusters <- as.factor (cutree (dendrogram, k = 3)) 
cols <- c ("dark blue", "orange", "#C02020") 


kmeandiff=kmeans(norm2,center=3)
#kmean=kmeans(deriv,center=3)
kmeandiff$centers
kmeandiff$cluster
kmeandiff$size

library(MASS)
mod = lda(kmean$cluster~.,data=moy1)
par(bg="gray")
plot(predict(mod,dimen=2)$x,col=kmean$cluster)
mod
predict(mod,dimen=2)



