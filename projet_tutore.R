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
#patient24,premiere visite
p24=dff[212:215,]
m24=mean(p24)
#patient25,premiere visite
p25=dff[224:226,]
m25=mean(p25)
#patient26,premiere visite
p26=dff[233:235,]
m26=mean(p26)
#patient27,premiere visite
p27=dff[242:244,]
m27=mean(p27)
#patient28,premiere visite
p28=dff[251:253,]
m28=mean(p28)
#patient29,premiere visite
p29=dff[261:263,]
m29=mean(p29)
#patient30,premiere visite
p30=dff[272:274,]
m30=mean(p30)
#patient31,premiere visite
p31=dff[281:283,]
m31=mean(p31)
#patient32,premiere visite
p32=dff[290:292,]
m32=mean(p32)
#patient33,premiere visite
p33=dff[299:301,]
m33=mean(p33)
#patient34,premiere visite
p34=dff[308:311,]
m34=mean(p34)
#patient35,premiere visite
p35=dff[320:322,]
m35=mean(p35)
#patient36,premiere visite
p36=dff[329:331,]
m36=mean(p36)
#patient37,premiere visite
p37=dff[338:341,]
m37=mean(p37)
#patient38,premiere visite
p38=dff[348:350,]
m38=mean(p38)
#patient39,premiere visite
p39=dff[357:359,]
m39=mean(p39)
#patient40,premiere visite
p40=dff[366:371,]
m40=mean(p40)
#patient41,premiere visite
p41=dff[378:380,]
m41=mean(p41)
#patient42,premiere visite
p42=dff[387:389,]
m42=mean(p42)
#patient43,premiere visite
p43=dff[396:398,]
m43=mean(p43)
#patient44,premiere visite
p44=dff[405:407,]
m44=mean(p44)
#patient45,premiere visite
p45=dff[414:416,]
m45=mean(p45)
#patient46,premiere visite
p46=dff[423:425,]
m46=mean(p46)
#patient47,premiere visite
p47=dff[432:434,]
m47=mean(p47)
#patient48,premiere visite
p48=dff[441:443,]
m48=mean(p48)
#patient49,premiere visite
p49=dff[450:455,]
m49=mean(p49)
#patient50,premiere visite
p50=dff[462:464,]
m50=mean(p50)
#patient51,premiere visite
p51=dff[471:473,]
m51=mean(p51)
#patient52,premiere visite
p52=dff[480:482,]
m52=mean(p52)
#patient53,premiere visite
p53=dff[489:491,]
m53=mean(p53)
#patient54,premiere visite
p54=dff[498:500,]
m54=mean(p54)
#patient55,premiere visite
p55=dff[507:509,]
m55=mean(p55)
#patient56,premiere visite
p56=dff[516:518,]
m56=mean(p56)
#patient57,premiere visite
p57=dff[525:527,]
m57=mean(p57)
#patient58,premiere visite
p58=dff[534:536,]
m58=mean(p58)
#patient59,premiere visite
p59=dff[543:546,]
m59=mean(p59)
#patient60,premiere visite
p60=dff[555:557,]
m60=mean(p60)
#patient61,premiere visite
p61=dff[564:566,]
m61=mean(p61)
#patient62,premiere visite
p62=dff[573:576,]
m62=mean(p62)
#patient63,premiere visite
p63=dff[585:587,]
m63=mean(p63)
#patient64,premiere visite
p64=dff[594:596,]
m64=mean(p64)
#patient65,premiere visite
p65=dff[603:605,]
m65=mean(p65)
#patient66,premiere visite
p66=dff[612:615,]
m66=mean(p66)
#patient67,premiere visite
p67=dff[624:626,]
m67=mean(p67)
#patient68,premiere visite
p68=dff[633:635,]
m68=mean(p68)
#patient69,premiere visite
p69=dff[642:644,]
m69=mean(p69)
#patient70,premiere visite
p70=dff[651:654,]
m70=mean(p70)
#patient71,premiere visite
p71=dff[661:663,]
m71=mean(p71)
#patient72,premiere visite
p72=dff[670:672,]
m72=mean(p72)
#patient73,premiere visite
p73=dff[679:681,]
m73=mean(p73)
#patient74,premiere visite
p74=dff[688:690,]
m74=mean(p74)
#patient75,premiere visite
p75=dff[697:700,]
m75=mean(p75)
#patient76,premiere visite
p76=dff[709:711,]
m76=mean(p76)
#patient77,premiere visite
p77=dff[718:720,]
m77=mean(p77)
#patient78,premiere visite
p78=dff[737:739,]
m78=mean(p78)
#patient79,premiere visite
p79=dff[746:748,]
m79=mean(p79)
#patient80,premiere visite
p80=dff[755:757,]
m80=mean(p80)
#patient81,premiere visite
p81=dff[764:766,]
m81=mean(p81)
#patient82,premiere visite
p82=dff[773:775,]
m82=mean(p82)
#patient83,premiere visite
p83=dff[782:784,]
m83=mean(p83)
#patient84,premiere visite
p84=dff[791:793,]
m84=mean(p84)
#patient85,premiere visite
p85=dff[800:802,]
m85=mean(p85)
#patient86,premiere visite
p86=dff[809:811,]
m86=mean(p86)
#patient87,premiere visite
p87=dff[818:820,]
m87=mean(p87)
#patient88,premiere visite
p88=dff[827:829,]
m88=mean(p88)
#patient89,premiere visite
p89=dff[836:838,]
m89=mean(p89)
#patient90,premiere visite
p90=dff[845:847,]
m90=mean(p90)
#patient91,premiere visite
p91=dff[854:856,]
m91=mean(p91)
#patient92,premiere visite
p92=dff[863:868,]
m92=mean(p92)
#patient93,premiere visite
p93=dff[875:877,]
m93=mean(p93)
#patient94,premiere visite
p94=dff[884:886,]
m94=mean(p94)
#patient95,premiere visite
p95=dff[893:895,]
m95=mean(p95)
#patient96,premiere visite
p96=dff[902:904,]
m96=mean(p96)
#patient97,premiere visite
p97=dff[911:913,]
m97=mean(p97)
#patient98,premiere visite
p98=dff[920:922,]
m98=mean(p98)
#patient99,premiere visite
p99=dff[928:930,]
m99=mean(p99)
#patient100,premiere visite
p100=dff[938:940,]
m100=mean(p100)
#patient101,premiere visite
p101=dff[947:949,]
m101=mean(p101)
#patient102,premiere visite
p102=dff[956:958,]
m102=mean(p102)
#patient103,premiere visite
p103=dff[965:968,]
m103=mean(p103)
#patient104,premiere visite
p104=dff[981:983,]
m104=mean(p104)
#patient105,premiere visite
p105=dff[990:992,]
m105=mean(p105)
#patient106,premiere visite
p106=dff[999:1001,]
m106=mean(p106)
#patient107,premiere visite
p107=dff[1008:1010,]
m107=mean(p107)
#patient108,premiere visite
p108=dff[1017:1022,]
m108=mean(p108)
#patient109,premiere visite
p109=dff[1026:1028,]
m109=mean(p109)
#patient110,premiere visite
p110=dff[1035:1037,]
m110=mean(p110)

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



