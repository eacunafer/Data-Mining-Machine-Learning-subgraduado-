#############################################################
#          COMANDOS EN R PARA EL CURSO DE MINERIA DE DATOS  #
#           Dr. Edgar Acuna, Febrero del 2018               #
#############################################################
#Si se  usa la libreria dprep quitar el comentario de la siguiente linea
#library(dprep)
#Cargando el espacio de trabajo del curso
#Entrar a R y elegir Archivos luego escoja Cargar area de Trabajo 
#load("H:\\rdm.Rdata")

#####################################################
############ PREPROCESAMIENTO DE DATOS###############
#####################################################
############I-IMPUTACION               ###############
######################################################
########  EXPLORANDO EL CONJUNTO DE DATOS ###########
#Cargando el conjunto de datos census solo si se usa la libreria dprep
#data(census)
#leyendo los datos de la internet
census=read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", sep=',',na.strings=" ?")
#Mostrando las primeras 15 filas del dataset
head(census,15)
#Mostrando las ultimas filas del dataset
tail(census)
#Contando el total de valores missing en el dataset
sum(is.na(census)!=0)
#Hallando que columnas tienen valores perdidos
which(colSums(is.na(census))!=0)
#Hallando las filas tienen valores perdidos 
rmiss=which(rowSums(is.na(census))!=0,arr.ind=T)
#Para ver el porcentaje de filas con valores perdidos
length(rmiss)*100/dim(census)[1]
#Para ver el porcentaje de missing  values en las columnas donde las hay 
colmiss=c(2,7,14)
per.miss.col=100*colSums(is.na(census[,colmiss]))/dim(census)[1]
per.miss.col
#Para eliminar las filas que contienen al menos un valor perdido
census.omit=na.omit(census)
dim(census.omit)
#Figura para ver los valores perdidos
library(dprep)
imagmiss(census, name="census")

#########  LIMPIANDO EL CONJUNTO DE DATOS DE VALORES FALTANTES #######################
#Se elimina las columnas con por lo menos 50% de entradas faltantes y filas con 
#por lo meno el 30% de entradas faltantes
hepatitis.cl=clean(hepatitis,tol.col=.5,tol.row=.3,name="cl.hepatitis")

#########  IMPUTACION MEDIA/MEDIANA/MODA ############################################
#Impuntando los missing values de census
census.mimp=ce.mimp(census,"mean",atr=c(2,7,14),nomatr=c(2,7,14)) 
census.mdimp=ce.mimp(census,"median",atr=c(2,7,14),nomatr=c(2,7,14)) 
#visualizando los datos despues de la imputacion
imagmiss(census.mimp, name="Census after mean imputation")
#Imputacion en el conjunto de datos hepatitis
data(hepatitis)
imagmiss(hepatitis, name="hepatitis")
hepa.mean.imp=ce.mimp(hepatitis,"mean",1:19)

########  IMPUTACION usando los k-vecinos mas cercanos ############
#Imputando census con k=3 vecinos mas cercanos
census.knn=ec.knnimp(censusn,k=3)
#imputando hepatitis con k=10 vecinos
hepa.knn=ec.knnimp(hepatitis,k=10)

######## II- NORMALIZACION ##########################
###########################################################
data(bupa)
#Aplicando normalizacion usando z-scores(estandarizacion)
zbupa=znorm(bupa)
#Aplicando  la normalizacion min-max
mmbupa=mmnorm(bupa)
#Aplicando la normalizacion por escala decimal 
dsbupa=decscale(bupa)
#Aplicando la normalizaciopn sigmoidal
sigbupa=signorm(bupa)
#Haciendo plots para ver el efecto de la normalizacion
plot(sort(bupa$V1))
plot(sort(sigbupa$V1))
#Aplicando la normalizacion softmax
softbupa=softmaxnorm(bupa)
#Normalizacion general
zbupa=rangenorm(bupa,method="znorm")
#Graficas de boxplot para ver el efecto de la normalizacion
par(mfrow=c(2,3))
boxplot(bupa[,1:6],main="bupa")
boxplot(zbupa[,1:6],main="znorm bupa")
boxplot(mmbupa[,1:6],main="min-max bupa")
boxplot(dsbupa[,1:6],main="dec scale bupa")
boxplot(sigbupa[,1:6],main="signorm bupa")
boxplot(softbupa[,1:6],main="softmax bupa")


################   III- Discretizacion #####################################
#############################################################################
#library(dprep)
#data(bupa)
################   Discretizacion con intervalos de igual amplitud ##############
dbupa=disc.ew(bupa,1:6)
table(dbupa[,1])
table(dbupa[,2])
table(dbupa[,3])
table(dbupa[,4])
table(dbupa[,5])
table(dbupa[,6])

################   Discretizacion con intervalos de igual frecuencia ###############
args(disc.ef)
dbupa=disc.ef(bupa,1:6,10)
table(dbupa[,1])
table(dbupa[,2])
table(dbupa[,3])
table(dbupa[,4])


################   Discretizacion 1R #################################

args(disc.1r)
dbupa=disc.1r(bupa,1:6,binsize=15)
table(dbupa[,1])
table(dbupa[,2])
table(dbupa[,3])
table(dbupa[,4])
table(dbupa[,5])
table(dbupa[,6])

################   Discretizacion usando entropia ##################

args(disc.mentr)
disc.mentr(bupa,1:7)

################   Discretizacion usando Chi-Merge##################

args(chiMerge)
dbupa=chiMerge(bupa,1:6,.05)
table(dbupa[,1])
table(dbupa[,2])
table(dbupa[,3])
table(dbupa[,4])
table(dbupa[,5])
table(dbupa[,6])

################ IV:  VISUALIZACION #########################
#Dr. Edgar Acuna, 2018
###########################################################
library(Rcmdr)
library(scatterplot3d)
library(lattice)

################  Visualizando datos univariados 1D #################################

x=c( 0.2, 1.5, 1.8, 3.3, 5.4, 5.9, 7.1, 8.4)
par(mfrow=c(3,1))
stripchart(x,vertical=T,col=2,main="Dotplot") 
hist(x,col=3)
boxplot(x,horizontal=T,col="blue",main="boxplot")

################   Visualizando datos en 3D ###############################
par(mfrow=c(1,1))
color=c("red","blue")[breastw[,10]]
scatterplot3d(breastw[,6],breastw[,4],breastw[,1],angle=120,color=color)
title("scatterplor3d de Breastw(3 main features)")
cloud(Sepal.Length ~ Petal.Length * Petal.Width | Species, data = iris, screen = list(x = -90, y = 70), distance = .4, zoom = .6) 

################   3-D Data (persp)################################

x =seq(-10, 10, length= 30); y=x
f =function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z =outer(x, y, f); z[is.na(z)] = 1; op = par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
wireframe(volcano, shade = TRUE, aspect = c(61/87, 0.4), light.source = c(10,0,10))

################   Visualizando en 4 o mas Dimensiones ################

################### Plot Matricial ####################################
pairs(bupa[,1:6],main="Plot Matricial de Bupa")

################### Surveyplot ########################################

surveyplot(breastw,"Breastw(ordered by attr=7)",orderon=7)
surveyplot(breastw,"Breastw(ordered by attr=6)",orderon=6)
surveyplot(bupa,"Bupa(outliers clase 1)",obs=c(190,205,316,345),lwd=2)

################### Plot de coordenadas Paralelas #####################

parallelplot(my.iris,name="iris",comb=0)

parallelplot(bupa,name="Bupa",comb=3)
parallelplot(bupa,name="Bupa(outliers clase 1)",class=1,obs=c(190,205,316,345))

##################  Visualizacion Radial ###############################
radviz2d(breastw,name="Breastw")

##################  Plot de coordenadas Estrella #######################
starcoord(breastw,main="Breastw",class=TRUE)


################   V: Reduccion de Dimensionalidad ###########
#Dr. Edgar Acuna, 2009
#################################################################
#A. Seleccion de variables predictoras
library(dprep)
library(class)
library(rpart)

data(breastw)

################   RELIEF #######################

relief(breastw,300,0)#Toma tiempo
relief(breastw,600,0.04)

relief(bupa,345,0.0003)

data(vehicle)
relief(vehicle,400,0.012)

################   LVF #############################

dbupa=disc.ew(bupa,1:6)
dbupa[1:10,]
inconsist(dbupa)
lvf(dbupa,.1,1000)

lvf(breastw,.01,2000)

################   FINCO #############################

finco(dbupa,.05)

finco(breastw,.01)
finco(breastw,.001)

################   SFS  #############################

sfs(bupa,"knn") #knn classifier
sfs(bupa,"lda") #Linear discriminant classifier
sfs(bupa,"rpart") #decision tree classifier

sfs(breastw,"knn")
sfs(breastw,"lda")
sfs(breastw,"rpart")

################   SFFS ##############################

sffs(bupa,"lda")
sffs(bupa,"knn")
sffs(bupa,"rpart")

sffs(breastw,"lda")
sffs(breastw,"knn")
sffs(breastw,"rpart")

#B. Extraccion de nuevas caracteristicas
################   PRINCIPAL COMPONENTS ################

bupapc=prcomp(bupa[,c(3,4)],scale=T,retx=T)
print(bupapc)
zbupa34=znorm(bupa)[,c(3,4)]
plot(zbupa34$V3,zbupa34$V4,xlab="V3",ylab="V4")
abline(bupapc$rot[,1])
abline(bupapc$rot[,2])
title("Effecto de los componenets principales")
text(4,1.5,"PC1")
text(1,-1,"PC2")
 
plot(bupapc$x[,1],bupapc$x[,2],xlab="PC1",ylab="PC2")

a=prcomp(bupa[,-7],scale=T)
print(a)
summary(a)

############### VI: OUTLIERS ################################
#Dr. Edgar Acuna
################################################################
#*************************************************************
#Deteccion de outliers outliers univariados por metodo clasico
#*************************************************************
zbupa=znorm(bupa)
zbupa1=zbupa[,1]
rownames(bupa[abs(zbupa1)>2,])
#*****************************************************************
#Deteccion de outliers univariados usando boxplot
#*****************************************************************
outliers=boxplot(bupa$V1,plot=F)$out
nout=as.character(outliers)
boxplot(bupa$V1,col="blue")
for(i in 1:length(outliers))
{
text(outliers[i],as.character(which(bupa$V1==outliers[i])),cex=.8,pos=4)
}
#*******************************************************************
#outliers multivariados, considerando los outliers por cada variable
#*******************************************************************
boxplot(bupa[bupa[,7]==1,1:6],bupa[bupa[,7]==1,7])

#******************************************************************
#Detectando outliers usando la distancia Mahalanobis
#******************************************************************
a=mahaout(bupa,1,T)
boxplot(a,col="red")

#***************************************************************************
#Detectando outliers usando estimadores robustos de la distancia Mahalanobis
#***************************************************************************
b=robout(bupa,1,"mve",10)
b=robout(bupa,2,"mve",10)

#***************************************************
#Deteccion de outliers usando clustering (PAM)
#***************************************************
library(cluster)
bupa1=bupa[bupa[,7]==1,1:6]
pambupa1=pam(bupa1,20,stand=T)
pambupa1$clusinfo
bupa1[pambupa1$clustering==19,]

#***************************************************
#Deteccion de outliers usando metodos basados en distancia 
#***************************************************

baysout(bupa[bupa[,7]==1,1:6], blocks=10, num.out=10)
#***************************************************
#Deteccion de outliers basado en densidad local
#***************************************************
lofbupa1=maxlof(bupa[bupa[,7]==1,-7],"lofbupa1",20,30)
lofbupa1[order(lofbupa1,decreasing=T)][1:10]

##################################################################
###############CLASIFICACION SUPERVISADA ##############################
#######################################################################
#************************************************ 
#A)Analisis discriminante Lineal
#Usa la funcion lda de la libreria MASS de Ripley
#************************************************ 
library(MASS)
#Leyendo los datos del ejemplo 1 de la internet
eje1dis=read.table("http://math.uprm.edu/~edgar/eje1disc.dat",header=T)
eje1dis
#Separando el conjunto de datos en dos grupos
pasan=eje1dis[eje1dis[,4]=="p",]
npasan=dim(pasan)[1]
fracasan=eje1dis[eje1dis[,4]=="f",]
nfracasan=dim(fracasan)[1]
#Hallando las medias de E1 y E2 en ambos grupos
medp<-mean(pasan[,1:2])
medp
medf<-mean(fracasan[,1:2])
medf
#******************************************************
#Ploteando los puntos y los centroides de cada grupo
#******************************************************
win.graph()
plot(fracasan$E1,fracasan$E2,xlim=c(0,100),ylim=c(0,100),type="n")
text(fracasan$E1,fracasan$E2,"f", col="red")
points(pasan$E1,pasan$E2,type="n")
text(pasan$E1,pasan$E2,"p",col="blue")
points(medp[1],medp[2],pch=19,col="dark blue")
points(medf[1],medf[2],pch=19,col="dark red")
text(medp[1],medp[2],"centroide pasan",cex=.7,adj=1)
text(medf[1],medf[2],"centroide fracasan",cex=.7,adj=1)
#**************************************************************
#Calculo de la linea de decision
#**********************************************************************
#Hallando las matrices de covarianzas en ambos grupos
cov1<-cov(pasan[,1:2])
cov1
cov2<-cov(fracasan[,1:2])
cov2
#
#Calculando los coeficientes de la funcion discriminante
#
covcomb=((npasan-1)*cov1+(nfracasan-1)*cov2)/(npasan+nfracasan-2)
coeflda<-(medp-medf)%*%solve(covcomb)
coeflda
#Calculando el termino independiente
indlda<-0.5*(medp-medf)%*%solve(covcomb)%*%(medp+medf)
indlda
#*************************************************************************
#Trazando la linea del clasificador lineal sobre el plot de puntos
#*****************************************************************************
win.graph()
plot(fracasan$E1,fracasan$E2,xlim=c(0,100),ylim=c(0,100),type="n",xlab="E1",ylab="E2")
title("Analisis discriminante Lineal para datos de examenes")
text(fracasan$E1,fracasan$E2,"f", col="red")
points(pasan$E1,pasan$E2,type="n")
text(pasan$E1,pasan$E2,"p",col="blue")
points(medp[1],medp[2], pch=19, col="blue")
points(medf[1],medf[2],pch=19, col="red")
# Ploteando la linea discriminante en la forma pendiente-intercepto
abline(indlda/coeflda[2],-coeflda[1]/coeflda[2])
text(25,30,bquote(.(coeflda[1])*E1+.(coeflda[2])*E2<.(indlda)),cex=.8)
text(75,70,bquote(.(coeflda[1])*E1+.(coeflda[2])*E2>.(indlda)),cex=.8)
#*****************************************************************************
# LDA con priors iguales
#****************************************************************************** 
lda1<-lda(Nota~E1+E2,eje1dis,prior=c(.5,.5))
lda1  
 plda1=predict(lda1,eje1dis[,-c(3,4)])$class
#Matriz de confusion
table(plda1,eje1dis[,4])
  error1=sum(plda1!=eje1dis[,4])
  error1
#**********************************************************************************
#LDA con priors ponderadas
#**********************************************************************************
lda2<-lda(Nota~E1+E2,eje1dis)
lda2  
plda2=predict(lda2,eje1dis[,-c(3,4)])$class
#matriz de confusion
table(plda2,eje1dis[,4])
error2=sum(plda2!=eje1dis[,4])
error2
#*************************************************************************************
#LDA para el conjunto Vehicle que tiene 4 clases
#*************************************************************************************** 
ldaveh=lda(vehicle[,1:18],vehicle[,19])
predict(ldaveh)$posterior
predict(ldaveh)$class
#Matriz de confusion
table(vehicle[,19],predict(ldaveh)$class)
#******************************************************
# Estimacion de la tasa de error de mala clasificacion
#******************************************************
#Por resustitucion
mean(vehicle[,19]!=predict(ldaveh)$class)
#Por el metodo, dejando uno afuera
ldaexa=lda(eje1dis[,1:2],eje1dis[,4],CV=TRUE)
mean(eje1dis[,4]!=ldaexa$class)
ldaveh=lda(vehicle[,1:18],vehicle[,19],CV=TRUE)
mean(vehicle[,19]!=ldaveh$class)
#Por el metodo de validacion cruzada
crossval(eje1dis[,c(1,2,4)],nparts=10,method="lda",repet=10)
crossval(vehicle,nparts=10,method="lda",repet=10)
##################################################################
#B) Clasificacion usando Naive Bayes
#con el comando naiveBayes de la libreria e1071
##################################################################
library(e1071)
# El clasificador se aplicara al conjunto naiveeje2
#Metodo 1. Discretizando la columna 4
dnaiveeje2=disc.ew(naiveeje2,c(4:5))

#Metodo 2. Sin discretizar la columna 4
#Media y desviacion estandar de la col4 en cada clase
mean(naiveeje2[naiveeje2[,5]==0,4])
mean(naiveeje2[naiveeje2[,5]==1,4])
sd(naiveeje2[naiveeje2[,5]==0,4])
sd(naiveeje2[naiveeje2[,5]==1,4])
#usando la funcion naivebayes de e1071
naivebayes21=as.data.frame(naiveeje2)
a=naiveBayes(col5~.,data=naivebayes21)
a
#Hallando las clases predichas
pred=predict(a,naivebayes21[,-5],type="raw")
pred1=max.col(pred)
pred1
#Hallando la matriz de confusion
table(pred1,naivebayes21[,5])   
# Ejemplo 2. Aplicando Naive Bayes a Bupa
# Sin discretizar
a=naiveBayes(V7~.,data=bupa)
pred=predict(a,bupa[,-7],type="raw")
pred1=max.col(pred)
# La matriz confusion
table(pred1,bupa[,7])   
error=mean(bupa[,7]!=pred1)
# Discretizando con el metodo de la entropia 
# Descomentar las siguientes 6 lineas si esta usando la libreria dprep
#dbupa=disc.mentr(bupa,1:7)
#for (i in 1:7)
#dbupa[,i]=as.factor(dbupa[,i])
#b=naiveBayes(V7~.,data=dbupa)
#pred=predict(b,dbupa[,-7])
#error=mean(pred!=bupa[,7])
#Discretizando  por el metodo ChiMerge
chibupa=chiMerge(bupa,1:6)
for (i in 1:7)
chibupa[,i]=as.factor(chibupa[,i])
b=naiveBayes(V7~.,data=chibupa)
pred=predict(b,chibupa[,-7])
error=mean(pred!=chibupa[,7])
error
#Discretizando usando intervalos de igual ancho
dbupa=disc.ew(bupa,1:6)
for (i in 1:7)
dbupa[,i]=as.factor(dbupa[,i])
b=naiveBayes(V7~.,data=dbupa)
pred=predict(b,dbupa[,-7])
error=mean(pred!=dbupa[,7])
error
# Ejemplo 3. Aplicacion de Naive Bayes a los datos mpg
#sin discretizar
b=naiveBayes(mpg~.,data=autompg)
pred=predict(b,autompg[,-1],type="raw")
pred1=max.col(pred)
#La matriz se confusion
table(pred1,autompg[,1])
error=mean(pred1!=autompg[,1])
error
#Discretizando manualmente
b=naiveBayes(mpg~.,data=autompg2)
pred=predict(b,autompg2[,-1])
#La matriz se confusion
table(pred,autompg2[,1])
error=mean(pred!=autompg[,1])
error
# Estimacion del error del clasificador naive Bayes usando validacion cruzada
# a) Sin discretizar
cvnaiveBayes(bupa,repet=10)
# b) Discretizandop con Chimerge
cvnaiveBayesd(bupa,method="ChiMerge",repet=10)

##################################################
#C) Clasificacion usando k-nn
################################################################

###Estimacion de funcion de densidad usando k-nn #######################

fdknn=function(y, npoints, k)
{
#Esta funcion estima una densidad univariada usando el metodo k-nn
#inputs:
#y es la muestra tomada
# npoints: es el numero de puntos donde se va a estimar la funcion de densidad
# k : es el numero de vecinos mas cercanos
#Output
# la densidad estimada en los puntos npoints y su plot
x <- seq(min(y) - 1, max(y) + 1, length = npoints)
d1 <- abs(outer(x, y, "-"))
m1 <- t(apply(d1, 1, sort))
d2 <- 2 * m1[, k]
fest <- k/(length(y) * d2)
plot(x, fest, type = "l")
fest
}
#Estimando la funcion de densidad basada en la muestra tomada y
y=c(7.03860,6.38018,6.95461,2.25521,7.24608,6.62930,3.92734,0.40701,5.59448,5.05751)
fdknn(y,10,1)

################### Grafica de la frontera de decision del clasificador k-nn####################

library(class)
library(MASS)
library(nnet)
#Funciones auxiliares

exaplot <- function(xp, yp, Z)
{
    plot(eje1dis[, 1], eje1dis[, 2], ylim=c(0,100),type = "n",
         xlab = "EX1", ylab = "EX2")
    for(il in 1:2) {
        set <- eje1dis$Class==levels(eje1dis$Class)[il]
        text(eje1dis[set, 1], eje1dis[set, 2],
             labels = as.character(eje1dis$Class[set])) }
    zp <- Z[, 2] - pmax(Z[, 2], Z[, 1])
    contour(xp,yp, matrix(zp, np),
            add = T, levels = 0,drawlabels=FALSE)
 #   zp <- Z[, 1] - pmax(Z[, 2], Z[, 1])
 #   contour(xp, yp, matrix(zp, np), add = T, levels = 0, drawlabels=FALSE)
    invisible()
}

xp <- seq(0, 100, length = 50)
yp <- seq(0, 100, length = 50)
np <- length(xp)
exaT <- expand.grid(E1 = xp, E2 = yp)
#Grafica de knn-1
tp <- eje1dis$Class[1:32, drop = T]
#Grafica de knn-1
Z= knn(eje1dis[1:32,c(1,2)], exaT, tp,k=1)
win.graph()
exaplot(xp, yp, class.ind(Z))
color=c("red","blue")[as.factor(Z)]
points(exaT[,1],exaT[,2],col=color)
title("Grafica de las fronteras de knn-1")
#Grafica de knn-7
Z= knn(eje1dis[1:32, c(1,2)], exaT, tp, k = 7)
win.graph()
exaplot(xp, yp, class.ind(Z))
color=c("red","blue")[as.factor(Z)]
points(exaT[,1],exaT[,2],col=color)
title("Grafica de las fronteras de knn-7")
#clasificador knn-3 para Bupa
knn(bupa[,1:6],bupa[,1:6],bupa[,7],k=3)
#Estimacion del error por resubstitucion
mean(knn(bupa[,1:6],bupa[,1:6],bupa[,7],k=3)!=bupa[,7])
#Estimando el error por el metodo de dejar uno afuera
knn3=knn.cv(bupa[,1:6],bupa[,7],k=3)
##La matriz de confusion
table(knn3,bupa[,7])
## El error de prediccion
mean(knn.cv(bupa[,1:6],bupa[,7],k=5)!=bupa[,7])
mean(knn.cv(bupa[,1:6],bupa[,7],k=7)!=bupa[,7])
mean(knn.cv(bupa[,1:6],bupa[,7],k=9)!=bupa[,7])
mean(knn.cv(bupa[,1:6],bupa[,7],k=11)!=bupa[,7])
#Estimando el error del knn por valiacion cruzada
crossval(bupa,method="knn",kvec=3,repet=10)
crossval(bupa,method="knn",kvec=5,repet=10)
crossval(bupa,method="knn",kvec=7,repet=10)
crossval(bupa,method="knn",kvec=9,repet=10)
crossval(bupa,method="knn",kvec=11,repet=10)


####################################################### 
D) clasificacion usando arboles de decision 
#############################################################

library(rpart)
eje1dis=read.table("http://math.uprm.edu/~edgar/eje1disc.dat",header=T)
arbol=rpart(Nota~E1+E2,data=eje1dis,method="class",control=rpart.control(minbucket=2))
 print(arbol)

plot(arbol,margin=.25)
text(arbol,use.n=T)
title("Clasificacion de datos de examenes usando arboles")

#####Graficando las regiones creadas por el arbol en que el espacio muestral 
plot(eje1dis$E1,eje1dis$E2,type="n",xlab="E1",ylab="E2")
text(eje1dis$E1,eje1dis$E2,as.character(eje1dis$Nota))
lines(c(75.5,75.5),c(0,43.5))
abline(h=43.5)
title("Particionamiento del espacio muestral hecha por el arbol")

#########Graficando la superficie de decision generada por el arbol######
ge1=seq(min(eje1dis$E1),max(eje1dis$E1),length=50)
ge2=seq(min(eje1dis$E2),max(eje1dis$E2),length=50)
grid1=list(E1=ge1,E2=ge2)
grid1=expand.grid(grid1)
estimado=predict(arbol,grid1)
classest=apply(estimado,1,which.max)
matest=matrix(classest,50,50)
persp(ge1, ge2, matest, theta=30, phi=45, xlab="E1", ylab="E2",
 zlab="Clases", col="lightblue")


###Criterios para parar el crecimiento del árbol ##################

arbolbupa=rpart(V7~V3+V5,data=bupa, method="class")
plot(arbolbupa,margin=.25)
text(arbolbupa,use.n=T)
arbolbupa=rpart(V7~V3+V5,data=bupa, method="class",minbucket=20)
plot(arbolbupa,margin=.25)
text(arbolbupa,use.n=T)
arbolbupa=rpart(V7~V3+V5,data=bupa, method="class",cp=.05)
plot(arbolbupa,margin=.25)
text(arbolbupa,use.n=T)
arbolbupa=rpart(V7~V3+V5,data=bupa, method="class",cp=.001)
plot(arbolbupa,margin=.25)
text(arbolbupa,use.n=T)
arbolbupa=rpart(V7~V3+V5,data=bupa, method="class",maxdepth=3)
plot(arbolbupa,margin=.25)
text(arbolbupa,use.n=T)

###Recortando (“prunning”) un árbol ####################
prune(arbolbupa,cp=.05)

###Estimación del Error de Clasificación  #################

arbolbupa=rpart(V7~.,data=bupa,method="class")
cvpred=xpred.rpart(arbolbupa,xval=10,cp=.1) 
error=mean(cvpred !=bupa[,7])

####################################################### 
E) Clasificacion usando regresion logistica 
##########################################################
#Ajustando una regresion lineal a los datos de examenes
plot(eje1log$E1,eje1log$notabin)
l1=lsfit(eje1log$E1,eje1log$notabin)
abline(l1)
title("regresion final para predecir nota final")
# Haciendo la regresion logistica conlas predictoras E1 y E2
color=c("red","blue")[eje1dis$Nota]
plot(eje1dis$E1,eje1dis$E2,col=color)
notaslog=glm(notabin~E1+E2,data=eje1log,family=binomial)
############################################################
# Graficando la frontera de decision de la logistica
############################################################
library(class)
library(MASS)
library(nnet)
#Funciones auxiliares

exaplot <- function(xp, yp, Z)
{
    plot(eje1log[, 1], eje1log[, 2], ylim=c(0,100),type = "n",
         xlab = "EX1", ylab = "EX2")
    for(il in 1:2) {
        set <- eje1log$Nota==levels(eje1log$Nota)[il]
        text(eje1log[set, 1], eje1log[set, 2],
             labels = as.character(eje1log$Nota[set])) }
    zp <- Z[, 2] - pmax(Z[, 2], Z[, 1])
    contour(xp,yp, matrix(zp, np),
            add = T, levels = 0,drawlabels=FALSE)
 #   zp <- Z[, 1] - pmax(Z[, 2], Z[, 1])
 #   contour(xp, yp, matrix(zp, np), add = T, levels = 0, drawlabels=FALSE)
    invisible()
}

xp <- seq(0, 100, length = 50)
yp <- seq(0, 100, length = 50)
np <- length(xp)
exaT <- expand.grid(E1 = xp, E2 = yp)

#Grafica de logistica
tp <- eje1log$notabin[1:32, drop = T]
exalog=multinom(notabin~E1+E2,data=eje1log)
Z=predict(exalog,exaT)
cat(Z)
win.graph()
exaplot(xp, yp, class.ind(Z))
color=c("red","blue")[as.factor(Z)]
points(exaT[,1],exaT[,2],col=color)
title("Grafica de las fronteras de logistica")


####################################
# Aplicando regresion logistica a Bupa
####################################
#La columna de clases deber 0 y 1
bupa1=bupa
bupa1[,7]=bupa1[,7]-1
bupalog=glm(V7~.,data=bupa1,family=binomial)
# Encontrando la probabilidad posterior de caer en la clase 1.
phat=bupalog$fit
# Encontrando las observaciones con posteriores mayores o iguales que .5
b=as.numeric(names(phat[phat>=.5]))
# Determinado el vector de clases predichas
nobs=345
clases=rep(0,nobs)
clases[b]=1
# Hallando el error de estimacion
mean(clases!=bupa1[,7])
#Haciendo la clasificacion usando  la sensitividad versus la especificidad 
p=seq(.1,.9,length=9) 
sensit=rep(0,9) 
especif=rep(0,9) 
for(j in 1:9) {
clases1=rep(0,nobs) 
for(i in 1:nobs) 
{if(phat[i]>=p[j]){clases1[i]=1} }
tempo=cbind(bupa1[,7],clases1) 
positivo=tempo[tempo[,1]==1,]
 negativo=tempo[tempo[,1]==0,]
 sensit[j]=mean(positivo[,1]==positivo[,2])
 especif[j]=mean(negativo[,1]==negativo[,2]) }
 tabla=cbind(p,sensit,especif) 
cat("Sensitividad y especifidad para varios valores de p\n")
print(tabla)
plot(p,sensit,type="l")
lines(p,especif)
text(p,sensit,labels=p)
 title("Ploteando la sentividad y especificidad para varios p") 
#La curva ROC
plot(1-especif,sensit,type="l")
text(1-especif,sensit,labels=p)
 title("La curva ROC ") 
#Clasificacion con el p de la curva ROC
clasesf<-rep(0,nobs)
for(i in 1:nobs)
{if(phat[i]>=0.57){clasesf[i]<-1}
}
ratef<-mean(clasesf!=bupa1[,7])
cat("la tasa de mala clasificacion optima es=",ratef,"\n")
# Regresion Logistica cuando hay varias clases
library(nnet)
tempo=multinom(V19~.,data=vehicle,MaxNWts=2500)
tempo1=predict(tempo,vehicle)
# Estimacion del error por resustitucion 
error=mean(tempo1 != as.numeric(vehicle[, 19]))
error
# Estimacion del error por Validacion cruzada
cv10log(vehicle,10)

####################################################### 
F) clasificacion usando redes Neurales  
########################################################
library(nnet) 
# Ejemplo: Notas en los examenes
clases=class.ind(eje1dis[,4])
a=nnet(eje1dis[1:32, c(1,2)],clases,entropy=T,size= 5)
error=mean(as.numeric(eje1dis[,4])!=max.col(a$fit))
#Para ver todos los parametros estimados en la red
summary(a)
# Ejemplo diabetes
clases=class.ind(diabetes$V9)
a=nnet(diabetes[,1:8],clases,entropy=T,size=5,maxit=1000)
summary(a)
error=mean(diabetes[,9]!=max.col(a$fit))
#Estimacion del error por validacion cruzada
cv10mlp(diabetes,units=5,repet=10)
cv10mlp(vehicle,units=36,repet=5)

####################################################### 
#Comparacion Final de clasificadores
#####################################  
## LDA
ldaveh=lda(vehicle[,1:18],vehicle[,19])
#Por resustitucion
mean(vehicle[,19]!=predict(ldaveh)$class)
#Por el metodo de validacion cruzada
crossval(vehicle,nparts=10,method="lda",repet=10)
# Naive Bayes 
# Sin discretizar
a=naiveBayes(V19~.,data=vehicle)
pred=predict(a,vehicle[,-19],type="raw")
pred1=max.col(pred)  
error=mean(vehicle[,19]!=pred1)
cvnaiveBayes(vehicle,repet=10)
#K-nn
library(class)
#EstimAcion del error por resubstitucion
mean(knn(vehicle[,1:18],vehicle[,1:18],vehicle[,19],k=5)!=vehicle[,19])
mean(knn(vehicle[,1:18],vehicle[,1:18],vehicle[,19],k=9)!=vehicle[,19])
mean(knn(vehicle[,1:18],vehicle[,1:18],vehicle[,19],k=11)!=vehicle[,19])
#Estimando el error por validacion cruzada
crossval(vehicle,method="knn",kvec=5,repet=10)
## Arboles
library(rpart)
#EstimAcion del error por resubstitucion
arbolveh=rpart(V19~.,data=vehicle,method="class")
tempo=predict(arbolveh,vehicle) 
error=mean(max.col(tempo)!=vehicle[,19])
error
crossval(vehicle,method="rpart",repet=10)
#Logistica
library(nnet)
tempo=multinom(V19~.,,data=vehicle,MaxNWts=2500)
tempo1=predict(tempo,vehicle)
# Estimacion del error por resustitucion 
error=mean(tempo1 != as.numeric(vehicle[, 19]))
error
# Estimacion del error por Validacion cruzada
cv10log(vehicle,10)
# Redes neurales
#EstimAcion del error por resubstitucion
clases=class.ind(vehicle$V19)
a=nnet(vehicle[,1:18],clases,entropy=T,size=40,maxit=1000)
error=mean(vehicle[,19]!=max.col(a$fit))
error
#Estimacion del error por validacion cruzada
cv10mlp(vehicle,40,5)
######################################################################
################CLASIFICACION NO SUPERVISADA CLUSTERING############### 
######################################################################
library(cluster)
# Calculando matrices de distancias
# Usando la funcion dist
dist(x)
dist(x, method=”manhattan”,diag = TRUE)
dist(x, method=”maximum”,upper = TRUE)
#  Usando la funcion daisy
daisy(x,metric="euclidean")
matclust=rbind(c(1,2),c(2,4),c(3,3),c(4,5),c(5,4),c(6,5))
daisy(matclust,metric="euclidean")
#Calculando la distancia Gower para datos nominales
matx=read.table("http://math.uprm.edu/~edgar/matx.txt",header=T)
a=daisy(matx,metric="gower")
a=as.matrix(a)
#######################
A) K-means
######################
a=kmeans(bupa[,1:6],2)
a$cluster
# identificando los tamanos de los clusters formados
table(a$cluster)
#  Los tamanos de los grupos verdaderos
table(bupa[,7])
#Comparando los grupos verdaderos con los clusters
table(a$cluster,bupa[,7])
# Formando tres clusters
kmeans(bupa[,1:6],3)
b= kmeans(bupa[,1:6],3)
table(b$cluster)
#K-means eligiendo como centros iniciales las observaciones 10 y 100 de Bupa 
medias<-bupa[c(10,100),1:6]
am=kmeans(bupa[,1:6],medias)
table(am$cluster)
#Kmeans para censusn con datos imputados
a=kmeans(census.mimp[,-14],2)
# los tamanos de los clusters formados
table(a$cluster)
# los tamanos de los grupos verdaderos
table(censusn[,14])
# Comparando los clusters con los grupos verdaderos 
table(a$cluster,censusn[,14])
######################
#B) PAM
######################
# Aplicando pam con dos grupos
pambupa=pam(bupa[,1:6],2,diss=F)
# Para ver a que cluster se asigna cada obervacion
pambupa$clustering
# Para ver los tamanos de los clusters
table(pambupa$clustering)
#Comparando los grupos verdaderos con los clusters
table(pambupa$clustering,bupa[,7])
# Aplicando pam con dos grupos pero estandarizando los datos
pambupa=pam(bupa[,1:6],2,diss=F,stand=T)
table(pambupa$clustering)
table(pambupa$clustering,bupa[,7])   
# Aplicando pam con dos grupos y matriz de disimilaridad
a=pam(daisy(bupa[,-7], metric = "euclidean"), 2, diss = T)
table(a$clustering)
######################
#SOM
######################
library(som)
#Haciendo el som para bupa
# normalizando los datos
bupa.n=normalize(bupa[,-7])
# Haciendo solo dos clusters
a=som(bupa.n,xdim=2,ydim=1,topol="hexa",neigh="gaussian")
#ploteando el som
plot(a)
#listando las asignaciones de los objetos a los clusters
a$visual
#el numero de instances asignadas al cluster (0,0)
dim(a$visual[(a$visual[,1]==0) & (a$visual[,2]==0),])[1]
# Identificando las observaciones que van el  cluster 0
rownames(a$visual[(a$visual[,1]==0)&(a$visual[,2]==0),])
# Identificando los clusters para cada obervacion 
clusters=a$visual$x+a$visual$y
clusters
table(clusters,bupa[,7])
############################
#A. Jerarquico Aglomerativo
############################
#Dendrograma
a=hclust(dist(bupa[,1:6]))
plot(a)
#correlacion cofonetica
disbupa=dist(bupa[,1:6])
hbupa=hclust(disbupa, method=”ward”)
denbupa=cophenetic(hbupa)
cor(disbupa,denbupa)
#Dendrograma y sus cortes
treebupa=as.dendrogram(hbupa)
bupita=cut(treebupa, h=100)
par(mfrow=c(2,3))
plot(treebupa)
plot(bupita$upper)
plot(bupita$lower[[1]])
plot(bupita$lower[[2]])
plot(bupita$lower[[3]])
plot(bupita$lower[[4]])
#clustering jerarquico aglomerativo usando hclust
disbupa=dist(bupa[,1:6])
hbupa=hclust(disbupa, method="ave")
hbupa=hclust(disbupa, method="ward")
a=cutree(hbupa,2)
table(a)
table(a,bupa[,7])
#Clustering jerarquico algomerativo usando agnes
library(cluster)
bupagl<-agnes(bupa[,1:6],metric="euclidean",method="ward")
b=cutree(bupagl,k=2)
table(b,bupa[,7])
table(cutree(bupagl,k=3))
######################
#B. Jerarquico Divisivo
########################

bupadiv=diana(bupa[,1:6],metric="euclidean")
plot(bupadiv,which=2)
bupadiv
b=cutree(bupadiv,k=2)
table(b)
table( b,bupa [,7])
table(cutree(bupadiv,k=3))

########################
#Validacion de clusters
########################
#Medidas Internas
#Indices internos: Indice de Dunn, Indice de Davies-Bouldin
disbupa=dist(bupa[,1:6])
bupadiv=diana(bupa[,1:6],metric="euclidean")
a=cutree(bupadiv,k=2)
library(fpc)
cluster.stats(disbupa,a,bupa[,7])
cluster.stats(disbupa,a,bupa[,7])$dunn
#La Silueta y el aglomerativo jerarquico
agbupa=agnes(dist(bupa[,1:6]),method="ward")
a=silhouette(cutree(agbupa,k=2),daisy(bupa[,1:6]))
b=silhouette(cutree(agbupa,k=3),daisy(bupa[,1:6]))
c=silhouette(cutree(agbupa,k=4),daisy(bupa[,1:6]))
par(mfrow=c(1,3))
plot(a,main=" ")
plot(b,main=" ")
plot(c,main=" ")
#Silueta y el pam
silicom=rep(0,9)
for(i in 1:9){ 
silicom[i]=pam(vehicle[,1:18],i+1,
diss=F,stand=T)$silinfo$avg.width}
plot(2:10,silicom)
plot(2:10,silicom,type="o",xla="clusters",main="plots de siluetas")
#Medidas externas de validacion de clusters:
#Rand, Jaccard, Hubert,Fowlkes and Mallows
#Medidas extermas de clustering para bupa
agbupa=agnes(dist(bupa[,1:6]),method="ward")
a=cutree(agbupa,k=2)
mexter(bupa[,7],a)
##########################################
#Clustering usando mezclas Guassianas
##########################################
library(mclust)
a=Mclust(bupa[,1:6],1:10)
a
a$bic
table(a$class)
a$parameters
plot(a,bupa[,-7])
######################################################################
################REGLAS DE ASOCIACION ############################ 
######################################################################
#Hallando los itemsets mas frecuentes
library(arules)
ardata=list(c(1, 3, 4),c(2, 3, 5),c(1,2,3,5),c(2,5))
b=as(ardata,"transactions")
#Hallando los itemsets mas frecuentes
itemfre=apriori(b,parameter = list(supp = 0.5,target="frequent itemsets"))
inspect(itemfre)
#Hallando las reglas de asociacion
ar=apriori(b,parameter = list(supp = 0.5, conf = 0.8,target="rules"))
inspect(ar)
 para bupa

agbupa=agnes(dist(bupa[,1:6]),method="ward")

a=cutree(agbupa,k=2)

mexter(bupa[,7],a)

##########################################

#Clustering usando mezclas Guassianas

##########################################

library(mclust)

a=Mclust(bupa[,1:6],1:10)

a

a$bic

table(a$class)

a$parameters

plot(a,bupa[,-7])

######################################################################

################REGLAS DE ASOCIACION ############################ 

######################################################################

#Hallando los itemsets mas frecuentes

library(arules)

ardata=list(c(1, 3, 4),c(2, 3, 5),c(1,2,3,5),c(2,5))

b=as(ardata,"transactions")

#Hallando los itemsets mas frecuentes

itemfre=apriori(b,parameter = list(supp = 0.5,target="frequent itemsets"))

inspect(itemfre)

#Hallando las reglas de asociacion

ar=apriori(b,parameter = list(supp = 0.5, conf = 0.8,target="rules"))

inspect(ar)

