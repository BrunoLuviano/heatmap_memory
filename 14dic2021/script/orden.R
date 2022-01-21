#descripcion
#son una serie de 16 diferentes tratamientos y 1 control

#cargar librerias
library(ggplot2)
library(reshape2)
library(here)
library(wesanderson)
library(plotrix)
library(ggnewscale)

#adicionar el tiempo
Time<-read.csv(here("data","Time.csv"))

plate_1<-read.csv(here("14dic2021","data","14dic2021_plate_1.csv"))
plate_2<-read.csv(here("14dic2021","data","14dic2021_plate_2.csv"))
plate_3<-read.csv(here("14dic2021","data","14dic2021_plate_3.csv"))
plate_4<-read.csv(here("14dic2021","data","14dic2021_plate_4.csv"))
plate_5<-read.csv(here("14dic2021","data","14dic2021_plate_5.csv"))

#eliminar las columnas de tiempo y temperatura
plate_1<-plate_1[,-c(1:2)]
plate_2<-plate_2[,-c(1:2)]
plate_3<-plate_3[,-c(1:2)]
plate_4<-plate_4[,-c(1:2)]
plate_5<-plate_5[,-c(1:2)]

#restar controles
a<-c(); ctrl_1<-c(); ctrl_2<-c(); ctrl_3<-c(); ctrl_4<-c(); ctrl_5<-c()
for (i in 1:4) {
  a<-plate_1[,c(166+24*i,168+24*i)]
  a<-as.matrix(a)
  ctrl_1<-cbind(a,ctrl_1)
  
  a<-plate_2[,c(166+24*i,168+24*i)]
  a<-as.matrix(a)
  ctrl_2<-cbind(a,ctrl_2)
  
  a<-plate_3[,c(166+24*i,168+24*i)]
  a<-as.matrix(a)
  ctrl_3<-cbind(a,ctrl_3)
  
  a<-plate_4[,c(166+24*i,168+24*i)]
  a<-as.matrix(a)
  ctrl_4<-cbind(a,ctrl_4)
  
  a<-plate_5[,c(166+24*i,168+24*i)]
  a<-as.matrix(a)
  ctrl_5<-cbind(a,ctrl_5)
}

#sacar promedio de los controles
m<-nrow(ctrl_1); n<-nrow(ctrl_2);o<-nrow(ctrl_3); p<-nrow(ctrl_4); q<-nrow(ctrl_5) 
m1<-c(); m2<-c(); m3<-c(); m4<-c(); m5<-c()
for (i in 1:m) {
  m1[i]<-mean(ctrl_1[i,])
}

for (i in 1:n) {
  m2[i]<-mean(ctrl_2[i,])
}

for (i in 1:o) {
  m3[i]<-mean(ctrl_3[i,])
}

for (i in 1:p) {
  m4[i]<-mean(ctrl_4[i,])
}

for (i in 1:q) {
  m5[i]<-mean(ctrl_5[i,])
}

#restar controles
plate_1<-as.matrix(plate_1-m1)
plate_2<-as.matrix(plate_2-m2)
plate_3<-as.matrix(plate_3-m3)
plate_4<-as.matrix(plate_4-m4)
plate_5<-as.matrix(plate_5-m5)

#elimar los pozos vacios y controles
#plate_1<-plate_1[,-c(141:144,165:168,189:192;213:216)]
n<-24*6
m<-24*6-3
for (i in 1:11) {
  plate_1<-plate_1[,-c(m:n)]
  plate_2<-plate_2[,-c(m:n)]
  plate_3<-plate_3[,-c(m:n)]
  plate_4<-plate_4[,-c(m:n)]
  plate_5<-plate_5[,-c(m:n)]
  n<-n+20
  m<-m+20
}

#separar el tratamiento 5
m<-21
n<-24
a<-c(); t5_1<-c(); t5_2<-c(); t5_3<-c(); t5_4<-c(); t5_5<-c()
for (i in 1:5){
  a<-plate_1[,c(m:n)]
  plate_1<-plate_1[,-c(m:n)]
  t5_1<-cbind(a,t5_1)
  
  a<-plate_2[,c(m:n)]
  plate_2<-plate_2[,-c(m:n)]
  t5_2<-cbind(a,t5_2)
  
  a<-plate_3[,c(m:n)]
  plate_3<-plate_3[,-c(m:n)]
  t5_3<-cbind(a,t5_3)
  
  a<-plate_4[,c(m:n)]
  plate_4<-plate_4[,-c(m:n)]
  t5_4<-cbind(a,t5_4)
  
  a<-plate_5[,c(m:n)]
  plate_5<-plate_5[,-c(m:n)]
  t5_5<-cbind(a,t5_5)
  
  m<-m+20
  n<-n+20
}


#seleccionar los pozos de cada plato/tiempo
#fase de crecimiento->plato1
#primer estimulo->plato2
#recupecion->plato3
#2do estimulo->plato4
#recuperacion->plato5
a<-c(); b<-c(); c<-c(); d<-c(); e<-c(); f<-c()

for (i in 1:64){
  a<-plate_1[,5*i-4]
  b<-cbind(a,b)
  
  a<-plate_2[,5*i-3]
  c<-cbind(a,c)
  
  a<-plate_3[,5*i-2]
  d<-cbind(a,d)
  
  a<-plate_4[,5*i-1]
  e<-cbind(a,e)
  
  a<-plate_5[,5*i]
  f<-cbind(a,f)
} 

#unir los platos/tiempos
tratamientos<-rbind(b,c,d,e,f)

#obtener promedio y error estandar
n<-nrow(tratamientos)
a<-c(); b<-c(); c<-c(); d<-c(); e<-c()
f<-c(); g<-c(); h<-c(); k<-c(); l<-c()
m<-c(); o<-c(); p<-c(); q<-c(); r<-c(); s<-c()

for (i in 1:n){
  a[i]<-mean(tratamientos[i,c(1,5,9,13)])
  b[i]<-mean(tratamientos[i,c(2,6,10,14)])
  c[i]<-mean(tratamientos[i,c(3,7,11,15)])
  d[i]<-mean(tratamientos[i,c(4,8,12,16)])
  e[i]<-mean(tratamientos[i,c(17,21,25,29)])
  f[i]<-mean(tratamientos[i,c(18,22,26,30)])
  g[i]<-mean(tratamientos[i,c(19,23,27,31)])
  h[i]<-mean(tratamientos[i,c(20,24,28,32)])
  k[i]<-mean(tratamientos[i,c(33,37,41,45)])
  l[i]<-mean(tratamientos[i,c(34,38,42,46)])
  m[i]<-mean(tratamientos[i,c(35,39,43,47)])
  o[i]<-mean(tratamientos[i,c(36,40,44,48)])
  p[i]<-mean(tratamientos[i,c(49,53,57,61)])
  q[i]<-mean(tratamientos[i,c(50,54,58,62)])
  r[i]<-mean(tratamientos[i,c(51,55,59,63)])
  s[i]<-mean(tratamientos[i,c(52,56,60,64)])
}

mean_treats<-cbind(a,b,c,d,e,f,g,h,k,l,m,o,p,q,r,s)

#obtener promedio y error estandar
n<-nrow(tratamientos)
a<-c(); b<-c(); c<-c(); d<-c(); e<-c()
f<-c(); g<-c(); h<-c(); k<-c(); l<-c()
m<-c(); o<-c(); p<-c(); q<-c(); r<-c(); s<-c()

for (i in 1:n){
  a[i]<-std.error(tratamientos[i,c(1,5,9,13)])
  b[i]<-std.error(tratamientos[i,c(2,6,10,14)])
  c[i]<-std.error(tratamientos[i,c(3,7,11,15)])
  d[i]<-std.error(tratamientos[i,c(4,8,12,16)])
  e[i]<-std.error(tratamientos[i,c(17,21,25,29)])
  f[i]<-std.error(tratamientos[i,c(18,22,26,30)])
  g[i]<-std.error(tratamientos[i,c(19,23,27,31)])
  h[i]<-std.error(tratamientos[i,c(20,24,28,32)])
  k[i]<-std.error(tratamientos[i,c(33,37,41,45)])
  l[i]<-std.error(tratamientos[i,c(34,38,42,46)])
  m[i]<-std.error(tratamientos[i,c(35,39,43,47)])
  o[i]<-std.error(tratamientos[i,c(36,40,44,48)])
  p[i]<-std.error(tratamientos[i,c(49,53,57,61)])
  q[i]<-std.error(tratamientos[i,c(50,54,58,62)])
  r[i]<-std.error(tratamientos[i,c(51,55,59,63)])
  s[i]<-std.error(tratamientos[i,c(52,56,60,64)])
}

stderror_treats<-cbind(a,b,c,d,e,f,g,h,k,l,m,o,p,q,r,s)

#agregar el tratamiento 5
t5_1<-t5_1[,c(17:20)]
t5_2<-t5_2[,c(13:16)]
t5_3<-t5_3[,c(9:12)]
t5_4<-t5_4[,c(5:8)]
t5_5<-t5_5[,c(1:4)]

t5<-rbind(t5_1,t5_2,t5_3,t5_4,t5_5)

#calcular promedio y error estandar
x<-c(); y<-c()
n<-nrow(t5)
for (i in 1:n) {
  x[i]<-mean(t5[i,])
  y[i]<-std.error(t5[i,])
}

#unir todos los tratamientos
mean_treats<-cbind(mean_treats[,c(1:4)],x,mean_treats[,c(5:16)])
stderror_treats<-cbind(stderror_treats[,c(1:4)],y,stderror_treats[,c(5:16)])

n<-nrow(Time)
mean_treats<-mean_treats[c(1:n),]
stderror_treats<-stderror_treats[c(1:n),]

#extrar la columa control con valores de OD
OD_control<-mean_treats[,1]
OD_control<-as.data.frame(OD_control)
n<-nrow(OD_control)
a<-rep("Control",n)
OD_control<-cbind(Time,a,OD_control)
colnames(OD_control)<-c("time","variable","mean")

#dividir todo entre el control para normalizar
#mean_treats<-mean_treats+abs(min(mean_treats))
#mean_treats<-mean_treats/mean_treats[,1]

#para descartar valores negativos
mean_treats<-mean_treats + abs(min(mean_treats)) + 0.001


#calcular las generaciones bacterianas
m<-mean_treats[c(1:19),]
n<-mean_treats[c(20:38),]
o<-mean_treats[c(39:51),]
p<-mean_treats[c(52:58),]
q<-mean_treats[c(59:131),]

a<-ncol(m); b<-nrow(m); c<-c(); d<-c(); e<-c()

for (j in 1:a) {
  for (i in 1:b){
    c<-m[,j]
    c<-as.vector(c)
    d[i]<-(log(c[i])-log(c[1]))/log(2)
    d<-as.matrix(d)
    
  }
  e<-cbind(e,d)
}

a<-ncol(n); b<-nrow(n); c<-c(); d<-c(); f<-c()

for (j in 1:a) {
  for (i in 1:b){
    c<-n[,j]
    c<-as.vector(c)
    d[i]<-(log(c[i])-log(c[1]))/log(2)
    d<-as.matrix(d)
    
  }
  f<-cbind(f,d)
}

a<-ncol(o); b<-nrow(o); c<-c(); d<-c(); g<-c()

for (j in 1:a) {
  for (i in 1:b){
    c<-o[,j]
    c<-as.vector(c)
    d[i]<-(log(c[i])-log(c[1]))/log(2)
    d<-as.matrix(d)
    
  }
  g<-cbind(g,d)
}

a<-ncol(p); b<-nrow(p); c<-c(); d<-c(); h<-c()

for (j in 1:a) {
  for (i in 1:b){
    c<-p[,j]
    c<-as.vector(c)
    d[i]<-(log(c[i])-log(c[1]))/log(2)
    d<-as.matrix(d)
    
  }
  h<-cbind(h,d)
}


a<-ncol(q); b<-nrow(q); c<-c(); d<-c(); k<-c()

for (j in 1:a) {
  for (i in 1:b){
    c<-q[,j]
    c<-as.vector(c)
    d[i]<-(log(c[i])-log(c[1]))/log(2)
    d<-as.matrix(d)
    
  }
  k<-cbind(k,d)
}


#unir las partes
mean_treats<-rbind(e,f,g,h,k)
#colnames(mean_treats)<-orden

#agregar nombre a las columnas
a<-c()
n<-ncol(mean_treats)-1
for (i in 1:n) {
  a[i]<-paste("Tratamiento",i)
}
a<-c("Control",a)
colnames(mean_treats)<-a
colnames(stderror_treats)<-a

#calcular el area bajo la curva para ordenar genes
x<-nrow(mean_treats)
y<-nrow(mean_treats)
Tiempo<-c(1:x)
z<-ncol(mean_treats)

x<-cbind(Tiempo,mean_treats)


require(pracma)
b<-c()
for (i in 1:z) {
  b[i]<-trapz(x[c(39:y),1],x[c(39:y),1+i])
}

b<-cbind(b,a)
b<-as.data.frame(b)

#ordenar de mayor a menor
b<-b[order(b$b),]
orden<-as.vector(b[,2])
orden<-rev(orden)
