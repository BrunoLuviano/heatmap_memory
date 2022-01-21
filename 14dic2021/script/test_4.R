#descripcion
#son una serie de 16 diferentes tratamientos y 1 control

#cargar librerias
library(ggplot2)
library(reshape2)
library(here)
library(wesanderson)
library(plotrix)
library(ggnewscale)
library(jcolors)

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

#combinar todos
complete<-rbind(plate_1,plate_2,plate_3,plate_4,plate_5)

#seleccionar solamente los pozos del control
a<-c(); b<-c()
for (i in 1:4) {
  a<-complete[,c(24*i-23,24*i-22,24*i-21,24*i-20,24*i-19)]
  b<-cbind(b,a)
}


#separar por transferencias
a<-c(); c<-c(); d<-c(); e<-c(); f<-c(); g<-c()
for (i in 1:4) {
  a<-b[,c(5*i-4)]
  c<-cbind(c,a)
  
  a<-b[,c(5*i-3)]
  d<-cbind(d,a)
  
  a<-b[,c(5*i-2)]
  e<-cbind(e,a)
  
  a<-b[,c(5*i-1)]
  f<-cbind(f,a)
  
  a<-b[,c(5*i)]
  g<-cbind(g,a)
}

#seleccionar solamente 12 horas
#se tomaron lecturas cada 10 min
n<-6*12 + 1
h<-nrow(plate_1) + 1
j<-nrow(plate_2) + h
k<-nrow(plate_3) + j
l<-nrow(plate_4) + k

o<-c[c(1:n),]
n<-n+h - 1
p<-d[c(h:n),]
n<-n + j - h
q<-e[c(j:n),]
n<-n + k - j
r<-f[c(k:n),]
n<-n + l - k
s<-g[c(l:n),]

#calcular promedio y error estandar
a<-c(); b<-c(); c<-c(); d<-c(); e<-c()
f<-c(); g<-c(); h<-c(); k<-c(); l<-c()
n<-nrow(o)

for (i in 1:n){
  a[i]<-mean(o[i,])
  b[i]<-mean(p[i,])
  c[i]<-mean(q[i,])
  d[i]<-mean(r[i,])
  e[i]<-mean(s[i,])
  
  f[i]<-std.error(o[i,])
  g[i]<-std.error(p[i,])
  h[i]<-std.error(q[i,])
  k[i]<-std.error(r[i,])
  l[i]<-std.error(s[i,])
}

OD<-cbind(a,b,c,d,e)
OD_sd<-cbind(f,g,h,k,l)
OD<-as.data.frame(OD)
OD_sd<-as.data.frame(OD_sd)

#calcular las generaciones bacterianas
a<-c(); b<-c(); c<-c(); d<-c(); e<-c()
v<-c(); w<-c(); x<-c(); y<-c(); z<-c()
n<-nrow(o)
#hay 4 replicas
for (j in 1:4) {
  for (i in 1:n) {
    a[i]<-(log(o[i,j])-log(o[1,j]))/log(2)
    b[i]<-(log(p[i,j])-log(p[1,j]))/log(2)
    c[i]<-(log(q[i,j])-log(q[1,j]))/log(2)
    d[i]<-(log(r[i,j])-log(r[1,j]))/log(2)
    e[i]<-(log(s[i,j])-log(s[1,j]))/log(2)
    
  }
  v<-cbind(v,a)
  w<-cbind(w,b)
  x<-cbind(x,c)
  y<-cbind(y,d)
  z<-cbind(z,e)
}

#calcular promedio y error estandar
a<-c(); b<-c(); c<-c(); d<-c(); e<-c()
f<-c(); g<-c(); h<-c(); k<-c(); l<-c()
n<-nrow(v)

for (i in 1:n){
  a[i]<-mean(v[i,])
  b[i]<-mean(w[i,])
  c[i]<-mean(x[i,])
  d[i]<-mean(y[i,])
  e[i]<-mean(z[i,])
  
  f[i]<-std.error(v[i,])
  g[i]<-std.error(w[i,])
  h[i]<-std.error(x[i,])
  k[i]<-std.error(y[i,])
  l[i]<-std.error(z[i,])
}

data<-cbind(a,b,c,d,e)
sd<-cbind(f,g,h,k,l)
data<-as.data.frame(data)
sd<-as.data.frame(sd)
Time<-Time[c(1:n),]
sd<-cbind(Time,sd)
a<-OD[1,]
a<-as.character(a)
colnames(data)<-a
a<-as.numeric(a)
a<-sort(a, decreasing = TRUE)
a<-as.character(a)
data<-data[,a]
data<-cbind(Time,data)
colnames(data)<-c("Tiempo",a)
OD<-cbind(Time,OD)
OD_sd<-cbind(Time,OD_sd)


#colnames(data)<-c("Tiempo","0.065","0.046","0.067","0.040","0.015")
colnames(OD)<-c("Tiempo","1","2","3","4","5")

#graficar
data<-melt(data)
sd<-melt(sd)
OD<-melt(OD)
OD_sd<-melt(OD_sd)
data<-cbind(data,sd[,3])
OD<-cbind(OD,OD_sd[,3])
colnames(data)<-c("Tiempo","variable","value","std")
colnames(OD)<-c("Tiempo","variable","value","std")

t_plot<-ggplot(data , aes(Tiempo, value,color=variable, group=variable)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=value-std, ymax=value+std), width=.2,alpha=0.5) +
  #facet_grid(~tratamiento,scales="free_x") +
  #scale_fill_gradientn(colours = pal,name="Tratamiento/Control")  +
  #scale_fill_distiller(name=bquote(~Placa)) +
  scale_color_jcolors(palette = "default",name=bquote(~DO[inicial])) +
  theme_bw() +
  #new_scale_fill() +
  #geom_tile(data = OD_control,aes(fill=mean)) +
  #scale_fill_viridis_c(option = "turbo",direction = -1,name=bquote(~DO[Control])) +
  #scale_fill_distiller(palette = "Greys",direction = 1,name=bquote(~DO[Control])) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Tiempo (HH:MM)",expand=c(0,0)) +
  scale_y_continuous("Generaciones",expand=c(0,0)) +
  theme(axis.text = element_text(size = 7.5),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=0.5,color="black",fill=NA)) +
  coord_equal()

print(t_plot)

OD_plot<-ggplot(OD , aes(Tiempo, value,color=variable, group=variable)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=value-std, ymax=value+std), width=.2,alpha=0.5) +
  #facet_grid(~tratamiento,scales="free_x") +
  #scale_fill_gradientn(colours = pal,name="Tratamiento/Control")  +
  #scale_fill_distiller(name=bquote(~Placa)) +
  scale_color_jcolors(palette = "default",name="Placa") +
  theme_bw() +
  #new_scale_fill() +
  #geom_tile(data = OD_control,aes(fill=mean)) +
  #scale_fill_viridis_c(option = "turbo",direction = -1,name=bquote(~DO[Control])) +
  #scale_fill_distiller(palette = "Greys",direction = 1,name=bquote(~DO[Control])) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Tiempo (HH:MM)",expand=c(0,0)) +
  scale_y_continuous("DO (630nm)",expand=c(0,0)) +
  theme(axis.text = element_text(size = 7.5),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=0.5,color="black",fill=NA)) 

#print(OD_plot)

