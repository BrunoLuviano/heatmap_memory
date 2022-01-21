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

#obtener promedio
h<-c(); j<-c(); k<-c(); l<-c(); m<-c()
n<-nrow(c)

for (i in 1:n) {
  h[i]<-mean(c[i,])
  j[i]<-mean(d[i,])
  k[i]<-mean(e[i,])
  l[i]<-mean(f[i,])
  m[i]<-mean(g[i,])
}

#seleccionar solamente 12 horas
#se tomaron lecturas cada 10 min
n<-6*12 + 1
a<-nrow(plate_1) + 1
b<-nrow(plate_2) + a
c<-nrow(plate_3) + b
d<-nrow(plate_4) + c

h<-h[1:n]
n<-n+a - 1
j<-j[a:n]
n<-n + b - a
k<-k[b:n]
n<-n + c - b
l<-l[c:n]
n<-n + d - c
m<-m[d:n]

#combinar
data<-cbind(h,j,k,l,m)
data<-as.data.frame(data)

#calcular las generaciones bacterianas
a<-c(); b<-c(); c<-c(); d<-c(); e<-c()
n<-nrow(data)

for (i in 1:n) {
  a[i]<-(log(data[i,1])-log(data[1,1]))/log(2)
  b[i]<-(log(data[i,2])-log(data[1,2]))/log(2)
  c[i]<-(log(data[i,3])-log(data[1,3]))/log(2)
  d[i]<-(log(data[i,4])-log(data[1,4]))/log(2)
  e[i]<-(log(data[i,5])-log(data[1,5]))/log(2)
}

data<-cbind(a,b,c,d,e)
data<-as.data.frame(data)
Time<-Time[c(1:n),]
data<-cbind(Time,data)
colnames(data)<-c("Tiempo","1","2","3","4","5")

#n<-nrow(data)
#n<-n/2 + 0.5
#data<-data[c(1:n),]

#graficar
data<-melt(data)

t_plot<-ggplot(data , aes(Tiempo, value,color=variable, group=variable)) + 
  geom_line() + 
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
  scale_y_continuous("Generaciones",expand=c(0,0)) +
  theme(axis.text = element_text(size = 7.5),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=0.5,color="black",fill=NA)) +
  coord_equal()

print(t_plot)

