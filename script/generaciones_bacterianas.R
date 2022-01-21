#cargar librerias
library(ggplot2)
library(reshape2)
library(here)
library(wesanderson)
library(plotrix)

#cargar los archivos
strains<-read.csv(here("data","Cepario.csv"))

plate_a_1<-read.csv(here("data","111121_plate_a_1_OD.csv"))
plate_a_2<-read.csv(here("data","111121_plate_a_2_OD.csv"))
plate_a_3<-read.csv(here("data","111121_plate_a_3_OD.csv"))
plate_a_4<-read.csv(here("data","111121_plate_a_4_OD.csv"))
plate_a_5<-read.csv(here("data","111121_plate_a_5_OD.csv"))
plate_b_1<-read.csv(here("data","111121_plate_b_1_OD.csv"))
plate_b_2<-read.csv(here("data","111121_plate_b_2_OD.csv"))
plate_b_3<-read.csv(here("data","111121_plate_b_3_OD.csv"))
plate_b_4<-read.csv(here("data","111121_plate_b_4_OD.csv"))
plate_b_5<-read.csv(here("data","111121_plate_b_5_OD.csv"))
plate_c_1<-read.csv(here("data","111121_plate_c_1_OD.csv"))
plate_c_2<-read.csv(here("data","111121_plate_c_2_OD.csv"))
plate_c_3<-read.csv(here("data","111121_plate_c_3_OD.csv"))
plate_c_4<-read.csv(here("data","111121_plate_c_4_OD.csv"))
plate_c_5<-read.csv(here("data","111121_plate_c_5_OD.csv"))

#adicionar el tiempo
Time<-read.csv(here("data","Time.csv"))

#eliminar el tiempo, temperatura y controles
plate_a_1<-as.matrix(plate_a_1[,-c(1,2)])
plate_a_2<-as.matrix(plate_a_2[,-c(1,2)])
plate_a_3<-as.matrix(plate_a_3[,-c(1,2)])
plate_a_4<-as.matrix(plate_a_4[,-c(1,2)])
plate_a_5<-as.matrix(plate_a_5[,-c(1,2)])
plate_b_1<-as.matrix(plate_b_1[,-c(1,2)])
plate_b_2<-as.matrix(plate_b_2[,-c(1,2)])
plate_b_3<-as.matrix(plate_b_3[,-c(1,2)])
plate_b_4<-as.matrix(plate_b_4[,-c(1,2)])
plate_b_5<-as.matrix(plate_b_5[,-c(1,2)])
plate_c_1<-as.matrix(plate_c_1[,-c(1,2)])
plate_c_2<-as.matrix(plate_c_2[,-c(1,2)])
plate_c_3<-as.matrix(plate_c_3[,-c(1,2)])
plate_c_4<-as.matrix(plate_c_4[,-c(1,2)])
plate_c_5<-as.matrix(plate_c_5[,-c(1,2)])


#restar el pozo control
x<-nrow(plate_a_1)
ctrl<-c()
for (i in 1:x){
  ctrl[i]<-mean(plate_b_1[i,c(384,382,380,378)])
}

ctrl<-mean(ctrl) - 0.003

plate_a_1<-plate_a_1 - ctrl
plate_a_2<-plate_a_2 - ctrl
plate_a_3<-plate_a_3 - ctrl
plate_a_4<-plate_a_4 - ctrl
plate_a_5<-plate_a_5 - ctrl
plate_b_1<-plate_b_1 - ctrl
plate_b_2<-plate_b_2 - ctrl
plate_b_3<-plate_b_3 - ctrl
plate_b_4<-plate_b_4 - ctrl
plate_b_5<-plate_b_5 - ctrl
plate_c_1<-plate_c_1 - ctrl
plate_c_2<-plate_c_2 - ctrl
plate_c_3<-plate_c_3 - ctrl
plate_c_4<-plate_c_4 - ctrl
plate_c_5<-plate_c_5 - ctrl

#calcular el promedio y la desviacion estandar
mean_a_1<-c(); mean_a_2<-c(); mean_a_3<-c(); mean_a_4<-c(); mean_a_5<-c()
mean_b_1<-c(); mean_b_2<-c(); mean_b_3<-c(); mean_b_4<-c(); mean_b_5<-c()
mean_c_1<-c(); mean_c_2<-c(); mean_c_3<-c(); mean_c_4<-c(); mean_c_5<-c()

v<-nrow(plate_a_1); w<-nrow(plate_a_2); x<-nrow(plate_a_3); y<-nrow(plate_a_4); z<-nrow(plate_a_5)

for (i in 1:v){
  mean_a_1[i]<-mean(plate_a_1[i,])
  mean_b_1[i]<-mean(plate_b_1[i,])
  mean_c_1[i]<-mean(plate_c_1[i,])
}

for (i in 1:w){
  mean_a_2[i]<-mean(plate_a_2[i,])
  mean_b_2[i]<-mean(plate_b_2[i,])
  mean_c_2[i]<-mean(plate_c_2[i,])
}

for (i in 1:x){
  mean_a_3[i]<-mean(plate_a_3[i,])
  mean_b_3[i]<-mean(plate_b_3[i,])
  mean_c_3[i]<-mean(plate_c_3[i,])
}

for (i in 1:y){
  mean_a_4[i]<-mean(plate_a_4[i,])
  mean_b_4[i]<-mean(plate_b_4[i,])
  mean_c_4[i]<-mean(plate_c_4[i,])
}

for (i in 1:z){
  mean_a_5[i]<-mean(plate_a_5[i,])
  mean_b_5[i]<-mean(plate_b_5[i,])
  mean_c_5[i]<-mean(plate_c_5[i,])
}

#para ajustar la primera fase de la curva con respecto al resto
e<-mean_b_1[1] - mean_a_1[1]
mean_b_1<-mean_b_1 - e

#calcular el numero de generaciones bacterianas

a_1<-c(); a_2<-c(); a_3<-c(); a_4<-c(); a_5<-c()
b_1<-c(); b_2<-c(); b_3<-c(); b_4<-c(); b_5<-c()
c_1<-c(); c_2<-c(); c_3<-c(); c_4<-c(); c_5<-c()

for (i in 1:v){
  a_1[i]<-(log(mean_a_1[i])-log(mean_a_1[1]))/log(2)
  b_1[i]<-(log(mean_b_1[i])-log(mean_b_1[1]))/log(2)
  c_1[i]<-(log(mean_c_1[i])-log(mean_c_1[1]))/log(2)
}

for (i in 1:w){
  a_2[i]<-(log(mean_a_2[i])-log(mean_a_2[1]))/log(2)
  b_2[i]<-(log(mean_b_2[i])-log(mean_b_2[1]))/log(2)
  c_2[i]<-(log(mean_c_2[i])-log(mean_c_2[1]))/log(2)
}

for (i in 1:x){
  a_3[i]<-(log(mean_a_3[i])-log(mean_a_3[1]))/log(2)
  b_3[i]<-(log(mean_b_3[i])-log(mean_b_3[1]))/log(2)
  c_3[i]<-(log(mean_c_3[i])-log(mean_c_3[1]))/log(2)
}

for (i in 1:y){
  a_4[i]<-(log(mean_a_4[i])-log(mean_a_4[1]))/log(2)
  b_4[i]<-(log(mean_b_4[i])-log(mean_b_4[1]))/log(2)
  c_4[i]<-(log(mean_c_4[i])-log(mean_c_4[1]))/log(2)
}

for (i in 1:z){
  a_5[i]<-(log(mean_a_5[i])-log(mean_a_5[1]))/log(2)
  b_5[i]<-(log(mean_b_5[i])-log(mean_b_5[1]))/log(2)
  c_5[i]<-(log(mean_c_5[i])-log(mean_c_5[1]))/log(2)
}




#unir los datos
plate_a<-c(a_1,a_2,a_3,a_4,a_5)
plate_b<-c(b_1,b_2,b_3,b_4,b_5)
plate_c<-c(c_1,c_2,c_3,c_4,c_5)

plate_a<-as.data.frame(plate_a)
plate_b<-as.data.frame(plate_b)
plate_c<-as.data.frame(plate_c)


x<-nrow(plate_a)
Time<-Time[c(1:x),]
tratamientos<-cbind(Time,plate_b,plate_a,plate_c)

colnames(tratamientos)<-c("Time","Control","Estrés 2","Estrés 1")
tratamientos<-melt(tratamientos)
colnames(tratamientos)<-c("Time","Tratamiento","value")


G_plot<-ggplot(tratamientos,aes(Time,value,color=Tratamiento,group=Tratamiento)) +
  geom_step() + theme_classic() +
  #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,alpha=0.5) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Tiempo (HH:MM)",expand=c(0,0)) +
  scale_y_continuous("#Generaciones bacterianas",breaks = seq(0.0, 13.0, by = 1.0)) +
  geom_vline(aes(xintercept = 20), 
             linetype = "dashed", colour = "black",size = 1) +
  geom_vline(aes(xintercept = 39), 
             linetype = "dashed", colour = "black",size = 1) +
  geom_vline(aes(xintercept = 52), 
             linetype = "dashed", colour = "black",size = 1) +
  geom_vline(aes(xintercept = 59), 
             linetype = "dashed", colour = "black",size = 1) +
  theme(axis.text = element_text(size = 8),
        title = element_text(size = 12,face="bold")) +
  ggtitle("Generaciones de E. coli ante ambientes fluctuantes") +
  theme(plot.title = element_text(hjust = 0.5))

print(G_plot)
