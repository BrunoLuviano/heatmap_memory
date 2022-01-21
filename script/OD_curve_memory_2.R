#Los datos de este script constan de 3 experimentos
#cada experimento son curvas de crecimiento ante ambientes fluctuantes
#el plato a estuvo expuesto en dos ocasiones a ampicilina
#el plato b es el control que no es expuesto al estrés
#el plato c es expuesto una unica vez a ampicilina
#se usa la concentracion de 2.5 y 5 ug/ml AMP

#cargar librerias
library(ggplot2)
library(reshape2)
library(here)
library(wesanderson)

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


#unir los platos que son del mismo tratamiento
treat_a<-rbind(plate_a_1,plate_a_2,plate_a_3,plate_a_4,plate_a_5)
treat_b<-rbind(plate_b_1,plate_b_2,plate_b_3,plate_b_4,plate_b_5)
treat_c<-rbind(plate_c_1,plate_c_2,plate_c_3,plate_c_4,plate_c_5)

#restar el pozo control
ctrl<-c()
for (i in 1:131){
  ctrl[i]<-mean(treat_b[i,c(384,382,380,378)])
}

ctrl<-mean(ctrl)

treat_a<-treat_a - ctrl
treat_b<-treat_b - ctrl
treat_c<-treat_c - ctrl

treat_a<-treat_a[,-c(384,382,380,378)]
treat_b<-treat_b[,-c(384,382,380,378)]
treat_c<-treat_c[,-c(384,382,380,378)]

#ajustar para solo mostrar m ejemplos
m<-12
treat_a<-treat_a[,c(1:m)]
treat_b<-treat_b[,c(1:m)]
treat_c<-treat_c[,c(1:m)]

treat_a<-cbind(Time,treat_a,treat_b,treat_c)

treat_a<-melt(treat_a)

n<-nrow(treat_a)/3

A<-rep("A",n)
B<-rep("B",n)
C<-rep("C",n)

treats<-c(A,B,C)

treat_a<-cbind(treat_a,treats)

#para graficar
OD_plot<-ggplot(treat_a,aes(Time,value,color=treats,group=variable)) +
  geom_line() + geom_point() + theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete("Tiempo (HH:MM)",expand=c(0,0)) +
  scale_y_continuous("Densidad optica (630 nm)",breaks = seq(0.0, 1.0, by = 0.1)) +
  geom_vline(aes(xintercept = 20), 
             linetype = "dashed", colour = "purple",size = 1) +
  geom_vline(aes(xintercept = 39), 
             linetype = "dashed", colour = "purple",size = 1) +
  geom_vline(aes(xintercept = 52), 
             linetype = "dashed", colour = "purple",size = 1) +
  geom_vline(aes(xintercept = 59), 
             linetype = "dashed", colour = "purple",size = 1) +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold")) +
  ggtitle("Curvas de crecimiento de E. coli ante ambientes fluctuantes") +
  theme(plot.title = element_text(hjust = 0.5))



print(OD_plot)
