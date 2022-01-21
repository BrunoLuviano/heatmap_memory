#descripcion
#el plato a es el que estuvo sometido en dos ocasiones a AMP, una vez a 2.5 ug/ml y la segunda ocasión a 5 ug/ml
#el plato b es el control que jamas ve el estres
#plato c solo es expuesto en una ocasion a AMP (5 ug/ml)

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


#unir los platos que son del mismo tratamiento
treat_a<-rbind(plate_a_1,plate_a_2,plate_a_3,plate_a_4,plate_a_5)
treat_b<-rbind(plate_b_1,plate_b_2,plate_b_3,plate_b_4,plate_b_5)
treat_c<-rbind(plate_c_1,plate_c_2,plate_c_3,plate_c_4,plate_c_5)

#restar el pozo control
ctrl<-c()
for (i in 1:131){
  ctrl[i]<-mean(treat_b[i,c(384,382,380,378)])
}

ctrl<-mean(ctrl) - 0.003

treat_a<-treat_a - ctrl
treat_b<-treat_b - ctrl
treat_c<-treat_c - ctrl

#seleccionar solo algunos genes
#treat_a<-treat_a[,c(289:301)]
#treat_b<-treat_b[,c(1,25,49,73,121,145,169,193,217,241)]
#treat_c<-treat_c[,c(289:312)]

#calcular el promedio y la desviacion estandar
mean_treat_a<-c(); sd_treat_a<-c()
mean_treat_b<-c(); sd_treat_b<-c()
mean_treat_c<-c(); sd_treat_c<-c()

for (i in 1:131){
  mean_treat_a[i]<-mean(treat_a[i,])
  mean_treat_b[i]<-mean(treat_b[i,])
  mean_treat_c[i]<-mean(treat_c[i,])
  
  sd_treat_a[i]<-std.error(treat_a[i,])
  sd_treat_b[i]<-std.error(treat_b[i,])
  sd_treat_c[i]<-std.error(treat_c[i,])
}

#para ajustar la primera fase de la curva con respecto al resto
a<-mean_treat_b[1] -mean_treat_a[1]
b<-mean_treat_b[c(1:19)] - a
mean_treat_b<-mean_treat_b[-c(1:19)]
mean_treat_b<-c(b,mean_treat_b)

#unir en una tabla los tratamientos
Estres_1<-mean_treat_a
Control<-mean_treat_b
Estres_2<-mean_treat_c

all_mean<-cbind(Control,Estres_1,Estres_2)
all_mean<-as.data.frame(all_mean)

all_sd<-cbind(sd_treat_b,sd_treat_a,sd_treat_c)
all_sd<-as.data.frame(all_sd)

all_mean<-cbind(Time,all_mean)
all_sd<-cbind(Time,all_sd)

colnames(all_mean)<-c("Time","Control","Estrés 2","Estrés 1")
#para graficar
all_mean<-melt(all_mean)
all_sd<-melt(all_sd)
all<-cbind(all_mean,all_sd[,3])
colnames(all)<-c("Time","Tratamientos","value","sd")

OD_plot<-ggplot(all,aes(Time,value,color=Tratamientos,group=Tratamientos)) +
  geom_line() + geom_point() + theme_classic() +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,alpha=0.5) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Tiempo (HH:MM)",expand=c(0,0)) +
  scale_y_continuous("Densidad optica (630 nm)",breaks = seq(0.0, 1.0, by = 0.1)) +
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
  ggtitle("Curvas de crecimiento de E. coli ante ambientes fluctuantes") +
  theme(plot.title = element_text(hjust = 0.5))

print(OD_plot)

