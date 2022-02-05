#Los datos de este script constan de 3 experimentos
#cada experimento son curvas de crecimiento ante ambientes fluctuantes
#el plato a estuvo expuesto en dos ocasiones a ampicilina
#el plato b es el control que no es expuesto al estrés
#el plato c es expuesto una unica vez a ampicilina
#se usa la concentracion de 2.5 y 5 ug/ml AMP, respectivamente

#cargar librerias
library(ggplot2)
library(reshape2)
library(here)
library(wesanderson)
library(ggpubr)
library(tidyverse)
library(RColorBrewer)
#library(scales)

#cargar los archivos
strains<-read.csv(here("data","Cepario.csv"))
OD_mean<-read.delim(here("data","OD_mean.txt"))

strains_order<-read.delim(here("data","genes_name.txt"),stringsAsFactors = F, header = T, sep = "\t")

OD_plate_a_1<-read.csv(here("data","111121_plate_a_1_OD.csv"))
GFP_plate_a_1<-read.csv(here("data","111121_plate_a_1_GFP.csv"))

OD_plate_a_2<-read.csv(here("data","111121_plate_a_2_OD.csv"))
GFP_plate_a_2<-read.csv(here("data","111121_plate_a_2_GFP.csv"))

OD_plate_a_3<-read.csv(here("data","111121_plate_a_3_OD.csv"))
GFP_plate_a_3<-read.csv(here("data","111121_plate_a_3_GFP.csv"))

OD_plate_a_4<-read.csv(here("data","111121_plate_a_4_OD.csv"))
GFP_plate_a_4<-read.csv(here("data","111121_plate_a_4_GFP.csv"))

OD_plate_a_5<-read.csv(here("data","111121_plate_a_5_OD.csv"))
GFP_plate_a_5<-read.csv(here("data","111121_plate_a_5_GFP.csv"))

OD_plate_b_1<-read.csv(here("data","111121_plate_b_1_OD.csv"))
GFP_plate_b_1<-read.csv(here("data","111121_plate_b_1_GFP.csv"))

OD_plate_b_2<-read.csv(here("data","111121_plate_b_2_OD.csv"))
GFP_plate_b_2<-read.csv(here("data","111121_plate_b_2_GFP.csv"))

OD_plate_b_3<-read.csv(here("data","111121_plate_b_3_OD.csv"))
GFP_plate_b_3<-read.csv(here("data","111121_plate_b_3_GFP.csv"))

OD_plate_b_4<-read.csv(here("data","111121_plate_b_4_OD.csv"))
GFP_plate_b_4<-read.csv(here("data","111121_plate_b_4_GFP.csv"))

OD_plate_b_5<-read.csv(here("data","111121_plate_b_5_OD.csv"))
GFP_plate_b_5<-read.csv(here("data","111121_plate_b_5_GFP.csv"))

OD_plate_c_1<-read.csv(here("data","111121_plate_c_1_OD.csv"))
GFP_plate_c_1<-read.csv(here("data","111121_plate_c_1_GFP.csv"))

OD_plate_c_2<-read.csv(here("data","111121_plate_c_2_OD.csv"))
GFP_plate_c_2<-read.csv(here("data","111121_plate_c_2_GFP.csv"))

OD_plate_c_3<-read.csv(here("data","111121_plate_c_3_OD.csv"))
GFP_plate_c_3<-read.csv(here("data","111121_plate_c_3_GFP.csv"))

OD_plate_c_4<-read.csv(here("data","111121_plate_c_4_OD.csv"))
GFP_plate_c_4<-read.csv(here("data","111121_plate_c_4_GFP.csv"))

OD_plate_c_5<-read.csv(here("data","111121_plate_c_5_OD.csv"))
GFP_plate_c_5<-read.csv(here("data","111121_plate_c_5_GFP.csv"))


#adicionar el tiempo
Time<-read.csv(here("data","Time.csv"))

#eliminar el tiempo, temperatura y controles
OD_plate_a_1<-as.matrix(OD_plate_a_1[,-c(1,2)])
OD_plate_a_2<-as.matrix(OD_plate_a_2[,-c(1,2)])
OD_plate_a_3<-as.matrix(OD_plate_a_3[,-c(1,2)])
OD_plate_a_4<-as.matrix(OD_plate_a_4[,-c(1,2)])
OD_plate_a_5<-as.matrix(OD_plate_a_5[,-c(1,2)])

OD_plate_b_1<-as.matrix(OD_plate_b_1[,-c(1,2)])
OD_plate_b_2<-as.matrix(OD_plate_b_2[,-c(1,2)])
OD_plate_b_3<-as.matrix(OD_plate_b_3[,-c(1,2)])
OD_plate_b_4<-as.matrix(OD_plate_b_4[,-c(1,2)])
OD_plate_b_5<-as.matrix(OD_plate_b_5[,-c(1,2)])

OD_plate_c_1<-as.matrix(OD_plate_c_1[,-c(1,2)])
OD_plate_c_2<-as.matrix(OD_plate_c_2[,-c(1,2)])
OD_plate_c_3<-as.matrix(OD_plate_c_3[,-c(1,2)])
OD_plate_c_4<-as.matrix(OD_plate_c_4[,-c(1,2)])
OD_plate_c_5<-as.matrix(OD_plate_c_5[,-c(1,2)])

GFP_plate_a_1<-as.matrix(GFP_plate_a_1[,-c(1,2)])
GFP_plate_a_2<-as.matrix(GFP_plate_a_2[,-c(1,2)])
GFP_plate_a_3<-as.matrix(GFP_plate_a_3[,-c(1,2)])
GFP_plate_a_4<-as.matrix(GFP_plate_a_4[,-c(1,2)])
GFP_plate_a_5<-as.matrix(GFP_plate_a_5[,-c(1,2)])

GFP_plate_b_1<-as.matrix(GFP_plate_b_1[,-c(1,2)])
GFP_plate_b_2<-as.matrix(GFP_plate_b_2[,-c(1,2)])
GFP_plate_b_3<-as.matrix(GFP_plate_b_3[,-c(1,2)])
GFP_plate_b_4<-as.matrix(GFP_plate_b_4[,-c(1,2)])
GFP_plate_b_5<-as.matrix(GFP_plate_b_5[,-c(1,2)])

GFP_plate_c_1<-as.matrix(GFP_plate_c_1[,-c(1,2)])
GFP_plate_c_2<-as.matrix(GFP_plate_c_2[,-c(1,2)])
GFP_plate_c_3<-as.matrix(GFP_plate_c_3[,-c(1,2)])
GFP_plate_c_4<-as.matrix(GFP_plate_c_4[,-c(1,2)])
GFP_plate_c_5<-as.matrix(GFP_plate_c_5[,-c(1,2)])

#calcular GFP/OD
plate_a_1<-GFP_plate_a_1/OD_plate_a_1
plate_a_2<-GFP_plate_a_2/OD_plate_a_2
plate_a_3<-GFP_plate_a_3/OD_plate_a_3
plate_a_4<-GFP_plate_a_4/OD_plate_a_4
plate_a_5<-GFP_plate_a_5/OD_plate_a_5

plate_b_1<-GFP_plate_b_1/OD_plate_b_1
plate_b_2<-GFP_plate_b_2/OD_plate_b_2
plate_b_3<-GFP_plate_b_3/OD_plate_b_3
plate_b_4<-GFP_plate_b_4/OD_plate_b_4
plate_b_5<-GFP_plate_b_5/OD_plate_b_5

plate_c_1<-GFP_plate_c_1/OD_plate_c_1
plate_c_2<-GFP_plate_c_2/OD_plate_c_2
plate_c_3<-GFP_plate_c_3/OD_plate_c_3
plate_c_4<-GFP_plate_c_4/OD_plate_c_4
plate_c_5<-GFP_plate_c_5/OD_plate_c_5


#unir los platos que son del mismo tratamiento
treat_a<-rbind(plate_c_1,plate_a_2,plate_a_3,plate_a_4,plate_a_5)
treat_b<-rbind(plate_a_1,plate_b_2,plate_b_3,plate_b_4,plate_b_5)
treat_c<-rbind(plate_c_1,plate_c_2,plate_c_3,plate_c_4,plate_c_5)
treat_d<-rbind(plate_a_1,plate_b_2,plate_b_3,plate_b_4,plate_b_5)

#comparar los tratamientos de estres vs el control sin estres
memory<-treat_a/treat_b
non_memory<-treat_c/treat_d

#acomodar la lista CORRECTAMENTE de genes
a<-c(); b<-c(); c<-c()
for (z in 1:16) {
  for (x in 1:12){
    a<-strains[x+z*24-24,]
    b<-strains[x+z*24-12,]
    c<-rbind(c,a,b)
  }
}

#adicionar nombre de genes
strains<-c
strains<-strains[,3]

memory<-as.data.frame(memory)
non_memory<-as.data.frame(non_memory)

colnames(memory)<-strains
colnames(non_memory)<-strains

#ordenar los datos de acuerdo a la matriz de coexpresion
names<-as.vector(t(strains_order))

memory<-select(memory,names)
non_memory<-select(non_memory,names)

memory<-select(memory,-c(dcuS))
non_memory<-select(non_memory,-c(dcuS))

#modificaciones para agregar segunda escala
memory<-cbind(memory,OD_mean)
non_memory<-cbind(non_memory,OD_mean)

#No hace falta calcular el area bajo la curva
#agregar columna de tiempo
x<-nrow(memory)
Time<-Time[1:x,]

memory<-cbind(Time,memory)
non_memory<-cbind(Time,non_memory)
OD_mean<-cbind(Time,OD_mean)

#graficar
memory<-melt(memory)
non_memory<-melt(non_memory)
OD_mean<-melt(OD_mean)

#para gradiente
pal <- wes_palette("Zissou1", 100, type = "continuous")

memory_plot<-ggplot(memory, aes(Time, variable,fill=value)) + 
  geom_tile() + 
  #facet_grid(~tratamiento,scales="free_x") +
  scale_fill_gradientn(colours = pal,name=bquote(frac(frac(~GFP["Tx"],~DO["Tx"]),frac(~GFP["Ctrl"],~DO["Ctrl"]))))  +
  theme_classic() +
  new_scale_fill() +
  geom_tile(data = OD_mean,aes(fill=value)) +
  scale_fill_viridis_c(option = "turbo",direction = -1,name=bquote(~DO["630 nm"])) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Tiempo (HH:MM)",expand=c(0,0)) +
  scale_y_discrete("Genes",expand=c(0,0)) +
  theme(axis.text = element_text(size = 2),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=0.5,color="black",fill=NA)) +
  geom_vline(aes(xintercept = 19.5), 
             linetype = "dashed", colour = "red",size = 1) +
  geom_vline(aes(xintercept = 38.5), 
             linetype = "dashed", colour = "red",size = 1) +
  geom_vline(aes(xintercept = 51.5), 
             linetype = "dashed", colour = "red",size = 1) +
  
  geom_vline(aes(xintercept = 58.5), 
             linetype = "dashed", colour = "red",size = 1)  + 
  coord_equal() 


#print(memory_plot)


non_memory_plot<-ggplot(non_memory, aes(Time, variable,fill=value)) + 
  geom_tile() + 
  #facet_grid(~tratamiento,scales="free_x") +
  scale_fill_gradientn(colours = pal,name=bquote(frac(~Tratamiento[1],~Control)))  +
  theme_classic() +
  new_scale_fill() +
  geom_tile(data = OD_mean,aes(fill=value)) +
  scale_fill_viridis_c(option = "turbo",direction = -1,name=bquote(~DO["630 nm"])) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Tiempo (HH:MM)",expand=c(0,0)) +
  scale_y_discrete("Genes",expand=c(0,0)) +
  theme(axis.text = element_text(size = 2),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=0.5,color="black",fill=NA)) +
  geom_vline(aes(xintercept = 19.5), 
             linetype = "dashed", colour = "red",size = 1) +
  geom_vline(aes(xintercept = 38.5), 
             linetype = "dashed", colour = "red",size = 1) +
  geom_vline(aes(xintercept = 51.5), 
             linetype = "dashed", colour = "red",size = 1) +
  
  geom_vline(aes(xintercept = 58.5), 
             linetype = "dashed", colour = "red",size = 1)  + 
  coord_equal()


figure <- ggarrange(non_memory_plot,memory_plot,
                    labels = c("A", "B"),ncol = 2, nrow = 1, legend = "right")
print(figure)
