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
library(pracma)

#library(scales)

#cargar los archivos
strains<-read.csv(here("data","Cepario.csv"))

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

#eliminar las bacterias que NO crecieron despues de transferencia
#eliminar los pozos controles
memory<-memory[,-c(384,382,380,378)]
non_memory<-non_memory[,-c(384,382,380,378)]

memory<-memory[,-c(290,292,294,296,298,300,302,304,306,308,310,312)]
non_memory<-non_memory[,-c(290,292,294,296,298,300,302,304,306,308,310,312)]

#calcular el area bajo la curva para ordenar genes
strains<-strains[-c(384,382,380,378,290,292,294,296,298,300,302,304,306,308,310,312)]

x<-nrow(memory)
Tiempo<-c(1:x)
z<-ncol(memory)

x<-cbind(Tiempo,memory)
y<-cbind(Tiempo,non_memory)


require(pracma)
a<-c(); b<-c()
for (i in 1:z) {
  a[i]<-trapz(x[,1],x[,1+i])
  b[i]<-trapz(y[,1],y[,1+i])
}

a<-as.data.frame(a)
b<-as.data.frame(b)
strains<-as.data.frame(strains)
memory_area<-as.data.frame(cbind(strains,a))
non_memory_area<-as.data.frame(cbind(strains,b))

memory_area<-memory_area[order(memory_area$a),]
non_memory_area<-non_memory_area[order(non_memory_area$b),]
strains_1<-as.vector(memory_area[,1])
strains_2<-as.vector(non_memory_area[,1])
memory_area<-memory_area[,2]
non_memory_area<-non_memory_area[,2]
memory_area<-as.data.frame(memory_area)
non_memory_area<-as.data.frame(non_memory_area)


rownames(memory_area)<-strains_1
rownames(non_memory_area)<-strains_2

memory_area<-as.data.frame(t(memory_area))
non_memory_area<-as.data.frame(t(non_memory_area))

memory_area<-memory_area[,strains_1]
non_memory_area<-non_memory_area[,strains_1]

memory_area<-melt(memory_area)
non_memory_area<-melt(non_memory_area)
#memory<-memory[,strains]
#non_memory<-non_memory[,strains]

memory_area_plot<-ggplot(memory_area , aes(x = variable , y = value, fill = value)) +
  geom_bar(position = 'identity', stat = "identity") +
  scale_fill_viridis_c(name="Valor") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Genes",expand=c(0,0)) +
  scale_y_discrete("ln(Área bajo la curva(Estrés 2/Estrés 1))",expand=c(0,0)) +
  theme(axis.text = element_text(size = 2),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=0.5,color="black",fill=NA)) +
  coord_flip()


non_memory_area_plot<-ggplot(non_memory_area , aes(x = variable , y = value, fill = value)) +
  geom_bar(position = 'identity', stat = "identity") +
  scale_fill_viridis_c(name="Valor") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Genes",expand=c(0,0)) +
  scale_y_discrete("ln(Área bajo la curva(Estrés 2/Estrés 1))",expand=c(0,0)) +
  theme(axis.text = element_text(size = 2),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=0.5,color="black",fill=NA)) +
  coord_flip()


figure <- ggarrange(non_memory_area_plot,memory_area_plot,
                    labels = c("A", "B"),ncol = 2, nrow = 1, legend = "right")
print(figure)

#common.legend = TRUE
