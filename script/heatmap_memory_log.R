#cargar librerias
library(ggplot2)
library(reshape2)
library(here)
library(wesanderson)
library(ggpubr)
library(RColorBrewer)

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
treat_a<-rbind(plate_a_1,plate_a_2,plate_a_3,plate_a_4,plate_a_5)
treat_b<-rbind(plate_b_1,plate_b_2,plate_b_3,plate_b_4,plate_b_5)
treat_c<-rbind(plate_c_1,plate_c_2,plate_c_3,plate_c_4,plate_c_5)

#comparar los tratamientos de estres vs el control sin estres
memory<-treat_a/treat_b
non_memory<-treat_c/treat_b

#calcular logaritmo para poder visualizar sub-expresion
memory<-log10(memory)
non_memory<-log10(non_memory)

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

#eliminar los pozos controles
memory<-memory[,-c(384,382,380,378)]
non_memory<-non_memory[,-c(384,382,380,378)]

memory<-cbind(Time,memory)
non_memory<-cbind(Time,non_memory)

#graficar
memory<-melt(memory)
non_memory<-melt(non_memory)

#para gradiente
pal <- wes_palette("GrandBudapest2", 100, type = "continuous")

memory_plot<-ggplot(memory, aes(Time, variable,fill=value)) + 
  geom_tile() + 
  #facet_grid(~tratamiento,scales="free_x") +
  scale_fill_viridis_c(name = "Estrés/Control") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete("Tiempo (HH:MM)",expand=c(0,0)) +
  scale_y_discrete("Genes",expand=c(0,0)) +
  theme(axis.text = element_text(size = 2),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA)) +
  geom_vline(aes(xintercept = 19.5), 
             linetype = "dashed", colour = "red",size = 1) +
  geom_vline(aes(xintercept = 38.5), 
             linetype = "dashed", colour = "red",size = 1) +
  geom_vline(aes(xintercept = 51.5), 
             linetype = "dashed", colour = "red",size = 1) +
  
  geom_vline(aes(xintercept = 58.5), 
             linetype = "dashed", colour = "red",size = 1)  + 
  coord_equal() 


print(memory_plot)


non_memory_plot<-ggplot(non_memory, aes(Time, variable,fill=value)) + 
  geom_tile() + 
  #facet_grid(~tratamiento,scales="free_x") +
  scale_fill_viridis_c(name = "Estrés/Control") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete("Tiempo (HH:MM)",expand=c(0,0)) +
  scale_y_discrete("Genes",expand=c(0,0)) +
  theme(axis.text = element_text(size = 2),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA)) +
  geom_vline(aes(xintercept = 19.5), 
             linetype = "dashed", colour = "red",size = 1) +
  geom_vline(aes(xintercept = 38.5), 
             linetype = "dashed", colour = "red",size = 1) +
  geom_vline(aes(xintercept = 51.5), 
             linetype = "dashed", colour = "red",size = 1) +
  
  geom_vline(aes(xintercept = 58.5), 
             linetype = "dashed", colour = "red",size = 1)  + 
  coord_equal() 


print(non_memory_plot)


figure <- ggarrange(non_memory_plot,memory_plot,
                    labels = c("Sin estímulo anticipatorio", "Con estímulo anticipatorio"),
                    ncol = 2, nrow = 1, common.legend = TRUE,legend = "right")
print(figure)
