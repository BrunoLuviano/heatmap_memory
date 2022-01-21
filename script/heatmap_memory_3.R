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
#eliminar genes
eliminar<-c(384,382,380,378,290,292,294,296,298,300,302,304,306,308,310,312)
memory<-select(memory,-eliminar)
non_memory<-select(non_memory,-eliminar)
strains<-strains[-eliminar]

#calcular el area bajo la curva para ordenar genes

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
c<-as.data.frame(a/b) #se divide el area bajo la curva de memoria/NO memoria

strains<-as.data.frame(strains)
diff_exp<-as.data.frame(cbind(strains,c))

diff_exp<-diff_exp[order(diff_exp$a),]
strains<-as.vector(diff_exp[,1])

memory<-memory[,strains] #se ordenaron los genes de mayor a menor expresion
non_memory<-non_memory[,strains]

diff_exp$a<-log(diff_exp$a)
diff_genes<-as.vector(diff_exp[,1])
diff_exp<-diff_exp[,2]
diff_exp<-as.data.frame(diff_exp)
rownames(diff_exp)<-diff_genes
diff_exp<-as.data.frame(t(diff_exp))
#diff_exp<-diff_exp[,] #eliminar los mismos genes de antes

#agregar columna de tiempo
x<-nrow(memory)
Time<-Time[1:x,]

memory<-cbind(Time,memory)
non_memory<-cbind(Time,non_memory)

#eliminar columnas de genes que salieron "mal"
#,torR,lacZ,U66,U139,araC,cdaR
memory<-select(memory,-c(dcuS,torR,lacZ,U66,U139,araC,cdaR))
non_memory<-select(non_memory,-c(dcuS,torR,lacZ,U66,U139,araC,cdaR))
diff_exp<-select(diff_exp,-c(dcuS,torR,lacZ,U66,U139,araC,cdaR))

#graficar
memory<-melt(memory)
non_memory<-melt(non_memory)
diff_exp<-melt(diff_exp)


#para gradiente
pal <- wes_palette("Zissou1", 100, type = "continuous")

memory_plot<-ggplot(memory, aes(Time, variable,fill=value)) + 
  geom_tile() + 
  #facet_grid(~tratamiento,scales="free_x") +
  scale_fill_gradientn(colours = pal,name="Estrés 2/Control")  +
  theme_classic() +
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
  scale_fill_gradientn(colours = pal,name="Estrés 1/Control")  +
  theme_classic() +
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


#print(non_memory_plot)

diff_exp_plot<-ggplot(diff_exp , aes(x = variable , y = value, fill = value)) +
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

#print(diff_exp_plot)


figure <- ggarrange(non_memory_plot,memory_plot,diff_exp_plot,
                    labels = c("A", "B", "C"),ncol = 3, nrow = 1, legend = "right")
print(figure)

#common.legend = TRUE
