#Los datos de este script constan de 3 experimentos
#cada experimento son curvas de crecimiento ante ambientes fluctuantes
#el plato a estuvo expuesto en dos ocasiones a ampicilina
#el plato b es el control que no es expuesto al estrés
#el plato c es expuesto una unica vez a ampicilina
#se usa la concentracion de 2.5 y 5 ug/ml AMP, respectivamente

#cargar librerias
library(fastDummies)
library(tidyverse)
library(here)
library(RColorBrewer)
library(pheatmap)
library(reshape2)

#cargar los archivos
network<-read.csv(here("data","interacciones.csv"))

#NOTA
# +=1, -=2, ?=3, +-=4, +?=5


network_1<-pivot_wider(network,names_from = gen,values_from = regulacion)
network_1<-as.data.frame(network_1)
regulador<-as.vector(network_1[,c(1)])
network_1<-as.data.frame(network_1[,-c(1)])
rownames(network_1)<-regulador

network_2<-pivot_wider(network,names_from = regulador,values_from = regulacion)
network_2<-as.data.frame(network_2)
gen<-as.vector(network_2[,c(1)])
network_2<-as.data.frame(network_2[,-c(1)])
rownames(network_2)<-gen

#sustituir todos los NULL por 0
n<-ncol(network_1)
a<-c(); b<-c()
for (i in 1:n){
  a<-gsub("NULL","0",network_1[,i])
  a<-as.data.frame(t(a))
  b<-rbind(b,a)
}

colnames(b)<-regulador
rownames(b)<-gen
network<-b

#write.csv(network,here("data","network.csv"), row.names = TRUE)
#sustitui el resto desde Excel

network<-read.csv(here("data","network.csv"))
network<-as.data.frame(network)
network<-melt(network)
network$value<-as.factor(network$value)

#color de la paleta
col.pal <- RColorBrewer::brewer.pal(3, "Set1")

network_plot<-ggplot(network, aes(variable,X,fill=value)) + 
  #geom_tile(colour = "snow3") +
  geom_tile()+
  scale_fill_manual(values = c("snow2", "red3","blue3","green3","turquoise3","darkgoldenrod1")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Reguladores",expand=c(0,0)) +
  scale_y_discrete("Genes",expand=c(0,0)) +
  theme(axis.text = element_text(size = 2),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=0.5,color="black",fill=NA)) +
  coord_equal() 


print(network_plot)

