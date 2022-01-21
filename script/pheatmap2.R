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
library(pacman)

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
genes<-as.vector(network[,1])
network<-network[,-c(1)]
rownames(network)<-genes
network<-as.data.frame(t(network))

#reordenar los genes de acuerdo a los niveles de expresion del experimento
source(here("script","orden3.R"))

network<-network[,b] #datos reordenados

#la variable diff_exp es importarte, contiene la información  de log(Estres2/Estres1)
diff_exp<-as.data.frame(t(diff_exp))
colnames(diff_exp)<-c("Estrés 2/Estrés 1")
annotation_row<-diff_exp
col.pal <- RColorBrewer::brewer.pal(9, "Set1")

#graficar
pacman::p_load(pheatmap)

pheatmap::pheatmap(network, 
                   cluster_row = T,
                   cluster_cols = F,
                   annotation_col = annotation_row,
                   color = c("snow2", "red3","blue3","green3","turquoise3","darkgoldenrod1") , 
                   fontsize = 6.5,
                   fontsize_row=2, 
                   fontsize_col = 3)

