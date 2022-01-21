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

#write.csv(network,here("data","network10.csv"), row.names = TRUE)
#sustitui el resto desde Excel
network<-read.csv(here("data","network10.csv"))
network<-as.data.frame(network)
genes<-as.vector(network[,1])
network<-network[,-c(1)]
rownames(network)<-genes
network<-as.data.frame(t(network))

#reordenar los genes de acuerdo a los niveles de expresion del experimento
source(here("script","orden6.R"))

strains_3<-as.vector(t(strains_3))
strains_4<-as.vector(t(strains_4))
strains_5<-as.vector(t(strains_5))

red<-network
red1<-network
network<-network[,strains_3] #datos reordenados
red<-red[,strains_4]
red1<-red1[,strains_5]

#seleccionar solo los lados extremos
n<-ncol(network)
m<-30
l<-n - m + 1
network<-network[,c(1:m,l:n)]
red<-red[,c(1:m,l:n)]
red1<-red1[,c(1:m,l:n)]
a<-diff_exp
diff_exp<-diff_exp[,c(1:m,l:n)]
b<-b[,c(1:m,l:n)]
c<-c[,c(1:m,l:n)]

#eliminar los renglones (factores transcripcionales) sin interacciones
#cero indica sin regulacion
network<-network[rowSums(network)!=0,]
red<-red[rowSums(red)!=0,]
red1<-red1[rowSums(red1)!=0,]

#marcar con direfente color los genes preferentemente regulados en una u otra condición
n<-ncol(network)
m<-n/2
l<-m + 1
network_1<-network[,c(1:m)]
network_2<-network[,c(l:n)]

red_1<-red[,c(1:m)]
red_2<-red[,c(l:n)]

red1_1<-red1[,c(1:m)]
red1_2<-red1[,c(l:n)]

network_2[network_2 == 1] <- 2
red_2[red_2 == 1] <- 2
red1_2[red1_2 == 1] <- 2


#unir las partes
network<-cbind(network_1,network_2)
red<-cbind(red_1,red_2)
red1<-cbind(red1_1,red1_2)

#eliminar los 2 reguladores mas comunes
network<-as.data.frame(t(network))
network<-select(network,-c(Sigma70, CRP))
network<-as.data.frame(t(network))

red<-as.data.frame(t(red))
red<-select(red,-c(Sigma70, CRP))
red<-as.data.frame(t(red))

red1<-as.data.frame(t(red1))
red1<-select(red1,-c(Sigma70, CRP))
red1<-as.data.frame(t(red1))

#la variable diff_exp es importarte, contiene la información  de log(Estres2/Estres1)
diff_exp<-as.data.frame(t(diff_exp))
annotation_col<-diff_exp
colnames(annotation_col)<-c("Estrés2/Estrés1")

#graficar
pacman::p_load(pheatmap)

pheatmap::pheatmap(network, 
                   cluster_row = T,
                   cluster_cols = F,
                   annotation_col = annotation_col,
                   color = c("snow2", "red3","blue3"),
                   main="Regulación transcripcional",
                   fontsize = 6.5,
                   fontsize_row=7, 
                   fontsize_col = 8)




b<-as.data.frame(t(b))
annotation_col<-b
colnames(annotation_col)<-c("Estrés2/Control")

#graficar
pacman::p_load(pheatmap)

pheatmap::pheatmap(red, 
                   cluster_row = T,
                   cluster_cols = F,
                   annotation_col = annotation_col,
                   color = c("snow2", "red3","blue3"),
                   main="Regulación transcripcional",
                   fontsize = 6.5,
                   fontsize_row=7, 
                   fontsize_col = 8)







c<-as.data.frame(t(c))
annotation_col<-c
colnames(annotation_col)<-c("Estrés1/Control")


#graficar
pacman::p_load(pheatmap)

pheatmap::pheatmap(red1, 
                   cluster_row = T,
                   cluster_cols = F,
                   annotation_col = annotation_col,
                   main="Regulación transcripcional",
                   color = c("snow2", "red3","blue3") , 
                   fontsize = 6.5,
                   fontsize_row=7, 
                   fontsize_col = 8)





