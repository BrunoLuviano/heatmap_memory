#script de experimento de Yeliza
#las dosis de AMP que uso fueron 2.5 y 13 ug/ml

#cargar librerias
library(ggplot2)
library(reshape2)
library(here)
library(wesanderson)
library(ggpubr)
library(tidyverse)
library(RColorBrewer)
library(jcolors)
#library(scales)

#cargar los archivos
OD<-read.csv(here("data","datos_yelitza.csv"))

ctrls<-OD[c(1:3),]
OD<-OD[-c(1:3),]

#hacer los tratamientos
estimulo_1h<-rep("1",3)
estimulo_2h<-rep("2",3)
estimulo_3h<-rep("3",3)
recuperacion<-rep(c(0:2),3)
recuperacion<-as.character(recuperacion)
recuperacion<-as.data.frame(recuperacion)

#crear tabla
estimulo<-c(estimulo_1h,estimulo_2h,estimulo_3h)
estimulo<-as.character(estimulo)
estimulo<-as.data.frame(estimulo)
estimulo<-cbind(estimulo,OD,recuperacion)

n<-max(estimulo$mean)

x_plot<-ggplot(estimulo , aes(x = estimulo , y = mean,color=recuperacion,group=recuperacion)) +
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.05,alpha=0.5) +
  #scale_color_jcolors(palette = "pal6",name = "Recuperación (h)") +
  #scale_color_tron(name = "Recuperación (h)") +
  scale_color_manual(values=c("darkorange1","chartreuse3","dodgerblue"),name = "Recuperación (h)") +
  theme_classic() +
  scale_x_discrete("Estímulo (h)") +
  scale_y_continuous("Supervivencia (%)") +
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 16,face="bold"),
        panel.border= element_rect(size=0.5,color="black",fill=NA)) +
  geom_hline(aes(yintercept = 100), 
             linetype = "dashed", colour = "black",size = 1) +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", colour = "black",size = 1) +
  geom_hline(aes(yintercept = n), 
             linetype = "dashed", colour = "red",size = 1) 
  #scale_color_discrete(name = "Recuperación (h)")


#print(x_plot)


y_plot<-ggplot(estimulo , aes(x = recuperacion , y = mean,color=estimulo,group=estimulo)) +
  geom_point() + 
  geom_line(linetype = "dashed") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.05,alpha=0.5) +
  #scale_color_jcolors(palette = "pal6",name = "Recuperación (h)") +
  #scale_color_tron(name="Estímulo (h)") +
  scale_color_manual(values=c("darkorange1","chartreuse3","dodgerblue"),name = "Estímulo (h)") +
  theme_classic() +
  scale_x_discrete("Recuperación (h)") +
  scale_y_continuous("Supervivencia (%)") +
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 16,face="bold"),
        panel.border= element_rect(size=0.5,color="black",fill=NA)) +
  geom_hline(aes(yintercept = 100), 
             linetype = "dashed", colour = "black",size = 1) +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", colour = "black",size = 1) +
  geom_hline(aes(yintercept = n), 
             linetype = "dashed", colour = "red",size = 1) 
#scale_color_discrete(name = "Recuperación (h)")

#print(y_plot)

figure <- ggarrange(x_plot,y_plot,
                    labels = c("A", "B"),ncol = 2, nrow = 1, legend = "top")

annotate_figure(figure,
                top = text_grob("Visualizing len"))

print(figure)

