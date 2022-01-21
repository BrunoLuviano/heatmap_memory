#cargar librerias
library(fastDummies)
library(tidyverse)
library(here)
library(RColorBrewer)
library(pheatmap)
library(reshape2)
library(pacman)
library(dplyr)

#cargar los archivos
GO_1<-read.csv(here("data","GO_2.csv"))
GO_component<-GO_1[GO_1$Componente=="cellular_component",]
GO_molecular<-GO_1[GO_1$Componente=="molecular_function",]
GO_biological<-GO_1[GO_1$Componente=="biological_process",]



GO_component<-table(GO_component$respuesta)
GO_component<-as.data.frame(GO_component)
GO_component<-GO_component[order(GO_component$Freq),]
component_names<-GO_component[,1]
#rownames(GO_component)<-component_names
GO_component$Var1 <- factor(GO_component$Var1, levels = GO_component$Var1[order(GO_component$Freq)])

GO_molecular<-table(GO_molecular$respuesta)
GO_molecular<-as.data.frame(GO_molecular)
GO_molecular<-GO_molecular[order(GO_molecular$Freq),]
GO_molecular$Var1 <- factor(GO_molecular$Var1, levels = GO_molecular$Var1[order(GO_molecular$Freq)])

GO_biological<-table(GO_biological$respuesta)
GO_biological<-as.data.frame(GO_biological)
GO_biological<-GO_biological[order(GO_biological$Freq),]
GO_biological$Var1 <- factor(GO_biological$Var1, levels = GO_biological$Var1[order(GO_biological$Freq)])

#graficar

component_1_plot<-ggplot(GO_component,aes(Var1,Freq)) +
  geom_bar(stat="identity",fill="steelblue") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Componentes celulares",expand=c(0,0)) +
  scale_y_continuous("No. genes") +
  theme(axis.text = element_text(size = 8),
        title = element_text(size = 12,face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  #scale_fill_distiller(palette = "Paired") +
  theme(legend.position="none") +
  ggtitle("Estrés 2")

#print(component_plot)


molecular_1_plot<-ggplot(GO_molecular,aes(Var1,Freq)) +
  geom_bar(position = 'dodge', stat='identity',fill="steelblue") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Función molecular") +
  scale_y_continuous("No. genes") +
  theme(axis.text = element_text(size = 5),
        title = element_text(size = 12,face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  ggtitle("Estrés 2")

#print(molecular_plot)


biological_1_plot<-ggplot(GO_biological,aes(Var1,Freq)) +
  geom_bar(position = 'dodge', stat='identity',fill="steelblue") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Proceso biológico",expand=c(0,0)) +
  scale_y_continuous("No. genes") +
  theme(axis.text = element_text(size = 5),
        title = element_text(size = 12,face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  ggtitle("Estrés 2")

#print(biological_plot)

#source(here("script","GO_Estres_1.R"))

#cargar los archivos
GO_1<-read.csv(here("data","GO_1.csv"))
GO_component<-GO_1[GO_1$Componente=="cellular_component",]
GO_molecular<-GO_1[GO_1$Componente=="molecular_function",]
GO_biological<-GO_1[GO_1$Componente=="biological_process",]



GO_component<-table(GO_component$respuesta)
GO_component<-as.data.frame(GO_component)
GO_component<-GO_component[order(GO_component$Freq),]
component_names<-GO_component[,1]
#rownames(GO_component)<-component_names
GO_component$Var1 <- factor(GO_component$Var1, levels = GO_component$Var1[order(GO_component$Freq)])

GO_molecular<-table(GO_molecular$respuesta)
GO_molecular<-as.data.frame(GO_molecular)
GO_molecular<-GO_molecular[order(GO_molecular$Freq),]
GO_molecular$Var1 <- factor(GO_molecular$Var1, levels = GO_molecular$Var1[order(GO_molecular$Freq)])

GO_biological<-table(GO_biological$respuesta)
GO_biological<-as.data.frame(GO_biological)
GO_biological<-GO_biological[order(GO_biological$Freq),]
GO_biological$Var1 <- factor(GO_biological$Var1, levels = GO_biological$Var1[order(GO_biological$Freq)])

#graficar

component_plot<-ggplot(GO_component,aes(Var1,Freq)) +
  geom_bar(stat="identity",fill="darkred") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Componentes celulares",expand=c(0,0)) +
  scale_y_continuous("No. genes") +
  theme(axis.text = element_text(size = 8),
        title = element_text(size = 12,face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  ggtitle("Estrés 1")

#print(component_plot)


molecular_plot<-ggplot(GO_molecular,aes(Var1,Freq)) +
  geom_bar(position = 'dodge', stat='identity',fill="darkred") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Función molecular",expand=c(0,0)) +
  scale_y_continuous("No. genes") +
  theme(axis.text = element_text(size = 5),
        title = element_text(size = 12,face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  ggtitle("Estrés 1")

#print(molecular_plot)


biological_plot<-ggplot(GO_biological,aes(Var1,Freq)) +
  geom_bar(position = 'dodge', stat='identity',fill="darkred") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete("Proceso biológico",expand=c(0,0)) +
  scale_y_continuous("No. genes") +
  theme(axis.text = element_text(size = 5),
        title = element_text(size = 12,face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  ggtitle("Estrés 1")

#print(biological_plot)


figure_1 <- ggarrange(component_plot,component_1_plot,
                    labels = c("A", "B"),ncol = 2, nrow = 1)

figure_2 <- ggarrange(molecular_plot,molecular_1_plot,
                      labels = c("A", "B"),ncol = 2, nrow = 1)

figure_3 <- ggarrange(biological_plot,biological_1_plot,
                      labels = c("A", "B"),ncol = 2, nrow = 1)

print(figure_1)
print(figure_2)
print(figure_3)
