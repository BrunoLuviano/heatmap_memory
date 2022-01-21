#prueba de cinetica tipo Michaelis-Menten
#adicionar el tiempo
Time<-read.csv(here("data","Time.csv")) #esta sera la variable X
Time<-Time[c(1:73),]

#km
a_km<-12.5
b_km<-11.5
c_km<-16

#Vmax
a_max<-9.76
b_max<-6.68
c_max<-6.78

#obtener los datos de la variable Y
a<-c(); b<-c(); c<-c()

for (i in 1:73) {
  a[i]<-(a_max*(i-1))/(a_km + (i-1))
  b[i]<-(b_max*(i-1))/(b_km + (i-1))
  c[i]<-(c_max*(i-1))/(c_km + (i-1))
}

a<-as.data.frame(a)
b<-as.data.frame(b)
c<-as.data.frame(c)
all<-cbind(Time,a,b,c)
all<-as.data.frame(all)
all<-melt(all)

control<-rep("Control",73)
estres1<-rep("Estrés 1",73)
estres2<-rep("Estrés 2",73)
tratamiento<-c(control,estres1,estres2)
tratamiento<-as.data.frame(tratamiento)

all<-cbind(all,tratamiento)



#graficar

G_plot<-ggplot(all,aes(Time,value,color=tratamiento,group=tratamiento)) +
  geom_line() + theme_classic() +
  #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,alpha=0.5) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
  scale_x_discrete() +
  scale_y_continuous() +
  ggtitle("Generaciones de E. coli ante ambientes fluctuantes") +
  theme(plot.title = element_text(hjust = 0.5))

print(G_plot)
