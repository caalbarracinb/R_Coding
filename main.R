#### PROYECTO PARA IMPUTAR LOS PRECIOS DE: HOJA, PASTA, BASE Y CLORHIDRATO ####
#### ANTECEDENTES: LAS SERIES GENERADAS POR LA POLICIA ESTAN INCOMPLETAS ####
#### LIMPIAR ####
gc()
rm(list=ls())
#### LIBRERIAS
library(readxl) # LEER EXCEL
library(plyr) # ORDENAR DATOS
library(dplyr) #ORDENAR DATOS
library(tidyr)
library(janitor)
library(writexl)
library(stats)
library(zoo) # HERRAMIENTAS DE TS
library(imputeFin) # IMPUTAR TS UNIVARIADAS
library(imputeTS) # IMPUTAR TS UNIVARIADAS 
library(ggplot2) # GRAFICOS
library(tsbox)
library(gridExtra)
library(ggfortify)
Sys.setlocale("LC_TIME", "es_ES.UTF-8")#### MESES EN ESPAÑOL 
####  LEER DATOS
precioshoja = read_excel("precio_drogas.xlsx")%>%clean_names()
tsprecioshoja = ts(precioshoja,start=c(2006,1),frequency = 12)
#### DIAGNOSTICO
plot(tsprecioshoja,main="Distribución de Valores Faltantes",col = "#0CBAA6")
ts = ts_xts(tsprecioshoja)
precioshojaimput = impute_AR1_Gaussian(ts)
ggplot_na_distribution(precioshoja)
tss = ts_ts(precioshojaimput)
lines(tss)
plot(tsprecioshoja)
base = autoplot(tsprecioshoja,color="#0CBAA6")+
  facet_grid(facets = 1:6)
base 
autoplot(tss,facets = T,color = "red",main = "Precio de Hoja de Coca - Valores Imputados",arrange =F)+
  autolayer(tsprecioshoja,color="#0CBAA6")
imput = na_kalman(tsprecioshoja,model = "auto.arima")
como = autoplot(imput,facets = T,color = "red",main = "Precio de Hoja de Coca - Valores Imputados")+
  autolayer(tsprecioshoja,color="#0CBAA6")
como
autoplot(tss,facets = T,color = "red",main="Hola")
  autoplot(tsprecioshoja)
plot(tss,main="Distribución con Valores Imputados",col = "#0CBAA6")

juju = na_kalman(tsprecioshoja)

grid.arrange(base@plots[[1]])
base$facet

jaja = precioshoja$central
jeje = impute_AR1_Gaussian(jaja)
plot(jeje,type="line",col="blue")
lines(jaja,type="line",col="red")

regiones <- c(
  `amazonia` = "Amazonía",
  `catatumbo` = "Catatumbo",
  `central` = "Central",
  `metaguav` = "Meta-Guaviare",
  `orinoquia` = "Orinoquía",
  `pacifico` = "Pacífico",
  `putca` = "Putumayo-Caquetá",
  `sierra` = "Sierra Nevada de Sta. Marta")




imput = as.data.frame(impute_AR1_Gaussian(tsprecioshoja,random_walk = F))
imput = imput%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2020-12-1"), by = "months"))%>%
  gather("region","preciohoja",-t)
#### MEDIA POR AÑO ####
base = precioshoja%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2020-12-1"), by = "months"))%>%
  gather("region","preciohoja",-t)
imput = precioshoja%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2020-12-1"), by = "months"))%>%
  gather("region","preciohoja",-t)%>%
  mutate(anno = format(t,"%Y"))%>%
  group_by(anno,region)%>%
  mutate_at(vars(-t),funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)))


ggplot()+
  geom_line(data = imput, aes(x=t,y=preciohoja), col="#0CBAA6",size=1.5)+
  geom_point(data = imput, aes(x=t,y=preciohoja), col="#F60C0C",size=1)+
  geom_point(data = base, aes(x=t,y=preciohoja), col="#0BF6F6",size=1)+
  facet_wrap(~region,scales="free",labeller = as_labeller(regiones),ncol=2)+
  theme_minimal()+
  theme(axis.line=element_line()) + 
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(limits=c(0,10000))+
  labs(x="Mes/Año",
       y= "Precio COP",
       title = "Serie Mensual Precios Hoja de Coca por Región - 2006-2020",
       subtitle = "Imputación: Media Región/Año")+
  theme(strip.text = element_text(face="bold",size=15))+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=15))


#### IMPUTACION AR (1) ERRORES GAUSIANOS

imput = as.data.frame(impute_AR1_Gaussian(tsprecioshoja))
imput = imput%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2020-12-1"), by = "months"))%>%
  gather("region","preciohoja",-t)
base = precioshoja%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2020-12-1"), by = "months"))%>%
  gather("region","preciohoja",-t)
ggplot()+
  geom_line(data = imput, aes(x=t,y=preciohoja), col="#0CBAA6",size=1.5)+
  geom_point(data = imput, aes(x=t,y=preciohoja), col="#F60C0C",size=0.75)+
  geom_point(data = base, aes(x=t,y=preciohoja), col="#0BF6F6",size=0.75)+
  facet_wrap(~region,scales="free",labeller = as_labeller(regiones),ncol=2)+
  theme_minimal()+
  theme(axis.line=element_line()) + 
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(limits=c(0,10000))+
  labs(x="Mes/Año",
       y= "Precio COP",
       title = "Serie Mensual Precios Hoja de Coca por Región - 2006-2020",
       subtitle = "Imputación: Modelo AR (1) con Errores Gaussianos")+
  theme(strip.text = element_text(face="bold",size=15))+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=15))

#### IMPUTACION  KALMAN ####

imput = as.data.frame(na_kalman(tsprecioshoja,model="auto.arima"))
imput = imput%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2020-12-1"), by = "months"))%>%
  gather("region","preciohoja",-t)
base = precioshoja%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2020-12-1"), by = "months"))%>%
  gather("region","preciohoja",-t)
ggplot()+
  geom_line(data = imput, aes(x=t,y=preciohoja), col="#0CBAA6",size=1.2)+
  geom_point(data = imput, aes(x=t,y=preciohoja), col="#F60C0C",size=0.75)+
  geom_point(data = base, aes(x=t,y=preciohoja), col="#0BF6F6",size=0.75)+
  facet_wrap(~region,scales="free",labeller = as_labeller(regiones),ncol=2)+
  theme_minimal()+
  theme(axis.line=element_line()) + 
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(limits=c(0,10000))+
  labs(x="Mes/Año",
       y= "Precio COP",
       title = "Serie Mensual Precios Hoja de Coca por Región - 2006-2020",
       subtitle = "Imputación: Algoritmo de Kalman Basado en ARIMA")+
  theme(strip.text = element_text(face="bold",size=15))+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=15))


imput
#### DESES
imput = as.data.frame(na_seasplit(tsprecioshoja,algorithm = "interpolation",find_frequency = T))
imput = imput%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2020-12-1"), by = "months"))%>%
  gather("region","preciohoja",-t)
base = precioshoja%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2020-12-1"), by = "months"))%>%
  gather("region","preciohoja",-t)
ggplot()+
  geom_line(data = imput, aes(x=t,y=preciohoja), col="#0CBAA6",size=1)+
  geom_point(data = imput, aes(x=t,y=preciohoja, color="#FF7F7F"),size=0.7)+
  geom_point(data = base, aes(x=t,y=preciohoja, color="#0BF6F6"),size=0.7)+
  scale_colour_manual(name="Colores",
                      labels=c("Valores Conocidos", "Valores Imputados"),
                      values=c("#0BF6F6","#FF7F7F"))+
  facet_wrap(~region,scales="free",labeller = as_labeller(regiones),ncol=2)+
  theme(axis.line=element_line()) + 
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(limits=c(0,10000),expand = c(0,0),breaks = c(seq(1000,10000,2000)))+
  labs(x="Mes/Año",
       y= "Precio COP",
       title = "Serie Mensual Precios Hoja de Coca por Región - 2006-2020",
       subtitle = "Imputación: División Estacional Algoritmo de Intrapolación Lineal")+
  theme(strip.text = element_text(face="bold",size=15))+
  theme(axis.text = element_text(color="black"))+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=15))+
  guides(color = guide_legend(override.aes = list(size=2)))+
  theme(legend.key=element_blank())+
  theme(legend.title = element_text(face = "bold",size=12))+
  theme(strip.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.minor = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))+
  theme(panel.grid.major = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))



#### DIAGNOSTICO ####
base = precioshoja%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2020-12-1"), by = "months"))%>%
  gather("region","preciohoja",-t)%>%
  mutate(mes = format(t,"%B"))
na = base%>%
  filter(is.na(preciohoja))
ggplot()+
  geom_line(data = base, aes(x=t,y=preciohoja), col="#0CBAA6",size=0.8)+
  geom_point(data = base, aes(x=t,y=preciohoja), col="#0CBAA6",size=0.8)+
  geom_vline(data = filter(base, is.na(preciohoja)),aes(xintercept = t),colour = "#FF7F7F",size=1,alpha=0.5)+
  facet_wrap(~region,scales="free",labeller = as_labeller(regiones),ncol=2)+
  theme(axis.line=element_line()) + 
  scale_x_date(breaks = "1 year",date_labels = "%b-%Y")+
  scale_y_continuous(limits=c(0,10000),expand = c(0,0),breaks = c(seq(1000,10000,2000)))+
  labs(x="Mes/Año",
       y= "Precio COP",
       title = "Diagnóstico Precios Hoja de Coca por Región - 2006-2020",
       subtitle = "Valores Faltantes Resaltados en Rojo")+
  theme(strip.text = element_text(face="bold",size=15))+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=15))+
  theme(strip.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.minor = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))+
  theme(panel.grid.major = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))
Sys.getenv("LANGUAGE")="SPA"
central = imput%>%
  filter(region=="central")

ku = format(as.Date("2021-01-01"), "%m-%y")
write_xlsx(ku,"ku.xlsx")
plot(central$preciohoja,type="line")

modelo = auto.arima(central$preciohoja,trace = T)
modelo = arima(central$preciohoja,order = c(2,1,1))
plot(forecast(modelo,h=12))

modelo = auto.arima(precioshoja$central)
modelo
plot(precioshoja$central,type="l",)
auto
