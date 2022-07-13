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
library(stats)
library(zoo) # HERRAMIENTAS DE TS
library(imputeFin) # IMPUTAR TS UNIVARIADAS
library(imputeTS) # IMPUTAR TS UNIVARIADAS 
library(ggplot2) # GRAFICOS
library(tsbox)
library(gridExtra)
library(ggfortify)
library(scales)
library(writexl)
Sys.setlocale("LC_TIME", "es_ES.UTF-8")#### MESES EN ESPAÑOL 
####  LEER DATOS
precioshoja = read_excel("precios_drogas_2.xlsx",sheet = 1)%>%clean_names()
preciospasta = read_excel("precios_drogas_2.xlsx",sheet = 2)%>%clean_names()
preciosbase= read_excel("precios_drogas_2.xlsx",sheet = 3)%>%clean_names()
preciosclor = read_excel("precios_drogas_2.xlsx",sheet = 4)%>%clean_names()
#### VAR ESTADO ####
regiones <- c(
  `amazonia` = "Amazonía",
  `catatumbo` = "Catatumbo",
  `central` = "Central",
  `metaguav` = "Meta-Guaviare",
  `orinoquia` = "Orinoquía",
  `pacifico` = "Pacífico",
  `putca` = "Putumayo-Caquetá",
  `sierra` = "Sierra Nevada de Sta. Marta")
#### DIAGNOSTICO 
### HOJA
precioshoja = precioshoja%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2021-1-1"), by = "months"))%>%
  gather("region","preciohoja",-t)%>%
  mutate(mes = format(t,"%B"))
nahoja = precioshoja%>%
  filter(is.na(preciohoja))
diaghoja = ggplot()+
  geom_line(data = precioshoja, aes(x=t,y=preciohoja), col="#0CBAA6",size=0.8)+
  geom_point(data = precioshoja, aes(x=t,y=preciohoja), col="#0CBAA6",size=0.8)+
  geom_vline(data = filter(precioshoja, is.na(preciohoja)),aes(xintercept = t),colour = "#FF7F7F",size=1,alpha=0.5)+
  facet_wrap(~region,scales="free",labeller = as_labeller(regiones),ncol=2)+
  theme(axis.line=element_line()) + 
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
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
  theme(panel.grid.major = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))
diaghoja

### BASE
preciosbase = preciosbase%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2021-1-1"), by = "months"))%>%
  gather("region","preciobase",-t)%>%
  mutate(mes = format(t,"%B"))
nabase = preciosbase%>%
  filter(is.na(preciobase))
diagbase = ggplot()+
  geom_line(data = preciosbase, aes(x=t,y=preciobase), col="#0CBAA6",size=0.8)+
  geom_point(data = preciosbase, aes(x=t,y=preciobase), col="#0CBAA6",size=0.8)+
  geom_vline(data = filter(preciosbase, is.na(preciobase)),aes(xintercept = t),colour = "#FF7F7F",size=1,alpha=0.5)+
  facet_wrap(~region,scales="free",labeller = as_labeller(regiones),ncol=2)+
  theme(axis.line=element_line()) + 
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(labels = unit_format(scale = 1e-6,unit=""),
                     expand = c(0,0),
                     limits=c(0,6000000),
                     breaks = c(seq(1000000,6000000,1000000)))+
  labs(x="Mes/Año",
       y= "Precio Millones COP",
       title = "Diagnóstico Precios Base de Coca por Región - 2006-2020",
       subtitle = "Valores Faltantes Resaltados en Rojo")+
  theme(strip.text = element_text(face="bold",size=15))+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=15))+
  theme(strip.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.minor = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))+
  theme(panel.grid.major = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))
diagbase

### PASTA
preciospasta = preciospasta%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2021-1-1"), by = "months"))%>%
  gather("region","preciopasta",-t)%>%
  mutate(mes = format(t,"%B"))
napasta = preciospasta%>%
  filter(is.na(preciopasta))
diagpasta = ggplot()+
  geom_line(data = preciospasta, aes(x=t,y=preciopasta), col="#0CBAA6",size=0.8)+
  geom_point(data = preciospasta, aes(x=t,y=preciopasta), col="#0CBAA6",size=0.8)+
  geom_vline(data = filter(preciospasta, is.na(preciopasta)),aes(xintercept = t),colour = "#FF7F7F",size=1,alpha=0.5)+
  facet_wrap(~region,scales="free",labeller = as_labeller(regiones),ncol=2)+
  theme(axis.line=element_line()) + 
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(labels = unit_format(scale = 1e-6,unit=""),
                     expand = c(0,0),
                     limits=c(0,6000000),
                     breaks = c(seq(1000000,6000000,1000000)))+
  labs(x="Mes/Año",
       y= "Precio Millones COP",
       title = "Diagnóstico Precios Pasta Básica de Cocaína por Región - 2006-2020",
       subtitle = "Valores Faltantes Resaltados en Rojo")+
  theme(strip.text = element_text(face="bold",size=15))+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=15))+
  theme(strip.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.minor = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))+
  theme(panel.grid.major = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))
diagpasta



### CLORHIDRATO DE COCAINA
preciosclor = preciosclor%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2021-1-1"), by = "months"))%>%
  gather("region","precioclor",-t)%>%
  mutate(mes = format(t,"%B"))
naclor = preciosclor%>%
  filter(is.na(precioclor))
diagclor = ggplot()+
  geom_line(data = preciosclor, aes(x=t,y=precioclor), col="#0CBAA6",size=0.8)+
  geom_point(data = preciosclor, aes(x=t,y=precioclor), col="#0CBAA6",size=0.8)+
  geom_vline(data = filter(preciosclor, is.na(precioclor)),aes(xintercept = t),colour = "#FF7F7F",size=1,alpha=0.5)+
  facet_wrap(~region,scales="free",labeller = as_labeller(regiones),ncol=2)+
  theme(axis.line=element_line()) + 
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(labels = unit_format(scale = 1e-6,unit=""),
                     expand = c(0,0),
                     limits=c(2000000,7000000),
                     breaks = c(seq(2000000,7000000,1000000)))+
  labs(x="Mes/Año",
       y= "Precio Millones COP",
       title = "Diagnóstico Precios Clorhidrato de Cocaína por Región - 2006-2020",
       subtitle = "Valores Faltantes Resaltados en Rojo")+
  theme(strip.text = element_text(face="bold",size=15))+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=15))+
  theme(strip.background = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.grid.minor = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))+
  theme(panel.grid.major = element_line(colour="#E0E0E0", size=0.5,linetype = "dotted"))
diagclor
#### IMPUTAR AR(1) ERRORES GAUSSIANOS ####
#### LIMPIAR ####
gc()
rm(list=ls())
#### VAR ESTADO ####
regiones <- c(
  `amazonia` = "Amazonía",
  `catatumbo` = "Catatumbo",
  `central` = "Central",
  `metaguav` = "Meta-Guaviare",
  `orinoquia` = "Orinoquía",
  `pacifico` = "Pacífico",
  `putca` = "Putumayo-Caquetá",
  `sierra` = "Sierra Nevada de Sta. Marta")
####  LEER DATOS
precioshoja = read_excel("precios_drogas_2.xlsx",sheet = 1)%>%clean_names()
preciospasta = read_excel("precios_drogas_2.xlsx",sheet = 2)%>%clean_names()
preciosbase= read_excel("precios_drogas_2.xlsx",sheet = 3)%>%clean_names()
preciosclor = read_excel("precios_drogas_2.xlsx",sheet = 4)%>%clean_names()
#### IMPUTACION 1 ####
set.seed(16)
precioshojaimput = as.data.frame(impute_AR1_Gaussian(ts(precioshoja,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
summary(precioshojaimput)
preciospastaimput = as.data.frame(impute_AR1_Gaussian(ts(preciospasta,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
preciosbaseimput = as.data.frame(impute_AR1_Gaussian(ts(preciosbase,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
preciosclorimput = as.data.frame(impute_AR1_Gaussian(ts(preciosclor,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
summary(preciosclorimput)


imputacion1 = list("hoja"=precioshojaimput,"pasta"=preciospastaimput,"base"=preciosbaseimput,"clorhidrato"=preciosclorimput)
write_xlsx(imputacion1,"ar1_gaussiano.xlsx")

#### IMPUT 2
precioshojaimput = as.data.frame(na_seasplit(ts(precioshoja,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
preciosbaseimput = as.data.frame(na_seasplit(ts(preciosbase,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
preciospastaimput = as.data.frame(na_seasplit(ts(preciospasta,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
preciosclorimput = as.data.frame(na_seasplit(ts(preciosclor,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)

imputacion2 = list("hoja"=precioshojaimput,"pasta"=preciospastaimput,"base"=preciosbaseimput,"clorhidrato"=preciosclorimput)
write_xlsx(imputacion2,"na_seasplit.xlsx")








#### IMPUT 3
precioshojaimput = as.data.frame(na_kalman(ts(precioshoja,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
preciosbaseimput = as.data.frame(na_kalman(ts(preciosbase,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
preciospastaimput = as.data.frame(na_kalman(ts(preciospasta,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
preciosclorimput = as.data.frame(na_kalman(ts(preciosclor,frequency = 12)))%>%
  mutate(fecha = seq(as.Date("2006-01-01"),as.Date("2021-1-1"),by="months"),
         anno = format(fecha,"%Y"),
         mes = format(fecha, "%B"))%>%
  select(anno,mes,everything(),-fecha)
summary(preciosclorimput)

imputacion3 = list("hoja"=precioshojaimput,"pasta"=preciospastaimput,"base"=preciosbaseimput,"clorhidrato"=preciosclorimput)
write_xlsx(imputacion3,"na_kalman.xlsx")











##### A NA KALMAN SE LE PUEDEN METER TODOS LOS ARGUMENTOS DE UN AUTOARIMA

#### GRAFICO DE PRUEBA
imput = as.data.frame(na_seasplit(ts(precioshoja,frequency = 12),find_frequency = T))
imput = imput%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2021-1-1"), by = "months"))%>%
  gather("region","preciohoja",-t)
base = preciosclor%>%
  mutate(t = seq(as.Date("2006-1-1"), as.Date("2021-1-1"), by = "months"))%>%
  gather("region","preciohoja",-t)
ggplot()+
  geom_line(data = imput, aes(x=t,y=preciohoja), col="#0CBAA6",size=1.5)+
  geom_point(data = imput, aes(x=t,y=preciohoja, color="#FF7F7F"),size=0.75)+
  geom_point(data = base, aes(x=t,y=preciohoja, color="#0BF6F6"),size=0.75)+
  scale_colour_manual(name="Colores",
                      labels=c("Valores Conocidos", "Valores Imputados"),
                      values=c("#0BF6F6","#FF7F7F"))+
  facet_wrap(~region,scales="free",labeller = as_labeller(regiones),ncol=2)+
  theme(axis.line=element_line()) + 
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(labels = unit_format(scale = 1e-6,unit="",accuracy = 1),
                     expand = c(0,0),
                     limits=c(2000000,7000000),
                     breaks = c(seq(2000000,7000000,1000000)))+
  labs(x="Mes/Año",
       y= "Precio - Millones de COP",
       title = "Serie Mensual Precios Hoja de Coca por Región - 2006-2020",
       subtitle = "Imputación: Modelo AR (1) con Errores Gaussianos")+
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
