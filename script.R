#### LIMPIAR ####
gc()
rm(list=ls())
#### LIBRERIAS ####
library(readxl)
library(janitor)
library(tidyverse)
library(MatchIt)
library(ggplot2)
library(stargazer)
library(haven)
#### LEER BASE ####
base = read_xlsx("data/base_nueva.xlsx")%>%clean_names()
base = base%>%
  filter(cod_mpio!=94663, #### MAPIRIPANA, GUANIA NO ES UN MUNICIPIO - POR LO GENERAL LAS ENTIDADES NO GENERAN INFORMACION PARA ELLOS
         !cod_dpto %in% c(11,88,97)) #### DE ENTRADA BOGOTA, VAUPES Y SAN ANDRES NO TIENEN INFORMACION PARA LAS VARIABLES DEL PSM ENTONCES SE DESCARTAN TAMBIEN
base$did = as.factor(base$tratamiento*base$tiempo)
base$year_f = as.factor(base$ano)
base$tratamiento_f = factor(base$tratamiento,labels = c("Control","Tratamiento"))
base$post_f = factor(base$post_f)
base$tiempo_f = factor(base$tiempo, labels = c("Antes de Guajira Azul","Primer año", "Segundo año"))


base_psm = base%>%
  group_by(dpto)%>%
  mutate(precipitacion_d = mean(precipitacion, na.rm = T),
         temperatura_d = mean(temperatura, na.rm = T),
         humedad_d = mean(humedad, na.rm = T),
         brillo_solar_d = mean(brillo_solar, na.rm = T),
         evaporacion_d = mean(evaporacion, na.rm = T))%>%
  ungroup()
base_psm = base_psm%>%
  mutate(precipitacion = log(ifelse(is.na(precipitacion),precipitacion_d,precipitacion)),
         temperatura = ifelse(is.na(temperatura),temperatura_d,temperatura),
         humedad = ifelse(is.na(humedad),humedad_d,humedad),
         brillo_solar = ifelse(is.na(brillo_solar),brillo_solar_d,brillo_solar),
         evaporacion = ifelse(is.na(evaporacion),evaporacion_d,evaporacion))%>%
  filter(ano==2019)
m.logit = glm(tratamiento ~ humedad+temperatura+cob_ac_rur+cat_dnp, data = base_psm, family = "binomial")
base_psm$psm = m.logit$fitted.values


stargazer(m.logit,type = "html",out = "logit_psm.html")
ggplot(data = base_psm, aes(x = psm, fill =  tratamiento_f, color = tratamiento_f))+
  geom_density(aes(y=..scaled..),alpha = 0.5)+
labs(title = str_wrap("Histograma del PS para los grupos de tratamiento y control antes del emparejamiento",50),
       y = "Densidad",
       x = "Propensity Score - PS",
       fill = "Grupo",
       color = "Grupo")+
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman", size = 25))
set.seed(100)
m.out0 <- matchit(tratamiento ~ humedad+temperatura+cob_ac_rur+cat_dnp, data = base_psm,
                  method = "nearest",distance = "logit", ratio = 2)
summary(m.out0)
m.out.sum = summary(m.out0)
plot(m.out.sum,threshold = 0.5)

base_match = match.data(m.out0)

ggplot(data = base_match, aes(x = psm, fill =  tratamiento_f, color = tratamiento_f))+
  geom_density(aes(y=..scaled..),alpha = 0.5)+
  labs(title = str_wrap("Histograma del PS para los grupos de tratamiento y control después del emparejamiento",50),
       y = "Densidad",
       x = "Propensity Score - PS",
       fill = "Grupo",
       color = "Grupo")+
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman", size = 25))

lista_psm = base_match$cod_mpio




#### DIFERENCIAS EN DIFERENCIAS

vag = read_excel("data/vag_2014_2019.xlsx")
mdm = read_excel("data/mdm_2016_2019.xlsx")
irca = read_excel("data/irca_2010_2019.xlsx")
acceso_bpn = read_excel("data/acceso_bpn.xlsx")
bpn_atenciones = read_dta("data/base_modelo.dta")%>%
  select(cod_mpio=id,
         ano=year,
         bpn_total,
         atenciones,
         prop_pob_wayuu,
         pob_wayuu)

#### MODELO 1 - PANEL COMPLETO SIN CONTROLES ####

base_did = base%>%
  filter(cod_mpio %in% lista_psm)


base_did = base_did%>%
  left_join(irca)%>%
  left_join(vag)%>%
  left_join(mdm)%>%
  left_join(bpn_atenciones)%>%
  left_join(acceso_bpn)
base_did = base_did%>%
  mutate(vag_capita = vag*100000000/pob_tot)
base_did = base_did%>%
  mutate(irca_pen = ifelse(is.na(irca),100,irca),
         irca_mod = 100-irca_pen,
         pips = ifelse(is.na(pips),0,pips),
         pbpn = ifelse(is.na(pbpn),1,pbpn))


base_did = pdata.frame(base_did, index = c("cod_mpio","ano"))
modelo1 = plm(tmi ~ tratamiento*tt_2*sentencia+tratamiento*tt_1*sentencia+temperatura+precipitacion, model="within", effect = "twoways",data=base_did)
summary(modelo1)

fixef(modelo1)

resumen_did = base_did%>%
  group_by(tratamiento_f, ano)%>%
  summarise(tmi_media = mean(tmi,na.rm=T))

a = resumen_did[[2,3]]-resumen_did[[1,3]]
b = resumen_did[[3,3]]-resumen_did[[2,3]]
c = resumen_did[[4,3]]-resumen_did[[3,3]]

contrafactual = resumen_did%>%
  filter(tratamiento_f == "Tratamiento")%>%
  mutate(tratamiento_f = "Contrafactual")

contrafactual[2,3] = contrafactual[[1,3]]+a
contrafactual[3,3] = contrafactual[[2,3]]+b
contrafactual[4,3] = contrafactual[[3,3]]+c

did_total = resumen_did%>%
  bind_rows(contrafactual)
  
ggplot(data = did_total)+
  geom_line(size=2,aes(x = ano, y = tmi_media, group = tratamiento_f, color = tratamiento_f,linetype = tratamiento_f))+
  geom_vline(xintercept = 3,linetype = "12", color = "grey50",size=1) +
  annotate(geom = "label", x = 3, y = 50, 
           label = "Implementación Guajira Azul", size = 4)+
  geom_vline(xintercept = 2,linetype = "12", color = "grey50",size=1) +
  annotate(geom = "label", x = 2, y = 40, 
           label = "Sentencia T-302/17", size = 4)+
  labs(title = str_wrap("Evolución de la TMI para los grupos de tratamiento y control",40),
       y = "Tasa de Mortalidad Infantil",
       x = "Año",
       fill = "Grupo",
       color = "Grupo",
       linetype = "Grupo")+
  scale_linetype_manual(values=c("11","solid","solid"))+
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman", size = 25))+
  theme(legend.key.width=unit(1,"cm"))

#### MODELO 2 - PANEL COMPLETO CON CONTROLES
modelo2 = plm(tmi ~ tratamiento*tt_1*sentencia+tratamiento*tt_2*sentencia+log(vag_capita)+mdm+irca_pen+pbpn+pips, model="within", effect = "individual",data=base_did)
summary(modelo2)



#### MODELO 3 - PANEL DOS MOMENTOS CON 

base_did = base_did = base%>%
  filter(cod_mpio %in% lista_psm,
         ano>=2018)
base_did = base_did%>%
  left_join(irca)%>%
  left_join(vag)%>%
  left_join(mdm)%>%
  left_join(bpn_atenciones)%>%
  left_join(acceso_bpn)
base_did = base_did%>%
  mutate(vag_capita = vag*100000000/pob_tot)
base_did = base_did%>%
  mutate(irca_pen = ifelse(is.na(irca),100,irca),
         irca_mod = 100-irca_pen,
         pips = ifelse(is.na(pips),0,pips),
         pbpn = ifelse(is.na(pbpn),1,pbpn))
base_did = pdata.frame(base_did, index = c("cod_mpio","ano"))
modelo3 = plm(tmi ~ tratamiento*tt_2*sentencia+tratamiento*tt_1*sentencia, model="within", effect = "individual",data=base_did)
summary(modelo3)

resumen_did = base_did%>%
  group_by(tratamiento_f, ano)%>%
  summarise(tmi_media = mean(tmi,na.rm=T))

a = resumen_did[[2,3]]-resumen_did[[1,3]]


contrafactual = resumen_did%>%
  filter(tratamiento_f == "Tratamiento")%>%
  mutate(tratamiento_f = "Contrafactual")

contrafactual[2,3] = contrafactual[[1,3]]+a


did_total = resumen_did%>%
  bind_rows(contrafactual)

ggplot(data = did_total)+
  geom_line(size=2,aes(x = ano, y = tmi_media, group = tratamiento_f, color = tratamiento_f,linetype = tratamiento_f))+
  geom_vline(xintercept = 1.5,linetype = "12", color = "grey50",size=1) +
  annotate(geom = "label", x = 1.5, y = 50, 
           label = "Implementación Guajira Azul", size = 4)+
  labs(title = str_wrap("Evolución de la TMI para los grupos de tratamiento y control",40),
       y = "Tasa de Mortalidad Infantil",
       x = "Año",
       fill = "Grupo",
       color = "Grupo",
       linetype = "Grupo")+
  scale_linetype_manual(values=c("11","solid","solid"))+
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman", size = 25))+
  theme(legend.key.width=unit(1,"cm"))
### MODELO 4 ###
modelo4 = plm(tmi ~ tratamiento*tt_2*sentencia+log(vag_capita)+mdm+pbpn+pips, model="within", effect = "twoways",data=base_did)
summary(modelo4)



stargazer(modelo1,modelo2,modelo3,modelo4,type = "html",out = "modelosdid1806_v1.html")

