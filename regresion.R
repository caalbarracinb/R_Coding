#### LIMPIAR ####
gc()
rm(list=ls())
#### LIBRERIAS ####
library(dplyr)
library(foreign)
library(stats)
library(janitor)
library(readxl)
library(survey)
#### LEER MARCOS ####
marco.putca = read.dbf("marcos/marco_putca.dbf")%>%clean_names()

#### LEER IDENTIFICACION ####
id.putca = read_xlsx("bases_pyr/pyr_putca/simci1.xlsx")%>%clean_names()%>%
  select(ident,grilla=n_grilla,estrato)

#### LEER RENDIMIENTO INFORMADO PYR ####
bd.inf.putca = read_xlsx("bases_pyr/pyr_putca/simci7.xlsx")%>%clean_names()

#### LEER FACTORES DE EXPANSION PYR ####
factor.putca = read_xlsx("bases_pyr/pyr_putca/factor.xlsx")%>%clean_names()%>%
  rename(grilla=n_grilla)


#### LEER RENDIMIENTO PRUEBA COSECHA PYR ####
bd.pc.putca

#### PUTUMAYO-CAQUETA ####
marco.putca = marco.putca%>%
  filter(cc_20!=0)
bd.inf.putca = bd.inf.putca%>%
  left_join(id.putca)%>%
  left_join(factor.putca)
bd.inf.putca = bd.inf.putca%>%
  mutate(cantidad.kg = cantidad*12.5/1000)
### AGREGAR POR UPAC ###
bd.inf.putca = bd.inf.putca%>%
  group_by(ident,finca,lote)%>%
  summarise(cantidad.kg = sum(cantidad.kg, na.rm = T),
            area_edad_productiva = max(area_edad_productiva, na.rm=T),
            area_plantada = max(area_plantada, na.rm = T),
            fex = mean(factor, na.rm=T),
            grilla = mean(grilla,na.rm=T),
            estrato = max(estrato,na.rm=T))
dis.putca = svydesign(ids = ~grilla+ident,
                      strata = ~estrato,
                      weights = ~fex,
                      data = bd.inf.putca,
                      nest = T)
svytotal(~area_edad_productiva,dis.putca)
svyratio(numerator = ~cantidad.kg,denominator = ~area_edad_productiva,dis.putca)

marco.putca = marco.putca%>%
  mutate(cluster = cluster.putca$cluster)
marco.putca%>%
  group_by(cluster)%>%
  summarise(min(altura),
            max(altura),
            n())%>%
  arrange(`min(altura)`)

kmedias = kmeans(marco.putca$)
