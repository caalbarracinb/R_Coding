gc()
rm(list=ls())
library(foreign)
library(janitor)
library(dplyr)
library(readxl)
library(survey)
library(tidyr)

#### LEER BASES ####

hogar = read_excel("bases/clave1.xlsx")%>%clean_names()
personas = read_excel("bases/COMPOSICION_HOGAR cap 1.xlsx")%>%clean_names()

#### BAJO LOGRO EDUCATIVO ####
ble = personas%>%
  filter(p122>=15)%>%
  mutate(anosestud = case_when(p125==99|p125==1~0,
                               p125 >= 2 & p125<=4 & p125a!= 99 ~ p125a,
                               p125 == 2 & p125a== 99 ~ 5,
                               p125 == 3 & p125a== 99 ~ 9,
                               p125 == 4 & p125a== 99 ~ 11,
                               p125 == 5 ~ 12,
                               p125 == 6 ~ 14,
                               p125 == 7 ~ 16,
                               p125 == 8 ~ 18))%>%
  group_by(ident)%>%
  summarise(promestud = mean(anosestud,na.rm=T))%>%
  mutate(ible = ifelse(promestud>=9,0,1))
#### ANALFABETISMO ####
analf = personas%>%
  mutate(analf = ifelse(p125==99,1,0))%>%
  group_by(ident)%>%
  summarise(sumanalf = sum(analf,na.rm=T))%>%
  mutate(ianalf = ifelse(sumanalf>0,1,0))
#### INASISTENCIA ESCOLAR ####
inasistesc = personas%>%
  mutate(estudia = case_when(p122>5&p122<17&p124!=1~1,
                             TRUE ~ 0))%>%
  group_by(ident)%>%
  summarise(inasis = sum(estudia[p122>5&p122<17], na.rm = T))%>%
  mutate(iinasistesc = ifelse(inasis>0,1,0))
#### REZAGO ####
rezago = personas%>%
  mutate(anosestud = case_when(p125==99|p125==1~0,
                               p125 >= 2 & p125<=4 & p125a!= 99 ~ p125a,
                               p125 == 2 & p125a== 99 ~ 5,
                               p125 == 3 & p125a== 99 ~ 9,
                               p125 == 4 & p125a== 99 ~ 11,
                               p125 == 5 ~ 12,
                               p125 == 6 ~ 14,
                               p125 == 7 ~ 16,
                               p125 == 8 ~ 18))%>%
  mutate(rezago = case_when(p122>=7&anosestud<1~1,
                            p122>=8&anosestud<2~1,
                            p122>=9&anosestud<3~1,
                            p122>=10&anosestud<4~1,
                            p122>=11&anosestud<5~1,
                            p122>=12&anosestud<6~1,
                            p122>=13&anosestud<7~1,
                            p122>=14&anosestud<8~1,
                            p122>=15&anosestud<9~1,
                            p122>=16&anosestud<10~1,
                            p122>=17&anosestud<11~1,
                            TRUE~0))%>%
  group_by(ident)%>%
  summarise(sumrezago = sum(rezago[p122>=7&p122<=17],na.rm = T))%>%
  mutate(irezago = ifelse(sumrezago>0,1,0))
#### PRIMERA INFANCIA ####
priminf = personas%>%
  mutate(salud = ifelse(p127==1,1,0),
         educ = ifelse(p126==1|p126==2|126==4|126==5,0,1),
         priminf = salud*educ)%>%
  group_by(ident)%>%
  summarise(sumpriminf = sum(priminf[p122<=5],na.rm = T))%>%
  mutate(ipriminf = ifelse(sumpriminf>0,1,0))
#### TRABAJO INFANTIL ####
trabinf = personas%>%
  mutate(trabajo = ifelse(p123==1,1,0))%>%
  group_by(ident)%>%
  summarise(sumtrabajoinf = sum(trabajo[p122>=5&p122<=17],na.rm=T))%>%
  mutate(itrabinf = ifelse(sumtrabajoinf>0,1,0))
#### TDE ####
tde = personas%>%
  mutate(trabajo = ifelse(p123==1,1,0))%>%
  group_by(ident)%>%
  summarise(npersonas = n(),
            sumtrabajo = sum(trabajo,na.rm=T))%>%
  mutate(pppocu = npersonas/sumtrabajo,
         itde = ifelse(pppocu>=3,1,0))
#### EMPLEO INFORMAL ####
empinf = personas%>%
  mutate(trabajo = ifelse(p123==1,1,0),
         cotiza = ifelse(p129 == 2,1,0),
         xd = trabajo*cotiza)%>%
  group_by(ident)%>%
  summarise(sumempinf = sum(trabajo,na.rm=T))%>%
  mutate(iempinf = ifelse(sumempinf>0,1,0))
#### SIN SALUD ####
sinsalud = personas%>%
  mutate(salud = ifelse(p127!=1,1,0))%>%
  group_by(ident)%>%
  summarise(afilsalud = sum(salud[p122>5],na.rm = T))%>%
  mutate(isinsalud = ifelse(afilsalud>0,1,0))
#### BARRERAS SALUD ####
barrsal = personas%>%
  mutate(problema = ifelse(p130==1,1,0),
         quehizo = ifelse(p131==1|p131==2,0,1),
         barrera = problema*quehizo)%>%
  group_by(ident)%>%
  summarise(sumbarrera = sum(barrera,na.rm = T))%>%
  mutate(ibarrerasal = ifelse(sumbarrera>0,1,0))
#### SIN AGUA MEJORADA ####
sinagua = hogar%>%
  mutate(agua = ifelse(p117==4|p117==5|p117==6|p117==7|p117==8|p117==9|p117==10,1,0))%>%
  group_by(ident)%>%
  summarise(isinagua = ifelse(agua>0,1,0))
#### SIN EXCRETAS ####
sinpopo = hogar%>%
  mutate(popo = ifelse(p118==3|p118==4|p118==5|p118==6|is.na(p118),1,0))%>%
  group_by(ident)%>%
  summarise(isinpopo = ifelse(popo>0,1,0))
#### SIN PISOS ####
sinpiso = hogar%>%
  mutate(piso = ifelse(p112==7,1,0))%>%
  group_by(ident)%>%
  summarise(isinpiso = ifelse(piso>0,1,0))
#### SIN PAREDES ####
sinpared = hogar%>%
  mutate(pared = ifelse(p111==7|p111==8|p111==9|p111==10,1,0))%>%
  group_by(ident)%>%
  summarise(isinpared = ifelse(pared>0,1,0))
#### HACINAMIENTO ####
npersonas = personas%>%
  group_by(ident)%>%
  summarise(npersonas = n())
hacinamiento = hogar%>%
  left_join(npersonas)%>%
  group_by(ident)%>%
  summarise(hacinamiento = p117/npersonas)%>%
  mutate(ihacinamiento =ifelse(hacinamiento>3,1,0))

#### FEX ####
fex = hogar%>%
  select(ident,gri_1k_f,departamento,estrato_dis,factor)


#### UNIR TODO ####
total = fex%>%
  left_join(ble)%>%
  left_join(analf)%>%
  left_join(inasistesc)%>%
  left_join(rezago)%>%
  left_join(priminf)%>%
  left_join(trabinf)%>%
  left_join(tde)%>%
  left_join(empinf)%>%
  left_join(sinsalud)%>%
  left_join(barrsal)%>%
  left_join(sinagua)%>%
  left_join(sinpopo)%>%
  left_join(sinpiso)%>%
  left_join(sinpared)%>%
  left_join(hacinamiento)%>%
  select(-promestud,-sumanalf,-sumrezago,-sumpriminf,-sumtrabajoinf,-sumtrabajo,-pppocu,-sumempinf,-afilsalud,-sumbarrera,-hacinamiento,-inasis)

total = total%>%
  mutate(pm = 
           0.1*ible +
           0.1*ianalf + 
           0.05*iinasistesc + 
           0.05*irezago + 
           0.05*ipriminf + 
           0.05*itrabinf +
           0.1*itde+
           0.1*iempinf+
           0.1*isinsalud+
           0.1*ibarrerasal+
           0.04*isinagua+
           0.04*isinpopo+
           0.04*isinpiso+
           0.04*isinpared+
           0.04*ihacinamiento)%>%
  mutate(ipm = ifelse(pm>0.33333,1,0))

xd = total%>%
  select(-npersonas,-pm)%>%
  gather("indicador","privacion",-ident,-departamento,-estrato_dis,-factor,-gri_1k_f)


jeje = xd%>%
  group_by(departamento,estrato_dis,indicador)%>%
  summarise(hogarespriv = sum(factor*privacion,na.rm = T),
            nhogares = sum(factor,na.rm = T),
            pp = hogarespriv/nhogares)%>%
  bind_rows(xd%>%
          group_by(departamento,indicador)%>%
          summarise(estrato_dis = "Total",
                    hogarespriv = sum(factor*privacion,na.rm = T),
                    nhogares = sum(factor,na.rm = T),
                    pp = hogarespriv/nhogares))


proporcion = jeje%>%
  select(-hogarespriv,-nhogares)%>%
  spread(estrato_dis,pp)
writexl::write_xlsx(proporcion,"proporcion_ipm.xlsx")


dencuesta = svydesign(ids = ~gri_1k_f+ident,
                      weights = ~factor,
                      strata = ~estrato_dis,
                      nest=TRUE,
                      data = total)

dd = as.svrepdesign(dencuesta, type = "bootstrap")
cves = svyby(~ ible +
               ianalf + 
               iinasistesc + 
               irezago + 
               ipriminf + 
               itrabinf +
               itde+
               iempinf+
               isinsalud+
               ibarrerasal+
               isinagua+
               isinpopo+
               isinpiso+
               isinpared+
               ihacinamiento+
               ipm,by = ~estrato_dis+departamento,dencuesta,svymean,vartype = "cvpct")

cvest = svyby(~ ible +
               ianalf + 
               iinasistesc + 
               irezago + 
               ipriminf + 
               itrabinf +
               itde+
               iempinf+
               isinsalud+
               ibarrerasal+
               isinagua+
               isinpopo+
               isinpiso+
               isinpared+
               ihacinamiento+
                ipm,by = ~departamento,dencuesta,svymean,vartype = "cvpct")
cvesx = cves%>%
  select(departamento,estrato_dis,19:34)
cvestx = cvest%>%
  select(departamento,18:33)%>%
  mutate(estrato_dis = "Total")

cvestotal = rbind(cvesx,cvestx)%>%
  gather("indicador","cv",-departamento,-estrato_dis)%>%
  spread(estrato_dis,cv)

writexl::write_xlsx(cvestotal,"cves_ipm.xlsx")
