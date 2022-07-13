#### PATHROW 0461 - RIO PURE ####
gc()
rm(list=ls())
#### LIBRERIAS ####
library(foreign)
library(dplyr)
library(janitor)
library(sf)
library(data.table)
library(writexl)
setwd("~/Library/CloudStorage/OneDrive-UnitedNations/EVOA_AGUA_2022_CAMILO/Rio_Pure")
#### RUTAS ####
## BASE ##
r_lb = "Base/L7_0461_20011125_ID_univ.dbf"
## ACTUAL ##
r_ac = "L8_0461_20220111/L8_0461_20220111_ID_univ.dbf"
## NO BASE - NEAR ##
r_nbn = "0461_Puntos_noBase_near.dbf"
## UNIVERSO ##
r_un = "Universo.dbf"



#### BD ####
## BASE ##
lb = read.dbf(r_lb)%>%clean_names()
## ACTUAL ##
ac = read.dbf(r_ac)%>%clean_names()
## NO BASE - NEAR ##
nbn = read.dbf(r_nbn)%>%clean_names()
## UNIVERSO ##
un = read.dbf(r_un)%>%clean_names()

remove(r_lb,r_ac,r_nbn,r_un)



#### NBN PROMEDIOS ###
imput_nbn = nbn%>%
  group_by(id_univers)%>%
  summarise(grid_code = mean(grid_code,na.rm=T))

intersect(lb$id_univers,nbn$id_univers)



#### UNIR NBN Y LB ###
lb_union = lb%>%
  select(-pointid)%>%
  bind_rows(imput_nbn)


#### MINIMO LOCAL ####

lb_union = lb_union%>%
  left_join(un)


d = 33 ## VENTANA DE 33 METROS
setDT(lb_union)
start = Sys.time()
lb_union[,`:=`(xmin=point_x-d,
               xmax=point_x+d,
               ymin=point_y-d,
               ymax=point_y+d)]
#### INCLUYENTE ####
lb_minimo = lb_union[lb_union,on=.(point_x>=xmin,
                                   point_x<=xmax,
                                   point_y>=ymin,
                                   point_y<=ymax)][order(id_univers)][,.(rollmin=min(i.grid_code),grid_code_base = mean(grid_code)),by=.(id_univers)]

#### LINEA BASE MENOS ACTUAL
analisis = ac%>%
  select(id_univers,grid_code)%>%
  left_join(lb_minimo)%>%
  mutate(indice = rollmin-grid_code)

names(analisis) = toupper(names(analisis))


#### ESCRIBIR RESULTADOS ####
path_resultados = "/Users/camilo/Library/CloudStorage/OneDrive-UnitedNations/EVOA_AGUA_2022_CAMILO/Resultados/"


### CAMBIAR ###
pathrow = "0461"
rio = "Rio_Pure/"
foto = "L8_0461_20220111"
path_final = paste0(path_resultados,rio,pathrow,"/EVOA_",foto,".xlsx")





write_xlsx(analisis,path_final)












