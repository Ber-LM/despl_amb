#############################
#### Trabajo final MC II ####

## Estudiante: Bernardo L. Mc Kelligan
## Profesor: Dr. Máximo Jaramillo-Molina

rm(list=ls())
options(scipen=999)

library (stringr)
library (dplyr)
library (Hmisc)
library (tidyverse)
library (ggplot2)
library (scales)
library (ggrepel)
library (ggthemes)
library (mosaic)
library (readxl)
library (stringi)
library (RColorBrewer)
library (viridis)
library (broom)
library (rgdal)
library (texreg)
library (rgeos)
library (RJSONIO)
library (sp)
library (sf)
library (rgdal)
library ("mxmaps")
library (flextable)

setwd("C:/Users/Admin/Documents/Inputs")

## Primera sección: desplazamiento ambiental

## Limpieza y configuración de datos ##
## Entorno urbano
  ## Diccionario de datos: https://www.inegi.org.mx/rnm/index.php/catalog/331/data-dictionary/F6?file_name=Localidades%202014
entorn_raw <- read.csv("resloc_naccsv14.csv")

glimpse(entorn_raw)
entorn_prob <- entorn_raw %>% 
  select(CVEGEO, ENT_NOM, 
         MUN_NOM, LOC_NOM, 
         TIPOLOC, DRENAJECOB, 
         ACTMIN, PROBLEMA)

table(entorn_prob$PROBLEMA)
t1_raw <- prop.table(table(entorn_prob$PROBLEMA))
t1 <- as.data.frame(t1)

t2_raw <- cbind(t1,t1_raw)
t2_raw <- t2_raw[,3:4]

t2_raw <- as.data.frame(t2_raw)

t2 <- t2_raw  %>% 
  mutate(porcent = (Freq*100))

# Saqué código de aquí: https://www.data-to-viz.com/caveat/pie.html
tiff("lollipop.tiff", units="in", width=12, height=7, res=300)
ggplot(t2,aes(x=Var1,
              y=porcent)) + 
  geom_segment( aes(x=Var1 ,xend=Var1, y=0, yend=porcent), color="grey") +
  geom_point(size=4, color="royalblue") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  labs(y="Porcentaje del total",
       x ="",
       title = "Frecuencia proporcional para cada problema principal
identificado por localidad (INEGI, 2014)",
       caption = "Elaboración: Bernardo L. Mc Kelligan")
dev.off()

## Rezago Social
IRS_raw <- read.csv("IRS_localidades_2020.csv")
IRS <- IRS_raw %>% 
  rename(ENT_NOM = Entidad.federativa,
         MUN_NOM = Municipio,
         LOC_NOM = Localidad)

IRS_entorn <- IRS %>% 
  inner_join(entorn_prob, by = c("ENT_NOM", "MUN_NOM", "LOC_NOM"))

IRS_entorn %>% 
  group_by(Grado.de.rezago.social) %>% 
  summarise(Total = n(),
            Prop = Total/110220)

IRS_entorn %>% 
  filter(PROBLEMA == "Problemas ambientales") %>% 
  group_by(Grado.de.rezago.social) %>% 
  summarise(Total = n()) %>% 
  mutate(Prop = Total/sum(.$Total))

IRS_entorn %>% 
  filter(PROBLEMA == "Carencia de agua y mala calidad") %>% 
  group_by(Grado.de.rezago.social) %>% 
  summarise(Total = n())%>% 
  mutate(Prop = Total/sum(.$Total))

IRS_entorn %>% 
  filter(ACTMIN == "Con actividad") %>% 
  group_by(Grado.de.rezago.social) %>% 
  summarise(Total = n())%>% 
  mutate(Prop = Total/sum(.$Total))

IRS_entorn %>%
  group_by(ACTMIN) %>% 
  summarise(Total = n())%>% 
  mutate(Prop = Total/sum(.$Total))

IRS_entorn %>%
  filter(PROBLEMA == "Problemas ambientales") %>% 
  group_by(ACTMIN) %>% 
  summarise(Total = n())%>% 
  mutate(Prop = Total/sum(.$Total))

IRS_entorn %>%
  filter(PROBLEMA == "Carencia de agua y mala calidad") %>% 
  group_by(ACTMIN) %>% 
  summarise(Total = n())%>% 
  mutate(Prop = Total/sum(.$Total))

IRS_entorn %>% 
  filter(ACTMIN == "Con actividad") %>% 
  group_by(PROBLEMA) %>% 
  summarise(Total = n())

## ¿Ser una localidad con actividad minera aumenta o reduce los momios
## de también ser una localidad con carencia de agua?

c_agua <- IRS_entorn %>% 
  mutate(caren_agua = 
           ifelse(PROBLEMA == "Carencia de agua y mala calidad",1,0)) %>% 
  mutate(ACTMIN_NUM = ifelse(ACTMIN == "Con actividad",1,0))


m1 <- glm(caren_agua ~ ACTMIN_NUM, 
    family = binomial("logit"), 
    data = c_agua, na.action=na.exclude)

exp(coef(m1))

filter(IRS_entorn, LOC_NOM == "Carrizalillo")
filter(IRS_entorn, MUN_NOM == "Cananea" & ACTMIN == "Con actividad")

table(IRS_entorn$ACTMIN)

## Las localidades con violencia o 'problemas ambientales' tienen un aumento
## o reducción en los momios de tener menor población, entre 2010 y 2020

filter(IRS_raw, Municipio == "Aguascalientes" & Localidad == "Granja Adelita")

c_prob_amb <- entorn_prob %>% 
  filter(PROBLEMA == "Problemas ambientales")

View(c_prob_amb[sample(nrow(c_prob_amb), 30), ]) # Checar la historia de los lugares
                                                 # con 'problemas ambientales'

entorn_prob %>% 
  filter(MUN_NOM == "Jáltipan" & PROBLEMA == "Problemas ambientales")

entorn_prob %>% 
  filter(MUN_NOM == "Jáltipan") %>% 
  group_by(PROBLEMA) %>% 
  summarise(freq = n())

## Descargar los datos poblacionales
inegi_2010_raw <- read.csv("censo_inegi_2010.csv")
inegi_2020_raw <- read.csv("censo_inegi_2020.csv")

inegi_2010 <- inegi_2010_raw %>% 
  select(1:10) %>% 
  rename(ENT_NOM = nom_ent,
         MUN_NOM = nom_mun,
         LOC_NOM = nom_loc,
         ENT = entidad,
         MUN = mun,
         LOC = loc,
         POBTOT_2010 = pobtot) %>% 
  select(-longitud, -latitud, -altitud)

inegi_2020 <- inegi_2020_raw %>% 
  select(1:10) %>% 
  rename(ENT_NOM = NOM_ENT,
         MUN_NOM = NOM_MUN,
         LOC_NOM = NOM_LOC,
         ENT = ENTIDAD,
         POBTOT_2020 = POBTOT) %>% 
  select(-LONGITUD, -ALTITUD, -LATITUD)

inegis <- inegi_2020 %>% 
  inner_join(inegi_2010, by = c("ENT", "MUN", "LOC"))

entorn_prob_ex <- entorn_raw %>% 
  select(ENT, MUN, LOC, TIPOLOC, PROBLEMA)

filter(entorn_prob_ex, LOC_NOM == "La Chiripa")

prob_y_pobs <- entorn_prob_ex  %>% 
  inner_join(inegis, by = c("ENT", "MUN", "LOC")) %>% 
  mutate(pob_red = ifelse(POBTOT_2010 > POBTOT_2020, 1, 0)) %>% 
  mutate(prob_amb = ifelse(PROBLEMA == "Problemas ambientales", 1, 0)) %>% 
  mutate(prob_ins = 
           ifelse(PROBLEMA == "Inseguridad delincuencia y adicciones", 1, 0)) %>% 
  mutate(prob_fenom_plag = 
           ifelse(PROBLEMA == "Afectaciones por fenomenos naturales y plagas",
                  1, 0)) %>% 
  mutate(prob_agua = 
           ifelse(PROBLEMA == "Carencia de agua y mala calidad", 1, 0)) %>% 
  mutate(prob_desempleo = 
           ifelse(PROBLEMA == "Desempleo, empleo deficiente", 1, 0))

prob_y_pobs %>%
  filter(MUN_NOM.x == "Cuernavaca")

m2 <- glm(pob_red ~ prob_amb, family = binomial("logit"), 
    data = prob_y_pobs, na.action=na.exclude)

summary(m2)
exp(coef(m2))

m3 <- glm(pob_red ~ prob_ins, family = binomial("logit"), 
          data = prob_y_pobs, na.action=na.exclude)

summary(m3)
exp(coef(m3))

m4 <- glm(pob_red ~ prob_desempleo, family = binomial("logit"), 
          data = prob_y_pobs, na.action=na.exclude)

summary(m4)
exp(coef(m4)) ## Positivo!!

m5 <- glm(pob_red ~ prob_agua, family = binomial("logit"), 
          data = prob_y_pobs, na.action=na.exclude)

summary(m5)
exp(coef(m5))

m6 <- glm(pob_red ~ prob_fenom_plag, family = binomial("logit"), 
          data = prob_y_pobs, na.action=na.exclude)

summary(m6)
exp(coef(m6)) ## Positivo!!

prob_y_pobs$PROBLEMA <- as.factor(prob_y_pobs$PROBLEMA)
prob_y_pobs <- within(prob_y_pobs, PROBLEMA <- relevel(PROBLEMA, ref = 22))

m6_01 <- glm(pob_red ~ PROBLEMA, family = binomial("logit"), 
          data = prob_y_pobs, na.action=na.exclude)

summary(m6_01)
exp(coef(m6_01))

as_flextable(m6_01)

prob_y_pobs_rur <- prob_y_pobs %>% 
  filter(TIPOLOC == "Rancho o finca" |
           TIPOLOC == "Caserío" |
           TIPOLOC == "Poblado tradicional")

prob_y_pobs_rur <- prob_y_pobs %>% 
  filter(TIPOLOC == "Rancho o finca")

prob_y_pobs_rur <- prob_y_pobs %>% 
  filter(TIPOLOC == "Caserío")

prob_y_pobs_rur <- prob_y_pobs %>% 
  filter(TIPOLOC == "Poblado tradicional")

m7 <- glm(pob_red ~ prob_amb, family = binomial("logit"), 
          data = prob_y_pobs_rur, na.action=na.exclude)

summary(m7)
exp(coef(m7))

m8 <- glm(pob_red ~ prob_ins, family = binomial("logit"), 
          data = prob_y_pobs_rur, na.action=na.exclude)

summary(m8)
exp(coef(m8))

## Incidencia del alto rezago social y fenómenos naturales en el desplazamiento
# Primero hay que hacer las variables ENT, MUN y LOC

IRS$Grado.de.rezago.social <- factor(IRS$Grado.de.rezago.social,
                                    levels=c("Muy bajo", 
                                             "Bajo",
                                             "Medio",
                                             "Alto",
                                             "Muy alto"))

margin_2010_raw <- read.csv("margin_conapo_2010.csv")

margin_2010_raw$GM_2010 <- factor(margin_2010_raw$GM_2010,
                                  levels= c("Muy bajo", 
                                            "Bajo",
                                            "Medio",
                                            "Alto",
                                            "Muy alto"))

margin_2010 <- margin_2010_raw %>% 
  select(ENT, NOM_ENT, MUN, NOM_MUN, LOC, NOM_LOC, GM_2010)

table(margin_2010$GM_2010)
t3_raw <- prop.table(table(margin_2010$GM_2010))
t3 <- as.data.frame(t3_raw)

t4_raw <- cbind(t3,t3_raw)
t4_raw <- t4_raw[,3:4]

t4_raw <- as.data.frame(t4_raw)

t4 <- t4_raw  %>% 
  mutate(porcent = (Freq*100))

## Lollipop marginación 2010
tiff("lolli_margin_2010.tiff", units="in", width=12, height=7, res=300)
ggplot(t4,aes(x=Var1,
              y=porcent)) + 
  geom_segment( aes(x=Var1 ,xend=Var1, y=0, yend=porcent), color="grey") +
  geom_point(size=4, color="purple") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  labs(y="Porcentaje del total",
       x ="",
       title = "Frecuencia proporcional para cada grado de marginación
identificado por localidad (CONAPO, 2010)",
       caption = "Elaboración: Bernardo L. Mc Kelligan")
dev.off()

## Aquí vienen las regresiones finales
prob_pobs_gm <- margin_2010 %>% 
  inner_join(prob_y_pobs, by = c("ENT", "MUN", "LOC"))

prob_pobs_gm$PROBLEMA <- as.factor(prob_pobs_gm$PROBLEMA)
prob_pobs_gm <- within(prob_pobs_gm, PROBLEMA <- relevel(PROBLEMA, ref = 22))

prob_pobs_rs <- IRS %>%
  mutate(Clave.localidad = as.character(Clave.localidad)) %>% 
  mutate(CVEGEO = str_pad(Clave.localidad, 9, pad = "0")) %>% 
  mutate(ENT = as.numeric(substring(CVEGEO,1,2)),
         MUN = as.numeric(substring(CVEGEO,3,5)),
         LOC = as.numeric(substring(CVEGEO,6,9))) %>% 
  select(-Clave.localidad, -Población.total, -Índice.de.rezago.social) %>% 
  inner_join(prob_y_pobs, by = c("ENT", "MUN", "LOC")) %>% 
  select(-ENT_NOM.x,
         -MUN_NOM.x,
         -LOC_NOM.x,
            -ENT_NOM.y,
            -MUN_NOM.y,
            -LOC_NOM.y) %>% 
  rename(grado_rs = Grado.de.rezago.social)

m9 <- glm(pob_red ~ prob_fenom_plag + grado_rs, family = binomial("logit"), 
          data = prob_pobs_rs, na.action=na.exclude)

summary(m9)
exp(coef(m9)) # Positivo!! Sí hay incremento en la probabilidad (sic) de que
              # se reduzca la población después de un fenómeno natural
              # o una plaga y también sí hay un mayor rezago social, ahora
              # quiero analizar su efecto si están combinados los valores

prob_pobs_rs_comb <- prob_pobs_rs %>% 
  mutate(fenom_y_rs = ifelse(prob_fenom_plag == 1 & 
                               grado_rs == "Muy alto",1,0))


m10 <- glm(pob_red ~ fenom_y_rs, family = binomial("logit"), 
          data = prob_pobs_rs_comb, na.action=na.exclude)

summary(m10)
exp(coef(m10)) # Aumenta en un 55% los momios de ser una localidad con menor
               # población en 2020 con respecto a 2010 una localidad con igual
               # o mayor población total en 2020 con respecto a 2010!

# Veamos la multicolinealidad de GM_2010 vs prob_fenom_plag
multi_test <- prob_pobs_gm %>% 
  mutate(GM_2010 = case_when(
    GM_2010 == "Muy bajo" ~ 1,
    GM_2010 == "Bajo" ~ 2,
    GM_2010 == "Medio" ~ 3,
    GM_2010 == "Alto" ~ 4,
    GM_2010 == "Muy alto" ~ 5
  ))

cor(multi_test$GM_2010, 
    multi_test$prob_fenom_plag, 
    method = "spearman") # -0.02340319 spearman

m11 <- glm(pob_red ~ prob_fenom_plag + GM_2010, family = binomial("logit"), 
           data = prob_pobs_gm, na.action=na.exclude)

summary(m11)
exp(coef(m11))

as_flextable(m11)

## Con variable de control TIPOLOC
prob_pobs_gm$TIPOLOC <- factor(prob_pobs_gm$TIPOLOC)

m11_1 <- glm(pob_red ~ prob_fenom_plag + GM_2010 + TIPOLOC,
             family = binomial("logit"), 
           data = prob_pobs_gm, na.action=na.exclude)

summary(m11_1)
exp(coef(m11_1))

as_flextable(m11_1)

## Regresión de la presentación
prob_pobs_gm_comb <- prob_pobs_gm %>% 
  mutate(fenom_y_gm = ifelse(prob_fenom_plag == 1 & 
                               GM_2010 == "Muy alto",1,0))

m12 <- glm(pob_red ~ fenom_y_gm, family = binomial("logit"), 
           data = prob_pobs_gm_comb, na.action=na.exclude)

summary(m12)
exp(coef(m12)) ## Con los datos de CONAPO sube hasta 80%!

options(scipen=999)
as_flextable(m12)

## Controlado por tamaño de localidad
m12_1 <- glm(pob_red ~ fenom_y_gm + TIPOLOC, family = binomial("logit"), 
           data = prob_pobs_gm_comb, na.action=na.exclude)

summary(m12_1)
exp(coef(m12_1))
as_flextable(m12_1)

m13 <- glm(pob_red ~ fenom_y_gm + prob_desempleo, family = binomial("logit"), 
           data = prob_pobs_gm_comb, na.action=na.exclude)

summary(m13)
exp(coef(m13))
as_flextable(m13)

m14 <- glm(pob_red ~ prob_fenom_plag + prob_desempleo + prob_agua + prob_ins, 
           family = binomial("logit"), 
           data = prob_pobs_gm, na.action=na.exclude)
exp(coef(m14))
as_flextable(m14)

## Probable regresión final (update: nel)
prob_pobs_gm_comb_02 <- prob_pobs_gm %>% 
  mutate(fenom_y_gm_ma = ifelse(prob_fenom_plag == 1 & 
                               GM_2010 == "Muy alto",1,0),
         fenom_y_gm_a = ifelse(prob_fenom_plag == 1 & 
                                  GM_2010 == "Alto",1,0),
         fenom_y_gm_m = ifelse(prob_fenom_plag == 1 & 
                                  GM_2010 == "Medio",1,0),
         fenom_y_gm_b = ifelse(prob_fenom_plag == 1 & 
                                  GM_2010 == "Bajo",1,0),
         fenom_y_gm_mb = ifelse(prob_fenom_plag == 1 & 
                                  GM_2010 == "Muy bajo",1,0))

m15 <- glm(pob_red ~ fenom_y_gm_ma + fenom_y_gm_a + fenom_y_gm_m +
             fenom_y_gm_b + fenom_y_gm_mb + TIPOLOC, data = prob_pobs_gm_comb_02,
           family = binomial("logit"), na.action = na.exclude)

summary(m15)
exp(coef(m15))

m16 <- glm(pob_red ~ PROBLEMA, data = prob_pobs_gm,
           family = binomial("logit"), na.action = na.exclude)

summary(m16)
exp(coef(m16))

m17 <- glm(pob_red ~ PROBLEMA + GM_2010, data = prob_pobs_gm,
           family = binomial("logit"), na.action = na.exclude)

summary(m17)
exp(coef(m17))

# Vamos a ver cómo afecta no tener problemas
prob_pobs_gm_sp <- prob_pobs_gm %>% 
  mutate(sin_prob = 
           ifelse(PROBLEMA == "Sin problemas", 1, 0))

m18 <- glm(pob_red ~ sin_prob, data = prob_pobs_gm_sp,
           family = binomial("logit"), na.action = na.exclude)

summary(m18)
exp(coef(m18))

test <- prob_pobs_gm_sp %>% 
  filter(PROBLEMA == "Sin problemas")

table(test$pob_red) #wha... hay más localidades con población reducida

m19 <- glm(pob_red ~ prob_fenom_plag, data = prob_pobs_gm_sp,
           family = binomial("logit"), na.action = na.exclude)

summary(m19)
exp(coef(m19))

## Segunda sección: cambio climático

## Gráfica de violín grados de marginación y distribución por tipo de exposición
# RCP 8.5 en los datos del atlas
setwd("C:/Users/Admin/Documents/Inputs")
inun_mun_raw <- read.csv("InunPob_E_raw.csv")
desl_mun_raw <- read.csv("DesPob_E_raw.csv")

inun_mun <- inun_mun_raw %>% 
  rename(NOM_ENT = Nombre.Entidad,
         NOM_MUN = Nombre.Municipio,
         CNRMC.M5_inun = CNRMC.M5,
         CVE_MUN = Clave.Municipio) %>% 
  select(CVE_MUN, CNRMC.M5_inun)

desl_mun <- desl_mun_raw %>% 
  rename(NOM_ENT = Nombre.Entidad,
         NOM_MUN = Nombre.Municipio,
         CNRMC.M5_desl = CNRMC.M5,
         CVE_MUN = Clave.Municipio) %>% 
  select(CVE_MUN, CNRMC.M5_desl)

margin_2020_raw <- read.csv("margin_conapo_2020.csv")

margin_2020 <- margin_2020_raw %>% 
  select(NOM_ENT, NOM_MUN,CVE_MUN, GM_2020)

margin_2020$GM_2020 <- factor(margin_2020$GM_2020, levels=c("Muy bajo",
                                                            "Bajo",
                                                            "Medio",
                                                            "Alto",
                                                            "Muy alto"))

glimpse(inun_mun)

## Limpiamos bases
inun_mun$NOM_ENT <- toupper(stri_trans_general(
  inun_mun$NOM_ENT,
  "Latin-ASCII"))

inun_mun$NOM_MUN <- toupper(stri_trans_general(
  inun_mun$NOM_MUN,
  "Latin-ASCII"))

desl_mun$NOM_ENT <- toupper(stri_trans_general(
  desl_mun$NOM_ENT,
  "Latin-ASCII"))

desl_mun$NOM_MUN <- toupper(stri_trans_general(
  desl_mun$NOM_MUN,
  "Latin-ASCII"))

margin_2020$NOM_ENT <- toupper(stri_trans_general(
  margin_2020$NOM_ENT,
  "Latin-ASCII"))

margin_2020$NOM_MUN <- toupper(stri_trans_general(
  margin_2020$NOM_MUN,
  "Latin-ASCII"))

table(margin_2020$NOM_ENT)
table(inun_mun$NOM_ENT)
table(desl_mun$NOM_ENT)

margin_2020$NOM_ENT[margin_2020$NOM_ENT == "VERACRUZ DE IGNACIO DE LA LLAVE"] <- "VERACRUZ"
margin_2020$NOM_ENT[margin_2020$NOM_ENT == "MICHOACAN DE OCAMPO"] <- "MICHOACAN"
margin_2020$NOM_ENT[margin_2020$NOM_ENT == "COAHUILA DE ZARAGOZA"] <- "COAHUILA"
margin_2020$NOM_ENT[margin_2020$NOM_ENT == "DISTRITO FEDERAL"] <- "CIUDAD DE MEXICO"

inun_mun$NOM_ENT[inun_mun$NOM_ENT == "VERACRUZ DE IGNACIO DE LA LLAVE"] <- "VERACRUZ"
inun_mun$NOM_ENT[inun_mun$NOM_ENT == "MICHOACAN DE OCAMPO"] <- "MICHOACAN"
inun_mun$NOM_ENT[inun_mun$NOM_ENT == "COAHUILA DE ZARAGOZA"] <- "COAHUILA"
inun_mun$NOM_ENT[inun_mun$NOM_ENT == "DISTRITO FEDERAL"] <- "CIUDAD DE MEXICO"

desl_mun$NOM_ENT[desl_mun$NOM_ENT == "VERACRUZ DE IGNACIO DE LA LLAVE"] <- "VERACRUZ"
desl_mun$NOM_ENT[desl_mun$NOM_ENT == "MICHOACAN DE OCAMPO"] <- "MICHOACAN"
desl_mun$NOM_ENT[desl_mun$NOM_ENT == "COAHUILA DE ZARAGOZA"] <- "COAHUILA"
desl_mun$NOM_ENT[desl_mun$NOM_ENT == "DISTRITO FEDERAL"] <- "CIUDAD DE MEXICO"

desl_mun$NOM_MUN <- str_trim(desl_mun$NOM_MUN, side = "right")
inun_mun$NOM_MUN <- str_trim(inun_mun$NOM_MUN, side = "right")
margin_2020$NOM_MUN <- str_trim(margin_2020$NOM_MUN, side = "right")

desl_mun$NOM_MUN <- str_trim(desl_mun$NOM_MUN, side = "left")
inun_mun$NOM_MUN <- str_trim(inun_mun$NOM_MUN, side = "left")
margin_2020$NOM_MUN <- str_trim(margin_2020$NOM_MUN, side = "left")

# Unimos bases
violin_raw <- margin_2020 %>% 
  inner_join(inun_mun, by = "CVE_MUN") %>% 
  inner_join(desl_mun, by = "CVE_MUN")

glimpse(violin_raw)
violin_raw[is.na(violin_raw)] <- -9999.000000

violin_inun <- violin_raw %>% 
  mutate(CNRMC.M5_inun = ifelse(CNRMC.M5_inun == -9999.000000, NA,CNRMC.M5_inun)) %>% 
  drop_na()

violin_desl <- violin_raw %>% 
  mutate(CNRMC.M5_desl = ifelse(CNRMC.M5_desl == -9999.000000, 
                                NA, CNRMC.M5_desl)) %>% 
  drop_na()

violin_desl %>% 
  summarise(media = mean(CNRMC.M5_desl))

violin_inun %>% 
  summarise(media = mean(CNRMC.M5_inun))

violin_desl %>% 
  filter(GM_2020 == "Medio" | 
           GM_2020 == "Alto" |
           GM_2020 == "Muy alto") %>% 
  filter(CNRMC.M5_desl >= 0.5) %>% 
  mutate(CVE_MUN = as.character(CVE_MUN)) %>% 
  mutate(CVE_MUN_02 = str_pad(CVE_MUN, 5, pad = "0")) %>% 
  mutate(ENT = as.numeric(substring(CVE_MUN_02,1,2)),
         MUN = as.numeric(substring(CVE_MUN_02,3,5))) %>% 
  inner_join(inegi_2020, by = c("ENT", "MUN")) %>% 
  summarise(POB = sum(POBTOT_2020)) ## 27,641,502

violin_desl %>% 
  filter(GM_2020 == "Muy alto") %>% 
  filter(CNRMC.M5_desl >= 0.5) %>% 
  mutate(CVE_MUN = as.character(CVE_MUN)) %>% 
  mutate(CVE_MUN_02 = str_pad(CVE_MUN, 5, pad = "0")) %>% 
  mutate(ENT = as.numeric(substring(CVE_MUN_02,1,2)),
         MUN = as.numeric(substring(CVE_MUN_02,3,5))) %>% 
  inner_join(inegi_2020, by = c("ENT", "MUN")) %>% 
  summarise(POB = sum(POBTOT_2020)) ## 4,200,249

violin_inun %>% 
  filter(GM_2020 == "Muy alto") %>% 
  filter(CNRMC.M5_desl >= 0.5) %>% 
  mutate(CVE_MUN = as.character(CVE_MUN)) %>% 
  mutate(CVE_MUN_02 = str_pad(CVE_MUN, 5, pad = "0")) %>% 
  mutate(ENT = as.numeric(substring(CVE_MUN_02,1,2)),
         MUN = as.numeric(substring(CVE_MUN_02,3,5))) %>% 
  inner_join(inegi_2020, by = c("ENT", "MUN")) %>% 
  summarise(POB = sum(POBTOT_2020)) ## 1,587,857



## Graficamos juju
# install.packages("hrbrthemes")
library(hrbrthemes)
setwd("C:/Users/Admin/Documents/Colmex/Tercer semestre/Métodos Cuantitativos II/Trabajo_final")

## Inundaciones
tiff("test.tiff", units="in", width=12, height=7, res=300)
ggplot(violin_inun, aes(x=CNRMC.M5_inun, y=GM_2020, fill = GM_2020)) +
  geom_violin(width=0.75, size=0.2) +
  scale_fill_manual(values = c("#fcd74e",
                               "#ffb14e",
                               "#fa8775",
                               "#d17393",
                               "#bb4da7"))+
  geom_vline(xintercept = 0.4917726, 
             linetype="twodash",
             color = "red",
             size = 1.75)+
  theme_ipsum()+
  theme(legend.position="none") +
  coord_flip()+
  labs(y="Grado de marginación (CONAPO, 2020)",
       x="Riesgo de exposición a inundaciones (INECC, 2019)",
       title="Distribución del riesgo de exposición a inundaciones por cambio climático
a nivel municipal segmentado por grado de marginación (México)",
       caption="Elaboración: Bernardo L. Mc Kelligan")
  dev.off()

## Deslaves
tiff("desl_margin.tiff", units="in", width=12, height=7, res=300)
ggplot(violin_desl, aes(x=CNRMC.M5_desl, y=GM_2020, 
                        fill=GM_2020)) +
  geom_violin(width=0.75, size=0.2) +
  scale_fill_manual(values = c("#fcd74e",
                               "#ffb14e",
                               "#fa8775",
                               "#d17393",
                               "#bb4da7"))+
  geom_vline(xintercept = 0.5902713, 
             linetype="twodash",
             color = "red",
             size = 1.65)+
  theme_ipsum() +
  theme(legend.position="none") +
  coord_flip()+
  labs(y="Grado de marginación (CONAPO, 2020)",
       x="Riesgo de exposición a deslaves (INECC, 2019)",
       title="Distribución del riesgo de exposición a deslaves por cambio climático
a nivel municipal segmentado por grado de marginación (México)",
       caption="Elaboración: Bernardo L. Mc Kelligan")
dev.off()


# Half-violin: https://ourcodingclub.github.io/tutorials/dataviz-beautification-synthesis/
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

## Violin_puntos_box_desl
tiff("desl_margin_dis.tiff", units="in", width=12, height=7, res=300)
ggplot(violin_desl, aes(x= GM_2020,
                        y=CNRMC.M5_desl, fill=GM_2020)) +
  geom_flat_violin(position = position_nudge(x = 0.15, y = 0), alpha =0.7) +
  geom_point(aes(y = CNRMC.M5_desl, color = GM_2020),
             position = position_jitter(width = 0.15), size = 1, alpha = 0.45) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
  scale_fill_manual(values = c("#fcd74e",
                               "#ffb14e",
                               "#fa8775",
                               "#d17393",
                               "#bb4da7"))+
  scale_colour_manual(values = c("#fcd74e",
                                 "#ffb14e",
                                 "#fa8775",
                                 "#d17393",
                                 "#bb4da7"))+
  theme_ipsum() +
  theme(legend.position="none") +
  labs(x="Grado de marginación (CONAPO, 2020)",
       y="Riesgo de exposición a deslaves (INECC, 2019)",
       title="Distribución del riesgo de exposición a deslaves por cambio climático
a nivel municipal segmentado por grado de marginación (México)",
       caption="Elaboración: Bernardo L. Mc Kelligan")
dev.off()

## Violin_puntos_box_inun
tiff("inun_margin_dis.tiff", units="in", width=12, height=7, res=300)
ggplot(violin_inun, aes(x= GM_2020,
                        y=CNRMC.M5_inun, fill=GM_2020)) +
  geom_flat_violin(position = position_nudge(x = 0.15, y = 0), alpha =0.7) +
  geom_point(aes(y = CNRMC.M5_inun, color = GM_2020),
             position = position_jitter(width = 0.15), size = 1, alpha = 0.45) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
  scale_fill_manual(values = c("#fcd74e",
                               "#ffb14e",
                               "#fa8775",
                               "#d17393",
                               "#bb4da7"))+
  scale_colour_manual(values = c("#fcd74e",
                                 "#ffb14e",
                                 "#fa8775",
                                 "#d17393",
                                 "#bb4da7"))+
  theme_ipsum() +
  theme(legend.position="none") +
  labs(x="Grado de marginación (CONAPO, 2020)",
       y="Riesgo de exposición a inundaciones (INECC, 2019)",
       title="Distribución del riesgo de exposición a inundaciones por cambio climático
a nivel municipal segmentado por grado de marginación (México)",
       caption="Elaboración: Bernardo L. Mc Kelligan")
dev.off()

-((1969/2456)-1) # falta 19.8% en desl
-((1103/2456)-1) # falta 55.1% en inun


## De aquí para abajo sólo es un intento frustrado por hacer un mapa de las
## localidades

## Ya sólo me falta el mapa!!
setwd("C:/Users/Admin/Documents/Inputs/mg_2020_integrado")
capa_municipios <- readOGR("conjunto_de_datos", layer="00mun")
capa_municipios@proj4string

capa_municipios_2 <- spTransform(capa_municipios, CRS("+proj=longlat +datum=WGS84"))
capa_municipios_2@proj4string

capa_municipios_df <- tidy(capa_municipios_2, region='CVEGEO')
#   Aparentemente no funciona esto con las localidades -.-

raw_01 <- entorn_prob %>%
  select(-DRENAJECOB, -ACTMIN) %>% 
  mutate(CVEGEO = str_pad(CVEGEO, 9, pad = "0")) %>% 
  mutate(id = substring(CVEGEO,1,5)) %>% 
  group_by(id, PROBLEMA) %>% 
  mutate(n = n()) %>% 
  data.frame() %>% 
  group_by(id) %>% 
  mutate(freq = n()) %>% 
  data.frame() %>% 
  mutate(porcen = n/freq) %>% 
  mutate(afect = ifelse(
    PROBLEMA == "Afectaciones por fenomenos naturales y plagas", 1,0))

raw_02 <- raw_01 %>% 
  group_by(id) %>% 
  mutate(afect_sum = sum(afect)) %>% 
  mutate(porcen_02 = ifelse(
    PROBLEMA == "Afectaciones por fenomenos naturales y plagas",
    porcen, afect_sum)) %>%
  data.frame()


raw_03 <- raw_02 %>% 
  filter(PROBLEMA == "Afectaciones por fenomenos naturales y plagas" |
         afect_sum == 0)

length(unique(raw_03$id))

filter(raw_03, PROBLEMA == "Problemas ambientales")

entorn_prob_amb_freq <- raw_03 %>% 
  group_by(id) %>% 
  summarise(freq_prob_amb = mean(porcen_02)) %>% 
  data.frame()

entorn_prob_amb_freq %>% 
  filter(freq_prob_amb == 0)

filter(raw_01, id == '01007' & 
         PROBLEMA == "Afectaciones por fenomenos naturales y plagas")
 ## SIIIII, LO LOGRÉ BABY

mun_prob <- capa_municipios_df %>% 
  right_join(entorn_prob_amb_freq, by = "id")

## Ahora sí, a mapear (update: intento frustrado)
ggplot(mun_prob) +  
  geom_polygon(aes(x=long, y=lat, 
                   group=group,       
                   fill=freq_prob_amb))+
  scale_fill_gradient(
    low = "white",
    high = "#E46726",
    space = "Lab",
    na.value = "white",
    guide = "colourbar",
    aesthetics = "fill")+
  theme_bw()

# install.packages("importinegi")
# library(importinegi)
# 
# ?sig_marcogeo
# loc_rural_raw <- sig_marcogeo(year=2010, mapa ="rural", version = "5.0")
# loc_rural <- spTransform(loc_rural_raw,
#                                  CRS("+proj=longlat +datum=WGS84"))

## No sirve "importinegi" para hacer mapas, puro clickbait >:[
