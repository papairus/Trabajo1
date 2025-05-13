# Preparando el trabajo --------------------------------------------------------
library(pacman)
pacman::p_load(labelled,
							 srvyr,
							 tidyverse,   
               sjPlot,    
               confintr,    
               gginference, 
               rempsyc,     
               broom,      
               sjmisc,   
               knitr)      

library(pacman)
options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo
data <- read_sav("input/data_orig/BBDD.sav")

#Explorando la bbdd.
dim(data) # dimension de la base
View(data)
names(data)



## Creando la BBDD procesada = infancias, esta considerará las siguientes 
#  variables: victim_vida_cuenta  = Variable que cuenta las respuestas “sí” en 
#                                   todas las victimizaciones vida consultadas.
#                            P1_2 = ¿Cuál es tu sexo?    	1  /  2 /  3
#                                   Hombre/ Mujer/ Otro.
#                     P1_4 / P1_5 = Variable en que pais naciste y en que pais 
#                                    nació tu madre
#                   PG_autoestima = esta variable la utilizaremos en otra ocasion
#                                   debido a la extension del trabajo.
#                                   es el indice de autoestima rango10 - 50


# Otras variables necesarias: Para que esta muestra sea representativa y debido 
# al desequilibrio muestral en la cantidad de estudiantes Chilenxs y Migrantes
# se han consideraron las siguientes variables (extraidas del manual de la base) 
# necesarios para ponderar y realizar un analisis poblacional de la muestra.
#            
#                        wgt_alu = Ponderador Raking a 5 margenes, truncado 
#                                   margen superior p99
#                      var_strat = Estrato: regiones y dependencias. 
#                                   Particulares agrupados en macrozonas
#                       var_unit = Conglomerados: rbds de escuelas



## Procesamiento BBDD data = proc_data ----------------------------------------
proc_data <- data %>% select(victim_vida_cuenta,
														 P1_2,
														 P1_4, 
														 P1_5,
														 wgt_alu,
														 var_strat,
														 var_unit)

#  Comprobar
names(proc_data)

## Recodificacion de variables -------------------------------------------------
# Creacion de la variable "migrante" para NNA´s unificando la variable 
# P1_4 = ¿En qué país naciste? (excluyendo la categoria de respuestas 1 = Chile)
# Por otro lado la variable P1_5 = ¿En qué país nació tu madre? (excluyendo la 
# misma respuesta. 
# Esta quedaria de la siquiente forma:   migrante = 1 = migrante /  0 = Chilenx
#          
# Se excluye la respuesta 13 de ambas variables considerandolas NA´s
# Se excluye la respuesta 13 de ambas variables considerandolas NA´s

proc_data <- proc_data %>%
  mutate(
    migrante = case_when(
      P1_4 == 13 | P1_5 == 13 ~ NA_integer_,  # "No sé" → NA
      P1_4 != 1 | P1_5 != 1   ~ 1,            # Migrante
      TRUE                   ~ 0             # Chilenx
      ),
    migrante = factor(migrante,
                      levels = c(0, 1),
                      labels = c("Chilenx", "Migrante")))



proc_data <- proc_data %>%
  mutate(
    sexo = factor(P1_2,
                  levels = c(1, 2, 3),
                  labels = c("Hombre", "Mujer", "Otro")))

# ️## Atributo de las variables

sjlabelled::get_label(proc_data) 

#Etiquetar variables clave

proc_data <- proc_data %>%
  set_variable_labels(
    victim_vida_cuenta = "Número de victimizaciones de por vida (0–31)",
    sexo               = "Sexo del estudiante",
    migrante           = "Origen")


## Frecuencia de las variables y reconocimiento NA´s
frq(proc_data$migrante) 
frq(proc_data$sexo) 
frq(proc_data$victim_vida_cuenta) 

proc_data <- proc_data %>% select(-P1_2,-P1_4,-P1_5)


proc_data <- proc_data %>%
  filter(
    !is.na(migrante),
    !is.na(sexo),
    !is.na(victim_vida_cuenta))

frq(proc_data$migrante) 
frq(proc_data$sexo) 
frq(proc_data$victim_vida_cuenta) 

proc_data <- proc_data %>%
  mutate(victim_vida_cuenta = as.numeric(victim_vida_cuenta))  # Convertir a numérico

proc_data <- proc_data %>%
  mutate(victim_vida_cuenta = ifelse(victim_vida_cuenta == 99, NA, victim_vida_cuenta))


# Save -------------------------------------------------------------------------
proc_data <-as.data.frame(proc_data) #tabla descriptiva, solo aparece la variable
#                                     numerica.
stargazer(proc_data, type="text")

save(proc_data,file = "procesamiento/proc_data.RData")

## Tabla descriptiva -----------------------------------------------------------
# Siendo entonces nuestra variable dependiente el numero de veces en que se 
# contesto "Sí" en victimizaciones en vida, una variable numerica continua con
# un rango de 0 a 30 victimizaciones

# Junto a dos independientes categoricas nominales . 1. el sexo
#                                                    2. origen.


# Diseño complejo
encuesta_design <- svydesign(
  ids     = ~var_unit,    # conglomerados
  strata  = ~var_strat,    # estratos
  weights = ~wgt_alu,      # ponderador final
  data    = proc_data,
  nest    = TRUE)







## Graficos univariados --------------------------------------------------------

