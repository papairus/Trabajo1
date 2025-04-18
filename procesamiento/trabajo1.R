# Preparando el trabajo--------------------------------------------------------

rm(list = ls()) # para limpiar el entorno de trabajo

pacman::p_load(knitr,
  labelled,
  srvyr,
  tidyverse,   
  sjPlot,    
  confintr,    
  gginference, 
  rempsyc,     
  broom,      
  sjmisc,   
  haven,
  stargazer,
  dplyr,
  sjlabelled)
install.packages("survey")

options(scipen = 999) # para desactivar notacion cientifica

library(pacman)
library(haven)
data <- read_sav("../Input/data_orig/BBDD.sav")

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

# Cargar paquetes
library(tidyverse)
library(sjmisc)
library(haven)
library(sjlabelled)
library(rempsyc)


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

#Para el analisis se combinaran las variables "migrante" con "sexo" para asi
# identificar el genero (hombre/mujer/otro) de lxs estudiantes de chilenxs y 
# extranjerxs.

library(dplyr)

proc_data <- proc_data %>%
  mutate(sexo_migrante = case_when(
    sexo == "Hombre" & migrante == "Migrante" ~ "Hombre migrante",
    sexo == "Hombre" & migrante == "Chilenx"  ~ "Hombre chilenx",
    sexo == "Mujer"  & migrante == "Migrante" ~ "Mujer migrante",
    sexo == "Mujer"  & migrante == "Chilenx"  ~ "Mujer chilenx",
    sexo == "Otro"   & migrante == "Migrante" ~ "Otro migrante",
    sexo == "Otro"   & migrante == "Chilenx"  ~ "Otro chilenx",
    TRUE ~ NA_character_  # Por si hay datos perdidos
  ))

library(sjmisc)
frq(proc_data$sexo_migrante)
class(proc_data$sexo_migrante)
proc_data$sexo_migrante <- factor(proc_data$sexo_migrante,
  levels = c(
    "Mujer chilenx", "Mujer migrante",
    "Hombre chilenx", "Hombre migrante",
    "Otro chilenx", "Otro migrante"))

# ️## Atributo de las variables

sjlabelled::get_label(proc_data)

library(sjlabelled)

library(labelled)  # Asegúrate de que también está cargado

proc_data <- proc_data %>%
  labelled::set_variable_labels(
    victim_vida_cuenta = "Número de victimizaciones de por vida (0–31)",
    sexo               = "Sexo del estudiante",
    migrante           = "Origen",
    sexo_migrante      = "Sexo del estudiante, según origen")


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
frq(proc_data$sexo_migrante) 

library(stargazer)

proc_data <-as.data.frame(proc_data) #tabla descriptiva, solo aparece la variable
#                                     numerica.
stargazer(proc_data, type="text")



## Tabla descriptiva -----------------------------------------------------------
# Siendo entonces nuestra variable dependiente el numero de veces en que se 
# contesto "Sí" en victimizaciones en vida, una variable numerica continua con
# un rango de 0 a 30 victimizaciones

# Junto a dos independientes categoricas nominales . 1. el sexo
#                                                    2. origen.



proc_data <- proc_data %>%
  mutate(victim_vida_cuenta = haven::zap_labels(victim_vida_cuenta)) %>%
  mutate(victim_vida_cuenta = as.numeric(victim_vida_cuenta))

library(dplyr)
library(survey)
library(srvyr)

# 1. Limpiar variable dependiente
proc_data <- proc_data %>%
  mutate(victim_vida_cuenta = haven::zap_labels(victim_vida_cuenta)) %>%
  mutate(victim_vida_cuenta = as.numeric(victim_vida_cuenta))

# 2. Crear diseño complejo (ponderado)
base_poli_diseño_complejo <- srvyr::as_survey_design(
  proc_data,
  ids = var_unit,
  strata = var_strat,
  weights = wgt_alu,
  nest = TRUE
)

# 3. Calcular n y porcentaje ponderados
totales <- svytable(~sexo_migrante, base_poli_diseño_complejo)
porcentajes <- prop.table(totales) * 100

desc_cat_pond <- data.frame(
  sexo_migrante = names(totales),
  n = as.numeric(totales),
  porcentaje = round(as.numeric(porcentajes), 2)
)

# 4. Calcular media y error estándar ponderado
media_ee <- svyby(
  ~victim_vida_cuenta,
  ~sexo_migrante,
  base_poli_diseño_complejo,
  svymean,
  na.rm = TRUE,
  vartype = "se"
)

# 5. Calcular mediana ponderada
niveles <- levels(proc_data$sexo_migrante)
mediana_lista <- lapply(niveles, function(grupo) {
  subset_dsgn <- subset(base_poli_diseño_complejo, sexo_migrante == grupo)
  mediana_val <- as.numeric(svyquantile(~victim_vida_cuenta, subset_dsgn, quantiles = 0.5, ci = FALSE, na.rm = TRUE))
  return(data.frame(sexo_migrante = grupo, mediana = mediana_val))
})
mediana <- bind_rows(mediana_lista)

# 6. Calcular desviación estándar ponderada
desv_std_raw <- svyby(
  ~victim_vida_cuenta,
  ~sexo_migrante,
  base_poli_diseño_complejo,
  svyvar,
  na.rm = TRUE
)
desv_std <- data.frame(
  sexo_migrante = desv_std_raw$sexo_migrante,
  sd = sqrt(desv_std_raw$victim_vida_cuenta)
)

# 7. Unir todo en tabla final
tabla_final <- media_ee %>%
  rename(
    media = victim_vida_cuenta,
    error_est = se
  ) %>%
  left_join(mediana, by = "sexo_migrante") %>%
  left_join(desv_std, by = "sexo_migrante") %>%
  left_join(desc_cat_pond, by = "sexo_migrante") %>%
  select(
    sexo_migrante, n, porcentaje,
    media, mediana, error_est, sd)


## Graficos univariados --------------------------------------------------------

#Grafico 1 con ponderación: Segun Sexo 


proc_data_expand <- proc_data %>%
  mutate(weight = round(wgt_alu)) %>% # redondear para replicar
  filter(!is.na(victim_vida_cuenta)) %>%
  uncount(weights = weight) # expandir según pesos
	library(ggplot2)

ggplot(proc_data_expand, aes(x = sexo, y = victim_vida_cuenta, fill = sexo)) +
  geom_boxplot(outlier.colour = "purple", outlier.size = 1, alpha = 0.7) +
  labs(
    title = "Boxplot con ponderación según Sexo",
    x = "Distinción Sexo",
    y = "Número de victimizaciones en la vida"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "PRGn")

# Grafico 2 con ponderación: Segun Origen


proc_data_expand <- proc_data %>%
  mutate(weight = round(wgt_alu)) %>% # redondear para replicar
  filter(!is.na(victim_vida_cuenta)) %>%
  uncount(weights = weight) # expandir según pesos
	library(ggplot2)

ggplot(proc_data_expand, aes(x = migrante, y = victim_vida_cuenta, fill = migrante)) +
  geom_boxplot(outlier.colour = "purple", outlier.size = 1, alpha = 0.7) +
  labs(
    title = "Boxplot con ponderación segun origen",
    x = "Origen",
    y = "Número de victimizaciones en la vida"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "PRGn")

## Grafico 3  con ponderación:Segun sexo y origen

library(dplyr)
library(tidyr)

# Data set


proc_data_expand <- proc_data %>%
  mutate(weight = round(wgt_alu)) %>% # redondear para replicar
  filter(!is.na(victim_vida_cuenta)) %>%
  uncount(weights = weight) # expandir según pesos
	library(ggplot2)

ggplot(proc_data_expand, aes(x = sexo_migrante, y = victim_vida_cuenta, fill = sexo_migrante)) +
  geom_boxplot(outlier.colour = "purple", outlier.size = 1, alpha = 0.7) +
  labs(
    title = "Boxplot con ponderación",
    x = "Distinción Sexo - origen",
    y = "Número de victimizaciones en la vida"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "PRGn")

# Save -------------------------------------------------------------------------
save(proc_data, file = "../procesamiento/proc_data.RData")
