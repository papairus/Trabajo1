# Trabajo 2:          ----------------------------------------------------------
# Construcción de Escala e interpretación de correlacion y Alfa de Crobanch


# Preparando el trabajo---------------------------------------------------------

rm(list = ls()) # para limpiar el entorno de trabajo

install.packages("pacman")

pacman::p_load(knitr,
							 GGally,
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
							 sjlabelled,
							 car,
							 summarytools,
							 psych, 
							 survey, 
							 kableExtra,
							 corrplot,
							 ggcorrplot,
							 DescTools,
							 ggplot2)


options(scipen = 999) # para desactivar notacion cientifica

library(pacman)
library(ggcorrplot)


data <- read_sav("../input/data_orig/BBDD.sav")

# Recordando las variables del trabajo 1 ---------------------------------------

# Creacion de la variable "migrante" para NNA´s unificando la variable 
# P1_4 = ¿En qué país naciste? (excluyendo la categoria de respuestas 1 = Chile)
# Por otro lado la variable P1_5 = ¿En qué país nació tu madre? (excluyendo la 
# misma respuesta. 
# Esta quedaria de la siquiente forma:   migrante = 1 = migrante /  0 = Chilenx
#          
# Se excluye la respuesta 13 de ambas variables considerandolas NA´s
# Se excluye la respuesta 13 de ambas variables considerandolas NA´s

#Para que apelar a la representatividad y equilibrio muestral en la cantidad de 
# estudiantes Chilenxs y Migrantes se han consideraron las siguientes variables
# (extraidas del manual de la base) necesarias para ponderar y realizar un 
# analisis poblacional de la muestra.
#            
# wgt_alu = Ponderador Raking a 5 margenes, truncado 
#           margen superior p99

# var_strat = Estrato: regiones y dependencias. 
#             Particulares agrupados en macrozonas

# var_unit = Conglomerados: rbds de escuelas


# Aclaración sobre la creación del Índice de Victimizaciones en Vida -----------
# En el Trabajo 1 se realizó un análisis descriptivo de las variables independientes
# previamente mencionadas, utilizando como variable dependiente una medida numérica
# que representa la cantidad de respuestas afirmativas ("sí") ante distintas situaciones 
# de victimización experimentadas al menos una vez en la vida. La base de datos 
# elaborada por DESUC a partir de la Segunda Encuesta de  Polivictimas (EPV 2023) 
# incluye una versión operacionalizada de esta variable.
# Sin embargo, para el desarrollo del Trabajo 2 se requiere construir esta medida
# de manera autónoma a partir de los ítems originales, lo cual se realiza en este script.

## Trabajo 1: Procesamiento BBDD data = proc_data ----------------------------------------
proc_data <- data %>% select(P1_2, P1_4, P1_5,
														 wgt_alu,
														 var_strat,
														 var_unit, PA_5, PA_6, PA_7, PA_3, PA_1, PA_2, PA_4,
                                       PB_1, PB_2, PB_3, PB_4,
                                       PC_1, PC_2, PC_3, PC_4, PC_5,
                                       PD_1, PD_2, PD_3, PD_4, PD_5, PD_6, PD_7,
                                       PE_1, PE_2, PE_3, PE_4, PE_5, PE_6, PE_7,
                                       PF_1, PF_2,
														     PG_1, PG_2, PG_3, PG_4, PG_5, PG_6, PG_7, PG_8,
												     		 PG_9, PG_10,
													 PH_1, PH_2, PH_3, PH_4, PH_5, PH_6, PH_7, PH_8, PH_9,
								 PH_10, PH_11, PH_12, PH_13, PH_14, PH_15, PH_16, PH_17, PH_18)
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
    sexo               = "Sexo del estudiante",
    migrante           = "Origen",
    sexo_migrante      = "Sexo del estudiante, según origen")


## Frecuencia de las variables y reconocimiento NA´s
frq(proc_data$migrante) 
frq(proc_data$sexo) 


proc_data <- proc_data %>% select(-P1_2,-P1_4,-P1_5)


proc_data <- proc_data %>%
  filter(
    !is.na(migrante),
    !is.na(sexo))
    
    
frq(proc_data$migrante) 
frq(proc_data$sexo) 
frq(proc_data$sexo_migrante) 



#TRABAJO 2: Filtrando las variables para el analisis de Indices y Escalas. ------------------


# diseño complejo / ponderado.
library(survey)
proc_data_complejo <- srvyr::as_survey_design (data,
ids = var_unit,
strata = var_strat,
weights = wgt_alu,
nest = TRUE) 

 na.omit(proc_data) %>% # Eliminar Na's
  mutate_all(~(as.numeric(.))) # Convertimos todas las variables a numéricas
 
 

# Construcción Indice de Victimización -----------------------------------------
# Por medio de 32 victimizaciones divididas en 6 dimensiones:
# Delitos comunes: PA_5, PA_6, PA_7, PA_3, PA_1, PA_2, PA_4,
# Maltrato de cuidadores: PB_1, PB_2, PB_3, PB_4,
# Maltrado por pares: PC_1, PC_2, PC_3, PC_4, PC_5,
# Sexual: PD_1, PD_2, PD_3, PD_4, PD_5, PD_6, PD_7,
# Entornos violentos: PE_1, PE_2, PE_3, PE_4, PE_5, PE_6, PE_7
# Digitales: PF_1, PF_2


# CONSTRUCCIÓN DE ÍNDICE DE VICTIMIZACIONES (conteo de respuestas "sí") ----------------

 indice_vj <- proc_data %>%
  rowwise() %>%
  mutate(
    victimizaciones = sum(c_across(c(
      PA_1, PA_2, PA_3, PA_4, PA_5, PA_6, PA_7,
      PB_1, PB_2, PB_3, PB_4,
      PC_1, PC_2, PC_3, PC_4, PC_5,
      PD_1, PD_2, PD_3, PD_4, PD_5, PD_6, PD_7,
      PE_1, PE_2, PE_3, PE_4, PE_5, PE_6, PE_7,
      PF_1, PF_2)) == 1, na.rm = TRUE)
  ) %>%
  ungroup()


#¿Cuál es el porcentaje en el cual estas victmizaciones cobran el sentido de polivictimizacion?---------
# Los indicadores de polivictimización se construyeron de acuerdo con las 
# orientaciones teóricas, donde se denomina polivictimizados a quienes se
# encuentran en el 10% superior de la muestra respecto a la suma de victimizaciones
# vida y año (separadamente)

# Calcular percentil 90 del índice de victimizaciones
p90_vict <- quantile(indice_vj$victimizaciones, 0.90, na.rm = TRUE)

# Crear variable dicotómica de polivictimización
indice_vj <- indice_vj %>%
  mutate(polivict_vida = ifelse(victimizaciones >= p90_vict, "sí", "no"))

# VERIFICACIÓN VICTIMIZACIÓN Y POLIVICTIMIZACIÓN ------------------------------------

# Proporción de casos polivictimizados
round(prop.table(table(indice_vj$polivict_vida)) * 100, 2) 

# no presenta polivictimización en vida   /   sí presenta polivictimización en vida.
#               89.07                                           10.93 

# Revisión resumen índice
summary(indice_vj$victimizaciones)

#Los resultados coinciden con los datos de victmización del Trabajo 1

#  Min.   1st Qu.  Median     Mean    3rd Qu.     Max. 
# 0.000   3.000     6.000    7.276    11.000    31.000 



# ESCALAS: -----------------------------------------------------------------------

# Variable de escala de autoestima y escala de depresión ----------------------
# El siguiente parrafo fue extraido del manual de codigos de la BBDD (pg 9 - 10)
#Las escalas de autoestima y escalas de depresión requirieron la construcción 
# de indicadores.
# La Escala de Autoestima de Rosenberg (EAR) considera un análisis donde se 
# identifican los ítems positivos y negativos, para después invertir el valor 
# negativo y sumar los valores totales, generando una variable continua que
# fluctúa entre los valores 10 a 50. Teóricamente, los valores de la escala 
# fluctúan entre 10 (indicador de baja autoestima) y 40 
# (indicador de alta autoestima) (Rojas-Barahona, Zegers P, & Förster M, 2009).
# El nombre de la variable en la base de datos se denomina PG_autoestima.
# La Escala de Depresión Infantil de Birleson se creó identificando ítems 
# positivos y negativos, para posteriormente invertir el valor negativo 
# y sumar los valores totales, generando una variable continua que fluctúa 
# entre 0 a 36. Esta variable se denominó PH_depresion. 
# Considerando las guías clínicas de MINSAL, se dicotomizó esta variable 
# considerando el umbral del puntaje de 19 o más como posibles síntomas 
# depresivos, y de 0 a 18 sin síntomas depresivos (MINSAL, 2013),
# denominando a la variable en la base de datos PH_depresion_dic.

# Construcción Escala Autoestima (ROSENBERG) -----------------------------------

# Selección de ítems de autoestima

proc_data <- proc_data %>%
  mutate(across(PG_1:PG_10, ~ as.numeric(as.character(.)))) %>%
  mutate(across(PG_1:PG_10, ~ ifelse(. >= 1 & . <= 5, ., NA)))


# Aplicar inversión solo a los ítems negativos
autoestima_items <- proc_data %>% select(PG_1:PG_10) %>%
  mutate(
    PG_3 = 6 - PG_3,
    PG_5 = 6 - PG_5,
    PG_8 = 6 - PG_8,
    PG_9 = 6 - PG_9,
    PG_10 = 6 - PG_10)

# Calcular la escala
proc_data <- proc_data %>%
  mutate(PG_autoestima = rowSums(autoestima_items, na.rm = TRUE))

# Calcular alfa de Cronbach correctamente
psych::alpha(autoestima_items)
hist(proc_data$PG_autoestima, breaks = 20, col = "slategray3", main = "Distribución de autoestima")

## Alfa de Cronbach = 0.84 → Muy buena consistencia interna. ------------------


# CONSTRUCCIÓN ESCALA DE DEPRESIÓN (BIRLESON) -------------------------------------
proc_data <- proc_data %>% mutate(across(PH_1:PH_18, ~as.numeric(.)))


# Ítems invertidos según el manual
depresion_items <- proc_data %>% select(PH_1:PH_18)

depresion_items <- depresion_items %>%
  mutate(
    PH_2 = 4 - PH_2,
    PH_3 = 4 - PH_3,
    PH_4 = 4 - PH_4,
    PH_10 = 4 - PH_10,
    PH_14 = 4 - PH_14,
    PH_15 = 4 - PH_15,
    PH_17 = 4 - PH_17,
    PH_18 = 4 - PH_18
  )

# Calcular escala de depresión
proc_data <- proc_data %>%
  mutate(
    PH_depresion = rowSums(depresion_items, na.rm = TRUE),
    PH_depresion_dic = ifelse(PH_depresion >= 19, 1, 0)
  )

# Alfa de Cronbach ----------------------------------------------------------------

psych::alpha(depresion_items)
hist(proc_data$PH_depresion, breaks = 20, col = "slategray3", main = "Distribución de depresion")

# Evaluación de consistencia interna de la escala de Depresión Infantil de Birleson

# Ítems invertidos: PH_2, PH_3, PH_4, PH_10, PH_14, PH_15, PH_17, PH_18 (según el manual)


# Resultado: Alfa de Cronbach = 0.87 → Excelente consistencia interna ----------------------------

# Asociacion de variables ----------------------------------------------------------

# Unificación de escala de autoestima, depresión e indice de victimización con ponderación

# Crear base limpia con las variables necesarias
proc_completo <- proc_data %>%
  select(PG_autoestima, PH_depresion, sexo_migrante, var_unit, var_strat, wgt_alu) %>%
  bind_cols(indice_vj %>% select(victimizaciones)) %>%
  drop_na()

# Crear diseño complejo para análisis ponderado
diseño_complejo <- svydesign(
  ids = ~var_unit,
  strata = ~var_strat,
  weights = ~wgt_alu,
  nest = TRUE,
  data = proc_completo
)
#TABLA DE CORRELACIONES -------------------------------------------------------

tabla_cor <- proc_completo %>%
  select(PG_autoestima, PH_depresion, victimizaciones)

# Gráfico de dispersión entre PG_autoestima, PH_depresion y victimizaciones
ggplot(tabla_cor, aes(x = PG_autoestima, y = PH_depresion, color = victimizaciones)) +
  geom_point() +
  labs(title = "Relación entre Autoestima, Depresión y Victimizaciones", x = "Autoestima", y = "Depresión", color = "Victimizaciones") +
  theme_minimal()

# graficar la matriz es con la función ggpairs del paquete GGally ---------------------
# que nos entrega no solo el valor del coeficiente y su significancia (***), 
# si no que también un scatter del cruce entre variables.

library(dplyr)
library(GGally)

# Crear gráfico ggpairs
ggpairs(proc_completo %>% 
          select(PG_autoestima, PH_depresion, victimizaciones),
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.5)),
        diag = list(continuous = wrap("barDiag", binwidth = 1))) +
  theme_minimal()


library(corrplot)

# Calcular matriz de correlaciones --------------------------------------------
# presentar matrices de correlación es mediante gráficos. Veamos un ejemplo con 
# la función corrplot.mixed de la librería corrplot sobre nuestra matriz M  creada.

M <- cor(proc_completo %>% 
           select(PG_autoestima, PH_depresion, victimizaciones), 
         use = "complete.obs")

# Graficar matriz mixta
corrplot.mixed(M, upper = "ellipse", lower = "number",
               tl.col = "black", tl.cex = 1.2, number.cex = 1.1)



# Gráfico 1: Autoestima y victimizaciones controlado por sexo ------------------------------------------------------------

g1 <- ggplot(proc_completo, aes(x = PG_autoestima, y = victimizaciones, color = sexo_migrante)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Victimizaciones según Autoestima por grupo sexo/migrante",
       x = "Autoestima", y = "Victimizaciones")

ggsave(filename = paste0(output_path, "/grafico_autoestima.png"),
       plot = g1, width = 8, height = 6, dpi = 300)

# Gráfico 2: Depresión y victimizaciones controlado por sexo --------------------------------------------
g2 <- ggplot(proc_completo, aes(x = PH_depresion, y = victimizaciones, color = sexo_migrante)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Victimizaciones según Depresión por grupo sexo/migrante",
       x = "Depresión", y = "Victimizaciones")

ggsave(filename = paste0(output_path, "/grafico_depresion.png"),
       plot = g2, width = 8, height = 6, dpi = 300)




