# Trabajo 2:          ----------------------------------------------------------
# Construcción de Escala e interpretación de correlacion y Alfa de Crobanch


# Preparando el trabajo---------------------------------------------------------

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
							 sjlabelled,
							 car,
							 summarytools,
							 psych, 
							 survey, 
							 kableExtra,
							 GGally,
							 corrplot)

options(scipen = 999) # para desactivar notacion cientifica

library(pacman)
library(survey)


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
														 var_unit,PA_5, PA_6, PA_7, PA_3, PA_1, PA_2, PA_4,
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


indice_vj <- proc_data %>%
  rowwise() %>%
  mutate(
    delitos = mean(c_across(c(PA_1:PA_7)), na.rm = TRUE),
    cuidadores = mean(c_across(c(PB_1:PB_4)), na.rm = TRUE),
    pares = mean(c_across(c(PC_1:PC_5)), na.rm = TRUE),
    sexual = mean(c_across(c(PD_1:PD_7)), na.rm = TRUE),
    entornos = mean(c_across(c(PE_1:PE_7)), na.rm = TRUE),
    digitales = mean(c_across(c(PF_1:PF_2)), na.rm = TRUE),
    indice_vj = mean(c(delitos, cuidadores, pares, sexual, entornos, digitales), na.rm = TRUE)
  ) %>%
  ungroup()



 indice_vj = indice_vj %>%
  rowwise() %>%
  mutate(victimizaciones = mean(c(delitos, cuidadores, pares, sexual, entornos, digitales), na.rm = TRUE)) %>%
  ungroup()

 datos_sin_na <- indice_vj %>%
  filter(!is.na(victimizaciones))


#¿Cuál es el porcentaje en el cual estas victmizaciones cobran el sentido de polivictimizacion?---------
# Los indicadores de polivictimización se construyeron de acuerdo con las 
# orientaciones teóricas, donde se denomina polivictimizados a quienes se
# encuentran en el 10% superior de la muestra respecto a la suma de victimizaciones
# vida y año (separadamente)

p90_victimizaciones <- quantile(indice_vj$victimizaciones, 0.90, na.rm = TRUE)

# variable dicotómica de polivictimización en vida
indice_vj <- indice_vj %>%
  mutate(
    polivict_vida = case_when(
      victimizaciones > p90_victimizaciones ~ "sí",
      TRUE ~ "no"))

# Ver proporción de casos polivictimizados
prop.table(table(indice_vj$polivict_vida)) * 100


indice_vj <- indice_vj %>%
  mutate(
    polivict_vida = ifelse(victimizaciones > quantile(victimizaciones, 0.9, na.rm = TRUE), "sí", "no")
  )


indice_vj <- indice_vj %>%
  mutate(
    polivict_vida = ifelse(victimizaciones > quantile(victimizaciones, 0.9, na.rm = TRUE), "sí", "no")
  )

summary(indice_vj$victimizaciones) # Resumen


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


#Creación escala Autoestima -----------------------------------------------------
# Crear escala de Autoestima
autoestima_items <- proc_data %>%
  select(PG_1:PG_10)

# Ítems negativos según Rosenberg: PG_2, PG_5, PG_6, PG_8, PG_9
autoestima_escala <- autoestima_items %>%
  mutate(
    PG_2 = 6 - PG_2,
    PG_5 = 6 - PG_5,
    PG_6 = 6 - PG_6,
    PG_8 = 6 - PG_8,
    PG_9 = 6 - PG_9
  ) %>%
  mutate(PG_autoestima = rowSums(., na.rm = TRUE))

# Agregar la variable a tu base principal
proc_data <- proc_data %>%
  mutate(PG_autoestima = autoestima_escala$PG_autoestima)

# Asegurarse de que todos los PG_1:PG_10 están en la base
stopifnot(all(paste0("PG_", 1:10) %in% names(proc_data)))

#Escala Depresion --------------------------------------------------------------


# Crear escala de Depresión
	depresion_items <- proc_data %>%
	  select(PH_1:PH_18)
	
	# Suma total (1 = nunca, 2 = a veces, 3 = siempre)
	depresion_escala <- depresion_items %>%
	  mutate(PH_depresion = rowSums(., na.rm = TRUE)) %>%
	  mutate(PH_depresion_dic = ifelse(PH_depresion >= 19, 1, 0))  # 1 = posibles síntomas depresivos
	
	# Agregar variables a base principal
	proc_data <- proc_data %>%
	  mutate(
	    PH_depresion = depresion_escala$PH_depresion,
	    PH_depresion_dic = depresion_escala$PH_depresion_dic
	  )







# Crear un nuevo data frame solo con las escalas (debes asegurarte que tengan el mismo número de casos)
escalas <- data.frame(
  PG_autoestima = proc_data$PG_autoestima,
  PH_depresion = proc_data$PH_depresion,
  victimizaciones = indice_vj$victimizaciones
)


# Filtrar filas completas (sin NA)
escalas_completas <- escalas %>% na.omit()
sjPlot::tab_corr(escalas_completas, triangle = "lower")


install.packages("ggcorrplot")  # Solo si no lo tienes
library(ggcorrplot)

# Calcular matriz de correlación
cor_matrix <- cor(escalas_completas, use = "complete.obs", method = "pearson")

# Gráfico heatmap
ggcorrplot(cor_matrix, 
           lab = TRUE, 
           type = "lower", 
           method = "circle", 
           colors = c("red", "white", "blue"),
           title = "Correlaciones entre escalas")


# Base unificada
escalas_con_grupo <- data.frame(
  sexo_migrante = proc_data$sexo_migrante,
  PG_autoestima = proc_data$PG_autoestima,
  PH_depresion = proc_data$PH_depresion,
  victimizaciones = indice_vj$victimizaciones
) %>% 
  filter(complete.cases(.))  # solo casos completos




library(dplyr)

# Calcular correlaciones entre escalas por grupo---------------------------------
#   Agrupa los datos por la variable sexo_migrante, lo que indica que se están creando subgrupos según los valores de esa variable.
#Calcula tres correlaciones dentro de cada grupo:
#r_auto_dep: Correlación entre la variable de autoestima (PG_autoestima) y depresión (PH_depresion).
#r_auto_vic: Correlación entre la variable de autoestima y victimizaciones (victimizaciones).
#r_dep_vic: Correlación entre la variable de depresión y victimizaciones.
escalas_con_grupo %>%
  group_by(sexo_migrante) %>%
  summarise(
    r_auto_dep = cor(PG_autoestima, PH_depresion),
    r_auto_vic = cor(PG_autoestima, victimizaciones),
    r_dep_vic  = cor(PH_depresion, victimizaciones)
  )


#Matrices de correlacion: Grafico de nube -------------------------------------

sjPlot::plot_scatter(proc_data, sexo_migrante, PG_autoestima)

sjPlot::plot_scatter(proc_data, sexo_migrante, PH_depresion)


#Estimacion de correlacion -----------------------------------------------------
cor(escalas_completas)

#Alfa de cobranch: analizar solo PG_AUTOESTIMA Y PH_DEPRESION-----------------
psych::alpha(autoestima_items)

psych::alpha(depresion_items)



