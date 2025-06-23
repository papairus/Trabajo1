# Trabajo 3: Regresion lineal.

# Preparacion del espacio de trabajo -----------------------------------------------------------

rm(list = ls()) # para limpiar el entorno de trabajo

install.packages("pacman")
install.packages("RColorBrewer")
pacman::p_load(dplyr, car, sjmisc, sjPlot, sjlabelled, stargazer, kableExtra, corrplot, texreg, ggplot2, ggpubr, haven)

library(pacman)
library(RColorBrewer)
options(scipen = 999) # para desactivar notacion cientifica

#Carga de bbdd

data <- read_sav("../input/data_orig/BBDD.sav")


# Seleccion de variables clave para el modelo
modelo_data <- proc_completo |>
  dplyr::select(victimizaciones, PG_autoestima, PH_depresion, sexo_migrante)

# Convertir sexo_migrante a factor con etiquetas claras
modelo_data$sexo_migrante <- factor(
  modelo_data$sexo_migrante,
  labels = c("Mujer chilena", "Mujer migrante", 
             "Hombre chileno", "Hombre migrante",
             "Otro chileno", "Otro migrante")
)

# Eliminar casos perdidos (para cumplir supuestos del modelo)
modelo_data <- na.omit(modelo_data)


# --- Modelos.R ---
library(ggplot2)
library(sjPlot)
library(dplyr)

# Modelo 1: Solo autoestima
modelo_1 <- lm(victimizaciones ~ PG_autoestima, data = modelo_data)

# Modelo 2: Autoestima + depresión
modelo_2 <- lm(victimizaciones ~ PG_autoestima + PH_depresion, data = modelo_data)

# Modelo 3: Autoestima + depresión + grupo sexo/migrante
modelo_3 <- lm(victimizaciones ~ PG_autoestima + PH_depresion + sexo_migrante, data = modelo_data)

# Comparar modelos
tab_model(modelo_1, modelo_2, modelo_3,
          dv.labels = c("Autoestima", "Autoestima + Depresión", "Modelo completo"),
          title = "Modelos de regresión sobre victimizaciones")


# --- Diagnostico.R ---
# Supuestos del modelo lineal: modelo_3
par(mfrow = c(2, 2))  # Distribución 2x2 de plots
plot(modelo_3)        # 1. residuos vs ajustados, 2. QQ, 3. sqrt|resid|, 4. leverage
par(mfrow = c(1, 1))

# Revisar multicolinealidad
library(car)
vif(modelo_3)  # Valores VIF < 5 deseables



library(ggplot2)
library(RColorBrewer)

# Asegurar que sexo_migrante es factor
modelo_data$sexo_migrante <- as.factor(modelo_data$sexo_migrante)

# Crear el gráfico
g1 <- ggplot(modelo_data, aes(x = sexo_migrante, y = pred_victimizaciones, fill = sexo_migrante)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PRGn") +
  labs(title = "Victimizaciones predichas según sexo y nacionalidad",
       x = "Grupo sexo/migrante", y = "Victimizaciones predichas") +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar el gráfico
print(g1)

# Guardar el gráfico como PNG
ggsave("../output/boxplot_victimizaciones.png",
       plot = g1,
       width = 8, height = 6, dpi = 300)

library(ggplot2)

# Asegurarse de que la carpeta "output" existe
if (!dir.exists("output")) {
  dir.create("output")
}

# Crear el gráfico
g2 <- ggplot(modelo_data, aes(x = PH_depresion, y = pred_victimizaciones, color = sexo_migrante)) +
  geom_line(stat = "smooth", method = "lm", se = FALSE) +
  labs(title = "Predicción de victimizaciones según depresión",
       x = "Depresión", y = "Victimizaciones predichas") +
  theme_minimal()

# Guardar el gráfico
ggsave(filename = "output/linea_pred_depresion.png",
       plot = g2, width = 8, height = 6, dpi = 300)


