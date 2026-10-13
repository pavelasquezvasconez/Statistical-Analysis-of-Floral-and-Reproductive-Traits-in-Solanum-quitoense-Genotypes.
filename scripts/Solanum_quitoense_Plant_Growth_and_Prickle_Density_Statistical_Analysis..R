library(readxl)

df <- read_excel("Test2.xlsx")
dput(head(df))
str(df)

# ================================================================
# ANÁLISIS DE TASAS DE CRECIMIENTO MEDIANTE PENDIENTES
# DBCA: Genotipo + Bloque
#
# β1 de Altura   = tasa de crecimiento en altura por día
# β1 de Diámetro = tasa de crecimiento en diámetro por día
# ================================================================

# PAQUETES -------------------------------------------------------

# Instalar solamente si es necesario:
#
# install.packages(c(
#   "dplyr",
#   "tidyr",
#   "ggplot2",
#   "janitor",
#   "missForest",
#   "car",
#   "emmeans",
#   "multcomp",
#   "multcompView",
#   "broom"
# ))

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(missForest)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(broom)


# REVISIÓN DE LA BASE ORIGINAL ----------------------------------

str(df)
summary(df)
dim(df)
names(df)


# LIMPIEZA DE NOMBRES --------------------------------------------

df_limpio <- janitor::clean_names(df)

names(df_limpio)

# Los nombres esperados después de clean_names() son:
#
# fecha
# evaluacion
# gen
# blq
# planta
# altura_planta
# diametro_tallo


# VERIFICACIÓN DE COLUMNAS ---------------------------------------

columnas_necesarias <- c(
  "fecha",
  "evaluacion",
  "gen",
  "blq",
  "planta",
  "altura_planta",
  "diametro_tallo",
  "diametro_copa"
  
  
)

columnas_faltantes <- setdiff(
  columnas_necesarias,
  names(df_limpio)
)

if(length(columnas_faltantes) > 0){
  
  stop(
    paste(
      "Faltan las siguientes columnas:",
      paste(columnas_faltantes, collapse = ", ")
    )
  )
}


# PREPARACIÓN DE VARIABLES ---------------------------------------

datos <- df_limpio %>%
  
  dplyr::mutate(
    
    fecha = as.Date(fecha),
    
    evaluacion = as.integer(evaluacion),
    
    genotipo = factor(gen),
    
    bloque = factor(blq),
    
    planta = factor(planta),
    
    altura = as.numeric(altura_planta),
    
    diametro_tallo = as.numeric(diametro_tallo),
    
    diametro_copa = as.numeric(diametro_copa)
    
  ) %>%
  
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    planta,
    altura,
    diametro_tallo,
    diametro_copa
  )

str(datos)
summary(datos)


# FECHAS DE EVALUACIÓN -------------------------------------------

tabla_fechas <- datos %>%
  
  dplyr::distinct(
    evaluacion,
    fecha
  ) %>%
  
  dplyr::arrange(
    evaluacion
  )

tabla_fechas


# Número de observaciones por fecha

datos %>%
  dplyr::count(
    evaluacion,
    fecha
  )


# DÍAS TRANSCURRIDOS ---------------------------------------------

# Se utilizan días reales y NO simplemente 1, 2, 3, 4.
#
# Así β1 tendrá unidades:
#
# Altura   -> unidades de altura / día
# Diámetro -> unidades de diámetro / día

datos <- datos %>%
  
  dplyr::mutate(
    
    dias = as.numeric(
      fecha - min(fecha, na.rm = TRUE)
    )
  )


datos %>%
  
  dplyr::distinct(
    evaluacion,
    fecha,
    dias
  ) %>%
  
  dplyr::arrange(
    evaluacion
  )


# IDENTIFICACIÓN DE PARCELA --------------------------------------

# Unidad experimental:
#
# Bloque × Genotipo
#
# Las plantas dentro de una parcela son submuestras.

datos <- datos %>%
  
  dplyr::mutate(
    
    parcela = interaction(
      bloque,
      genotipo,
      drop = TRUE,
      sep = "_"
    ),
    
    id_planta = interaction(
      bloque,
      genotipo,
      planta,
      drop = TRUE,
      sep = "_"
    )
  )


# VERIFICACIÓN DE DUPLICADOS -------------------------------------

# Una planta debería tener una observación por fecha.

duplicados <- datos %>%
  
  dplyr::count(
    fecha,
    id_planta,
    name = "n"
  ) %>%
  
  dplyr::filter(
    n > 1
  )

duplicados


if(nrow(duplicados) > 0){
  
  datos_duplicados <- datos %>%
    
    dplyr::semi_join(
      duplicados,
      by = c(
        "fecha",
        "id_planta"
      )
    ) %>%
    
    dplyr::arrange(
      fecha,
      genotipo,
      bloque,
      planta
    )
  
  print(
    datos_duplicados,
    n = Inf
  )
}


# VERIFICACIÓN DEL DBCA ------------------------------------------

# Número de bloques presentes para cada genotipo.

balance_genotipos <- datos %>%
  
  dplyr::distinct(
    genotipo,
    bloque
  ) %>%
  
  dplyr::count(
    genotipo,
    name = "numero_bloques"
  )

balance_genotipos

# Mostrar genotipos que no tienen 5 bloques

balance_genotipos %>%
  
  dplyr::filter(
    numero_bloques != 5
  )


# VALORES FALTANTES ----------------------------------------------

colSums(
  is.na(datos)
)

datos_faltantes <- datos %>%
  
  dplyr::mutate(
    fila = dplyr::row_number()
  ) %>%
  
  dplyr::filter(
    is.na(altura) |
      is.na(diametro_tallo)|
    is.na(diametro_copa) 
  )

datos_faltantes

# REVISIÓN DE VALORES EXTREMOS ----------------------------------

# Altura

datos %>%
  
  dplyr::arrange(
    dplyr::desc(altura)
  ) %>%
  
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    planta,
    altura
    
  ) %>%
  
  head(20)
 

# Diámetro de tallo

datos %>%
  
  dplyr::arrange(
    dplyr::desc(diametro_tallo)
  ) %>%
  
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    planta,
    diametro_tallo
  
  ) %>%
  
  head(20)

# Diametro de copa 

datos %>%
  
  dplyr::arrange(
    dplyr::desc(diametro_copa)
  ) %>%
  
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    planta,
    diametro_copa,
    
  ) %>%
  
  head(20)





# IMPUTACIÓN CON MISSFOREST --------------------------------------

# Guardar posiciones originales de los NA.

NA_altura <- which(
  is.na(datos$altura)
)

NA_diametro_tallo <- which(
  is.na(datos$diametro_tallo)
)

NA_diametro_copa <- which(
  is.na(datos$diametro_copa)
)



# Base para missForest

datos_mf <- datos %>%
  
  dplyr::transmute(
    
    evaluacion = factor(evaluacion),
    
    genotipo = factor(genotipo),
    
    bloque = factor(bloque),
    
    planta = factor(planta),
    
    altura = as.numeric(altura),
    
    diametro_tallo = as.numeric(diametro_tallo),
    
    diametro_copa = as.numeric(diametro_copa)
  
    
    
  ) %>%
  
  as.data.frame()


colSums(
  is.na(datos_mf)
)


# Ejecutar imputación solamente si existen NA.

if(anyNA(datos_mf)){
  
  set.seed(12345)
  
  imp_mf <- missForest::missForest(
    
    xmis = datos_mf,
    
    ntree = 1000,
    
    maxiter = 10,
    
    verbose = TRUE
  )
  
  
  # Error OOB
  
  cat(
    "\nError OOB de missForest:\n"
  )
  
  print(
    imp_mf$OOBerror
  )
  
  
  # Base imputada
  
  datos_imp <- datos
  
  datos_imp$altura <- as.numeric(
    imp_mf$ximp$altura
  )
  
  datos_imp$diametro_tallo <- as.numeric(
    imp_mf$ximp$diametro_tallo
  )
  
  datos_imp$diametro_copa <- as.numeric(
    imp_mf$ximp$diametro_copa
  )
  
  
  
} else {
  
  datos_imp <- datos
  
  message(
    "No existen valores faltantes. No fue necesario aplicar missForest."
  )
}


# VERIFICACIÓN DE LA IMPUTACIÓN ---------------------------------

colSums(
  is.na(datos_imp)
)


# Mostrar valores imputados de altura

if(length(NA_altura) > 0){
  
  imputacion_altura <- data.frame(
    
    fila = NA_altura,
    
    original = datos$altura[NA_altura],
    
    imputado = datos_imp$altura[NA_altura]
  )
  
  print(
    imputacion_altura
  )
}


# Mostrar valores imputados de diámetro de tallo

if(length(NA_diametro_tallo) > 0){
  
  imputacion_diametro_tallo <- data.frame(
    
    fila = NA_diametro_tallo,
    
    original = datos$diametro_tallo[NA_diametro_tallo],
    
    imputado = datos_imp$diametro_tallo[NA_diametro_tallo]
  )
  
  print(
    imputacion_diametro_tallo
  )
}

# Mostrar valores imputados de diámetro de copa

if(length(NA_diametro_copa) > 0){
  
  imputacion_diametro_copa <- data.frame(
    
    fila = NA_diametro_copa,
    
    original = datos$diametro_copa[NA_diametro_copa],
    
    imputado = datos_imp$diametro_copa[NA_diametro_copa]
  )
  
  print(
    imputacion_diametro_copa
  )
}





# CONSTRUCCIÓN DE LA BASE DE PARCELAS ---------------------------

# Las dos plantas son submuestras.
#
# Para cada Bloque × Genotipo × Fecha
# calculamos la media de las plantas.

datos_parcela <- datos_imp %>%
  
  dplyr::group_by(
    bloque,
    genotipo,
    parcela,
    fecha,
    evaluacion,
    dias
  ) %>%
  
  dplyr::summarise(
    
    n_plantas = dplyr::n(),
    
    altura = mean(
      altura,
      na.rm = TRUE
    ),
    
    diametro_tallo = mean(
      diametro_tallo,
      na.rm = TRUE
    ),
    
    diametro_copa = mean(
      diametro_copa,
      na.rm = TRUE
    ),
    
    
    .groups = "drop"
    
  ) %>%
  
  dplyr::mutate(
    
    bloque = factor(bloque),
    
    genotipo = factor(genotipo),
    
    parcela = factor(parcela)
  )


str(datos_parcela)

summary(datos_parcela)


# VERIFICACIÓN DE LA BASE DE PARCELAS ---------------------------

# Número de plantas utilizadas en cada media

table(
  datos_parcela$n_plantas
)


# Parcelas-fecha con número diferente de dos plantas

datos_parcela %>%
  
  dplyr::filter(
    n_plantas != 2
  )


# Número de fechas disponibles por parcela

numero_fechas_parcela <- datos_parcela %>%
  
  dplyr::count(
    parcela,
    name = "n_fechas"
  )


table(
  numero_fechas_parcela$n_fechas
)


# Parcelas que no tienen cuatro fechas

parcelas_incompletas <- numero_fechas_parcela %>%
  
  dplyr::filter(
    n_fechas != 4
  )

parcelas_incompletas


# GRÁFICO DE CURVAS DE CRECIMIENTO - ALTURA ---------------------

# Cada línea representa una parcela experimental.

ggplot2::ggplot(
  
  datos_parcela,
  
  ggplot2::aes(
    x = dias,
    y = altura,
    group = parcela
  )
  
) +
  
  ggplot2::geom_line(
    alpha = 0.45
  ) +
  
  ggplot2::geom_point(
    alpha = 0.65
  ) +
  
  ggplot2::facet_wrap(
    ~ genotipo
  ) +
  
  ggplot2::labs(
    x = "Días desde la primera evaluación",
    y = "Altura media de parcela",
    title = "Curvas de crecimiento en altura"
  ) +
  
  ggplot2::theme_bw()


# GRÁFICO DE CURVAS DE CRECIMIENTO - DIÁMETRO DE TALLO -------------------

ggplot2::ggplot(
  
  datos_parcela,
  
  ggplot2::aes(
    x = dias,
    y = diametro_tallo,
    group = parcela
  )
  
) +
  
  ggplot2::geom_line(
    alpha = 0.45
  ) +
  
  ggplot2::geom_point(
    alpha = 0.65
  ) +
  
  ggplot2::facet_wrap(
    ~ genotipo
  ) +
  
  ggplot2::labs(
    x = "Días desde la primera evaluación",
    y = "Diámetro de tallo medio de parcela",
    title = "Curvas de crecimiento del diámetro de tallo"
  ) +
  
  ggplot2::theme_bw()


  # GRÁFICO DE CURVAS DE CRECIMIENTO - DIÁMETRO DE COPA  -------------------

ggplot2::ggplot(
  
  datos_parcela,
  
  ggplot2::aes(
    x = dias,
    y = diametro_copa,
    group = parcela
  )
  
) +
  
  ggplot2::geom_line(
    alpha = 0.45
  ) +
  
  ggplot2::geom_point(
    alpha = 0.65
  ) +
  ggplot2::facet_wrap(
    ~ genotipo
  ) +
  
  ggplot2::labs(
    x = "Días desde la primera evaluación",
    y = "Diámetro de copa medio de parcela",
    title = "Curvas de crecimiento del diámetro de copa"
  ) +
  
  ggplot2::theme_bw()



# REGRESIONES POR PARCELA - ALTURA ------------------------------

# ============================================================
# REGRESIONES POR PARCELA - ALTURA
# Días después del trasplante (DDT)
# Fecha de trasplante: 02/0305/2026
#
# Modelo:
# altura = β0 + β1 * ddt
#
# β1 = tasa de crecimiento promedio en cm/día
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 1. Crear variable DDT
# ------------------------------------------------------------
# Si la variable "dias" actualmente contiene:
# 0, 28, 63, 103
#
# Entonces los días después del trasplante serán:
# 53, 81, 116, 156
#
# porque la primera evaluación fue a los 53 DDT.
#
#
#
#
# MODIFICAR AQUÍ EL NÚMERO DE DÍAS A EVALUACIÓN
# YO LE PUSE 53 PERO NO SÉ SI ESTA BIEN.
datos_parcela <- datos_parcela %>%
  dplyr::mutate(
    ddt = dias + 16
  )


# ------------------------------------------------------------
# 2. Verificar los DDT
# ------------------------------------------------------------

sort(
  unique(
    datos_parcela$ddt
  )
)


# ------------------------------------------------------------
# 3. Ajustar regresión lineal por parcela
# ------------------------------------------------------------

tasas_altura <- datos_parcela %>%
  
  dplyr::group_by(
    bloque,
    genotipo,
    parcela
  ) %>%
  
  # Se requieren al menos 3 fechas diferentes
  dplyr::filter(
    dplyr::n_distinct(ddt) >= 3
  ) %>%
  
  dplyr::group_modify(
    
    ~ {
      
      # Modelo de regresión lineal
      modelo <- stats::lm(
        altura ~ ddt,
        data = .x
      )
      
      # Resumen del modelo
      resumen <- summary(modelo)
      
      # Extraer resultados
      data.frame(
        
        # Intercepto:
        # altura estimada cuando DDT = 0
        beta0 = unname(
          coef(modelo)[1]
        ),
        
        # Pendiente:
        # tasa de crecimiento en cm/día
        beta1 = unname(
          coef(modelo)[2]
        ),
        
        # Error estándar de la pendiente
        se_beta1 = resumen$coefficients[
          "ddt",
          "Std. Error"
        ],
        
        # Estadístico t de la pendiente
        t_beta1 = resumen$coefficients[
          "ddt",
          "t value"
        ],
        
        # Valor p de la pendiente
        p_beta1 = resumen$coefficients[
          "ddt",
          "Pr(>|t|)"
        ],
        
        # Coeficiente de determinación
        r2 = resumen$r.squared,
        
        # R2 ajustado
        r2_ajustado = resumen$adj.r.squared,
        
        # Error cuadrático medio
        rmse = sqrt(
          mean(
            residuals(modelo)^2
          )
        ),
        
        # Número de fechas utilizadas
        n_fechas = stats::nobs(modelo)
      )
    }
  ) %>%
  
  dplyr::ungroup()

# ------------------------------------------------------------
# 4. Mostrar resultados
# ------------------------------------------------------------

tasas_altura

write.csv(
  tasas_altura,
  file = file.path(
    Sys.getenv("USERPROFILE"),
    "Downloads",
    "tasas_altura2.csv"
  ),
  row.names = FALSE
)

# RESUMEN DE R2 - ALTURA ----------------------------------------

summary(
  tasas_altura$r2
)

# Distribución de R2

hist(
  tasas_altura$r2,
  breaks = 15,
  main = "Distribución de R² - Regresiones de altura",
  xlab = "R²"
)


# Parcelas con menor ajuste lineal

tasas_altura %>%
  
  dplyr::arrange(
    r2
  ) %>%
  
  dplyr::select(
    bloque,
    genotipo,
    parcela,
    beta1,
    r2,
    rmse,
    n_fechas
  ) %>%
  
  head(20)


# Proporción con R2 alto
#
# Se muestra como diagnóstico descriptivo.
# NO utilizar automáticamente para eliminar parcelas.

mean(
  tasas_altura$r2 >= 0.80
)

mean(
  tasas_altura$r2 >= 0.90
)


# REGRESIONES POR PARCELA - DIÁMETRO  TALLO ----------------------------

tasas_diametro_tallo <- datos_parcela %>%
  
  dplyr::group_by(
    bloque,
    genotipo,
    parcela
  ) %>%
  
  dplyr::filter(
    dplyr::n_distinct(dias) >= 3
  ) %>%
  
  dplyr::group_modify(
    
    ~ {
      
      modelo <- stats::lm(
        diametro_tallo ~ dias,
        data = .x
      )
      
      resumen <- summary(modelo)
      
      data.frame(
        
        beta0 = unname(
          coef(modelo)[1]
        ),
        
        beta1 = unname(
          coef(modelo)[2]
        ),
        
        se_beta1 = resumen$coefficients[
          "dias",
          "Std. Error"
        ],
        
        t_beta1 = resumen$coefficients[
          "dias",
          "t value"
        ],
        
        p_beta1 = resumen$coefficients[
          "dias",
          "Pr(>|t|)"
        ],
        
        r2 = resumen$r.squared,
        
        r2_ajustado = resumen$adj.r.squared,
        
        rmse = sqrt(
          mean(
            residuals(modelo)^2
          )
        ),
        
        n_fechas = stats::nobs(modelo)
      )
    }
  ) %>%
  
  dplyr::ungroup()


tasas_diametro_tallo


# RESUMEN DE R2 - DIÁMETRO TALLO --------------------------------------

summary(
  tasas_diametro_tallo$r2
)


hist(
  tasas_diametro_tallo$r2,
  breaks = 15,
  main = "Distribución de R² - Regresiones de diámetro tallo",
  xlab = "R²"
)


# Regresiones con menor ajuste

tasas_diametro_tallo %>%
  
  dplyr::arrange(
    r2
  ) %>%
  
  dplyr::select(
    bloque,
    genotipo,
    parcela,
    beta1,
    r2,
    rmse,
    n_fechas
  ) %>%
  
  head(20)


mean(
  tasas_diametro_tallo$r2 >= 0.80
)

mean(
  tasas_diametro_tallo$r2 >= 0.90
)

# REGRESIONES POR PARCELA - DIÁMETRO  COPA ----------------------------

tasas_diametro_copa <- datos_parcela %>%
  
  dplyr::group_by(
    bloque,
    genotipo,
    parcela
  ) %>%
  
  dplyr::filter(
    dplyr::n_distinct(dias) >= 3
  ) %>%
  
  dplyr::group_modify(
    
    ~ {
      
      modelo <- stats::lm(
        diametro_copa ~ dias,
        data = .x
      )
      
      resumen <- summary(modelo)
      
      data.frame(
        
        beta0 = unname(
          coef(modelo)[1]
        ),
        
        beta1 = unname(
          coef(modelo)[2]
        ),
        
        se_beta1 = resumen$coefficients[
          "dias",
          "Std. Error"
        ],
        
        t_beta1 = resumen$coefficients[
          "dias",
          "t value"
        ],
        
        p_beta1 = resumen$coefficients[
          "dias",
          "Pr(>|t|)"
        ],
        
        r2 = resumen$r.squared,
        
        r2_ajustado = resumen$adj.r.squared,
        
        rmse = sqrt(
          mean(
            residuals(modelo)^2
          )
        ),
        
        n_fechas = stats::nobs(modelo)
      )
    }
  ) %>%
  
  dplyr::ungroup()


tasas_diametro_copa


# RESUMEN DE R2 - DIÁMETRO COPA --------------------------------------

summary(
  tasas_diametro_copa$r2
)


hist(
  tasas_diametro_copa$r2,
  breaks = 15,
  main = "Distribución de R² - Regresiones de diámetro copa",
  xlab = "R²"
)


# Regresiones con menor ajuste

tasas_diametro_copa %>%
  
  dplyr::arrange(
    r2
  ) %>%
  
  dplyr::select(
    bloque,
    genotipo,
    parcela,
    beta1,
    r2,
    rmse,
    n_fechas
  ) %>%
  
  head(20)


mean(
  tasas_diametro_copa$r2 >= 0.80
)

mean(
  tasas_diametro_copa$r2 >= 0.90
)


# GRÁFICO DE PENDIENTES - ALTURA --------------------------------

ggplot2::ggplot(
  
  tasas_altura,
  
  ggplot2::aes(
    x = genotipo,
    y = beta1
  )
  
) +
  
  ggplot2::geom_boxplot(
    outlier.shape = NA
  ) +
  
  ggplot2::geom_jitter(
    width = 0.12,
    height = 0
  ) +
  
  ggplot2::labs(
    x = "Genotipo",
    y = expression(beta[1]~"(altura por día)"),
    title = "Tasa de crecimiento en altura por genotipo"
  ) +
  
  ggplot2::theme_bw()


# GRÁFICO DE PENDIENTES - DIÁMETRO TALLO ------------------------------

ggplot2::ggplot(
  
  tasas_diametro_tallo,
  
  ggplot2::aes(
    x = genotipo,
    y = beta1
  )
  
) +
  
  ggplot2::geom_boxplot(
    outlier.shape = NA
  ) +
  
  ggplot2::geom_jitter(
    width = 0.12,
    height = 0
  ) +
  
  ggplot2::labs(
    x = "Genotipo",
    y = expression(beta[1]~"(diámetro del tallo por día)"),
    title = "Tasa de crecimiento del diámetro del tallo por genotipo"
  ) +
  
  ggplot2::theme_bw()

#GRÁFICO DE PENDIENTES - DIÁMETRO COPA ------------------------------
  
  ggplot2::ggplot(
    
    tasas_diametro_copa,
    
    ggplot2::aes(
      x = genotipo,
      y = beta1
    )
    
  ) +
  
  ggplot2::geom_boxplot(
    outlier.shape = NA
  ) +
  
  ggplot2::geom_jitter(
    width = 0.12,
    height = 0
  ) +
  
  ggplot2::labs(
    x = "Genotipo",
    y = expression(beta[1]~"(diámetro de la copa por día)"),
    title = "Tasa de crecimiento del diámetro de la copa por genotipo"
  ) +
  
  ggplot2::theme_bw()



# ================================================================
# ANÁLISIS DE β1 - ALTURA
# ================================================================


# MODELO DBCA - ALTURA ------------------------------------------

# β1 = efecto de genotipo + efecto de bloque
#
# Debido a que existen algunos desbalances,
# usamos lm + ANOVA tipo II.

modelo_beta_altura <- stats::lm(
  
  beta1 ~
    bloque +
    genotipo,
  
  data = tasas_altura
)


summary(
  modelo_beta_altura
)


# ANOVA - ALTURA -------------------------------------------------

anova_beta_altura <- car::Anova(
  
  modelo_beta_altura,
  
  type = 2
)

anova_beta_altura


# SUPUESTO DE NORMALIDAD - ALTURA -------------------------------

residuos_beta_altura <- residuals(
  modelo_beta_altura
)


shapiro_beta_altura <- shapiro.test(
  residuos_beta_altura
)

shapiro_beta_altura


# QQ-PLOT - ALTURA ----------------------------------------------

qqnorm(
  residuos_beta_altura,
  main = "QQ-plot de residuos - β1 de altura"
)

qqline(
  residuos_beta_altura,
  col = "red",
  lwd = 2
)


# HISTOGRAMA - ALTURA -------------------------------------------

hist(
  residuos_beta_altura,
  breaks = 15,
  main = "Distribución de residuos - β1 altura",
  xlab = "Residuo"
)


# HOMOGENEIDAD DE VARIANZAS - ALTURA ---------------------------

levene_beta_altura <- car::leveneTest(
  
  beta1 ~ genotipo,
  
  data = tasas_altura
)

levene_beta_altura


# RESIDUOS VS AJUSTADOS - ALTURA -------------------------------

plot(
  fitted(modelo_beta_altura),
  residuos_beta_altura,
  xlab = "Valores ajustados",
  ylab = "Residuos",
  main = "Residuos vs ajustados - β1 altura"
)

abline(
  h = 0,
  col = "red",
  lwd = 2
)

# ATÍPICOS - ALTURA ---------------------------------------------

# Residuales studentizados.

tasas_altura_diag <- tasas_altura %>%
  
  dplyr::mutate(
    
    ajustado = fitted(
      modelo_beta_altura
    ),
    
    residuo = residuals(
      modelo_beta_altura
    ),
    
    residuo_student = rstudent(
      modelo_beta_altura
    ),
    
    cook = cooks.distance(
      modelo_beta_altura
    )
  )


# Potenciales atípicos:
# |residuo studentizado| > 3

atipicos_beta_altura <- tasas_altura_diag %>%
  
  dplyr::filter(
    abs(residuo_student) > 3
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(
      abs(residuo_student)
    )
  )

atipicos_beta_altura


# OBSERVACIONES INFLUYENTES - ALTURA ----------------------------

# Regla exploratoria:
#
# Cook > 4/n

limite_cook_altura <- 4 / nrow(
  tasas_altura_diag
)

limite_cook_altura


influyentes_altura <- tasas_altura_diag %>%
  
  dplyr::filter(
    cook > limite_cook_altura
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(cook)
  )

influyentes_altura


# MEDIAS AJUSTADAS - ALTURA -------------------------------------

emmeans_beta_altura <- emmeans::emmeans(
  
  modelo_beta_altura,
  
  ~ genotipo
)

emmeans_beta_altura


# TUKEY - ALTURA -------------------------------------------------

tukey_beta_altura <- emmeans::contrast(
  
  emmeans_beta_altura,
  
  method = "pairwise",
  
  adjust = "tukey"
)

tukey_beta_altura


# LETRAS DE TUKEY - ALTURA --------------------------------------

letras_beta_altura <- multcomp::cld(
  
  emmeans_beta_altura,
  
  adjust = "tukey",
  
  Letters = letters,
  
  alpha = 0.05
)


letras_beta_altura


letras_beta_altura_df <- as.data.frame(
  letras_beta_altura
) %>%
  
  dplyr::mutate(
    .group = trimws(.group)
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(emmean)
  )


letras_beta_altura_df


# ANÁLISIS DE β1 - DIÁMETRO TALLO---------


# MODELO DBCA - DIÁMETRO TALLO ----------------------------------------

modelo_beta_diametro_tallo <- stats::lm(
  
  beta1 ~
    bloque +
    genotipo,
  
  data = tasas_diametro_tallo
)


summary(
  modelo_beta_diametro_tallo
)


# ANOVA - DIÁMETRO TALLO ----------------------------------------------

anova_beta_diametro_tallo <- car::Anova(
  
  modelo_beta_diametro_tallo,
  
  type = 2
)

anova_beta_diametro_tallo


# NORMALIDAD - DIÁMETRO TALLO -----------------------------------------

residuos_beta_diametro_tallo <- residuals(
  modelo_beta_diametro_tallo
)


shapiro_beta_diametro_tallo <- shapiro.test(
  residuos_beta_diametro_tallo
)

shapiro_beta_diametro_tallo


# QQ-PLOT - DIÁMETRO TALLO --------------------------------------------

qqnorm(
  residuos_beta_diametro_tallo,
  main = "QQ-plot de residuos - β1 de diámetro tallo"
)

qqline(
  residuos_beta_diametro_tallo,
  col = "red",
  lwd = 2
)


# HISTOGRAMA - DIÁMETRO TALLO-----------------------------------------

hist(
  residuos_beta_diametro_tallo,
  breaks = 15,
  main = "Distribución de residuos - β1 diámetro tallo",
  xlab = "Residuo"
)


# HOMOGENEIDAD - DIÁMETRO TALLO---------------------------------------

levene_beta_diametro_tallo <- car::leveneTest(
  
  beta1 ~ genotipo,
  
  data = tasas_diametro_tallo
)

levene_beta_diametro_tallo


# RESIDUOS VS AJUSTADOS - DIÁMETRO TALLO -----------------------------

plot(
  fitted(modelo_beta_diametro_tallo),
  residuos_beta_diametro_tallo,
  xlab = "Valores ajustados",
  ylab = "Residuos",
  main = "Residuos vs ajustados - β1 diámetro tallo"
)

abline(
  h = 0,
  col = "red",
  lwd = 2
)


# ATÍPICOS - DIÁMETRO TALLO-------------------------------------------

tasas_diametro_tallo_diag <- tasas_diametro_tallo %>%
  
  dplyr::mutate(
    
    ajustado = fitted(
      modelo_beta_diametro_tallo
    ),
    
    residuo = residuals(
      modelo_beta_diametro_tallo
    ),
    
    residuo_student = rstudent(
      modelo_beta_diametro_tallo
    ),
    
    cook = cooks.distance(
      modelo_beta_diametro_tallo
    )
  )


atipicos_beta_diametro_tallo <- tasas_diametro_tallo_diag %>%
  
  dplyr::filter(
    abs(residuo_student) > 3
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(
      abs(residuo_student)
    )
  )

atipicos_beta_diametro_tallo


# OBSERVACIONES INFLUYENTES - DIÁMETRO TALLO --------------------------

limite_cook_diametro_tallo <- 4 / nrow(
  tasas_diametro_tallo_diag
)

limite_cook_diametro_tallo


influyentes_diametro_tallo <- tasas_diametro_tallo_diag %>%
  
  dplyr::filter(
    cook > limite_cook_diametro_tallo
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(cook)
  )

influyentes_diametro_tallo


# MEDIAS AJUSTADAS - DIÁMETRO TALLO  -----------------------------------

emmeans_beta_diametro_tallo <- emmeans::emmeans(
  
  modelo_beta_diametro_tallo,
  
  ~ genotipo
)

emmeans_beta_diametro_tallo


# TUKEY - DIÁMETRO TALLO   ----------------------------------------------

tukey_beta_diametro_tallo <- emmeans::contrast(
  
  emmeans_beta_diametro_tallo,
  
  method = "pairwise",
  
  adjust = "tukey"
)

tukey_beta_diametro_tallo


# LETRAS DE TUKEY - DIÁMETRO TALLO------------------------------------

letras_beta_diametro_tallo <- multcomp::cld(
  
  emmeans_beta_diametro_tallo,
  
  adjust = "tukey",
  
  Letters = letters,
  
  alpha = 0.05
)

letras_beta_diametro_tallo


letras_beta_diametro_tallo_df <- as.data.frame(
  letras_beta_diametro_tallo
) %>%
  
  dplyr::mutate(
    .group = trimws(.group)
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(emmean)
  )


letras_beta_diametro_tallo_df


# ANÁLISIS DE β1 - DIÁMETRO COPA---------


# MODELO DBCA - DIÁMETRO COPA ----------------------------------------

modelo_beta_diametro_copa <- stats::lm(
  
  beta1 ~
    bloque +
    genotipo,
  
  data = tasas_diametro_copa
)


summary(
  modelo_beta_diametro_copa
)


# ANOVA - DIÁMETRO COPA ----------------------------------------------

anova_beta_diametro_copa <- car::Anova(
  
  modelo_beta_diametro_copa,
  
  type = 2
)

anova_beta_diametro_copa


# NORMALIDAD - DIÁMETRO COPA -----------------------------------------

residuos_beta_diametro_copa <- residuals(
  modelo_beta_diametro_copa
)


shapiro_beta_diametro_copa <- shapiro.test(
  residuos_beta_diametro_copa
)

shapiro_beta_diametro_copa


# QQ-PLOT - DIÁMETRO COPA --------------------------------------------

qqnorm(
  residuos_beta_diametro_copa,
  main = "QQ-plot de residuos - β1 de diámetro copa"
)

qqline(
  residuos_beta_diametro_copa,
  col = "red",
  lwd = 2
)


# HISTOGRAMA - DIÁMETRO COPA-----------------------------------------

hist(
  residuos_beta_diametro_copa,
  breaks = 15,
  main = "Distribución de residuos - β1 diámetro copa",
  xlab = "Residuo"
)


# HOMOGENEIDAD - DIÁMETRO COPA---------------------------------------

levene_beta_diametro_copa <- car::leveneTest(
  
  beta1 ~ genotipo,
  
  data = tasas_diametro_copa
)

levene_beta_diametro_copa


# RESIDUOS VS AJUSTADOS - DIÁMETRO COPA -----------------------------

plot(
  fitted(modelo_beta_diametro_copa),
  residuos_beta_diametro_copa,
  xlab = "Valores ajustados",
  ylab = "Residuos",
  main = "Residuos vs ajustados - β1 diámetro copa"
)

abline(
  h = 0,
  col = "red",
  lwd = 2
)


# ATÍPICOS - DIÁMETRO COPA-------------------------------------------

tasas_diametro_copa_diag <- tasas_diametro_copa %>%
  
  dplyr::mutate(
    
    ajustado = fitted(
      modelo_beta_diametro_copa
    ),
    
    residuo = residuals(
      modelo_beta_diametro_copa
    ),
    
    residuo_student = rstudent(
      modelo_beta_diametro_copa
    ),
    
    cook = cooks.distance(
      modelo_beta_diametro_copa
    )
  )


atipicos_beta_diametro_copa <- tasas_diametro_copa_diag %>%
  
  dplyr::filter(
    abs(residuo_student) > 3
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(
      abs(residuo_student)
    )
  )

atipicos_beta_diametro_copa


# OBSERVACIONES INFLUYENTES - DIÁMETRO COPA --------------------------

limite_cook_diametro_copa <- 4 / nrow(
  tasas_diametro_copa_diag
)

limite_cook_diametro_copa


influyentes_diametro_copa <- tasas_diametro_copa_diag %>%
  
  dplyr::filter(
    cook > limite_cook_diametro_copa
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(cook)
  )

influyentes_diametro_copa


# MEDIAS AJUSTADAS - DIÁMETRO COPA  -----------------------------------

emmeans_beta_diametro_copa <- emmeans::emmeans(
  
  modelo_beta_diametro_copa,
  
  ~ genotipo
)

emmeans_beta_diametro_copa


# TUKEY - DIÁMETRO COPA   ----------------------------------------------

tukey_beta_diametro_copa <- emmeans::contrast(
  
  emmeans_beta_diametro_copa,
  
  method = "pairwise",
  
  adjust = "tukey"
)

tukey_beta_diametro_copa


# LETRAS DE TUKEY - DIÁMETRO COPA------------------------------------

letras_beta_diametro_copa <- multcomp::cld(
  
  emmeans_beta_diametro_copa,
  
  adjust = "tukey",
  
  Letters = letters,
  
  alpha = 0.05
)

letras_beta_diametro_copa


letras_beta_diametro_copa_df <- as.data.frame(
  letras_beta_diametro_copa
) %>%
  
  dplyr::mutate(
    .group = trimws(.group)
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(emmean)
  )


letras_beta_diametro_copa_df



# GRÁFICO - β1 Altura -------------------------------------
# ================================================================
# GRÁFICO DE CRECIMIENTO EN ALTURA
# REGRESIÓN LINEAL + GRUPOS DE TUKEY DE β1
# ================================================================

# PAQUETES -------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)


# ETIQUETAS DE GENOTIPOS -----------------------------------------

# Conversión:
#
# 1     -> UNR001
# 3     -> UNR003
# ...
# 20    -> UNR020
# selva -> La Selva

datos_parcela <- datos_parcela %>%
  
  dplyr::mutate(
    
    genotipo_original = as.character(genotipo),
    
    genotipo_num = suppressWarnings(
      as.numeric(genotipo_original)
    ),
    
    genotipo_label = dplyr::case_when(
      
      tolower(genotipo_original) == "selva" ~
        "La Selva",
      
      TRUE ~ paste0(
        "UNR",
        sprintf(
          "%03d",
          genotipo_num
        )
      )
    )
  )


# ORDEN DE LOS GENOTIPOS -----------------------------------------

orden_genotipos <- datos_parcela %>%
  
  dplyr::distinct(
    genotipo_original,
    genotipo_num,
    genotipo_label
  ) %>%
  
  dplyr::arrange(
    is.na(genotipo_num),
    genotipo_num
  ) %>%
  
  dplyr::pull(
    genotipo_label
  )



orden_genotipos



# Aplicar orden

datos_parcela <- datos_parcela %>%
  
  dplyr::mutate(
    
    genotipo_label = factor(
      genotipo_label,
      levels = orden_genotipos
    )
  )


# Convertir a data frame

letras_beta_altura_df <- as.data.frame(
  letras_beta_altura
) %>%
  
  dplyr::mutate(
    
    genotipo = as.character(genotipo),
    
    .group = trimws(.group)
  )


letras_beta_altura_df


# ================================================================
# DATOS PARA EL GRÁFICO
# ================================================================


# Para el gráfico:
#
# cada punto = media del genotipo entre bloques en una fecha.
#
# Las líneas NO unen puntos.
# Cada línea es una regresión lineal.

datos_grafico_altura <- datos_parcela %>%
  
  dplyr::group_by(
    genotipo_label,
    fecha,
    ddt
  ) %>%
  
  dplyr::summarise(
    
    n = sum(
      !is.na(altura)
    ),
    
    mean_height = mean(
      altura,
      na.rm = TRUE
    ),
    
    sd_height = sd(
      altura,
      na.rm = TRUE
    ),
    
    se_height = sd_height / sqrt(n),
    
    .groups = "drop"
  ) %>%
  
  dplyr::mutate(
    
    genotipo_label = factor(
      genotipo_label,
      levels = orden_genotipos
    )
  ) %>%
  
  dplyr::arrange(
    genotipo_label,
    ddt
  )


datos_grafico_altura


# ================================================================
# ASOCIAR GENOTIPOS CON LETRAS DE TUKEY
# ================================================================


# Tabla de correspondencia

tabla_etiquetas_genotipo <- datos_parcela %>%
  
  dplyr::distinct(
    genotipo,
    genotipo_label
  ) %>%
  
  dplyr::mutate(
    
    genotipo = as.character(genotipo),
    
    genotipo_label = as.character(genotipo_label)
  )


tabla_etiquetas_genotipo


# Unir las letras

letras_leyenda_altura <- letras_beta_altura_df %>%
  
  dplyr::left_join(
    
    tabla_etiquetas_genotipo,
    
    by = "genotipo"
  ) %>%
  
  dplyr::mutate(
    
    etiqueta_leyenda = paste0(
      genotipo_label,
      " (",
      .group,
      ")"
    )
  ) %>%
  
  dplyr::arrange(
    
    match(
      genotipo_label,
      orden_genotipos
    )
  )


letras_leyenda_altura

# VECTOR PARA LA LEYENDA -----------------------------------------

etiquetas_altura <- letras_leyenda_altura$etiqueta_leyenda

names(etiquetas_altura) <- letras_leyenda_altura$genotipo_label


etiquetas_altura


# ================================================================
# GRÁFICO FINAL
# ================================================================


grafico_regresion_altura_sig <- ggplot2::ggplot(
  
  datos_grafico_altura,
  
  ggplot2::aes(
    x = ddt,
    y = mean_height,
    color = genotipo_label
  )
  
) +
  
  # Puntos observados
  ggplot2::geom_point(
    size = 2.8,
    alpha = 0.95
  ) +
  
  # Recta de regresión lineal de cada genotipo
  ggplot2::geom_smooth(
    
    ggplot2::aes(
      group = genotipo_label
    ),
    
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.9,
    alpha = 0.9
  ) +
  
  # Colores y letras de Tukey en la leyenda
  ggplot2::scale_color_viridis_d(
    
    option = "turbo",
    end = 0.95,
    
    breaks = orden_genotipos,
    
    labels = etiquetas_altura[
      orden_genotipos
    ],
    
    drop = FALSE
  ) +
  
  # Eje X
  ggplot2::scale_x_continuous(
    
    breaks = c(
      119,
      79,
      44,
      16
    )
  ) +
  
  # Eje Y
  ggplot2::scale_y_continuous(
    
    breaks = seq(
      10,
      50,
      by = 10
    )
  ) +
  
  # Etiquetas
  ggplot2::labs(
    
    x = "Days after transplant",
    
    y = "Mean plant height (cm)",
    
    color = "Genotype",
    
    title = "A."
  ) +
  
  # Tema
  ggplot2::theme_classic(
    base_size = 13
  ) +
  
  ggplot2::theme(
    
    # Sin líneas de fondo
    panel.grid.major = ggplot2::element_blank(),
    
    panel.grid.minor = ggplot2::element_blank(),
    
    
    # Fondo blanco
    panel.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    
    
    # Ejes
    axis.line = ggplot2::element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.ticks = ggplot2::element_line(
      color = "black",
      linewidth = 0.5
    ),
    
    axis.ticks.length = grid::unit(
      2.5,
      "mm"
    ),
    
    
    # Texto de los ejes
    axis.title = ggplot2::element_text(
      size = 13,
      color = "black"
    ),
    
    axis.text = ggplot2::element_text(
      size = 11,
      color = "black"
    ),
    
    
    # Leyenda
    legend.position = "right",
    
    legend.background = ggplot2::element_blank(),
    
    legend.key = ggplot2::element_blank(),
    
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = ggplot2::element_text(
      size = 8.5
    ),
    
    
    # Etiqueta A.
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 14,
      hjust = 0,
      margin = ggplot2::margin(
        b = 5
      )
    ),
    
    
    # Márgenes
    plot.margin = ggplot2::margin(
      t = 8,
      r = 8,
      b = 8,
      l = 8
    )
  ) +
  
  # Apariencia de la leyenda
  ggplot2::guides(
    
    color = ggplot2::guide_legend(
      
      override.aes = list(
        linewidth = 1.2,
        size = 2.8
      )
    )
  )


# MOSTRAR GRÁFICO ------------------------------------------------

grafico_regresion_altura_sig



# GRÁFICO - β1 diametro tallo -------------------------------------
# ================================================================
# GRÁFICO DE CRECIMIENTO EN DIAMETRO DEL TALLO
# REGRESIÓN LINEAL + GRUPOS DE TUKEY DE β1
# ================================================================


# ETIQUETAS DE GENOTIPOS -----------------------------------------

# Conversión:
#
# 1     -> UNR001
# 3     -> UNR003
# ...
# 20    -> UNR020
# selva -> La Selva

datos_parcela <- datos_parcela %>%
  
  dplyr::mutate(
    
    genotipo_original = as.character(genotipo),
    
    genotipo_num = suppressWarnings(
      as.numeric(genotipo_original)
    ),
    
    genotipo_label = dplyr::case_when(
      
      tolower(genotipo_original) == "selva" ~
        "La Selva",
      
      TRUE ~ paste0(
        "UNR",
        sprintf(
          "%03d",
          genotipo_num
        )
      )
    )
  )


# ORDEN DE LOS GENOTIPOS -----------------------------------------

orden_genotipos <- datos_parcela %>%
  
  dplyr::distinct(
    genotipo_original,
    genotipo_num,
    genotipo_label
  ) %>%
  
  dplyr::arrange(
    is.na(genotipo_num),
    genotipo_num
  ) %>%
  
  dplyr::pull(
    genotipo_label
  )


orden_genotipos


# Aplicar orden

datos_parcela <- datos_parcela %>%
  
  dplyr::mutate(
    
    genotipo_label = factor(
      genotipo_label,
      levels = orden_genotipos
    )
  )



# Convertir a data frame

letras_beta_diametro_tallo_df <- as.data.frame(
  letras_beta_diametro_tallo
) %>%
  
  dplyr::mutate(
    
    genotipo = as.character(genotipo),
    
    .group = trimws(.group)
  )


letras_beta_diametro_tallo_df


# ================================================================
# DATOS PARA EL GRÁFICO
# ================================================================


# Para el gráfico:
#
# cada punto = media del genotipo entre bloques en una fecha.
#
# Las líneas NO unen puntos.
# Cada línea es una regresión lineal.

datos_grafico_diametro_tallo <- datos_parcela %>%
  
  dplyr::group_by(
    genotipo_label,
    fecha,
    ddt
  ) %>%
  
  dplyr::summarise(
    
    n = sum(
      !is.na(diametro_tallo)
    ),
    
    mean_stem_diameter = mean(  #cambiar aquiiiiii
      diametro_tallo,
      na.rm = TRUE
    ),
    
    sd_stem_diameter = sd(
      diametro_tallo,
      na.rm = TRUE
    ),
    
    se_stem_diameter = sd_stem_diameter / sqrt(n),
    
    .groups = "drop"
  ) %>%
  
  dplyr::mutate(
    
    genotipo_label = factor(
      genotipo_label,
      levels = orden_genotipos
    )
  ) %>%
  
  dplyr::arrange(
    genotipo_label,
    ddt
  )


datos_grafico_diametro_tallo


# ================================================================
# ASOCIAR GENOTIPOS CON LETRAS DE TUKEY
# ================================================================


# Tabla de correspondencia

tabla_etiquetas_genotipo <- datos_parcela %>%
  
  dplyr::distinct(
    genotipo,
    genotipo_label
  ) %>%
  
  dplyr::mutate(
    
    genotipo = as.character(genotipo),
    
    genotipo_label = as.character(genotipo_label)
  )


tabla_etiquetas_genotipo


# Unir las letras

letras_leyenda_diametro_tallo <- letras_beta_diametro_tallo_df %>%
  
  dplyr::left_join(
    
    tabla_etiquetas_genotipo,
    
    by = "genotipo"
  ) %>%
  
  dplyr::mutate(
    
    etiqueta_leyenda = paste0(
      genotipo_label,
      " (",
      .group,
      ")"
    )
  ) %>%
  
  dplyr::arrange(
    
    match(
      genotipo_label,
      orden_genotipos
    )
  )


letras_leyenda_diametro_tallo

# VECTOR PARA LA LEYENDA -----------------------------------------

etiquetas_diametro_tallo <- letras_leyenda_diametro_tallo$etiqueta_leyenda

names(etiquetas_diametro_tallo) <- letras_leyenda_diametro_tallo$genotipo_label


etiquetas_diametro_tallo


# ================================================================
# GRÁFICO FINAL
# ================================================================


grafico_regresion_diametro_tallo_sig <- ggplot2::ggplot(
  
  datos_grafico_diametro_tallo,
  
  ggplot2::aes(
    x = ddt,
    y = mean_stem_diameter,    #aqui tambiiiien
    color = genotipo_label
  )
  
) +
  
  # Puntos observados
  ggplot2::geom_point(
    size = 2.8,
    alpha = 0.95
  ) +
  
  # Recta de regresión lineal de cada genotipo
  ggplot2::geom_smooth(
    
    ggplot2::aes(
      group = genotipo_label
    ),
    
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.9,
    alpha = 0.9
  ) +
  
  # Colores y letras de Tukey en la leyenda
  ggplot2::scale_color_viridis_d(
    
    option = "turbo",
    end = 0.95,
    
    breaks = orden_genotipos,
    
    labels = etiquetas_diametro_tallo[
      orden_genotipos
    ],
    
    drop = FALSE
  ) +
  
  # Eje X
  ggplot2::scale_x_continuous(
    
    breaks = c(
      119,
      79,
      44,
      16
    )
  ) +
  
  # Eje Y
  ggplot2::scale_y_continuous(
    breaks = seq(4, 20, by = 5),
    limits = c(4, 20)
  ) +
  
  # Etiquetas
  ggplot2::labs(
    
    x = "Days after transplant",
    
    y = "Mean stem diameter (mm)",
    
    color = "Genotype",
    
    title = "B."
  ) +
  
  # Tema
  ggplot2::theme_classic(
    base_size = 13
  ) +
  
  ggplot2::theme(
    
    # Sin líneas de fondo
    panel.grid.major = ggplot2::element_blank(),
    
    panel.grid.minor = ggplot2::element_blank(),
    
    
    # Fondo blanco
    panel.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    
    
    # Ejes
    axis.line = ggplot2::element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.ticks = ggplot2::element_line(
      color = "black",
      linewidth = 0.5
    ),
    
    axis.ticks.length = grid::unit(
      2.5,
      "mm"
    ),
    
    
    # Texto de los ejes
    axis.title = ggplot2::element_text(
      size = 13,
      color = "black"
    ),
    
    axis.text = ggplot2::element_text(
      size = 11,
      color = "black"
    ),
    
    
    # Leyenda
    legend.position = "right",
    
    legend.background = ggplot2::element_blank(),
    
    legend.key = ggplot2::element_blank(),
    
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = ggplot2::element_text(
      size = 8.5
    ),
    
    
    # Etiqueta A.
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 14,
      hjust = 0,
      margin = ggplot2::margin(
        b = 5
      )
    ),
    
    
    # Márgenes
    plot.margin = ggplot2::margin(
      t = 8,
      r = 8,
      b = 8,
      l = 8
    )
  ) +
  
  # Apariencia de la leyenda
  ggplot2::guides(
    
    color = ggplot2::guide_legend(
      
      override.aes = list(
        linewidth = 1.2,
        size = 2.8
      )
    )
  )


# MOSTRAR GRÁFICO ------------------------------------------------

grafico_regresion_diametro_tallo_sig


# GRÁFICO - β1 diametro copa -------------------------------------
# ================================================================
# GRÁFICO DE CRECIMIENTO EN DIAMETRO DE LA COPA
# REGRESIÓN LINEAL + GRUPOS DE TUKEY DE β1
# ================================================================


# ETIQUETAS DE GENOTIPOS -----------------------------------------

# Conversión:
#
# 1     -> UNR001
# 3     -> UNR003
# ...
# 20    -> UNR020
# selva -> La Selva

datos_parcela <- datos_parcela %>%
  
  dplyr::mutate(
    
    genotipo_original = as.character(genotipo),
    
    genotipo_num = suppressWarnings(
      as.numeric(genotipo_original)
    ),
    
    genotipo_label = dplyr::case_when(
      
      tolower(genotipo_original) == "selva" ~
        "La Selva",
      
      TRUE ~ paste0(
        "UNR",
        sprintf(
          "%03d",
          genotipo_num
        )
      )
    )
  )


# ORDEN DE LOS GENOTIPOS -----------------------------------------

orden_genotipos <- datos_parcela %>%
  
  dplyr::distinct(
    genotipo_original,
    genotipo_num,
    genotipo_label
  ) %>%
  
  dplyr::arrange(
    is.na(genotipo_num),
    genotipo_num
  ) %>%
  
  dplyr::pull(
    genotipo_label
  )


orden_genotipos


# Aplicar orden

datos_parcela <- datos_parcela %>%
  
  dplyr::mutate(
    
    genotipo_label = factor(
      genotipo_label,
      levels = orden_genotipos
    )
  )



# Convertir a data frame

letras_beta_diametro_copa_df <- as.data.frame(
  letras_beta_diametro_copa
) %>%
  
  dplyr::mutate(
    
    genotipo = as.character(genotipo),
    
    .group = trimws(.group)
  )


letras_beta_diametro_copa_df


# ================================================================
# DATOS PARA EL GRÁFICO
# ================================================================


# Para el gráfico:
#
# cada punto = media del genotipo entre bloques en una fecha.
#
# Las líneas NO unen puntos.
# Cada línea es una regresión lineal.

datos_grafico_diametro_copa <- datos_parcela %>%
  
  dplyr::group_by(
    genotipo_label,
    fecha,
    ddt
  ) %>%
  
  dplyr::summarise(
    
    n = sum(
      !is.na(diametro_copa)
    ),
    
    mean_canopy_diameter = mean(  #cambiar aquiiiiii
      diametro_copa,
      na.rm = TRUE
    ),
    
    sd_canopy_diameter = sd(   #cambiar aquiii
      diametro_copa,
      na.rm = TRUE
    ),
    
    se_canopy_diameter = sd_canopy_diameter / sqrt(n),    #cambiar aqui
    
    .groups = "drop"
  ) %>%
  
  dplyr::mutate(
    
    genotipo_label = factor(
      genotipo_label,
      levels = orden_genotipos
    )
  ) %>%
  
  dplyr::arrange(
    genotipo_label,
    ddt
  )


datos_grafico_diametro_copa


# ================================================================
# ASOCIAR GENOTIPOS CON LETRAS DE TUKEY
# ================================================================


# Tabla de correspondencia

tabla_etiquetas_genotipo <- datos_parcela %>%
  
  dplyr::distinct(
    genotipo,
    genotipo_label
  ) %>%
  
  dplyr::mutate(
    
    genotipo = as.character(genotipo),
    
    genotipo_label = as.character(genotipo_label)
  )


tabla_etiquetas_genotipo


# Unir las letras

letras_leyenda_diametro_copa <- letras_beta_diametro_copa_df %>%
  
  dplyr::left_join(
    
    tabla_etiquetas_genotipo,
    
    by = "genotipo"
  ) %>%
  
  dplyr::mutate(
    
    etiqueta_leyenda = paste0(
      genotipo_label,
      " (",
      .group,
      ")"
    )
  ) %>%
  
  dplyr::arrange(
    
    match(
      genotipo_label,
      orden_genotipos
    )
  )


letras_leyenda_diametro_copa

# VECTOR PARA LA LEYENDA -----------------------------------------

etiquetas_diametro_copa <- letras_leyenda_diametro_copa$etiqueta_leyenda

names(etiquetas_diametro_copa) <- letras_leyenda_diametro_copa$genotipo_label


etiquetas_diametro_copa


# ================================================================
# GRÁFICO FINAL
# ================================================================


grafico_regresion_diametro_copa_sig <- ggplot2::ggplot(
  
  datos_grafico_diametro_copa,
  
  ggplot2::aes(
    x = ddt,
    y = mean_canopy_diameter,    #aqui tambiiiien
    color = genotipo_label
  )
  
) +
  
  # Puntos observados
  ggplot2::geom_point(
    size = 2.8,
    alpha = 0.95
  ) +
  
  # Recta de regresión lineal de cada genotipo
  ggplot2::geom_smooth(
    
    ggplot2::aes(
      group = genotipo_label
    ),
    
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.9,
    alpha = 0.9
  ) +
  
  # Colores y letras de Tukey en la leyenda
  ggplot2::scale_color_viridis_d(
    
    option = "turbo",
    end = 0.95,
    
    breaks = orden_genotipos,
    
    labels = etiquetas_diametro_copa[
      orden_genotipos
    ],
    
    drop = FALSE
  ) +
  
  # Eje X
  ggplot2::scale_x_continuous(
    
    breaks = c(
      119,
      79,
      44,
      16
    )
  ) +
  
  # Eje Y
  ggplot2::scale_y_continuous(
    breaks = seq(10, 60, by = 10),
    limits = c(10, 60)
  ) +
  
  # Etiquetas
  ggplot2::labs(
    
    x = "Days after transplant",
    
    y = "Mean canopy diameter (cm)",
    
    color = "Genotype",
    
    title = "C."
  ) +
  
  # Tema
  ggplot2::theme_classic(
    base_size = 13
  ) +
  
  ggplot2::theme(
    
    # Sin líneas de fondo
    panel.grid.major = ggplot2::element_blank(),
    
    panel.grid.minor = ggplot2::element_blank(),
    
    
    # Fondo blanco
    panel.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    
    
    # Ejes
    axis.line = ggplot2::element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.ticks = ggplot2::element_line(
      color = "black",
      linewidth = 0.5
    ),
    
    axis.ticks.length = grid::unit(
      2.5,
      "mm"
    ),
    
    
    # Texto de los ejes
    axis.title = ggplot2::element_text(
      size = 13,
      color = "black"
    ),
    
    axis.text = ggplot2::element_text(
      size = 11,
      color = "black"
    ),
    
    
    # Leyenda
    legend.position = "right",
    
    legend.background = ggplot2::element_blank(),
    
    legend.key = ggplot2::element_blank(),
    
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = ggplot2::element_text(
      size = 8.5
    ),
    
    
    # Etiqueta A.
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 14,
      hjust = 0,
      margin = ggplot2::margin(
        b = 5
      )
    ),
    
    
    # Márgenes
    plot.margin = ggplot2::margin(
      t = 8,
      r = 8,
      b = 8,
      l = 8
    )
  ) +
  
  # Apariencia de la leyenda
  ggplot2::guides(
    
    color = ggplot2::guide_legend(
      
      override.aes = list(
        linewidth = 1.2,
        size = 2.8
      )
    )
  )


# MOSTRAR GRÁFICO ------------------------------------------------

grafico_regresion_diametro_copa_sig


###################################################


library(readxl)
df<- read_excel("Test2.xlsx")

dput(head(df))
str(df)

# ================================================================
# ANÁLISIS DE TASAS DE CRECIMIENTO MEDIANTE PENDIENTES
# DBCA: Genotipo + Bloque
#
# β1 de Altura   = tasa de crecimiento en altura por día
# β1 de Diámetro = tasa de crecimiento en diámetro por día
# ================================================================

# PAQUETES -------------------------------------------------------

# Instalar solamente si es necesario:
#
#install.packages(c(
  # "dplyr",
  # "tidyr",
  #"ggplot2",
  # "janitor",
  #"missForest",
  #"car",
  #"emmeans",
  #"multcomp",
  #"multcompView",
  #"broom"
#))

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(missForest)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(broom)



# LIMPIEZA DE NOMBRES --------------------------------------------

df_limpio <- janitor::clean_names(df)

names(df_limpio)

# Los nombres esperados después de clean_names() son:
#
# fecha
# evaluacion
# gen
# blq
# planta
# densidad de espinas


# VERIFICACIÓN DE COLUMNAS ---------------------------------------

columnas_necesarias <- c(
  "fecha",
  "evaluacion",
  "gen",
  "blq",
  "planta",
  "densidad_de_espinas"
)

columnas_faltantes <- setdiff(
  columnas_necesarias,
  names(df_limpio)
)

if(length(columnas_faltantes) > 0){
  
  stop(
    paste(
      "Faltan las siguientes columnas:",
      paste(columnas_faltantes, collapse = ", ")
    )
  )
}


# PREPARACIÓN DE VARIABLES ---------------------------------------

datos <- df_limpio %>%
  
  dplyr::mutate(
    
    fecha = as.Date(fecha),
    
    evaluacion = as.integer(evaluacion),
    
    genotipo = factor(gen),
    
    bloque = factor(blq),
    
    planta = factor(planta),
    
    densidad_de_espinas= as.numeric(densidad_de_espinas)
    
    
  ) %>%
  
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    planta,
    densidad_de_espinas
  )

str(datos)
summary(datos)

# FECHAS DE EVALUACIÓN -------------------------------------------

tabla_fechas <- datos %>%
  
  dplyr::distinct(
    evaluacion,
    fecha
  ) %>%
  
  dplyr::arrange(
    evaluacion
  )

tabla_fechas


# Número de observaciones por fecha

datos %>%
  dplyr::count(
    evaluacion,
    fecha
  )


# DÍAS TRANSCURRIDOS ---------------------------------------------

# Se utilizan días reales y NO simplemente 1, 2, 3, 4.
#
# Así β1 tendrá unidades:
#
# Altura   -> unidades de altura / día
# Diámetro -> unidades de diámetro / día

datos <- datos %>%
  
  dplyr::mutate(
    
    dias = as.numeric(
      fecha - min(fecha, na.rm = TRUE)
    )
  )


datos %>%
  
  dplyr::distinct(
    evaluacion,
    fecha,
    dias
  ) %>%
  
  dplyr::arrange(
    evaluacion
  )


# IDENTIFICACIÓN DE PARCELA --------------------------------------

# Unidad experimental:
#
# Bloque × Genotipo
#
# Las plantas dentro de una parcela son submuestras.

datos <- datos %>%
  
  dplyr::mutate(
    
    parcela = interaction(
      bloque,
      genotipo,
      drop = TRUE,
      sep = "_"
    ),
    
    id_planta = interaction(
      bloque,
      genotipo,
      planta,
      drop = TRUE,
      sep = "_"
    )
  )


# VERIFICACIÓN DE DUPLICADOS -------------------------------------

# Una planta debería tener una observación por fecha.

duplicados <- datos %>%
  
  dplyr::count(
    fecha,
    id_planta,
    name = "n"
  ) %>%
  
  dplyr::filter(
    n > 1
  )

duplicados


if(nrow(duplicados) > 0){
  
  datos_duplicados <- datos %>%
    
    dplyr::semi_join(
      duplicados,
      by = c(
        "fecha",
        "id_planta"
      )
    ) %>%
    
    dplyr::arrange(
      fecha,
      genotipo,
      bloque,
      planta
    )
  
  print(
    datos_duplicados,
    n = Inf
  )
}


# VERIFICACIÓN DEL DBCA ------------------------------------------

# Número de bloques presentes para cada genotipo.

balance_genotipos <- datos %>%
  
  dplyr::distinct(
    genotipo,
    bloque
  ) %>%
  
  dplyr::count(
    genotipo,
    name = "numero_bloques"
  )

balance_genotipos

# Mostrar genotipos que no tienen 5 bloques

balance_genotipos %>%
  
  dplyr::filter(
    numero_bloques != 5
  )


# VALORES FALTANTES ----------------------------------------------

colSums(
  is.na(datos)
)

datos_faltantes <- datos %>%
  
  dplyr::mutate(
    fila = dplyr::row_number()
  ) %>%
  
  dplyr::filter(
    is.na(densidad_de_espinas) |
      is.na(densidad_de_espinas)
  )

datos_faltantes

# REVISIÓN DE VALORES EXTREMOS ----------------------------------

# DENSIDAD DE ESPINAS

datos %>%
  
  dplyr::arrange(
    dplyr::desc(densidad_de_espinas)
  ) %>%
  
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    planta,
    densidad_de_espinas
  ) %>%
  
  head(20)




# Detectar la densidad de hojas extremadamente altos

valores_sospechosos <- datos %>%
  
  dplyr::filter(
    densidad_de_espinas > 100
  ) %>%
  
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    planta,
    densidad_de_espinas
  )

valores_sospechosos

if(nrow(valores_sospechosos) > 0){
  
  warning(
    paste(
      
    )
  )
}


# IMPORTANTE:
#
# Si comprueba en la libreta que 634 realmente corresponde,
# por ejemplo, a 6.34:
#
# datos$diametro[datos$diametro == 634] <- 6.34
#
# NO ejecutar sin verificar el dato original.


# IMPUTACIÓN CON MISSFOREST --------------------------------------

# Guardar posiciones originales de los NA.

NA_densidad_de_espinas <- which(
  is.na(datos$densidad_de_espinas)
)



# Base para missForest

datos_mf <- datos %>%
  
  dplyr::transmute(
    
    evaluacion = factor(evaluacion),
    
    genotipo = factor(genotipo),
    
    bloque = factor(bloque),
    
    planta = factor(planta),
    
    densidad_de_espinas = as.numeric(densidad_de_espinas),
    
    
    
  ) %>%
  
  as.data.frame()


colSums(
  is.na(datos_mf)
)


# Ejecutar imputación solamente si existen NA.

if(anyNA(datos_mf)){
  
  set.seed(12345)
  
  imp_mf <- missForest::missForest(
    
    xmis = datos_mf,
    
    ntree = 1000,
    
    maxiter = 10,
    
    verbose = TRUE
  )
  
  
  # Error OOB
  
  cat(
    "\nError OOB de missForest:\n"
  )
  
  print(
    imp_mf$OOBerror
  )
  
  
  # Base imputada
  
  datos_imp <- datos
  
  datos_imp$densidad_de_espinas <- as.numeric(
    imp_mf$ximp$densidad_de_espinas
  )
  
  
  
} else {
  
  datos_imp <- datos
  
  message(
    "No existen valores faltantes. No fue necesario aplicar missForest."
  )
}


# VERIFICACIÓN DE LA IMPUTACIÓN ---------------------------------

colSums(
  is.na(datos_imp)
)


# Mostrar valores imputados de densidad de espinas

if(length(NA_densidad_de_espinas) > 0){
  
  imputacion_densidad_de_espinas <- data.frame(
    
    fila = NA_densidad_de_espinas,
    
    original = datos$densidad_de_espinas[NA_densidad_de_espinas],
    
    imputado = datos_imp$densidad_de_espinas[NA_densidad_de_espinas]
  )
  
  print(
    imputacion_densidad_de_espinas
  )
}




# CONSTRUCCIÓN DE LA BASE DE PARCELAS ---------------------------

# Las dos plantas son submuestras.
#
# Para cada Bloque × Genotipo × Fecha
# calculamos la media de las plantas.

datos_parcela <- datos_imp %>%
  
  dplyr::group_by(
    bloque,
    genotipo,
    parcela,
    fecha,
    evaluacion,
    dias
  ) %>%
  
  dplyr::summarise(
    
    n_plantas = dplyr::n(),
    
    densidad_de_espinas = mean(
      densidad_de_espinas,
      na.rm = TRUE
    ),
    
    densidad_de_espinas = mean(
      densidad_de_espinas,
      na.rm = TRUE
    ),
    
    .groups = "drop"
    
  ) %>%
  
  dplyr::mutate(
    
    bloque = factor(bloque),
    
    genotipo = factor(genotipo),
    
    parcela = factor(parcela)
  )


str(datos_parcela)

summary(datos_parcela)


# VERIFICACIÓN DE LA BASE DE PARCELAS ---------------------------

# Número de plantas utilizadas en cada media

table(
  datos_parcela$n_plantas
)


# Parcelas-fecha con número diferente de dos plantas

datos_parcela %>%
  
  dplyr::filter(
    n_plantas != 2
  )


# Número de fechas disponibles por parcela

numero_fechas_parcela <- datos_parcela %>%
  
  dplyr::count(
    parcela,
    name = "n_fechas"
  )


table(
  numero_fechas_parcela$n_fechas
)


# Parcelas que no tienen cuatro fechas

parcelas_incompletas <- numero_fechas_parcela %>%
  
  dplyr::filter(
    n_fechas != 4
  )

parcelas_incompletas


# GRÁFICO DE CURVAS DE CRECIMIENTO - DENSIDAD DE ESPINAS ---------------------

# Cada línea representa una parcela experimental.

ggplot2::ggplot(
  
  datos_parcela,
  
  ggplot2::aes(
    x = dias,
    y = densidad_de_espinas,
    group = parcela
  )
  
) +
  
  ggplot2::geom_line(
    alpha = 0.45
  ) +
  
  ggplot2::geom_point(
    alpha = 0.65
  ) +
  
  ggplot2::facet_wrap(
    ~ genotipo
  ) +
  
  ggplot2::labs(
    x = "Días desde la primera evaluación",
    y = "Densidad de espinas",
    title = "Curvas de crecimiento de densidad de espinas"
  ) +
  
  ggplot2::theme_bw()





# REGRESIONES POR PARCELA - DENSIDAD DE ESPINAS ------------------------------

# ============================================================
# REGRESIONES POR PARCELA - DENSIDAD DE ESPINAS
# Días después del trasplante (DDT)
# Fecha de trasplante: 02/05/2026
#
# Modelo:
# altura = β0 + β1 * ddt
#
# β1 = tasa de crecimiento promedio en cm/día
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 1. Crear variable DDT
# ------------------------------------------------------------
# Si la variable "dias" actualmente contiene:
# 0, 28, 63, 103
#
# Entonces los días después del trasplante serán:
# 53, 81, 116, 156
#
# porque la primera evaluación fue a los 53 DDT.
#
#
#
#
# MODIFICAR AQUÍ EL NÚMERO DE DÍAS A EVALUACIÓN
# YO LE PUSE 53 PERO NO SÉ SI ESTA BIEN.
datos_parcela <- datos_parcela %>%
  dplyr::mutate(
    ddt = dias + 16
  )


# ------------------------------------------------------------
# 2. Verificar los DDT
# ------------------------------------------------------------

sort(
  unique(
    datos_parcela$ddt
  )
)


# ------------------------------------------------------------
# 3. Ajustar regresión lineal por parcela
# ------------------------------------------------------------

tasas_densidad_de_espinas <- datos_parcela %>%
  
  dplyr::group_by(
    bloque,
    genotipo,
    parcela
  ) %>%
  
  # Se requieren al menos 3 fechas diferentes
  dplyr::filter(
    dplyr::n_distinct(ddt) >= 3
  ) %>%
  
  dplyr::group_modify(
    
    ~ {
      
      # Modelo de regresión lineal
      modelo <- stats::lm(
        densidad_de_espinas ~ ddt,
        data = .x
      )
      
      # Resumen del modelo
      resumen <- summary(modelo)
      
      # Extraer resultados
      data.frame(
        
        # Intercepto:
        # altura estimada cuando DDT = 0
        beta0 = unname(
          coef(modelo)[1]
        ),
        
        # Pendiente:
        # tasa de crecimiento en cm/día
        beta1 = unname(
          coef(modelo)[2]
        ),
        
        # Error estándar de la pendiente
        se_beta1 = resumen$coefficients[
          "ddt",
          "Std. Error"
        ],
        
        # Estadístico t de la pendiente
        t_beta1 = resumen$coefficients[
          "ddt",
          "t value"
        ],
        
        # Valor p de la pendiente
        p_beta1 = resumen$coefficients[
          "ddt",
          "Pr(>|t|)"
        ],
        
        # Coeficiente de determinación
        r2 = resumen$r.squared,
        
        # R2 ajustado
        r2_ajustado = resumen$adj.r.squared,
        
        # Error cuadrático medio
        rmse = sqrt(
          mean(
            residuals(modelo)^2
          )
        ),
        
        # Número de fechas utilizadas
        n_fechas = stats::nobs(modelo)
      )
    }
  ) %>%
  
  dplyr::ungroup()

# ------------------------------------------------------------
# 4. Mostrar resultados
# ------------------------------------------------------------

tasas_densidad_de_espinas

write.csv(
  tasas_densidad_de_espinas,
  file = file.path(
    Sys.getenv("USERPROFILE"),
    "Downloads",
    "tasas_densidad_de_espinas.csv"
  ),
  row.names = FALSE
)

# RESUMEN DE R2 - ALTURA ----------------------------------------

summary(
  tasas_densidad_de_espinas$r2
)

# Distribución de R2

hist(
  tasas_densidad_de_espinas$r2,
  breaks = 15,
  main = "Distribución de R² - Regresiones de densidad de espinas",
  xlab = "R²"
)


# Parcelas con menor ajuste lineal

tasas_densidad_de_espinas %>%
  
  dplyr::arrange(
    r2
  ) %>%
  
  dplyr::select(
    bloque,
    genotipo,
    parcela,
    beta1,
    r2,
    rmse,
    n_fechas
  ) %>%
  
  head(20)


# Proporción con R2 alto
#
# Se muestra como diagnóstico descriptivo.
# NO utilizar automáticamente para eliminar parcelas.

mean(
  tasas_densidad_de_espinas$r2 >= 0.80
)

mean(
  tasas_densidad_de_espinas$r2 >= 0.90
)


# REGRESIONES POR PARCELA - DENSIDAD DE ESPINAS----------------------------

tasas_densidad_de_espinas <- datos_parcela %>%
  
  dplyr::group_by(
    bloque,
    genotipo,
    parcela
  ) %>%
  
  dplyr::filter(
    dplyr::n_distinct(dias) >= 3
  ) %>%
  
  dplyr::group_modify(
    
    ~ {
      
      modelo <- stats::lm(
        densidad_de_espinas ~ dias,
        data = .x
      )
      
      resumen <- summary(modelo)
      
      data.frame(
        
        beta0 = unname(
          coef(modelo)[1]
        ),
        
        beta1 = unname(
          coef(modelo)[2]
        ),
        
        se_beta1 = resumen$coefficients[
          "dias",
          "Std. Error"
        ],
        
        t_beta1 = resumen$coefficients[
          "dias",
          "t value"
        ],
        
        p_beta1 = resumen$coefficients[
          "dias",
          "Pr(>|t|)"
        ],
        
        r2 = resumen$r.squared,
        
        r2_ajustado = resumen$adj.r.squared,
        
        rmse = sqrt(
          mean(
            residuals(modelo)^2
          )
        ),
        
        n_fechas = stats::nobs(modelo)
      )
    }
  ) %>%
  
  dplyr::ungroup()


tasas_densidad_de_espinas


# RESUMEN DE R2 - DENSIDAD DE ESPINAS --------------------------------------

summary(
  tasas_densidad_de_espinas$r2
)


hist(
  tasas_densidad_de_espinas$r2,
  breaks = 15,
  main = "Distribución de R² - Regresiones de densidad de espinas",
  xlab = "R²"
)


# Regresiones con menor ajuste

tasas_densidad_de_espinas %>%
  
  dplyr::arrange(
    r2
  ) %>%
  
  dplyr::select(
    bloque,
    genotipo,
    parcela,
    beta1,
    r2,
    rmse,
    n_fechas
  ) %>%
  
  head(20)


mean(
  tasas_densidad_de_espinas$r2 >= 0.80
)

mean(
  tasas_densidad_de_espinas$r2 >= 0.90
)


# GRÁFICO DE PENDIENTES - DENSIDAD DE ESPINAS --------------------------------

ggplot2::ggplot(
  
  tasas_densidad_de_espinas,
  
  ggplot2::aes(
    x = genotipo,
    y = beta1
  )
  
) +
  
  ggplot2::geom_boxplot(
    outlier.shape = NA
  ) +
  
  ggplot2::geom_jitter(
    width = 0.12,
    height = 0
  ) +
  
  ggplot2::labs(
    x = "Genotipo",
    y = expression(beta[1]~"(densidad de espinas)"),
    title = "Tasa de densidad de espinas  por genotipo"
  ) +
  
  ggplot2::theme_bw()


# ================================================================
# ANÁLISIS DE β1 - DENSIDAD DE ESPINAS
# ================================================================


# MODELO DBCA - DENSIDAD DE ESPINAS ------------------------------------------

# β1 = efecto de genotipo + efecto de bloque
#
# Debido a que existen algunos desbalances,
# usamos lm + ANOVA tipo II.

modelo_beta_densidad_de_espinas <- stats::lm(
  
  beta1 ~
    bloque +
    genotipo,
  
  data = tasas_densidad_de_espinas
)


summary(
  modelo_beta_densidad_de_espinas  
)


# ANOVA - DENSIDAD DE ESPINAS -------------------------------------------------

anova_beta_densidad_de_espinas  <- car::Anova(
  
  modelo_beta_densidad_de_espinas,
  
  type = 2
)

anova_beta_densidad_de_espinas


# SUPUESTO DE NORMALIDAD - DENSIDAD DE ESPINAS -------------------------------

residuos_beta_densidad_de_espinas <- residuals(
  modelo_beta_densidad_de_espinas
)


shapiro_beta_densidad_de_espinas <- shapiro.test(
  residuos_beta_densidad_de_espinas
)

shapiro_beta_densidad_de_espinas


# QQ-PLOT - DENSIDAD DE ESPINAS ----------------------------------------------

qqnorm(
  residuos_beta_densidad_de_espinas,
  main = "QQ-plot de residuos - β1 de densidad de espinas"
)

qqline(
  residuos_beta_densidad_de_espinas,
  col = "red",
  lwd = 2
)


# HISTOGRAMA - DENSIDAD DE ESPINAS  -------------------------------------------

hist(
  residuos_beta_densidad_de_espinas,
  breaks = 15,
  main = "Distribución de residuos - β1 densidad de espinas",
  xlab = "Residuo"
)


# HOMOGENEIDAD DE VARIANZAS -  -----DENSIDAD DE ESPINAS----------------------

levene_beta_densidad_de_espinas <- car::leveneTest(
  
  beta1 ~ genotipo,
  
  data = tasas_densidad_de_espinas
)

levene_beta_densidad_de_espinas


# RESIDUOS VS AJUSTADOS - DENSIDAD DE ESPINAS -------------------------------

plot(
  fitted(modelo_beta_densidad_de_espinas),
  residuos_beta_densidad_de_espinas,
  xlab = "Valores ajustados",
  ylab = "Residuos",
  main = "Residuos vs ajustados - β1 densidad de espinas"
)

abline(
  h = 0,
  col = "red",
  lwd = 2
)

# ATÍPICOS - DENSIDAD DE ESPINAS ---------------------------------------------

# Residuales studentizados.

tasas_densidad_de_espinas_diag <- tasas_densidad_de_espinas %>%
  
  dplyr::mutate(
    
    ajustado = fitted(
      modelo_beta_densidad_de_espinas
    ),
    
    residuo = residuals(
      modelo_beta_densidad_de_espinas
    ),
    
    residuo_student = rstudent(
      modelo_beta_densidad_de_espinas
    ),
    
    cook = cooks.distance(
      modelo_beta_densidad_de_espinas
    )
  )


# Potenciales atípicos:
# |residuo studentizado| > 3

atipicos_beta_densidad_de_espinas <- tasas_densidad_de_espinas_diag %>%
  
  dplyr::filter(
    abs(residuo_student) > 3
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(
      abs(residuo_student)
    )
  )

atipicos_beta_densidad_de_espinas


# OBSERVACIONES INFLUYENTES - DENSIDAD DE ESPINAS  ----------------------------

# Regla exploratoria:
#
# Cook > 4/n

limite_cook_densidad_de_espinas <- 4 / nrow(
  tasas_densidad_de_espinas_diag
)

limite_cook_densidad_de_espinas


influyentes_densidad_de_espinas <- tasas_densidad_de_espinas_diag %>%
  
  dplyr::filter(
    cook > limite_cook_densidad_de_espinas
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(cook)
  )

influyentes_densidad_de_espinas


# MEDIAS AJUSTADAS - DENSIDAD DE ESPINAS -------------------------------------

emmeans_beta_densidad_de_espinas <- emmeans::emmeans(
  
  modelo_beta_densidad_de_espinas,
  
  ~ genotipo
)

emmeans_beta_densidad_de_espinas


# TUKEY - DENSIDAD DE ESPINAS  -------------------------------------------------

tukey_beta_densidad_de_espinas <- emmeans::contrast(
  
  emmeans_beta_densidad_de_espinas,
  
  method = "pairwise",
  
  adjust = "tukey"
)

tukey_beta_densidad_de_espinas


# LETRAS DE TUKEY - DENSIDAD DE ESPINAS --------------------------------------

letras_beta_densidad_de_espinas <- multcomp::cld(
  
  emmeans_beta_densidad_de_espinas,
  
  adjust = "tukey",
  
  Letters = letters,
  
  alpha = 0.05
)


letras_beta_densidad_de_espinas


letras_beta_densidad_de_espinas_df <- as.data.frame(
  letras_beta_densidad_de_espinas
) %>%
  
  dplyr::mutate(
    .group = trimws(.group)
  ) %>%
  
  dplyr::arrange(
    dplyr::desc(emmean)
  )


letras_beta_densidad_de_espinas_df




# GRÁFICO - β1 densidad de espinas-------------------------------------
# ================================================================
# GRÁFICO DE CRECIMIENTO EN ALTURA
# REGRESIÓN LINEAL + GRUPOS DE TUKEY DE β1
# ================================================================

# PAQUETES -------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)

# ETIQUETAS DE GENOTIPOS -----------------------------------------



datos_parcela <- datos_parcela %>%
  
  dplyr::mutate(
    
    genotipo_original = as.character(genotipo),
    
    genotipo_num = suppressWarnings(
      as.numeric(genotipo_original)
    ),
    
    genotipo_label = dplyr::case_when(
      
      tolower(genotipo_original) == "selva" ~
        "La Selva",
      
      TRUE ~ paste0(
        "UNR",
        sprintf(
          "%03d",
          genotipo_num
        )
      )
    )
  )

colnames(datos_parcela)
# ORDEN DE LOS GENOTIPOS -----------------------------------------

orden_genotipos <- datos_parcela %>%
  
  dplyr::distinct(
    genotipo_original,
    genotipo_num,
    genotipo_label
  ) %>%
  
  dplyr::arrange(
    is.na(genotipo_num),
    genotipo_num
  ) %>%
  
  dplyr::pull(
    genotipo_label
  )


orden_genotipos


# Aplicar orden

datos_parcela <- datos_parcela %>%
  
  dplyr::mutate(
    
    genotipo_label = factor(
      genotipo_label,
      levels = orden_genotipos
    )
  )



# Convertir a data frame

letras_beta_densidad_de_espinas_df <- as.data.frame(
  letras_beta_densidad_de_espinas
) %>%
  
  dplyr::mutate(
    
    genotipo = as.character(genotipo),
    
    .group = trimws(.group)
  )


letras_beta_densidad_de_espinas_df


# ================================================================
# DATOS PARA EL GRÁFICO
# ================================================================


# Para el gráfico:
#
# cada punto = media del genotipo entre bloques en una fecha.
#
# Las líneas NO unen puntos.
# Cada línea es una regresión lineal.

datos_grafico_densidad_de_espinas <- datos_parcela %>%
  
  dplyr::group_by(
    genotipo_label,
    fecha,
    ddt
  ) %>%
  
  dplyr::summarise(
    
    n = sum(
      !is.na(densidad_de_espinas)
    ),
    
    mean_height = mean(
      densidad_de_espinas,
      na.rm = TRUE
    ),
    
    sd_height = sd(
      densidad_de_espinas,
      na.rm = TRUE
    ),
    
    se_height = sd_height / sqrt(n),
    
    .groups = "drop"
  ) %>%
  
  dplyr::mutate(
    
    genotipo_label = factor(
      genotipo_label,
      levels = orden_genotipos
    )
  ) %>%
  
  dplyr::arrange(
    genotipo_label,
    ddt
  )


datos_grafico_densidad_de_espinas


# ================================================================
# ASOCIAR GENOTIPOS CON LETRAS DE TUKEY
# ================================================================


# Tabla de correspondencia

tabla_etiquetas_genotipo <- datos_parcela %>%
  
  dplyr::distinct(
    genotipo,
    genotipo_label
  ) %>%
  
  dplyr::mutate(
    
    genotipo = as.character(genotipo),
    
    genotipo_label = as.character(genotipo_label)
  )


tabla_etiquetas_genotipo


# Unir las letras

letras_leyenda_densidad_de_espinas <- letras_beta_densidad_de_espinas_df %>%
  
  dplyr::left_join(
    
    tabla_etiquetas_genotipo,
    
    by = "genotipo"
  ) %>%
  
  dplyr::mutate(
    
    etiqueta_leyenda = paste0(
      genotipo_label,
      " (",
      .group,
      ")"
    )
  ) %>%
  
  dplyr::arrange(
    
    match(
      genotipo_label,
      orden_genotipos
    )
  )


letras_leyenda_densidad_de_espinas

# VECTOR PARA LA LEYENDA -----------------------------------------

etiquetas_densidad_de_espinas <- letras_leyenda_densidad_de_espinas$etiqueta_leyenda

names(etiquetas_densidad_de_espinas) <- letras_leyenda_densidad_de_espinas$genotipo_label


etiquetas_densidad_de_espinas


# ================================================================
# GRÁFICO FINAL
# ================================================================


grafico_regresion_densidad_de_espinas_sig <- ggplot2::ggplot(
  
  datos_grafico_densidad_de_espinas,
  
  ggplot2::aes(
    x = ddt,
    y = mean_height,
    color = genotipo_label
  )
  
) +
  
  # Puntos observados
  ggplot2::geom_point(
    size = 2.8,
    alpha = 0.95
  ) +
  
  # Recta de regresión lineal de cada genotipo
  ggplot2::geom_smooth(
    
    ggplot2::aes(
      group = genotipo_label
    ),
    
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.9,
    alpha = 0.9
  ) +
  
  # Colores y letras de Tukey en la leyenda
  ggplot2::scale_color_viridis_d(
    
    option = "turbo",
    end = 0.95,
    
    breaks = orden_genotipos,
    
    labels = etiquetas_densidad_de_espinas[
      orden_genotipos
    ],
    
    drop = FALSE
  ) +
  
  # Eje X
  ggplot2::scale_x_continuous(
    
    breaks = c(
      119,
      79,
      44,
      16
    )
  ) +
  
  # Eje Y
  ggplot2::scale_y_continuous(
    
    breaks = seq(
      20,
      150,
      by = 20
    )
  ) +
  
  # Etiquetas
  ggplot2::labs(
    
    x = "Days after transplant",
    
    y = "Number of prickles per cm²",
    
    color = "Genotype",
    
    title = "D."
  ) +
  
  # Tema
  ggplot2::theme_classic(
    base_size = 13
  ) +
  
  ggplot2::theme(
    
    # Sin líneas de fondo
    panel.grid.major = ggplot2::element_blank(),
    
    panel.grid.minor = ggplot2::element_blank(),
    
    
    # Fondo blanco
    panel.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    
    
    # Ejes
    axis.line = ggplot2::element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.ticks = ggplot2::element_line(
      color = "black",
      linewidth = 0.5
    ),
    
    axis.ticks.length = grid::unit(
      2.5,
      "mm"
    ),
    
    
    # Texto de los ejes
    axis.title = ggplot2::element_text(
      size = 13,
      color = "black"
    ),
    
    axis.text = ggplot2::element_text(
      size = 11,
      color = "black"
    ),
    
    
    # Leyenda
    legend.position = "right",
    
    legend.background = ggplot2::element_blank(),
    
    legend.key = ggplot2::element_blank(),
    
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = ggplot2::element_text(
      size = 8.5
    ),
    
    
    # Etiqueta A.
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 14,
      hjust = 0,
      margin = ggplot2::margin(
        b = 5
      )
    ),
    
    
    # Márgenes
    plot.margin = ggplot2::margin(
      t = 8,
      r = 8,
      b = 8,
      l = 8
    )
  ) +
  
  # Apariencia de la leyenda
  ggplot2::guides(
    
    color = ggplot2::guide_legend(
      
      override.aes = list(
        linewidth = 1.2,
        size = 2.8
      )
    )
  )


# MOSTRAR GRÁFICO ------------------------------------------------

grafico_regresion_densidad_de_espinas_sig















# GRÁFICO MULTIPANEL - ALTURA + DIÁMETRO TALLO + DIAMETRO COPA + DENSIDAD DE ESPINAS -------------------------

# Instalar solamente si es necesario:
# install.packages("patchwork")

library(patchwork)


# PANEL A - ALTURA -----------------------------------------------

panel_altura <- grafico_regresion_altura_sig +
  
  ggplot2::theme(
    
    legend.position = "right",
    
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 9.5
    ),
    
    legend.text = ggplot2::element_text(
      size = 7.5
    ),
    
    legend.key.height = grid::unit(
      0.34,
      "cm"
    ),
    
    legend.key.width = grid::unit(
      0.50,
      "cm"
    ),
    
    legend.spacing.y = grid::unit(
      0.02,
      "cm"
    ),
    
    legend.margin = ggplot2::margin(
      l = 3,
      r = 5
    )
  )


# PANEL B - DIÁMETRO TALLO ---------------------------------------------

panel_diametro_tallo <- grafico_regresion_diametro_tallo_sig +
  
  ggplot2::theme(
    
    legend.position = "right",
    
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 9.5
    ),
    
    legend.text = ggplot2::element_text(
      size = 7.5
    ),
    
    legend.key.height = grid::unit(
      0.34,
      "cm"
    ),
    
    legend.key.width = grid::unit(
      0.50,
      "cm"
    ),
    
    legend.spacing.y = grid::unit(
      0.02,
      "cm"
    ),
    
    legend.margin = ggplot2::margin(
      l = 3,
      r = 5
    )
  )


# PANEL c - DIAMETRO COPA -----------------------------------------------

panel_diametro_copa <- grafico_regresion_diametro_copa_sig +
  
  ggplot2::theme(
    
    legend.position = "right",
    
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 9.5
    ),
    
    legend.text = ggplot2::element_text(
      size = 7.5
    ),
    
    legend.key.height = grid::unit(
      0.34,
      "cm"
    ),
    
    legend.key.width = grid::unit(
      0.50,
      "cm"
    ),
    
    legend.spacing.y = grid::unit(
      0.02,
      "cm"
    ),
    
    legend.margin = ggplot2::margin(
      l = 3,
      r = 5
    )
  )


# PANEL D - DENSIDAD DE ESPINAS -----------------------------------------------

panel_densidad_de_espinas <- grafico_regresion_densidad_de_espinas_sig +
  
  ggplot2::theme(
    
    legend.position = "right",
    
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 9.5
    ),
    
    legend.text = ggplot2::element_text(
      size = 7.5
    ),
    
    legend.key.height = grid::unit(
      0.34,
      "cm"
    ),
    
    legend.key.width = grid::unit(
      0.50,
      "cm"
    ),
    
    legend.spacing.y = grid::unit(
      0.02,
      "cm"
    ),
    
    legend.margin = ggplot2::margin(
      l = 3,
      r = 5
    )
  )



# CONSTRUIR MULTIPANEL -------------------------------------------

grafico_multipanel_sig <- (
  (panel_altura | panel_diametro_tallo) /
    (panel_diametro_copa | panel_densidad_de_espinas)
) + 
  patchwork::plot_layout(
    guides = "keep"
  )




# 1. Agranda las letras de la leyenda en todo el multipanel de golpe
grafico_multipanel_sig <- grafico_multipanel_sig & 
  theme(
    legend.text = element_text(size = 11),  # Cambia el tamaño de los nombres de los genotipos
    legend.title = element_text(size = 12)  # Cambia el tamaño del título "Genotype"
  )
# MOSTRAR FIGURA -------------------------------------------------

grafico_multipanel_sig


# GUARDAR MULTIPANEL EN ALTA RESOLUCIÓN (600 DPI)
ggplot2::ggsave(
  filename = "grafico_multipanel_lulo.png", 
  plot = grafico_multipanel_sig,
  width = 12,      # Ancho en pulgadas (ideal para 2 columnas en formato carta/A4)
  height = 10,     # Alto en pulgadas (proporcionado para las 2 filas)
  dpi = 600,       # Máxima resolución para publicaciones científicas y tesis
  type = "cairo"   # Mejora el suavizado de líneas y fuentes (opcional, requiere paquete cairo)
)


ggplot2::ggsave(
  filename = "grafico_multipanel_lulo.pdf",  # Cambias la extensión a .pdf
  plot = grafico_multipanel_sig,
  width = 12,      # Ancho en pulgadas
  height = 10      # Alto en pulgadas
)
