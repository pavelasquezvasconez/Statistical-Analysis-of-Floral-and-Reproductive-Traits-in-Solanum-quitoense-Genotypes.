library(readxl)

df<- read_excel("D:/02 de junio del 2026/UDENAR/Orientados/2026 Freddy y Diana/Test6.xlsx")
dput(df)

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
  "diametro_copa",
  "area_foliar_total",
  "iaf"
  
  
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
    
    clorofilometro_spad = as.numeric(clorofilometro_spad),
    
    numero_de_hojas = as.numeric(numero_de_hojas),
    
    af = as.numeric(area_foliar_total)

    
  ) %>%
  
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    planta,
    numero_de_hojas,
    clorofilometro_spad,
    af
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
    is.na(numero_de_hojas) |
      is.na(clorofilometro_spad)
  )

datos_faltantes

# REVISIÓN DE VALORES EXTREMOS ----------------------------------

# clorofilometro_spad

datos %>%
  
  dplyr::arrange(
    dplyr::desc(clorofilometro_spad)
  ) %>%
  
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    clorofilometro_spad,
    
    
  ) %>%
  
  head(20)
 

#    numero_de_hojas
datos %>%
  
  dplyr::arrange(
    dplyr::desc(numero_de_hojas)
  ) %>%
  
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    planta,
    numero_de_hojas
  
  ) %>%
  
  head(20)

# IMPUTACIÓN CON MISSFOREST --------------------------------------

# Guardar posiciones originales de los NA.

NA_numero_de_hojas <- which(
  is.na(datos$numero_de_hojas)
)

# Base para missForest

datos_mf <- datos %>%
  
  dplyr::transmute(
    
    evaluacion = factor(evaluacion),
    
    genotipo = factor(genotipo),
    
    bloque = factor(bloque),
    
    planta = factor(planta),
    
    numero_de_hojas = as.numeric(numero_de_hojas)
    
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
  
  datos_imp$numero_de_hojas <- as.numeric(
    imp_mf$ximp$numero_de_hojas
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

if(length(NA_numero_de_hojas) > 0){
  
  imputacion_altura <- data.frame(
    
    fila = NA_numero_de_hojas,
    
    original = datos$numero_de_hojas[NA_numero_de_hojas],
    
    imputado = datos_imp$numero_de_hojas[NA_numero_de_hojas]
  )
  
  print(
    imputacion_altura
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
    
    numero_de_hojas = mean(
      numero_de_hojas,
      na.rm = TRUE
    ),
    
    clorofilometro_spad = mean(
      clorofilometro_spad,
      na.rm = TRUE
    ),
    
    
    .groups = "drop"
    
  ) %>%
  
  dplyr::mutate(
    
    bloque = factor(bloque),
    
    genotipo = factor(genotipo),
    
    parcela = factor(parcela)
  )

dput(datos_parcela)

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


# GRÁFICO DE CURVAS DE CRECIMIENTO - clorofilometro_spad ---------------------

# Cada línea representa una parcela experimental.

ggplot2::ggplot(
  
  datos_parcela,
  
  ggplot2::aes(
    x = dias,
    y = clorofilometro_spad,
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
    y = "SPAD Clorofil de parcela",
    title = "Curvas de crecimiento en SPAD Clorofila"
  ) +
  
  ggplot2::theme_bw()


# GRÁFICO DE CURVAS DE CRECIMIENTO - Número de hojas -------------------

ggplot2::ggplot(
  
  datos_parcela,
  
  ggplot2::aes(
    x = dias,
    y = numero_de_hojas,
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
    y = "numero_de_hojas medio de parcela",
    title = "Curvas de crecimiento de numero_de_hojas"
  ) +
  
  ggplot2::theme_bw()

# REGRESIONES POR PARCELA - numero_de_hojas ------------------------------

# ============================================================
# REGRESIONES POR PARCELA - numero_de_hojas
# Días después del trasplante (DDT)
# Fecha de trasplante: 26/03/2026
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

datos_parcela$ddt

tasas_n_hojas <- datos_parcela %>%
  
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
        numero_de_hojas ~ ddt,
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

# Mostrar todas las filas y columnas en la consola
tasas_n_hojas[order(tasas_n_hojas$r2), ]

dput(tasas_n_hojas[order(tasas_n_hojas$r2), ])

# RESUMEN DE R2 - Numero de hojas ----------------------------------------

summary(
  tasas_n_hojas$r2
)

# Distribución de R2

hist(
  tasas_n_hojas$r2,
  breaks = 15,
  main = "Distribución de R² - Regresiones de número de hojas",
  xlab = "R²"
)

# Por el comportamiento de la variable optamos por modelos mixtos ------
# El r2 fué muy bajito para todos, no hay un comportamiento lineal 
# Esto se debe probablemente a senecencia, podas, etc.
#
#
#
#### aqui ----------------
# 
#
# -----------------

# 51. LIBRARIES FOR LONGITUDINAL ANALYSIS------------------

paquetes <- c(
  "dplyr",
  "ggplot2",
  "nlme",
  "emmeans",
  "multcomp",
  "multcompView"
)

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  install.packages(
    faltantes,
    repos = "https://cloud.r-project.org"
  )
}

invisible(
  lapply(paquetes, library, character.only = TRUE)
)

# 52. PREPARE LONGITUDINAL DATA------------------

if (!exists("datos_parcela")) {
  stop("No existe el objeto datos_parcela.")
}

columnas_modelo <- c(
  "bloque",
  "genotipo",
  "parcela",
  "fecha",
  "evaluacion",
  "dias",
  "numero_de_hojas"
)

if (!all(columnas_modelo %in% names(datos_parcela))) {
  stop("Faltan columnas necesarias en datos_parcela.")
}

hojas_mixto <- datos_parcela %>%
  dplyr::mutate(
    bloque = factor(bloque),
    genotipo = factor(genotipo),
    parcela = factor(
      paste(bloque, genotipo, sep = "_")
    ),
    evaluacion = factor(evaluacion),
    fecha = as.Date(fecha),
    dias = as.numeric(dias),
    numero_de_hojas = as.numeric(numero_de_hojas),
    tiempo_30 = dias / 30,
    ddt_real = as.numeric(
      fecha - as.Date("2026-03-26")
    )
  ) %>%
  dplyr::arrange(parcela, dias) %>%
  droplevels()

# 53. VERIFY DATA STRUCTURE------------------

if (anyNA(
  hojas_mixto[, c(
    "bloque", "genotipo", "parcela",
    "evaluacion", "dias", "numero_de_hojas"
  )]
)) {
  stop("Existen NA en las variables del modelo.")
}

duplicados <- hojas_mixto %>%
  dplyr::count(parcela, evaluacion) %>%
  dplyr::filter(n > 1)

if (nrow(duplicados) > 0) {
  print(duplicados)
  stop("Hay más de un registro por parcela y evaluación.")
}

tabla_tiempo <- hojas_mixto %>%
  dplyr::distinct(
    evaluacion, fecha, dias, ddt_real
  ) %>%
  dplyr::arrange(dias)

print(tabla_tiempo)

if (nrow(tabla_tiempo) != 4) {
  stop("Verifique las cuatro fechas de evaluación.")
}

cat("\nNúmero de genotipos:",
    dplyr::n_distinct(hojas_mixto$genotipo))

cat("\nNúmero de bloques:",
    dplyr::n_distinct(hojas_mixto$bloque))

cat("\nNúmero de parcelas:",
    dplyr::n_distinct(hojas_mixto$parcela))

cat("\nNúmero de observaciones:",
    nrow(hojas_mixto), "\n")

# 54. CREATE OUTPUT DIRECTORY------------------

carpeta <- "Resultados_Numero_Hojas_LMM"

dir.create(
  carpeta,
  showWarnings = FALSE,
  recursive = TRUE
)

# 55. DESCRIPTIVE STATISTICS------------------

descriptivos <- hojas_mixto %>%
  dplyr::group_by(genotipo, evaluacion) %>%
  dplyr::summarise(
    n = dplyr::n(),
    media = mean(numero_de_hojas),
    desviacion = sd(numero_de_hojas),
    error_estandar = desviacion / sqrt(n),
    minimo = min(numero_de_hojas),
    maximo = max(numero_de_hojas),
    .groups = "drop"
  )

print(descriptivos, n = Inf)

write.csv(
  descriptivos,
  file.path(carpeta, "01_Descriptive_statistics.csv"),
  row.names = FALSE
)

# 56. MODEL CONTROL SETTINGS------------------

control_modelo <- nlme::lmeControl(
  opt = "optim",
  msMaxIter = 300,
  msMaxEval = 500,
  niterEM = 50
)

formula_modelo <- numero_de_hojas ~
  bloque + genotipo * evaluacion

# 57. BASE RANDOM INTERCEPT MODEL------------------

modelo_base <- nlme::lme(
  fixed = formula_modelo,
  random = ~ 1 | parcela,
  data = hojas_mixto,
  method = "REML",
  control = control_modelo
)

# 58. CONTINUOUS AR(1) CORRELATION------------------

modelo_car1 <- try(
  update(
    modelo_base,
    correlation = nlme::corCAR1(
      form = ~ tiempo_30 | parcela
    )
  ),
  silent = TRUE
)

# 59. HETEROGENEOUS RESIDUAL VARIANCES------------------

modelo_hetero <- try(
  update(
    modelo_base,
    weights = nlme::varIdent(
      form = ~ 1 | evaluacion
    )
  ),
  silent = TRUE
)

# 60. AR(1) WITH HETEROGENEOUS VARIANCES------------------

modelo_car1_hetero <- try(
  update(
    modelo_base,
    correlation = nlme::corCAR1(
      form = ~ tiempo_30 | parcela
    ),
    weights = nlme::varIdent(
      form = ~ 1 | evaluacion
    )
  ),
  silent = TRUE
)

# 61. COLLECT SUCCESSFUL MODELS------------------

modelos <- list(
  Intercepto_aleatorio = modelo_base
)

if (inherits(modelo_car1, "lme")) {
  modelos$CAR1 <- modelo_car1
}

if (inherits(modelo_hetero, "lme")) {
  modelos$Heterogeneo <- modelo_hetero
}

if (inherits(modelo_car1_hetero, "lme")) {
  modelos$CAR1_Heterogeneo <- modelo_car1_hetero
}

# 62. MODEL SELECTION BY AIC------------------

tabla_aic <- data.frame(
  Modelo = names(modelos),
  AIC = sapply(modelos, AIC),
  BIC = sapply(modelos, BIC),
  logLik = sapply(
    modelos,
    function(x) as.numeric(logLik(x))
  ),
  row.names = NULL
) %>%
  dplyr::arrange(AIC) %>%
  dplyr::mutate(
    Delta_AIC = AIC - min(AIC)
  )

print(tabla_aic, row.names = FALSE)

nombre_seleccionado <- tabla_aic$Modelo[1]

modelo_completo <- modelos[[nombre_seleccionado]]

cat(
  "\nEstructura de covarianza seleccionada:",
  nombre_seleccionado, "\n"
)

write.csv(
  tabla_aic,
  file.path(carpeta, "02_Model_selection_AIC.csv"),
  row.names = FALSE
)

# 63. TEST GENOTYPE BY TIME INTERACTION------------------

modelo_completo_ML <- update(
  modelo_completo,
  method = "ML"
)

modelo_aditivo_ML <- update(
  modelo_completo,
  fixed = numero_de_hojas ~
    bloque + genotipo + evaluacion,
  method = "ML"
)

prueba_interaccion <- anova(
  modelo_aditivo_ML,
  modelo_completo_ML
)

cat("\nTEST OF GENOTYPE BY TIME INTERACTION\n")
print(prueba_interaccion)

p_interaccion <- as.numeric(
  prueba_interaccion[["p-value"]][2]
)

if (length(p_interaccion) != 1 ||
    !is.finite(p_interaccion)) {
  stop("No fue posible obtener el p de interacción.")
}

alfa <- 0.05

interaccion_significativa <- p_interaccion < alfa

cat(
  "\nInteraction p-value:",
  p_interaccion, "\n"
)

# 64. SELECT FINAL FIXED EFFECTS------------------

if (interaccion_significativa) {
  
  modelo_final <- modelo_completo
  
  cat(
    "\nSignificant interaction.",
    "\nGenotype comparisons will be made",
    "within each assessment.\n"
  )
  
} else {
  
  modelo_final <- update(
    modelo_aditivo_ML,
    method = "REML"
  )
  
  cat(
    "\nNon-significant interaction.",
    "\nGenotype main effects will be compared.\n"
  )
}

# 65. FINAL MIXED-MODEL ANOVA------------------

anova_final <- anova(
  modelo_final,
  type = "marginal"
)

cat("\nFINAL MIXED MODEL ANOVA\n")
print(anova_final)

tabla_anova <- data.frame(
  Efecto = rownames(anova_final),
  as.data.frame(anova_final),
  row.names = NULL,
  check.names = FALSE
)

write.csv(
  tabla_anova,
  file.path(carpeta, "03_Mixed_model_ANOVA.csv"),
  row.names = FALSE
)

capture.output(
  summary(modelo_final),
  file = file.path(
    carpeta,
    "04_Final_model_summary.txt"
  )
)

# 66. NORMALIZED RESIDUALS------------------

diagnosticos <- hojas_mixto %>%
  dplyr::mutate(
    ajustado = as.numeric(
      fitted(modelo_final)
    ),
    residuo = as.numeric(
      resid(modelo_final, type = "response")
    ),
    residuo_normalizado = as.numeric(
      resid(modelo_final, type = "normalized")
    )
  )

# 67. SHAPIRO-WILK TEST------------------

prueba_shapiro <- shapiro.test(
  diagnosticos$residuo_normalizado
)

cat("\nSHAPIRO-WILK TEST\n")
print(prueba_shapiro)

# 68. RESIDUAL QQ PLOT------------------

grafico_qq <- ggplot(
  diagnosticos,
  aes(sample = residuo_normalizado)
) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    x = "Theoretical quantiles",
    y = "Observed quantiles",
    title = "Normal Q-Q plot"
  ) +
  theme_bw(base_size = 12)

print(grafico_qq)

ggsave(
  file.path(carpeta, "05_Residual_QQ.pdf"),
  grafico_qq,
  width = 7,
  height = 6
)

# 69. RESIDUALS VS FITTED VALUES------------------

grafico_residuos <- ggplot(
  diagnosticos,
  aes(
    x = ajustado,
    y = residuo_normalizado
  )
) +
  geom_point(alpha = 0.6) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "blue"
  ) +
  labs(
    x = "Fitted values",
    y = "Normalized residuals",
    title = "Residuals vs fitted values"
  ) +
  theme_bw(base_size = 12)

print(grafico_residuos)

ggsave(
  file.path(carpeta, "06_Residuals_fitted.pdf"),
  grafico_residuos,
  width = 7,
  height = 6
)

# 70. CHECK RESIDUAL VARIANCE BY ASSESSMENT------------------

tabla_varianza <- diagnosticos %>%
  dplyr::group_by(evaluacion) %>%
  dplyr::summarise(
    n = dplyr::n(),
    media_residual = mean(residuo_normalizado),
    sd_residual = sd(residuo_normalizado),
    varianza_residual = var(residuo_normalizado),
    .groups = "drop"
  )

print(tabla_varianza)

grafico_varianza <- ggplot(
  diagnosticos,
  aes(
    x = evaluacion,
    y = residuo_normalizado
  )
) +
  geom_boxplot(fill = "grey85") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    x = "Assessment",
    y = "Normalized residuals",
    title = "Residual variance by assessment"
  ) +
  theme_bw(base_size = 12)

print(grafico_varianza)

ggsave(
  file.path(carpeta, "07_Residual_variance.pdf"),
  grafico_varianza,
  width = 7,
  height = 6
)

# 71. TEMPORAL AUTOCORRELATION CHECK------------------

acf_modelo <- nlme::ACF(
  modelo_final,
  resType = "normalized",
  maxLag = 3
)

cat("\nNORMALIZED RESIDUAL AUTOCORRELATION\n")
print(acf_modelo)

pdf(
  file.path(carpeta, "08_Residual_ACF.pdf"),
  width = 7,
  height = 6
)

plot(acf_modelo)

dev.off()

# 72. IDENTIFY LARGE STANDARDIZED RESIDUALS------------------

residuos_extremos <- diagnosticos %>%
  dplyr::filter(
    abs(residuo_normalizado) > 3
  ) %>%
  dplyr::select(
    bloque,
    genotipo,
    parcela,
    evaluacion,
    numero_de_hojas,
    ajustado,
    residuo_normalizado
  )

cat("\nOBSERVATIONS TO REVIEW\n")
print(residuos_extremos, n = Inf)

write.csv(
  residuos_extremos,
  file.path(carpeta, "09_Residuals_to_review.csv"),
  row.names = FALSE
)

# 73. ESTIMATED MARGINAL MEANS BY ASSESSMENT------------------

medias_fecha <- emmeans::emmeans(
  modelo_final,
  ~ genotipo | evaluacion
)

tabla_medias_fecha <- as.data.frame(
  summary(medias_fecha, infer = c(TRUE, TRUE))
)

cat("\nESTIMATED MEANS BY ASSESSMENT\n")
print(tabla_medias_fecha, row.names = FALSE)

write.csv(
  tabla_medias_fecha,
  file.path(carpeta, "10_Estimated_means_by_date.csv"),
  row.names = FALSE
)

# 74. TUKEY MULTIPLE COMPARISONS------------------

if (interaccion_significativa) {
  
  # Tukey comparisons within each assessment
  
  medias_comparacion <- emmeans::emmeans(
    modelo_final,
    ~ genotipo | evaluacion
  )
  
} else {
  
  # Genotype main-effect comparisons
  
  medias_comparacion <- emmeans::emmeans(
    modelo_final,
    ~ genotipo
  )
}

comparaciones_tukey <- pairs(
  medias_comparacion,
  adjust = "tukey"
)

tabla_tukey <- as.data.frame(
  summary(
    comparaciones_tukey,
    infer = c(TRUE, TRUE)
  )
)

cat("\nTUKEY MULTIPLE COMPARISONS\n")
print(tabla_tukey, row.names = FALSE)

write.csv(
  tabla_tukey,
  file.path(carpeta, "11_Tukey_comparisons.csv"),
  row.names = FALSE
)

# 75. TUKEY LETTER GROUPING------------------

grupos_tukey <- multcomp::cld(
  medias_comparacion,
  adjust = "tukey",
  alpha = alfa,
  Letters = letters,
  sort = FALSE
)

tabla_grupos <- as.data.frame(grupos_tukey) %>%
  dplyr::mutate(
    .group = trimws(.group)
  ) %>%
  dplyr::arrange(
    dplyr::desc(emmean)
  )

cat("\nTUKEY LETTER GROUPING\n")
print(tabla_grupos, row.names = FALSE)

write.csv(
  tabla_grupos,
  file.path(carpeta, "12_Tukey_letter_groups.csv"),
  row.names = FALSE
)

# 76. GENOTYPE COMPARISONS AGAINST LA SELVA------------------

control_selva <- match(
  "selva",
  levels(hojas_mixto$genotipo)
)

if (!is.na(control_selva)) {
  
  comparaciones_selva <- emmeans::contrast(
    medias_comparacion,
    method = "trt.vs.ctrl",
    ref = control_selva,
    adjust = "dunnett"
  )
  
  tabla_selva <- as.data.frame(
    summary(
      comparaciones_selva,
      infer = c(TRUE, TRUE)
    )
  )
  
  cat("\nGENOTYPES VS LA SELVA\n")
  print(tabla_selva, row.names = FALSE)
  
  write.csv(
    tabla_selva,
    file.path(carpeta, "13_Comparisons_La_Selva.csv"),
    row.names = FALSE
  )
}

# 77. NET LEAF NUMBER CHANGE------------------

medias_tiempo <- emmeans::emmeans(
  modelo_final,
  ~ evaluacion | genotipo
)

if (length(levels(hojas_mixto$evaluacion)) == 4) {
  
  cambio_neto <- emmeans::contrast(
    medias_tiempo,
    method = list(
      "Final minus initial" = c(-1, 0, 0, 1)
    )
  )
  
  tabla_cambio <- as.data.frame(
    summary(
      cambio_neto,
      infer = c(TRUE, TRUE)
    )
  )
  
  cat("\nNET CHANGE BETWEEN FIRST AND LAST ASSESSMENT\n")
  print(tabla_cambio, row.names = FALSE)
  
  write.csv(
    tabla_cambio,
    file.path(carpeta, "14_Net_leaf_change.csv"),
    row.names = FALSE
  )
}

# 78. MODEL-BASED LEAF TRAJECTORIES------------------

tabla_grafico <- as.data.frame(
  medias_fecha
) %>%
  dplyr::left_join(
    tabla_tiempo %>%
      dplyr::select(evaluacion, dias),
    by = "evaluacion"
  )

grafico_trayectorias <- ggplot() +
  
  geom_line(
    data = hojas_mixto,
    aes(
      x = dias,
      y = numero_de_hojas,
      group = parcela
    ),
    color = "grey70",
    alpha = 0.35
  ) +
  
  geom_point(
    data = hojas_mixto,
    aes(
      x = dias,
      y = numero_de_hojas
    ),
    color = "grey55",
    alpha = 0.35,
    size = 1
  ) +
  
  geom_line(
    data = tabla_grafico,
    aes(
      x = dias,
      y = emmean,
      group = genotipo
    ),
    color = "blue",
    linewidth = 1
  ) +
  
  geom_point(
    data = tabla_grafico,
    aes(
      x = dias,
      y = emmean
    ),
    color = "blue",
    size = 1.8
  ) +
  
  geom_errorbar(
    data = tabla_grafico,
    aes(
      x = dias,
      ymin = lower.CL,
      ymax = upper.CL
    ),
    color = "blue",
    width = 3
  ) +
  
  facet_wrap(
    ~ genotipo,
    ncol = 6,
    scales = "free_y"
  ) +
  
  labs(
    x = "Days after first assessment",
    y = "Number of leaves per plant",
    title = "Leaf number dynamics across genotypes"
  ) +
  
  theme_bw(base_size = 11)




# 78. LEAF NUMBER TRAJECTORIES------------------

grafico_trayectorias <- ggplot() +
  
  geom_line(
    data = hojas_mixto,
    aes(
      x = dias,
      y = numero_de_hojas,
      group = parcela
    ),
    color = "grey70",
    alpha = 0.35
  ) +
  
  geom_point(
    data = hojas_mixto,
    aes(
      x = dias,
      y = numero_de_hojas
    ),
    color = "grey55",
    alpha = 0.35,
    size = 1
  ) +
  
  geom_line(
    data = tabla_grafico,
    aes(
      x = dias,
      y = emmean,
      group = genotipo
    ),
    color = "blue",
    linewidth = 1
  ) +
  
  geom_point(
    data = tabla_grafico,
    aes(
      x = dias,
      y = emmean
    ),
    color = "blue",
    size = 1.8
  ) +
  
  geom_errorbar(
    data = tabla_grafico,
    aes(
      x = dias,
      ymin = lower.CL,
      ymax = upper.CL
    ),
    color = "blue",
    width = 3
  ) +
  
  facet_wrap(
    ~ genotipo,
    ncol = 6,
    scales = "free_y"
  ) +
  
  scale_x_continuous(
    breaks = c(0, 28, 63, 103),
    labels = c("14", "44", "79", "119")
  ) +
  
  labs(
    x = "Days after transplanting (DAT)",
    y = "Number of leaves per plant",
    title = "Leaf number dynamics across genotypes"
  ) +
  
  theme_bw(base_size = 11)

print(grafico_trayectorias)

print(grafico_trayectorias)


# 78. GENOTYPE LABELS------------------

etiquetas_genotipo <- function(x) {
  x <- as.character(x)
  
  es_selva <- tolower(x) == "selva"
  x[es_selva] <- "La Selva"
  
  es_numero <- grepl("^[0-9]+$", x)
  x[es_numero] <- sprintf(
    "UNR%03d",
    as.integer(x[es_numero])
  )
  
  return(x)
}

# 78.1. LEAF NUMBER TRAJECTORIES------------------

grafico_trayectorias <- ggplot() +
  
  geom_line(
    data = hojas_mixto,
    aes(
      x = dias,
      y = numero_de_hojas,
      group = parcela
    ),
    color = "grey70",
    alpha = 0.35
  ) +
  
  geom_point(
    data = hojas_mixto,
    aes(
      x = dias,
      y = numero_de_hojas
    ),
    color = "grey55",
    alpha = 0.35,
    size = 1
  ) +
  
  geom_line(
    data = tabla_grafico,
    aes(
      x = dias,
      y = emmean,
      group = genotipo
    ),
    color = "blue",
    linewidth = 1
  ) +
  
  geom_point(
    data = tabla_grafico,
    aes(
      x = dias,
      y = emmean
    ),
    color = "blue",
    size = 1.8
  ) +
  
  geom_errorbar(
    data = tabla_grafico,
    aes(
      x = dias,
      ymin = lower.CL,
      ymax = upper.CL
    ),
    color = "blue",
    width = 3
  ) +
  
  facet_wrap(
    ~ genotipo,
    ncol = 6,
    scales = "free_y",
    labeller = labeller(
      genotipo = etiquetas_genotipo
    )
  ) +
  
  scale_x_continuous(
    breaks = c(0, 28, 63, 103),
    labels = c("14", "44", "79", "119")
  ) +
  
  labs(
    x = "Days after transplanting (DAT)",
    y = "Number of leaves per plant",
    title = "Leaf number dynamics across genotypes"
  ) +
  
  theme_bw(base_size = 11) +
  
  theme(
    strip.text = element_text(
    #  face = "bold",
      size = 11
    ),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

print(grafico_trayectorias)

# 78.2. SAVE FIGURE AS VECTOR PDF------------------
getwd()
ggsave(
  "Leaf_number_trajectories.pdf",
  plot = grafico_trayectorias,
  width = 10,
  height = 6,
  units = "in"
)

# 78.3. SAVE FIGURE AS HIGH-RESOLUTION PNG------------------

ggsave(
  "Leaf_number_trajectories.png",
  plot = grafico_trayectorias,
  width = 10,
  height = 6,
  units = "in",
  dpi = 600
)

ggsave(
  file.path(carpeta, "15_Leaf_trajectories.pdf"),
  grafico_trayectorias,
  width = 10,
  height = 6
)

ggsave(
  file.path(carpeta, "15_Leaf_trajectories.png"),
  grafico_trayectorias,
  width = 15,
  height = 10,
  dpi = 600
)

# 79. TUKEY GROUPING FIGURE------------------

grafico_grupos <- ggplot(
  tabla_grupos,
  aes(
    x = reorder(genotipo, -emmean),
    y = emmean
  )
) +
  
  geom_point(
    color = "black",
    size = 2.4
  ) +
  
  geom_errorbar(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    width = 0.2,
    color = "grey35"
  ) +
  
  geom_text(
    aes(
      y = upper.CL,
      label = .group
    ),
    vjust = -0.6,
    size = 3.1
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0.05, 0.22)
    )
  ) +
  
  labs(
    x = "Genotype",
    y = "Estimated number of leaves per plant",
    title = "Genotype comparisons (Tukey, p < 0.05)"
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

if (interaccion_significativa) {
  
  grafico_grupos <- grafico_grupos +
    facet_wrap(
      ~ evaluacion,
      ncol = 2,
      scales = "free_y"
    )
}

print(grafico_grupos)


# 79. GENOTYPE LABELS------------------

etiquetas_genotipo <- function(x) {
  x <- as.character(x)
  
  es_selva <- tolower(x) == "selva"
  x[es_selva] <- "La Selva"
  
  es_numero <- grepl("^[0-9]+$", x)
  
  x[es_numero] <- sprintf(
    "UNR%03d",
    as.integer(x[es_numero])
  )
  
  return(x)
}

# 79.1. ORDER GENOTYPES NUMERICALLY------------------

genotipos_originales <- unique(
  as.character(tabla_grupos$genotipo)
)

genotipos_numericos <- genotipos_originales[
  grepl("^[0-9]+$", genotipos_originales)
]

genotipos_numericos <- genotipos_numericos[
  order(as.integer(genotipos_numericos))
]

genotipo_selva <- genotipos_originales[
  tolower(genotipos_originales) == "selva"
]

niveles_ordenados <- c(
  genotipos_numericos,
  genotipo_selva
)

# 79.2. APPLY GENOTYPE ORDER------------------

tabla_grupos_graf <- tabla_grupos %>%
  dplyr::mutate(
    genotipo = factor(
      as.character(genotipo),
      levels = niveles_ordenados
    ),
    .group = trimws(.group)
  )

print(niveles_ordenados)

# 79.3. DAYS AFTER TRANSPLANTING------------------

etiquetas_evaluacion <- c(
  "1" = "16 DAT",
  "2" = "44 DAT",
  "3" = "79 DAT",
  "4" = "119 DAT"
)

# 79.4. TUKEY GROUPING FIGURE------------------

grafico_grupos <- ggplot(
  tabla_grupos_graf,
  aes(
    x = genotipo,
    y = emmean
  )
) +
  
  geom_point(
    color = "black",
    size = 2.4
  ) +
  
  geom_errorbar(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    width = 0.2,
    color = "grey35",
    linewidth = 0.5
  ) +
  
  geom_text(
    aes(
      y = upper.CL,
      label = .group
    ),
    vjust = -0.6,
    size = 3.2,
    color = "black"
  ) +
  
  scale_x_discrete(
    limits = niveles_ordenados,
    labels = etiquetas_genotipo,
    drop = FALSE
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0.05, 0.22)
    )
  ) +
  
  labs(
    x = "Genotype",
    y = "Estimated number of leaves per plant",
    title = ""
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 10,
      color = "black"
    ),
    
    axis.text.y = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.title = element_text(
      size = 12,
     # face = "bold"
    ),
    
    strip.text = element_text(
   #   face = "bold",
      size = 12,
      color = "black"
    ),
    
    strip.background = element_rect(
      fill = "grey90",
      color = "grey50"
    ),
    
    plot.title = element_text(
   #   face = "bold",
      hjust = 0.5,
      size = 14
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# 79.5. FACET LABELS WITH DAT------------------

if (interaccion_significativa) {
  
  grafico_grupos <- grafico_grupos +
    
    facet_wrap(
      ~ evaluacion,
      ncol = 2,
      scales = "free_y",
      labeller = labeller(
        evaluacion = as_labeller(
          etiquetas_evaluacion
        )
      )
    )
}

# 79.6. DISPLAY FIGURE------------------

print(grafico_grupos)

# 79.7. SAVE AS VECTOR PDF------------------

ggsave(
  filename = "Tukey_grouping_leaf_number_ordered.pdf",
  plot = grafico_grupos,
  width = 10,
  height = 6,
  units = "in"
)

# 79.8. SAVE AS HIGH-RESOLUTION PNG------------------

ggsave(
  filename = "Tukey_grouping_leaf_number_ordered.png",
  plot = grafico_grupos,
  width = 10,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)


# 80. FINAL ANALYSIS SUMMARY------------------

cat("\n")
cat("LONGITUDINAL MIXED MODEL RESULTS\n")
cat("--------------------------------\n")

cat("Covariance structure:",
    nombre_seleccionado, "\n")

cat("Genotype x time p-value:",
    p_interaccion, "\n")

cat("Significant interaction:",
    interaccion_significativa, "\n")

cat("\nFINAL ANOVA\n")
print(anova_final)

cat("\nFINAL TUKEY LETTER GROUPS\n")
print(tabla_grupos, row.names = FALSE)

cat("\nShapiro-Wilk p-value:",
    prueba_shapiro$p.value, "\n")

cat("\nResults saved in:",
    normalizePath(carpeta), "\n")

#-----
#
#
#
# af ---------
#
#
#
#
#-----


# 82. LIBRARIES FOR LEAF AREA ANALYSIS------------------

paquetes_af <- c(
  "dplyr",
  "ggplot2",
  "missForest",
  "nlme",
  "emmeans",
  "multcomp",
  "multcompView"
)

faltantes_af <- paquetes_af[
  !vapply(
    paquetes_af,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(faltantes_af) > 0) {
  install.packages(
    faltantes_af,
    repos = "https://cloud.r-project.org"
  )
}

invisible(
  lapply(paquetes_af, library, character.only = TRUE)
)

# 83. VERIFY ORIGINAL DATA------------------

if (!exists("datos")) {
  stop("No existe el objeto 'datos'.")
}

columnas_af <- c(
  "fecha",
  "evaluacion",
  "genotipo",
  "bloque",
  "planta",
  "numero_de_hojas",
  "clorofilometro_spad",
  "af"
)

if (!all(columnas_af %in% names(datos))) {
  stop("Faltan columnas necesarias en 'datos'.")
}

str(datos)
summary(datos$af)
dim(datos)

# 84. DEFINE GENOTYPE LABELS------------------

etiquetas_genotipo_af <- function(x) {
  
  x <- as.character(x)
  
  es_selva <- tolower(x) == "selva"
  
  x[es_selva] <- "La Selva"
  
  es_numero <- grepl("^[0-9]+$", x)
  
  x[es_numero] <- sprintf(
    "UNR%03d",
    as.integer(x[es_numero])
  )
  
  return(x)
}

# 85. DEFINE GENOTYPE ORDER------------------

genotipos_af <- unique(
  as.character(datos$genotipo)
)

genotipos_numericos_af <- genotipos_af[
  grepl("^[0-9]+$", genotipos_af)
]

genotipos_numericos_af <- genotipos_numericos_af[
  order(as.integer(genotipos_numericos_af))
]

genotipo_selva_af <- genotipos_af[
  tolower(genotipos_af) == "selva"
]

niveles_ordenados_af <- c(
  genotipos_numericos_af,
  genotipo_selva_af
)

print(
  etiquetas_genotipo_af(niveles_ordenados_af)
)

# 86. PREPARE PLANT-LEVEL DATA------------------

af_planta <- datos %>%
  dplyr::mutate(
    fecha = as.Date(fecha),
    evaluacion = factor(
      as.character(evaluacion),
      levels = as.character(1:4)
    ),
    genotipo = factor(
      as.character(genotipo),
      levels = niveles_ordenados_af
    ),
    bloque = factor(bloque),
    planta = factor(planta),
    af = as.numeric(af),
    numero_de_hojas = as.numeric(numero_de_hojas),
    clorofilometro_spad = as.numeric(clorofilometro_spad)
  ) %>%
  dplyr::mutate(
    parcela = interaction(
      bloque,
      genotipo,
      drop = TRUE,
      sep = "_"
    ),
    dias = as.numeric(
      fecha - min(fecha, na.rm = TRUE)
    ),
    dat = dias + 16,
    tiempo_30 = dias / 30
  ) %>%
  dplyr::arrange(
    parcela,
    evaluacion,
    planta
  )

if (anyNA(
  af_planta[, c(
    "fecha", "evaluacion", "genotipo",
    "bloque", "planta"
  )]
)) {
  stop("Hay identificadores o fechas faltantes.")
}

if (any(af_planta$af < 0, na.rm = TRUE)) {
  stop("Existen valores negativos de área foliar.")
}

# 87. VERIFY EXPERIMENTAL STRUCTURE------------------

cat("\nGENOTYPES:",
    dplyr::n_distinct(af_planta$genotipo))

cat("\nBLOCKS:",
    dplyr::n_distinct(af_planta$bloque))

cat("\nPLOTS:",
    dplyr::n_distinct(af_planta$parcela))

cat("\nPLANT OBSERVATIONS:",
    nrow(af_planta), "\n")

tabla_fechas_af <- af_planta %>%
  dplyr::distinct(
    evaluacion,
    fecha,
    dias,
    dat
  ) %>%
  dplyr::arrange(evaluacion)

print(tabla_fechas_af)

if (nrow(tabla_fechas_af) != 4) {
  stop("Se esperaban cuatro evaluaciones.")
}

# 88. VERIFY DUPLICATES------------------

duplicados_af <- af_planta %>%
  dplyr::count(
    parcela,
    evaluacion,
    planta
  ) %>%
  dplyr::filter(n > 1)

if (nrow(duplicados_af) > 0) {
  print(duplicados_af)
  stop("Existen plantas duplicadas por evaluación.")
}

# 89. CHECK MISSING VALUES------------------

cat("\nMISSING VALUES\n")

print(
  colSums(
    is.na(af_planta)
  )
)

NA_af <- which(
  is.na(af_planta$af)
)

cat(
  "\nMissing leaf area observations:",
  length(NA_af), "\n"
)

# 90. REVIEW EXTREME LEAF AREA VALUES------------------

valores_extremos_af <- af_planta %>%
  dplyr::arrange(dplyr::desc(af)) %>%
  dplyr::select(
    fecha,
    evaluacion,
    genotipo,
    bloque,
    planta,
    af
  ) %>%
  head(30)

print(valores_extremos_af)

# 91. MISSFOREST IMPUTATION FOR LEAF AREA------------------

af_planta$af_original <- af_planta$af

af_planta$imputada_af <- is.na(af_planta$af)

if (length(NA_af) > 0) {
  
  if (all(is.na(af_planta$af))) {
    stop("No hay valores observados de af.")
  }
  
  datos_mf_af <- af_planta %>%
    dplyr::transmute(
      evaluacion = factor(evaluacion),
      genotipo = factor(genotipo),
      bloque = factor(bloque),
      planta = factor(planta),
      numero_de_hojas = numero_de_hojas,
      clorofilometro_spad = clorofilometro_spad,
      af = af
    ) %>%
    as.data.frame()
  
  set.seed(12345)
  
  imp_mf_af <- missForest::missForest(
    xmis = datos_mf_af,
    ntree = 1000,
    maxiter = 10,
    verbose = TRUE
  )
  
  cat("\nMISSFOREST OOB ERROR\n")
  print(imp_mf_af$OOBerror)
  
  af_planta$af <- as.numeric(
    imp_mf_af$ximp$af
  )
  
} else {
  
  message(
    "No missing leaf area values. Imputation not required."
  )
}

# 92. VERIFY IMPUTED VALUES------------------

if (anyNA(af_planta$af)) {
  stop("La variable af todavía contiene NA.")
}

tabla_imputacion_af <- af_planta %>%
  dplyr::filter(imputada_af) %>%
  dplyr::select(
    bloque,
    genotipo,
    planta,
    evaluacion,
    af_original,
    af
  )

print(tabla_imputacion_af, n = Inf)

# 93. CONSTRUCT PLOT-LEVEL DATA------------------

af_parcela <- af_planta %>%
  dplyr::group_by(
    bloque,
    genotipo,
    parcela,
    fecha,
    evaluacion,
    dias,
    dat,
    tiempo_30
  ) %>%
  dplyr::summarise(
    n_plantas = dplyr::n(),
    n_imputados = sum(imputada_af),
    af = mean(af),
    desviacion_plantas = sd(af),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    parcela,
    dias
  ) %>%
  droplevels() %>%
  as.data.frame()

str(af_parcela)
summary(af_parcela)

# 94. VERIFY PLOT-LEVEL DATA------------------

cat("\nPLANTS PER PLOT AND DATE\n")

print(
  table(af_parcela$n_plantas)
)

cat("\nASSESSMENTS PER PLOT\n")

print(
  af_parcela %>%
    dplyr::count(parcela) %>%
    dplyr::count(n)
)

if (anyDuplicated(
  af_parcela[, c("parcela", "evaluacion")]
) > 0) {
  stop("Hay registros duplicados por parcela y fecha.")
}

# 95. CREATE OUTPUT DIRECTORY------------------

carpeta_af <- "Resultados_Area_Foliar_LMM"

dir.create(
  carpeta_af,
  showWarnings = FALSE,
  recursive = TRUE
)

write.csv(
  af_parcela,
  file.path(carpeta_af, "01_Plot_level_data.csv"),
  row.names = FALSE
)

write.csv(
  tabla_imputacion_af,
  file.path(carpeta_af, "02_Imputed_values.csv"),
  row.names = FALSE
)

# 96. DESCRIPTIVE STATISTICS------------------

descriptivos_af <- af_parcela %>%
  dplyr::group_by(
    genotipo,
    evaluacion,
    dat
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    media = mean(af),
    desviacion = sd(af),
    error_estandar = desviacion / sqrt(n),
    minimo = min(af),
    maximo = max(af),
    .groups = "drop"
  )

print(descriptivos_af, n = Inf)

write.csv(
  descriptivos_af,
  file.path(carpeta_af, "03_Descriptive_statistics.csv"),
  row.names = FALSE
)

# 97. EXPLORATORY LEAF AREA TRAJECTORIES------------------

grafico_exploratorio_af <- ggplot(
  af_parcela,
  aes(
    x = dat,
    y = af,
    group = parcela
  )
) +
  geom_line(
    color = "grey55",
    alpha = 0.55
  ) +
  geom_point(
    color = "grey40",
    alpha = 0.65
  ) +
  facet_wrap(
    ~ genotipo,
    ncol = 6,
    scales = "free_y",
    labeller = labeller(
      genotipo = etiquetas_genotipo_af
    )
  ) +
  scale_x_continuous(
    breaks = c(16, 44, 79, 119)
  ) +
  labs(
    x = "Days after transplanting (DAT)",
    y = "Leaf area per plant",
    title = "Observed leaf area trajectories"
  ) +
  theme_bw(base_size = 11)

print(grafico_exploratorio_af)

ggsave(
  file.path(carpeta_af, "04_Observed_trajectories.pdf"),
  grafico_exploratorio_af,
  width = 15,
  height = 10
)

# 98. INDIVIDUAL LINEAR REGRESSIONS------------------

tasas_af <- af_parcela %>%
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
      
      modelo <- lm(
        af ~ dias,
        data = .x
      )
      
      resumen <- summary(modelo)
      
      data.frame(
        beta0 = unname(coef(modelo)[1]),
        beta1 = unname(coef(modelo)[2]),
        se_beta1 = resumen$coefficients[
          "dias", "Std. Error"
        ],
        t_beta1 = resumen$coefficients[
          "dias", "t value"
        ],
        p_beta1 = resumen$coefficients[
          "dias", "Pr(>|t|)"
        ],
        r2 = resumen$r.squared,
        r2_ajustado = resumen$adj.r.squared,
        rmse = sqrt(mean(residuals(modelo)^2)),
        n_fechas = nobs(modelo)
      )
    }
  ) %>%
  dplyr::ungroup()

cat("\nINDIVIDUAL REGRESSION RESULTS\n")

print(
  tasas_af %>%
    dplyr::arrange(r2),
  n = Inf
)

summary(tasas_af$r2)

write.csv(
  tasas_af,
  file.path(carpeta_af, "05_Individual_regressions.csv"),
  row.names = FALSE
)

# 99. DISTRIBUTION OF R-SQUARED------------------

grafico_r2_af <- ggplot(
  tasas_af,
  aes(x = r2)
) +
  geom_histogram(
    bins = 15,
    fill = "grey80",
    color = "black"
  ) +
  labs(
    x = expression(R^2),
    y = "Frequency",
    title = "Distribution of R-squared values"
  ) +
  theme_bw(base_size = 12)

print(grafico_r2_af)

ggsave(
  file.path(carpeta_af, "06_R2_distribution.pdf"),
  grafico_r2_af,
  width = 7,
  height = 5
)

# 100. MIXED-MODEL CONTROL SETTINGS------------------

control_af <- nlme::lmeControl(
  opt = "optim",
  msMaxIter = 300,
  msMaxEval = 500,
  niterEM = 50
)

formula_af <- af ~ bloque + genotipo * evaluacion

# 101. BASE LONGITUDINAL MIXED MODEL------------------

modelo_base_af <- nlme::lme(
  fixed = formula_af,
  random = ~ 1 | parcela,
  data = af_parcela,
  method = "REML",
  control = control_af
)

# 102. CONTINUOUS-TIME AR(1) CORRELATION------------------

modelo_car1_af <- try(
  update(
    modelo_base_af,
    correlation = nlme::corCAR1(
      form = ~ tiempo_30 | parcela
    )
  ),
  silent = TRUE
)

# 103. HETEROGENEOUS RESIDUAL VARIANCES------------------

modelo_hetero_af <- try(
  update(
    modelo_base_af,
    weights = nlme::varIdent(
      form = ~ 1 | evaluacion
    )
  ),
  silent = TRUE
)

# 104. AR(1) AND HETEROGENEOUS VARIANCES------------------

modelo_car1_hetero_af <- try(
  update(
    modelo_base_af,
    correlation = nlme::corCAR1(
      form = ~ tiempo_30 | parcela
    ),
    weights = nlme::varIdent(
      form = ~ 1 | evaluacion
    )
  ),
  silent = TRUE
)

# 105. COLLECT SUCCESSFUL MODELS------------------

modelos_af <- list(
  Intercepto_aleatorio = modelo_base_af
)

if (inherits(modelo_car1_af, "lme")) {
  modelos_af$CAR1 <- modelo_car1_af
}

if (inherits(modelo_hetero_af, "lme")) {
  modelos_af$Heterogeneo <- modelo_hetero_af
}

if (inherits(modelo_car1_hetero_af, "lme")) {
  modelos_af$CAR1_Heterogeneo <- modelo_car1_hetero_af
}

# 106. COMPARE COVARIANCE STRUCTURES------------------

tabla_aic_af <- data.frame(
  Modelo = names(modelos_af),
  AIC = unname(sapply(modelos_af, AIC)),
  BIC = unname(sapply(modelos_af, BIC)),
  logLik = unname(sapply(
    modelos_af,
    function(x) as.numeric(logLik(x))
  ))
) %>%
  dplyr::arrange(AIC) %>%
  dplyr::mutate(
    Delta_AIC = AIC - min(AIC)
  )

print(tabla_aic_af, row.names = FALSE)

nombre_seleccionado_af <- tabla_aic_af$Modelo[1]

modelo_completo_af <- modelos_af[[nombre_seleccionado_af]]

cat(
  "\nSELECTED COVARIANCE STRUCTURE:",
  nombre_seleccionado_af, "\n"
)

write.csv(
  tabla_aic_af,
  file.path(carpeta_af, "07_Model_selection_AIC.csv"),
  row.names = FALSE
)

# 107. TEST GENOTYPE BY TIME INTERACTION------------------

modelo_completo_ML_af <- update(
  modelo_completo_af,
  method = "ML"
)

modelo_aditivo_ML_af <- update(
  modelo_completo_af,
  fixed = af ~ bloque + genotipo + evaluacion,
  method = "ML"
)

prueba_interaccion_af <- anova(
  modelo_aditivo_ML_af,
  modelo_completo_ML_af
)

cat("\nGENOTYPE BY TIME INTERACTION\n")

print(prueba_interaccion_af)

p_interaccion_af <- as.numeric(
  prueba_interaccion_af[["p-value"]][2]
)

if (
  length(p_interaccion_af) != 1 ||
  !is.finite(p_interaccion_af)
) {
  stop("No se pudo obtener el p de interacción.")
}

alfa_af <- 0.05

interaccion_significativa_af <-
  p_interaccion_af < alfa_af

cat(
  "\nInteraction p-value:",
  p_interaccion_af, "\n"
)

# 108. SELECT FINAL FIXED EFFECTS------------------

if (interaccion_significativa_af) {
  
  modelo_final_af <- modelo_completo_af
  
  cat(
    "\nSignificant interaction.",
    "\nComparisons within each assessment.\n"
  )
  
} else {
  
  modelo_final_af <- update(
    modelo_aditivo_ML_af,
    method = "REML"
  )
  
  cat(
    "\nNon-significant interaction.",
    "\nGenotype main-effect comparisons.\n"
  )
}

# 109. FINAL MIXED-MODEL ANOVA------------------

anova_final_af <- anova(
  modelo_final_af,
  type = "marginal"
)

cat("\nFINAL MIXED MODEL ANOVA\n")

print(anova_final_af)

tabla_anova_af <- data.frame(
  Efecto = rownames(anova_final_af),
  as.data.frame(anova_final_af),
  row.names = NULL,
  check.names = FALSE
)

write.csv(
  tabla_anova_af,
  file.path(carpeta_af, "08_Mixed_model_ANOVA.csv"),
  row.names = FALSE
)

capture.output(
  summary(modelo_final_af),
  file = file.path(
    carpeta_af,
    "09_Final_model_summary.txt"
  )
)

# 110. EXTRACT MODEL RESIDUALS------------------

diagnosticos_af <- af_parcela %>%
  dplyr::mutate(
    ajustado = as.numeric(
      fitted(modelo_final_af)
    ),
    residuo = as.numeric(
      resid(modelo_final_af, type = "response")
    ),
    residuo_normalizado = as.numeric(
      resid(modelo_final_af, type = "normalized")
    )
  )

# 111. SHAPIRO-WILK NORMALITY TEST------------------

prueba_shapiro_af <- shapiro.test(
  diagnosticos_af$residuo_normalizado
)

cat("\nSHAPIRO-WILK TEST\n")

print(prueba_shapiro_af)

# 112. NORMAL Q-Q PLOT------------------

grafico_qq_af <- ggplot(
  diagnosticos_af,
  aes(sample = residuo_normalizado)
) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    x = "Theoretical quantiles",
    y = "Observed quantiles",
    title = "Normal Q-Q plot: Leaf area"
  ) +
  theme_bw(base_size = 12)

print(grafico_qq_af)

ggsave(
  file.path(carpeta_af, "10_Residual_QQ.pdf"),
  grafico_qq_af,
  width = 7,
  height = 6
)

# 113. RESIDUALS VS FITTED VALUES------------------

grafico_residuos_af <- ggplot(
  diagnosticos_af,
  aes(
    x = ajustado,
    y = residuo_normalizado
  )
) +
  geom_point(
    alpha = 0.65
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "blue"
  ) +
  labs(
    x = "Fitted values",
    y = "Normalized residuals",
    title = "Residuals vs fitted values"
  ) +
  theme_bw(base_size = 12)

print(grafico_residuos_af)

ggsave(
  file.path(carpeta_af, "11_Residuals_fitted.pdf"),
  grafico_residuos_af,
  width = 7,
  height = 6
)

# 114. RESIDUAL VARIANCE BY ASSESSMENT------------------

tabla_varianza_af <- diagnosticos_af %>%
  dplyr::group_by(evaluacion) %>%
  dplyr::summarise(
    n = dplyr::n(),
    media_residual = mean(residuo_normalizado),
    sd_residual = sd(residuo_normalizado),
    varianza_residual = var(residuo_normalizado),
    .groups = "drop"
  )

print(tabla_varianza_af)

grafico_varianza_af <- ggplot(
  diagnosticos_af,
  aes(
    x = evaluacion,
    y = residuo_normalizado
  )
) +
  geom_boxplot(fill = "grey85") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "16 DAT",
      "2" = "44 DAT",
      "3" = "79 DAT",
      "4" = "119 DAT"
    )
  ) +
  labs(
    x = "Days after transplanting",
    y = "Normalized residuals",
    title = "Residual variance by assessment"
  ) +
  theme_bw(base_size = 12)

print(grafico_varianza_af)

ggsave(
  file.path(carpeta_af, "12_Residual_variance.pdf"),
  grafico_varianza_af,
  width = 7,
  height = 6
)

# 115. TEMPORAL RESIDUAL AUTOCORRELATION------------------

acf_modelo_af <- nlme::ACF(
  modelo_final_af,
  resType = "normalized",
  maxLag = 3
)

print(acf_modelo_af)

pdf(
  file.path(carpeta_af, "13_Residual_ACF.pdf"),
  width = 7,
  height = 6
)

plot(acf_modelo_af)

dev.off()

# 116. IDENTIFY LARGE NORMALIZED RESIDUALS------------------

residuos_extremos_af <- diagnosticos_af %>%
  dplyr::filter(
    abs(residuo_normalizado) > 3
  ) %>%
  dplyr::select(
    bloque,
    genotipo,
    parcela,
    evaluacion,
    af,
    ajustado,
    residuo_normalizado
  )

cat("\nOBSERVATIONS TO REVIEW\n")

print(residuos_extremos_af)

write.csv(
  residuos_extremos_af,
  file.path(carpeta_af, "14_Residuals_to_review.csv"),
  row.names = FALSE
)

# 117. OPTIONAL LOG-TRANSFORMATION SENSITIVITY CHECK------------------

# Diagnostic only: do not automatically replace
# the main model solely based on Shapiro-Wilk.

if (all(af_parcela$af >= 0)) {
  
  formula_log_af <- if (
    interaccion_significativa_af
  ) {
    log1p(af) ~ bloque + genotipo * evaluacion
  } else {
    log1p(af) ~ bloque + genotipo + evaluacion
  }
  
  modelo_log_af <- try(
    update(
      modelo_final_af,
      fixed = formula_log_af,
      method = "REML"
    ),
    silent = TRUE
  )
  
  if (inherits(modelo_log_af, "lme")) {
    
    residuos_log_af <- as.numeric(
      resid(modelo_log_af, type = "normalized")
    )
    
    cat("\nLOG1P MODEL DIAGNOSTICS\n")
    
    print(
      shapiro.test(residuos_log_af)
    )
    
    pdf(
      file.path(carpeta_af, "15_Log_model_QQ.pdf"),
      width = 7,
      height = 6
    )
    
    qqnorm(
      residuos_log_af,
      main = "Log1p leaf area model"
    )
    
    qqline(
      residuos_log_af,
      col = "red"
    )
    
    dev.off()
    
  }
}

# 118. ESTIMATED MARGINAL MEANS BY DATE------------------

medias_fecha_af <- emmeans::emmeans(
  modelo_final_af,
  ~ genotipo | evaluacion
)

tabla_medias_fecha_af <- as.data.frame(
  summary(
    medias_fecha_af,
    infer = c(TRUE, TRUE)
  )
)

cat("\nESTIMATED LEAF AREA MEANS\n")

print(tabla_medias_fecha_af, row.names = FALSE)

write.csv(
  tabla_medias_fecha_af,
  file.path(carpeta_af, "16_Estimated_means.csv"),
  row.names = FALSE
)

# 119. TUKEY MULTIPLE COMPARISONS------------------

if (interaccion_significativa_af) {
  
  medias_comparacion_af <- emmeans::emmeans(
    modelo_final_af,
    ~ genotipo | evaluacion
  )
  
} else {
  
  medias_comparacion_af <- emmeans::emmeans(
    modelo_final_af,
    ~ genotipo
  )
}

comparaciones_tukey_af <- pairs(
  medias_comparacion_af,
  adjust = "tukey"
)

tabla_tukey_af <- as.data.frame(
  summary(
    comparaciones_tukey_af,
    infer = c(TRUE, TRUE)
  )
)

cat("\nTUKEY MULTIPLE COMPARISONS\n")

print(tabla_tukey_af, row.names = FALSE)

write.csv(
  tabla_tukey_af,
  file.path(carpeta_af, "17_Tukey_comparisons.csv"),
  row.names = FALSE
)

# 120. TUKEY LETTER GROUPING------------------

grupos_tukey_af <- multcomp::cld(
  medias_comparacion_af,
  adjust = "tukey",
  alpha = alfa_af,
  Letters = letters,
  sort = FALSE
)

tabla_grupos_af <- as.data.frame(
  grupos_tukey_af
) %>%
  dplyr::mutate(
    .group = trimws(.group),
    genotipo = factor(
      as.character(genotipo),
      levels = niveles_ordenados_af
    )
  ) %>%
  dplyr::arrange(
    genotipo
  )

if ("evaluacion" %in% names(tabla_grupos_af)) {
  tabla_grupos_af <- tabla_grupos_af %>%
    dplyr::arrange(
      evaluacion,
      genotipo
    )
}

cat("\nTUKEY LETTER GROUPING\n")

print(tabla_grupos_af, row.names = FALSE)

write.csv(
  tabla_grupos_af,
  file.path(carpeta_af, "18_Tukey_letter_groups.csv"),
  row.names = FALSE
)

# 121. COMPARISONS AGAINST LA SELVA------------------

control_selva_af <- match(
  "selva",
  levels(af_parcela$genotipo)
)

if (!is.na(control_selva_af)) {
  
  comparaciones_selva_af <- emmeans::contrast(
    medias_comparacion_af,
    method = "trt.vs.ctrl",
    ref = control_selva_af,
    adjust = "dunnett"
  )
  
  tabla_selva_af <- as.data.frame(
    summary(
      comparaciones_selva_af,
      infer = c(TRUE, TRUE)
    )
  )
  
  cat("\nGENOTYPES VS LA SELVA\n")
  
  print(tabla_selva_af, row.names = FALSE)
  
  write.csv(
    tabla_selva_af,
    file.path(carpeta_af, "19_Comparisons_La_Selva.csv"),
    row.names = FALSE
  )
}

# 122. NET LEAF AREA CHANGE------------------

medias_tiempo_af <- emmeans::emmeans(
  modelo_final_af,
  ~ evaluacion | genotipo
)

cambio_neto_af <- emmeans::contrast(
  medias_tiempo_af,
  method = list(
    "Final minus initial" = c(-1, 0, 0, 1)
  )
)

tabla_cambio_af <- as.data.frame(
  summary(
    cambio_neto_af,
    infer = c(TRUE, TRUE)
  )
)

cat("\nNET LEAF AREA CHANGE\n")

print(tabla_cambio_af, row.names = FALSE)

write.csv(
  tabla_cambio_af,
  file.path(carpeta_af, "20_Net_leaf_area_change.csv"),
  row.names = FALSE
)

# 123. PREPARE MODEL-BASED TRAJECTORY DATA------------------

tabla_grafico_af <- as.data.frame(
  medias_fecha_af
) %>%
  dplyr::left_join(
    tabla_fechas_af %>%
      dplyr::select(evaluacion, dat),
    by = "evaluacion"
  )

# 124. MODEL-BASED LEAF AREA TRAJECTORIES------------------

grafico_trayectorias_af <- ggplot() +
  
  geom_line(
    data = af_parcela,
    aes(
      x = dat,
      y = af,
      group = parcela
    ),
    color = "grey75",
    alpha = 0.4
  ) +
  
  geom_point(
    data = af_parcela,
    aes(
      x = dat,
      y = af
    ),
    color = "grey65",
    alpha = 0.4,
    size = 1
  ) +
  
  geom_line(
    data = tabla_grafico_af,
    aes(
      x = dat,
      y = emmean,
      group = genotipo
    ),
    color = "blue",
    linewidth = 1
  ) +
  
  geom_point(
    data = tabla_grafico_af,
    aes(
      x = dat,
      y = emmean
    ),
    color = "blue",
    size = 1.8
  ) +
  
  geom_errorbar(
    data = tabla_grafico_af,
    aes(
      x = dat,
      ymin = lower.CL,
      ymax = upper.CL
    ),
    color = "blue",
    width = 3
  ) +
  
  facet_wrap(
    ~ genotipo,
    ncol = 6,
    scales = "free_y",
    labeller = labeller(
      genotipo = etiquetas_genotipo_af
    )
  ) +
  
  scale_x_continuous(
    breaks = c(16, 44, 79, 119)
  ) +
  
  labs(
    x = "Days after transplanting (DAT)",
    y = "Leaf area per plant",
    title = ""
  ) +
  
  theme_bw(base_size = 11) +
  
  theme(
    strip.text = element_text(
      size = 11
    ),
    strip.background = element_rect(
      fill = "grey90",
      color = "grey50"
    ),
    panel.grid.minor = element_blank()
  )

print(grafico_trayectorias_af)

# 125. SAVE LEAF AREA TRAJECTORIES------------------

ggsave(
  file.path(carpeta_af, "21_Leaf_area_trajectories.pdf"),
  plot = grafico_trayectorias_af,
  width = 15,
  height = 10,
  units = "in"
)

ggsave(
  file.path(carpeta_af, "21_Leaf_area_trajectories.png"),
  plot = grafico_trayectorias_af,
  width = 10,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

# 126. DEFINE EVALUATION LABELS------------------

etiquetas_evaluacion_af <- c(
  "1" = "16 DAT",
  "2" = "44 DAT",
  "3" = "79 DAT",
  "4" = "119 DAT"
)

# 127. TUKEY GROUPING FIGURE------------------

grafico_grupos_af <- ggplot(
  tabla_grupos_af,
  aes(
    x = genotipo,
    y = emmean
  )
) +
  
  geom_point(
    color = "black",
    size = 2.4
  ) +
  
  geom_errorbar(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    width = 0.2,
    color = "grey35",
    linewidth = 0.5
  ) +
  
  geom_text(
    aes(
      y = upper.CL,
      label = .group
    ),
    vjust = -0.6,
    size = 3.2,
    color = "black"
  ) +
  
  scale_x_discrete(
    limits = niveles_ordenados_af,
    labels = etiquetas_genotipo_af,
    drop = FALSE
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0.05, 0.22)
    )
  ) +
  
  labs(
    x = "Genotype",
    y = expression("Estimated leaf area per plant (cm"^2*")"),
    title = ""
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 10,
      color = "black"
    ),
    axis.title = element_text(
      size = 12
    ),
    strip.text = element_text(
      size = 12,
      color = "black"
    ),
    strip.background = element_rect(
      fill = "grey90",
      color = "grey50"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# 128. ADD FACETS IF INTERACTION IS SIGNIFICANT------------------

if (interaccion_significativa_af) {
  
  grafico_grupos_af <- grafico_grupos_af +
    
    facet_wrap(
      ~ evaluacion,
      ncol = 2,
      scales = "free_y",
      labeller = labeller(
        evaluacion = as_labeller(
          etiquetas_evaluacion_af
        )
      )
    )
}

print(grafico_grupos_af)

# 129. SAVE TUKEY FIGURE------------------

ggsave(
  file.path(carpeta_af, "22_Tukey_leaf_area.pdf"),
  plot = grafico_grupos_af,
  width = 10,
  height = 6,
  units = "in"
)

ggsave(
  file.path(carpeta_af, "22_Tukey_leaf_area.png"),
  plot = grafico_grupos_af,
  width = 10,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

# 130. FINAL ANALYSIS SUMMARY------------------

cat("\n")
cat("LONGITUDINAL LEAF AREA ANALYSIS\n")
cat("--------------------------------\n")

cat(
  "Selected covariance structure:",
  nombre_seleccionado_af, "\n"
)

cat(
  "Genotype x time p-value:",
  p_interaccion_af, "\n"
)

cat(
  "Significant interaction:",
  interaccion_significativa_af, "\n"
)

cat("\nFINAL ANOVA\n")
print(anova_final_af)

cat("\nTUKEY LETTER GROUPS\n")
print(tabla_grupos_af, row.names = FALSE)

cat(
  "\nShapiro-Wilk p-value:",
  prueba_shapiro_af$p.value, "\n"
)

cat(
  "\nResults saved in:",
  normalizePath(carpeta_af), "\n"
)

# 131. SAVE COMPLETE ANALYSIS OBJECTS------------------

saveRDS(
  list(
    plant_data = af_planta,
    plot_data = af_parcela,
    covariance_models = modelos_af,
    model_selection = tabla_aic_af,
    final_model = modelo_final_af,
    interaction_test = prueba_interaccion_af,
    anova = anova_final_af,
    residual_diagnostics = diagnosticos_af,
    estimated_means = tabla_medias_fecha_af,
    tukey = tabla_tukey_af,
    tukey_groups = tabla_grupos_af,
    net_change = tabla_cambio_af,
    individual_regressions = tasas_af
  ),
  file = file.path(
    carpeta_af,
    "23_Complete_leaf_area_analysis.rds"
  )
)

cat("\nLEAF AREA ANALYSIS COMPLETED\n")
