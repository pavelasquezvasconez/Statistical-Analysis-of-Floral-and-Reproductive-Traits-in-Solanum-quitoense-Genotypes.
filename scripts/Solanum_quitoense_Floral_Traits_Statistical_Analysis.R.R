library(readxl)
df <- read_excel("D:/02 de junio del 2026/UDENAR/Orientados/2026 Freddy y Diana/Test4.xlsx",
                 sheet = "Hoja2")

# 0. INSTALAR PAQUETES ---------------------

paquetes <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "glmmTMB",
  "DHARMa",
  "emmeans",
  "multcomp",
  "multcompView"
)

faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

if(length(faltantes) > 0){
  install.packages(faltantes)
}


# 1. CARGAR PAQUETES ---------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(multcomp)
library(multcompView)


# 2. IMPORTAR DATOS ---------------------

df <- read_excel(
  "D:/02 de junio del 2026/UDENAR/Orientados/2026 Freddy y Diana/Test4.xlsx",
  sheet = "Hoja2"
)


# 3. PREPARAR LOS DATOS ---------------------

columnas_requeridas <- c(
  "Gen.",
  "Blq.",
  "PLANTA",
  "Botones florales",
  "Inflorescencias",
  "Flores abiertas",
  "Flores pistilo largo",
  "Flores pistilo medio",
  "Flores pistilo corto",
  "Flores pistilo ausente",
  "Flores pistilo atrofiado"
)

columnas_faltantes <- setdiff(
  columnas_requeridas,
  names(df)
)

if(length(columnas_faltantes) > 0){
  stop(
    paste(
      "Faltan las siguientes columnas en df:",
      paste(columnas_faltantes, collapse = ", ")
    )
  )
}

df2 <- df %>%
  rename(
    genotipo        = `Gen.`,
    bloque          = `Blq.`,
    planta          = PLANTA,
    botones         = `Botones florales`,
    inflorescencias = Inflorescencias,
    abiertas        = `Flores abiertas`,
    largo           = `Flores pistilo largo`,
    medio           = `Flores pistilo medio`,
    corto           = `Flores pistilo corto`,
    ausente         = `Flores pistilo ausente`,
    atrofiado       = `Flores pistilo atrofiado`
  ) %>%
  mutate(
    genotipo = trimws(as.character(genotipo)),
    bloque   = factor(bloque),
    planta   = factor(planta)
  )


# 4. ORDENAR LOS GENOTIPOS ---------------------

niveles_originales <- unique(df2$genotipo)

niveles_numericos <- niveles_originales[
  grepl("^[0-9]+$", niveles_originales)
]

niveles_texto <- niveles_originales[
  !grepl("^[0-9]+$", niveles_originales)
]

niveles_numericos <- as.character(
  sort(as.numeric(niveles_numericos))
)

orden_genotipos <- c(
  niveles_numericos,
  sort(niveles_texto)
)

df2 <- df2 %>%
  mutate(
    genotipo = factor(
      genotipo,
      levels = orden_genotipos
    )
  )

cat("\nOrden de genotipos:\n")
print(levels(df2$genotipo))


# 5. CREAR IDENTIFICADOR DE PARCELA ---------------------

# Diseño de bloques completos al azar (DBCA):
# Genotipo = efecto fijo.
# Bloque   = efecto fijo de ajuste.
# Parcela Genotipo x Bloque = unidad experimental.
# Las dos plantas dentro de cada parcela = submuestras.

df2 <- df2 %>%
  mutate(
    parcela = interaction(
      bloque,
      genotipo,
      drop = TRUE,
      sep = "_"
    )
  )


# 6. REVISAR ESTRUCTURA ---------------------

str(df2)
summary(df2)

cat("\nNúmero de genotipos:", nlevels(df2$genotipo), "\n")
cat("Número de bloques:", nlevels(df2$bloque), "\n")
cat("Número de observaciones individuales:", nrow(df2), "\n")
cat("Número de parcelas observadas:", nlevels(df2$parcela), "\n")


# 7. COMPROBAR VALORES PERDIDOS ---------------------

cat("\nNúmero de valores NA por variable:\n")
print(
  sapply(
    df2,
    function(x){
      sum(is.na(x))
    }
  )
)

variables_analisis <- c(
  "genotipo",
  "bloque",
  "planta",
  "botones",
  "inflorescencias",
  "abiertas",
  "largo",
  "medio",
  "corto",
  "ausente",
  "atrofiado"
)

if(anyNA(df2[variables_analisis])){
  stop(
    "Existen valores NA en variables necesarias para el análisis. Revise los datos antes de continuar."
  )
}


# 8. VERIFICAR DISEÑO GENOTIPO X BLOQUE ---------------------

tabla_diseno <- df2 %>%
  count(
    genotipo,
    bloque,
    name = "n_plantas"
  ) %>%
  arrange(
    genotipo,
    bloque
  )

cat("\nDiseño observado:\n")
print(
  tabla_diseno,
  n = Inf
)


# 9. IDENTIFICAR PARCELAS FALTANTES ---------------------

diseno_esperado <- expand_grid(
  genotipo = levels(df2$genotipo),
  bloque   = levels(df2$bloque)
)

diseno_observado <- df2 %>%
  transmute(
    genotipo = as.character(genotipo),
    bloque   = as.character(bloque)
  ) %>%
  distinct()

parcelas_faltantes <- diseno_esperado %>%
  mutate(
    genotipo = as.character(genotipo),
    bloque   = as.character(bloque)
  ) %>%
  anti_join(
    diseno_observado,
    by = c("genotipo", "bloque")
  )

cat("\nParcelas faltantes:\n")
print(
  parcelas_faltantes,
  n = Inf
)

cat("\nParcelas esperadas:", nrow(diseno_esperado), "\n")
cat("Parcelas observadas:", nrow(diseno_observado), "\n")
cat("Parcelas faltantes:", nrow(parcelas_faltantes), "\n")


# 10. VERIFICAR NÚMERO DE PLANTAS POR PARCELA ---------------------

plantas_parcela <- df2 %>%
  count(
    bloque,
    genotipo,
    parcela,
    name = "n_plantas"
  )

cat("\nFrecuencia del número de plantas por parcela:\n")
print(
  plantas_parcela %>%
    count(n_plantas)
)


# 11. DETENER SI ALGUNA PARCELA NO TIENE DOS PLANTAS ---------------------

if(!all(plantas_parcela$n_plantas == 2)){
  stop(
    paste(
      "Existen parcelas que no contienen exactamente dos plantas.",
      "El análisis debe revisarse antes de continuar."
    )
  )
}

cat(
  "\nCORRECTO: todas las parcelas observadas contienen exactamente dos plantas.\n"
)


# 12. VERIFICAR VARIABLES DE CONTEO ---------------------

variables_conteo <- c(
  "botones",
  "inflorescencias",
  "abiertas",
  "largo",
  "medio",
  "corto",
  "ausente",
  "atrofiado"
)

cat("\nValores negativos:\n")
print(
  sapply(
    df2[variables_conteo],
    function(x){
      sum(x < 0, na.rm = TRUE)
    }
  )
)

cat("\nValores no enteros:\n")
print(
  sapply(
    df2[variables_conteo],
    function(x){
      sum(
        abs(x - round(x)) > 1e-8,
        na.rm = TRUE
      )
    }
  )
)

if(
  any(
    sapply(
      df2[variables_conteo],
      function(x){
        any(x < 0, na.rm = TRUE)
      }
    )
  )
){
  stop("Existen conteos negativos. Revise los datos antes de continuar.")
}

if(
  any(
    sapply(
      df2[variables_conteo],
      function(x){
        any(abs(x - round(x)) > 1e-8, na.rm = TRUE)
      }
    )
  )
){
  stop("Existen valores no enteros en variables de conteo. Revise los datos antes de continuar.")
}


# 13. AGREGAR LAS DOS PLANTAS POR PARCELA ---------------------

# Para los modelos de conteo se utiliza la SUMA de las dos plantas.
# La parcela es la unidad experimental.
# Las medias finales se expresan por planta dividiendo entre 2.

df_parcela <- df2 %>%
  group_by(
    genotipo,
    bloque,
    parcela
  ) %>%
  summarise(
    n_plantas = n(),
    botones = sum(botones),
    inflorescencias = sum(inflorescencias),
    abiertas = sum(abiertas),
    largo = sum(largo),
    medio = sum(medio),
    corto = sum(corto),
    ausente = sum(ausente),
    atrofiado = sum(atrofiado),
    .groups = "drop"
  ) %>%
  mutate(
    genotipo = droplevels(genotipo),
    bloque = droplevels(bloque),
    parcela = droplevels(parcela)
  )


# 14. REVISAR BASE A NIVEL DE PARCELA ---------------------

str(df_parcela)
summary(df_parcela)

cat(
  "\nNúmero final de unidades experimentales:",
  nrow(df_parcela),
  "\n"
)

cat("\nNúmero de plantas por parcela:\n")
print(
  table(df_parcela$n_plantas)
)


# 15. CREAR VARIABLES PROMEDIO POR PLANTA ---------------------

# Se usan para interpretación, tablas descriptivas y gráficos.
# Los modelos de conteo se ajustan con las sumas por parcela.

df_parcela <- df_parcela %>%
  mutate(
    botones_promedio = botones / n_plantas,
    inflorescencias_promedio = inflorescencias / n_plantas,
    abiertas_promedio = abiertas / n_plantas
  )


# 16. RESUMEN DESCRIPTIVO POR GENOTIPO ---------------------

resumen_genotipo <- df_parcela %>%
  group_by(genotipo) %>%
  summarise(
    n_parcelas = n(),
    botones_media = mean(botones_promedio),
    botones_sd = sd(botones_promedio),
    inflorescencias_media = mean(inflorescencias_promedio),
    inflorescencias_sd = sd(inflorescencias_promedio),
    abiertas_media = mean(abiertas_promedio),
    abiertas_sd = sd(abiertas_promedio),
    .groups = "drop"
  )

cat("\nResumen descriptivo por genotipo:\n")
print(
  resumen_genotipo,
  n = Inf
)


# 17. VERIFICAR ESTRUCTURA DE LOS PISTILOS ---------------------

df_parcela <- df_parcela %>%
  mutate(
    suma_pistilos = largo + medio + corto + ausente,
    diferencia_pistilos = abiertas - suma_pistilos
  )

cat("\nFlores abiertas - suma de tipos de pistilo:\n")
print(
  table(
    df_parcela$diferencia_pistilos,
    useNA = "ifany"
  )
)

if(any(df_parcela$diferencia_pistilos != 0, na.rm = TRUE)){
  warning(
    paste(
      "Existen parcelas donde largo + medio + corto + ausente",
      "no coincide con flores abiertas."
    )
  )
}


# 18. VERIFICAR PISTILO ATROFIADO ---------------------
cat("\n¿Atrofiado <= flores abiertas?\n")
print(
  table(
    df_parcela$atrofiado <= df_parcela$abiertas,
    useNA = "ifany"
  )
)


# 19. FUNCIÓN PARA CAPTURAR ERRORES DE AJUSTE ---------------------

ajuste_seguro <- function(expr){
  tryCatch(
    expr,
    error = function(e){
      message(
        "El modelo no pudo ajustarse: ",
        conditionMessage(e)
      )
      NULL
    }
  )
}


# 20. FUNCIÓN PARA NORMALIZAR LA LLAMADA DEL MODELO ---------------------

# Esto evita problemas posteriores si se consulta el modelo fuera de la función.

normalizar_llamada <- function(modelo, formula_objeto, nombre_datos){
  
  if(is.null(modelo)){
    return(NULL)
  }
  
  modelo$call$formula <- formula_objeto
  modelo$call$data <- as.name(nombre_datos)
  
  return(modelo)
}


# 21. FUNCIÓN PARA VERIFICAR CONVERGENCIA ---------------------

modelo_valido <- function(modelo){
  
  if(is.null(modelo)){
    return(FALSE)
  }
  
  if(inherits(modelo, "try-error")){
    return(FALSE)
  }
  
  if(
    !is.null(modelo$fit$convergence) &&
    modelo$fit$convergence != 0
  ){
    return(FALSE)
  }
  
  if(
    !is.null(modelo$sdr$pdHess) &&
    !isTRUE(modelo$sdr$pdHess)
  ){
    return(FALSE)
  }
  
  return(TRUE)
}


# 22. FUNCIÓN PARA COMPARAR AIC ---------------------

comparar_AIC <- function(modelos){
  
  modelos_validos <- modelos[
    sapply(
      modelos,
      modelo_valido
    )
  ]
  
  if(length(modelos_validos) == 0){
    stop("Ninguno de los modelos convergió correctamente.")
  }
  
  tabla <- data.frame(
    modelo = names(modelos_validos),
    df = sapply(
      modelos_validos,
      function(x){
        attr(logLik(x), "df")
      }
    ),
    AIC = sapply(
      modelos_validos,
      AIC
    ),
    row.names = NULL,
    check.names = FALSE
  )
  
  tabla <- tabla %>%
    arrange(AIC) %>%
    mutate(
      Delta_AIC = AIC - min(AIC)
    )
  
  return(tabla)
}


# 23. FUNCIÓN PARA CONVERTIR RESULTADOS A UNIDADES POR PLANTA ---------------------

convertir_por_planta <- function(tabla, divisor = 2){
  
  columnas_escalar <- intersect(
    c(
      "response",
      "rate",
      "emmean",
      "SE",
      "asymp.LCL",
      "asymp.UCL",
      "lower.CL",
      "upper.CL"
    ),
    names(tabla)
  )
  
  if(length(columnas_escalar) > 0){
    tabla[columnas_escalar] <- lapply(
      tabla[columnas_escalar],
      function(x){
        x / divisor
      }
    )
  }
  
  return(tabla)
}


# 24. FUNCIÓN PARA ANALIZAR VARIABLES DE CONTEO ---------------------

analizar_conteo <- function(variable, nombre_variable){
  
  cat("\n\n")
  cat("############################################################\n")
  cat("VARIABLE:", nombre_variable, "\n")
  cat("############################################################\n")
  
  
  # 24.1. DEFINIR FÓRMULA DEL MODELO COMPLETO ---------------------
  
  formula_modelo <- reformulate(
    termlabels = c("genotipo", "bloque"),
    response = variable
  )
  
  environment(formula_modelo) <- .GlobalEnv
  
  cat("\nFórmula del modelo:\n")
  print(formula_modelo)
  
  
  # 24.2. AJUSTAR MODELO POISSON ---------------------
  
  m_pois <- ajuste_seguro(
    glmmTMB(
      formula = formula_modelo,
      family = poisson(),
      data = df_parcela,
      ziformula = ~0
    )
  )
  
  m_pois <- normalizar_llamada(
    m_pois,
    formula_modelo,
    "df_parcela"
  )
  
  
  # 24.3. AJUSTAR MODELO NB1 ---------------------
  
  m_nb1 <- ajuste_seguro(
    glmmTMB(
      formula = formula_modelo,
      family = nbinom1(),
      data = df_parcela,
      ziformula = ~0
    )
  )
  
  m_nb1 <- normalizar_llamada(
    m_nb1,
    formula_modelo,
    "df_parcela"
  )
  
  
  # 24.4. AJUSTAR MODELO NB2 ---------------------
  
  m_nb2 <- ajuste_seguro(
    glmmTMB(
      formula = formula_modelo,
      family = nbinom2(),
      data = df_parcela,
      ziformula = ~0
    )
  )
  
  m_nb2 <- normalizar_llamada(
    m_nb2,
    formula_modelo,
    "df_parcela"
  )
  
  
  # 24.5. COMPROBAR CONVERGENCIA ---------------------
  
  modelos <- list(
    Poisson = m_pois,
    NB1 = m_nb1,
    NB2 = m_nb2
  )
  
  convergencia <- sapply(
    modelos,
    modelo_valido
  )
  
  cat("\nConvergencia de modelos:\n")
  print(convergencia)
  
  modelos_validos <- modelos[convergencia]
  
  if(length(modelos_validos) == 0){
    stop(
      paste(
        "Ningún modelo convergió correctamente para",
        nombre_variable
      )
    )
  }
  
  
  # 24.6. COMPARAR MODELOS MEDIANTE AIC ---------------------
  
  tabla_AIC <- comparar_AIC(
    modelos_validos
  )
  
  cat("\nComparación mediante AIC:\n")
  print(tabla_AIC)
  
  
  # 24.7. SELECCIONAR MODELO CON MENOR AIC ---------------------
  
  nombre_base <- tabla_AIC$modelo[1]
  
  modelo_final <- modelos_validos[[nombre_base]]
  
  cat(
    "\nModelo inicialmente seleccionado:",
    nombre_base,
    "\n"
  )
  
  
  # 24.8. DIAGNÓSTICO DHARMa INICIAL ---------------------
  
  set.seed(123)
  
  residuos <- simulateResiduals(
    fittedModel = modelo_final,
    n = 1000
  )
  
  plot(residuos)
  
  uniformidad <- testUniformity(residuos)
  dispersion <- testDispersion(residuos)
  ceros <- testZeroInflation(residuos)
  
  cat("\nPrueba de uniformidad:\n")
  print(uniformidad)
  
  cat("\nPrueba de dispersión:\n")
  print(dispersion)
  
  cat("\nPrueba de inflación de ceros:\n")
  print(ceros)
  
  
  # 24.9. EVITAR POISSON SI PRESENTA DISPERSIÓN INADECUADA ---------------------
  
  if(
    nombre_base == "Poisson" &&
    dispersion$p.value < 0.05
  ){
    
    cat(
      "\nPoisson presenta dispersión inadecuada.\n",
      "Se evaluarán NB1 y NB2.\n"
    )
    
    candidatos_nb <- modelos_validos[
      names(modelos_validos) %in% c("NB1", "NB2")
    ]
    
    if(length(candidatos_nb) > 0){
      
      tabla_nb <- comparar_AIC(candidatos_nb)
      
      nombre_base <- tabla_nb$modelo[1]
      
      modelo_final <- candidatos_nb[[nombre_base]]
      
      cat(
        "\nSe cambia al modelo:",
        nombre_base,
        "\n"
      )
      
      set.seed(123)
      
      residuos <- simulateResiduals(
        fittedModel = modelo_final,
        n = 1000
      )
      
      plot(residuos)
      
      uniformidad <- testUniformity(residuos)
      dispersion <- testDispersion(residuos)
      ceros <- testZeroInflation(residuos)
      
      cat("\nUniformidad:\n")
      print(uniformidad)
      
      cat("\nDispersión:\n")
      print(dispersion)
      
      cat("\nInflación de ceros:\n")
      print(ceros)
    }
  }
  
  
  # 24.10. EVALUAR MODELO ZERO-INFLATED ---------------------
  
  modelo_es_ZI <- FALSE
  comparacion_zi <- NULL
  
  if(
    is.finite(ceros$p.value) &&
    ceros$p.value < 0.05
  ){
    
    cat(
      "\nSe detectó evidencia de exceso de ceros.\n",
      "Se evaluará un modelo con inflación de ceros.\n"
    )
    
    if(nombre_base == "Poisson"){
      
      modelo_zi <- ajuste_seguro(
        glmmTMB(
          formula = formula_modelo,
          family = poisson(),
          data = df_parcela,
          ziformula = ~1
        )
      )
      
    } else if(nombre_base == "NB1"){
      
      modelo_zi <- ajuste_seguro(
        glmmTMB(
          formula = formula_modelo,
          family = nbinom1(),
          data = df_parcela,
          ziformula = ~1
        )
      )
      
    } else if(nombre_base == "NB2"){
      
      modelo_zi <- ajuste_seguro(
        glmmTMB(
          formula = formula_modelo,
          family = nbinom2(),
          data = df_parcela,
          ziformula = ~1
        )
      )
      
    } else {
      
      modelo_zi <- NULL
    }
    
    modelo_zi <- normalizar_llamada(
      modelo_zi,
      formula_modelo,
      "df_parcela"
    )
    
    if(modelo_valido(modelo_zi)){
      
      AIC_base <- AIC(modelo_final)
      AIC_zi <- AIC(modelo_zi)
      mejora_AIC <- AIC_base - AIC_zi
      
      comparacion_zi <- data.frame(
        modelo = c(
          "Convencional",
          "Zero_inflated"
        ),
        AIC = c(
          AIC_base,
          AIC_zi
        ),
        row.names = NULL
      )
      
      comparacion_zi$Delta_AIC <-
        comparacion_zi$AIC -
        min(comparacion_zi$AIC)
      
      cat("\nComparación convencional vs zero-inflated:\n")
      print(comparacion_zi)
      
      if(mejora_AIC >= 2){
        
        modelo_final <- modelo_zi
        modelo_es_ZI <- TRUE
        
        cat(
          "\nEl modelo zero-inflated mejora el AIC",
          "en al menos 2 unidades.\n",
          "Se selecciona como modelo final.\n"
        )
        
      } else if(mejora_AIC > 0){
        
        cat(
          "\nLos dos modelos son competitivos (Delta AIC < 2).\n",
          "Se conserva el modelo convencional por parsimonia.\n"
        )
        
      } else {
        
        cat(
          "\nEl modelo zero-inflated no mejora el ajuste.\n",
          "Se conserva el modelo convencional.\n"
        )
      }
    }
  }
  
  
  # 24.11. NOMBRE DEL MODELO FINAL ---------------------
  
  if(modelo_es_ZI){
    nombre_final <- paste0(nombre_base, "_ZI")
  } else {
    nombre_final <- nombre_base
  }
  
  
  # 24.12. DIAGNÓSTICO DEL MODELO FINAL ---------------------
  
  set.seed(123)
  
  residuos <- simulateResiduals(
    fittedModel = modelo_final,
    n = 1000
  )
  
  plot(residuos)
  
  uniformidad <- testUniformity(residuos)
  dispersion <- testDispersion(residuos)
  ceros <- testZeroInflation(residuos)
  
  cat("\nDiagnóstico del modelo final:\n")
  
  cat("\nUniformidad:\n")
  print(uniformidad)
  
  cat("\nDispersión:\n")
  print(dispersion)
  
  cat("\nInflación de ceros:\n")
  print(ceros)
  
  
  # 24.13. RESUMEN DEL MODELO FINAL ---------------------
  
  cat("\n")
  cat("============================================================\n")
  cat("MODELO FINAL\n")
  cat("============================================================\n")
  
  print(summary(modelo_final))
  
  cat(
    "\nModelo seleccionado:",
    nombre_final,
    "\n"
  )
  
  
  # 24.14. CREAR MODELO REDUCIDO SIN GENOTIPO ---------------------
  
  formula_reducida <- reformulate(
    termlabels = "bloque",
    response = variable
  )
  
  environment(formula_reducida) <- .GlobalEnv
  
  if(nombre_base == "Poisson"){
    
    modelo_reducido <- ajuste_seguro(
      glmmTMB(
        formula = formula_reducida,
        family = poisson(),
        data = df_parcela,
        ziformula = if(modelo_es_ZI) ~1 else ~0
      )
    )
    
  } else if(nombre_base == "NB1"){
    
    modelo_reducido <- ajuste_seguro(
      glmmTMB(
        formula = formula_reducida,
        family = nbinom1(),
        data = df_parcela,
        ziformula = if(modelo_es_ZI) ~1 else ~0
      )
    )
    
  } else if(nombre_base == "NB2"){
    
    modelo_reducido <- ajuste_seguro(
      glmmTMB(
        formula = formula_reducida,
        family = nbinom2(),
        data = df_parcela,
        ziformula = if(modelo_es_ZI) ~1 else ~0
      )
    )
    
  } else {
    
    modelo_reducido <- NULL
  }
  
  modelo_reducido <- normalizar_llamada(
    modelo_reducido,
    formula_reducida,
    "df_parcela"
  )
  
  if(!modelo_valido(modelo_reducido)){
    stop(
      paste(
        "El modelo reducido no convergió para",
        nombre_variable
      )
    )
  }
  
  
  # 24.15. PRUEBA GLOBAL DEL EFECTO DE GENOTIPO ---------------------
  
  prueba_genotipo <- anova(
    modelo_reducido,
    modelo_final
  )
  
  cat("\n")
  cat("============================================================\n")
  cat("PRUEBA GLOBAL DEL EFECTO DE GENOTIPO\n")
  cat("============================================================\n")
  
  print(prueba_genotipo)
  
  
  # 24.16. MEDIAS MARGINALES AJUSTADAS ---------------------
  
  if(modelo_es_ZI){
    
    emm <- emmeans(
      modelo_final,
      ~ genotipo,
      component = "response",
      weights = "equal"
    )
    
  } else {
    
    emm <- emmeans(
      modelo_final,
      ~ genotipo,
      type = "response",
      weights = "equal"
    )
  }
  
  
  # 24.17. MEDIAS AJUSTADAS POR PLANTA ---------------------
  
  emm_parcela <- as.data.frame(emm)
  
  emm_planta <- convertir_por_planta(
    emm_parcela,
    divisor = 2
  )
  
  cat("\n")
  cat("============================================================\n")
  cat("MEDIAS AJUSTADAS POR PLANTA\n")
  cat("============================================================\n")
  
  print(
    emm_planta,
    row.names = FALSE
  )
  
  
  # 24.18. COMPARACIONES MÚLTIPLES DE TUKEY ---------------------
  
  tukey <- pairs(
    emm,
    adjust = "tukey"
  )
  
  cat("\n")
  cat("============================================================\n")
  cat("COMPARACIONES MÚLTIPLES DE TUKEY\n")
  cat("============================================================\n")
  
  print(tukey)
  
  
  # 24.19. LETRAS DE AGRUPACIÓN ---------------------
  
  letras <- multcomp::cld(
    emm,
    adjust = "tukey",
    Letters = letters,
    sort = FALSE
  )
  
  letras_parcela <- as.data.frame(letras)
  
  letras_planta <- convertir_por_planta(
    letras_parcela,
    divisor = 2
  )
  
  letras_planta$.group <- gsub(
    " ",
    "",
    letras_planta$.group
  )
  
  cat("\n")
  cat("============================================================\n")
  cat("GRUPOS DE TUKEY - MEDIAS POR PLANTA\n")
  cat("============================================================\n")
  
  print(
    letras_planta,
    row.names = FALSE
  )
  
  
  # 24.20. RETORNAR RESULTADOS ---------------------
  
  return(
    list(
      modelo_final = modelo_final,
      modelo_reducido = modelo_reducido,
      modelo_seleccionado = nombre_final,
      AIC = tabla_AIC,
      comparacion_ZI = comparacion_zi,
      residuos = residuos,
      uniformidad = uniformidad,
      dispersion = dispersion,
      ceros = ceros,
      prueba_genotipo = prueba_genotipo,
      emmeans = emm,
      emmeans_por_planta = emm_planta,
      tukey = tukey,
      letras = letras_planta
    )
  )
}


# 25. ANALIZAR BOTONES FLORALES ---------------------

resultado_botones <- analizar_conteo(
  variable = "botones",
  nombre_variable = "Botones florales"
)


# 26. ANALIZAR INFLORESCENCIAS ---------------------

resultado_inflo <- analizar_conteo(
  variable = "inflorescencias",
  nombre_variable = "Inflorescencias"
)


# 27. ANALIZAR FLORES ABIERTAS ---------------------

resultado_abiertas <- analizar_conteo(
  variable = "abiertas",
  nombre_variable = "Flores abiertas"
)


# 28. CREAR BASE PARA ANÁLISIS DE PISTILOS ---------------------

# Las proporciones solo están definidas si hay al menos una flor abierta.

niveles_genotipo_total <- levels(df_parcela$genotipo)

df_pistilo <- df_parcela %>%
  filter(abiertas > 0) %>%
  droplevels()

genotipos_sin_flores <- setdiff(
  niveles_genotipo_total,
  levels(df_pistilo$genotipo)
)

cat("\nParcelas totales:", nrow(df_parcela), "\n")
cat(
  "Parcelas con al menos una flor abierta:",
  nrow(df_pistilo),
  "\n"
)
cat(
  "Parcelas sin flores abiertas:",
  sum(df_parcela$abiertas == 0),
  "\n"
)

if(length(genotipos_sin_flores) > 0){
  cat(
    "\nGenotipos sin ninguna parcela con flores abiertas:",
    paste(genotipos_sin_flores, collapse = ", "),
    "\n"
  )
}


# 29. NÚMERO DE PARCELAS EVALUABLES POR GENOTIPO ---------------------

cat("\nParcelas con flores abiertas por genotipo:\n")
print(
  df_pistilo %>%
    count(
      genotipo,
      name = "n_parcelas"
    ),
  n = Inf
)


# 30. FUNCIÓN PARA ANALIZAR PROPORCIONES DE PISTILO ---------------------

analizar_proporcion <- function(variable, nombre_variable){
  
  cat("\n\n")
  cat("############################################################\n")
  cat("VARIABLE:", nombre_variable, "\n")
  cat("############################################################\n")
  
  
  # 30.1. VERIFICAR ÉXITOS <= TOTAL ---------------------
  
  if(
    any(
      df_pistilo[[variable]] > df_pistilo$abiertas,
      na.rm = TRUE
    )
  ){
    stop(
      paste(
        "La variable",
        variable,
        "presenta valores mayores que el número de flores abiertas."
      )
    )
  }
  
  
  # 30.2. DEFINIR FÓRMULA DEL MODELO COMPLETO ---------------------
  
  respuesta_prop <- paste0(
    "cbind(",
    variable,
    ", abiertas - ",
    variable,
    ")"
  )
  
  formula_prop <- as.formula(
    paste0(
      respuesta_prop,
      " ~ genotipo + bloque"
    )
  )
  
  environment(formula_prop) <- .GlobalEnv
  
  cat("\nFórmula:\n")
  print(formula_prop)
  
  
  # 30.3. MODELO BINOMIAL ---------------------
  
  m_bin <- ajuste_seguro(
    glmmTMB(
      formula = formula_prop,
      family = binomial(),
      data = df_pistilo,
      ziformula = ~0
    )
  )
  
  m_bin <- normalizar_llamada(
    m_bin,
    formula_prop,
    "df_pistilo"
  )
  
  
  # 30.4. MODELO BETA-BINOMIAL ---------------------
  
  m_bb <- ajuste_seguro(
    glmmTMB(
      formula = formula_prop,
      family = betabinomial(),
      data = df_pistilo,
      ziformula = ~0
    )
  )
  
  m_bb <- normalizar_llamada(
    m_bb,
    formula_prop,
    "df_pistilo"
  )
  
  
  # 30.5. COMPROBAR CONVERGENCIA ---------------------
  
  modelos <- list(
    Binomial = m_bin,
    Beta_binomial = m_bb
  )
  
  convergencia <- sapply(
    modelos,
    modelo_valido
  )
  
  cat("\nConvergencia:\n")
  print(convergencia)
  
  modelos_validos <- modelos[convergencia]
  
  if(length(modelos_validos) == 0){
    stop(
      paste(
        "Ningún modelo convergió para",
        nombre_variable
      )
    )
  }
  
  
  # 30.6. COMPARAR MODELOS ---------------------
  
  tabla_AIC <- comparar_AIC(
    modelos_validos
  )
  
  cat("\nComparación mediante AIC:\n")
  print(tabla_AIC)
  
  
  # 30.7. SELECCIONAR MODELO ---------------------
  
  nombre_mejor <- tabla_AIC$modelo[1]
  
  modelo_final <- modelos_validos[[nombre_mejor]]
  
  cat(
    "\nModelo inicialmente seleccionado:",
    nombre_mejor,
    "\n"
  )
  
  
  # 30.8. DIAGNÓSTICO DHARMa ---------------------
  
  set.seed(123)
  
  residuos <- simulateResiduals(
    fittedModel = modelo_final,
    n = 1000
  )
  
  plot(residuos)
  
  uniformidad <- testUniformity(residuos)
  dispersion <- testDispersion(residuos)
  
  cat("\nPrueba de uniformidad:\n")
  print(uniformidad)
  
  cat("\nPrueba de dispersión:\n")
  print(dispersion)
  
  
  # 30.9. CAMBIAR A BETA-BINOMIAL SI BINOMIAL PRESENTA SOBREDISPERSIÓN ---------------------
  
  if(
    nombre_mejor == "Binomial" &&
    dispersion$p.value < 0.05 &&
    "Beta_binomial" %in% names(modelos_validos)
  ){
    
    cat(
      "\nEl modelo binomial presenta sobredispersión.\n",
      "Se utilizará el modelo beta-binomial.\n"
    )
    
    nombre_mejor <- "Beta_binomial"
    modelo_final <- modelos_validos[["Beta_binomial"]]
    
    set.seed(123)
    
    residuos <- simulateResiduals(
      fittedModel = modelo_final,
      n = 1000
    )
    
    plot(residuos)
    
    uniformidad <- testUniformity(residuos)
    dispersion <- testDispersion(residuos)
    
    cat("\nUniformidad:\n")
    print(uniformidad)
    
    cat("\nDispersión:\n")
    print(dispersion)
  }
  
  
  # 30.10. RESUMEN DEL MODELO FINAL ---------------------
  
  cat("\n")
  cat("============================================================\n")
  cat("MODELO FINAL\n")
  cat("============================================================\n")
  
  print(summary(modelo_final))
  
  cat(
    "\nModelo seleccionado:",
    nombre_mejor,
    "\n"
  )
  
  
  # 30.11. CREAR MODELO REDUCIDO SIN GENOTIPO ---------------------
  
  formula_reducida <- as.formula(
    paste0(
      respuesta_prop,
      " ~ bloque"
    )
  )
  
  environment(formula_reducida) <- .GlobalEnv
  
  if(nombre_mejor == "Binomial"){
    
    modelo_reducido <- ajuste_seguro(
      glmmTMB(
        formula = formula_reducida,
        family = binomial(),
        data = df_pistilo,
        ziformula = ~0
      )
    )
    
  } else {
    
    modelo_reducido <- ajuste_seguro(
      glmmTMB(
        formula = formula_reducida,
        family = betabinomial(),
        data = df_pistilo,
        ziformula = ~0
      )
    )
  }
  
  modelo_reducido <- normalizar_llamada(
    modelo_reducido,
    formula_reducida,
    "df_pistilo"
  )
  
  if(!modelo_valido(modelo_reducido)){
    stop(
      paste(
        "El modelo reducido no convergió para",
        nombre_variable
      )
    )
  }
  
  
  # 30.12. PRUEBA GLOBAL DEL EFECTO DE GENOTIPO ---------------------
  
  prueba_genotipo <- anova(
    modelo_reducido,
    modelo_final
  )
  
  cat("\n")
  cat("============================================================\n")
  cat("PRUEBA GLOBAL DEL EFECTO DE GENOTIPO\n")
  cat("============================================================\n")
  
  print(prueba_genotipo)
  
  
  # 30.13. PROPORCIONES AJUSTADAS ---------------------
  
  emm <- emmeans(
    modelo_final,
    ~ genotipo,
    type = "response",
    weights = "equal"
  )
  
  cat("\n")
  cat("============================================================\n")
  cat("PROPORCIONES AJUSTADAS\n")
  cat("============================================================\n")
  
  print(emm)
  
  
  # 30.14. COMPARACIONES DE TUKEY ---------------------
  
  tukey <- pairs(
    emm,
    adjust = "tukey"
  )
  
  cat("\n")
  cat("============================================================\n")
  cat("COMPARACIONES MÚLTIPLES DE TUKEY\n")
  cat("============================================================\n")
  
  print(tukey)
  
  
  # 30.15. LETRAS DE AGRUPACIÓN ---------------------
  
  letras <- multcomp::cld(
    emm,
    adjust = "tukey",
    Letters = letters,
    sort = FALSE
  )
  
  letras <- as.data.frame(letras)
  
  letras$.group <- gsub(
    " ",
    "",
    letras$.group
  )
  
  cat("\n")
  cat("============================================================\n")
  cat("GRUPOS DE TUKEY\n")
  cat("============================================================\n")
  
  print(
    letras,
    row.names = FALSE
  )
  
  
  # 30.16. RETORNAR RESULTADOS ---------------------
  
  return(
    list(
      modelo_final = modelo_final,
      modelo_reducido = modelo_reducido,
      modelo_seleccionado = nombre_mejor,
      AIC = tabla_AIC,
      residuos = residuos,
      uniformidad = uniformidad,
      dispersion = dispersion,
      prueba_genotipo = prueba_genotipo,
      emmeans = emm,
      tukey = tukey,
      letras = letras
    )
  )
}


# 31. ANALIZAR PISTILO LARGO ---------------------

resultado_largo <- analizar_proporcion(
  variable = "largo",
  nombre_variable = "Proporción de flores con pistilo largo"
)


# 32. ANALIZAR PISTILO MEDIO ---------------------

resultado_medio <- analizar_proporcion(
  variable = "medio",
  nombre_variable = "Proporción de flores con pistilo medio"
)


# 33. ANALIZAR PISTILO CORTO ---------------------

resultado_corto <- analizar_proporcion(
  variable = "corto",
  nombre_variable = "Proporción de flores con pistilo corto"
)


# 34. ANALIZAR PISTILO AUSENTE ---------------------

resultado_ausente <- analizar_proporcion(
  variable = "ausente",
  nombre_variable = "Proporción de flores con pistilo ausente"
)


# 35. ANALIZAR PISTILO ATROFIADO ---------------------

resultado_atrofiado <- NULL

if(
  all(
    df_pistilo$atrofiado <= df_pistilo$abiertas,
    na.rm = TRUE
  )
){
  
  resultado_atrofiado <- analizar_proporcion(
    variable = "atrofiado",
    nombre_variable = "Proporción de flores con pistilo atrofiado"
  )
  
} else {
  
  warning(
    paste(
      "Pistilo atrofiado presenta valores mayores que flores abiertas.",
      "No se analizará como proporción."
    )
  )
}


# 36. MOSTRAR MODELOS FINALES SELECCIONADOS ---------------------

cat("\n")
cat("############################################################\n")
cat("MODELOS FINALES SELECCIONADOS\n")
cat("############################################################\n")

cat("\nBotones florales:", resultado_botones$modelo_seleccionado)
cat("\nInflorescencias:", resultado_inflo$modelo_seleccionado)
cat("\nFlores abiertas:", resultado_abiertas$modelo_seleccionado)
cat("\nPistilo largo:", resultado_largo$modelo_seleccionado)
cat("\nPistilo medio:", resultado_medio$modelo_seleccionado)
cat("\nPistilo corto:", resultado_corto$modelo_seleccionado)
cat("\nPistilo ausente:", resultado_ausente$modelo_seleccionado)

if(!is.null(resultado_atrofiado)){
  cat("\nPistilo atrofiado:", resultado_atrofiado$modelo_seleccionado)
}

cat("\n")


# 37. MOSTRAR PRUEBAS GLOBALES ---------------------

cat("\n")
cat("############################################################\n")
cat("PRUEBAS GLOBALES DEL EFECTO DE GENOTIPO\n")
cat("############################################################\n")

cat("\nBOTONES FLORALES\n")
print(resultado_botones$prueba_genotipo)

cat("\nINFLORESCENCIAS\n")
print(resultado_inflo$prueba_genotipo)

cat("\nFLORES ABIERTAS\n")
print(resultado_abiertas$prueba_genotipo)

cat("\nPISTILO LARGO\n")
print(resultado_largo$prueba_genotipo)

cat("\nPISTILO MEDIO\n")
print(resultado_medio$prueba_genotipo)

cat("\nPISTILO CORTO\n")
print(resultado_corto$prueba_genotipo)

cat("\nPISTILO AUSENTE\n")
print(resultado_ausente$prueba_genotipo)

if(!is.null(resultado_atrofiado)){
  cat("\nPISTILO ATROFIADO\n")
  print(resultado_atrofiado$prueba_genotipo)
}


# 38. MOSTRAR LETRAS DE TUKEY ---------------------

cat("\n")
cat("############################################################\n")
cat("AGRUPACIONES DE TUKEY\n")
cat("############################################################\n")

cat("\nBOTONES FLORALES - POR PLANTA\n")
print(
  resultado_botones$letras,
  row.names = FALSE
)

cat("\nINFLORESCENCIAS - POR PLANTA\n")
print(
  resultado_inflo$letras,
  row.names = FALSE
)

cat("\nFLORES ABIERTAS - POR PLANTA\n")
print(
  resultado_abiertas$letras,
  row.names = FALSE
)

cat("\nPISTILO LARGO\n")
print(
  resultado_largo$letras,
  row.names = FALSE
)

cat("\nPISTILO MEDIO\n")
print(
  resultado_medio$letras,
  row.names = FALSE
)

cat("\nPISTILO CORTO\n")
print(
  resultado_corto$letras,
  row.names = FALSE
)

cat("\nPISTILO AUSENTE\n")
print(
  resultado_ausente$letras,
  row.names = FALSE
)

if(!is.null(resultado_atrofiado)){
  cat("\nPISTILO ATROFIADO\n")
  print(
    resultado_atrofiado$letras,
    row.names = FALSE
  )
}


# 39. GUARDAR RESULTADOS DESCRIPTIVOS ---------------------

write.csv(
  resumen_genotipo,
  "00_Resumen_descriptivo_genotipos.csv",
  row.names = FALSE
)

write.csv(
  parcelas_faltantes,
  "01_Parcelas_faltantes.csv",
  row.names = FALSE
)


# 40. GUARDAR RESULTADOS DE BOTONES ---------------------

write.csv(
  resultado_botones$AIC,
  "02_AIC_Botones.csv",
  row.names = FALSE
)

write.csv(
  resultado_botones$emmeans_por_planta,
  "03_Medias_ajustadas_Botones_por_planta.csv",
  row.names = FALSE
)

write.csv(
  resultado_botones$letras,
  "04_Tukey_Botones_por_planta.csv",
  row.names = FALSE
)


# 41. GUARDAR RESULTADOS DE INFLORESCENCIAS ---------------------

write.csv(
  resultado_inflo$AIC,
  "05_AIC_Inflorescencias.csv",
  row.names = FALSE
)

write.csv(
  resultado_inflo$emmeans_por_planta,
  "06_Medias_ajustadas_Inflorescencias_por_planta.csv",
  row.names = FALSE
)

write.csv(
  resultado_inflo$letras,
  "07_Tukey_Inflorescencias_por_planta.csv",
  row.names = FALSE
)


# 42. GUARDAR RESULTADOS DE FLORES ABIERTAS ---------------------

write.csv(
  resultado_abiertas$AIC,
  "08_AIC_Flores_abiertas.csv",
  row.names = FALSE
)

write.csv(
  resultado_abiertas$emmeans_por_planta,
  "09_Medias_ajustadas_Flores_abiertas_por_planta.csv",
  row.names = FALSE
)

write.csv(
  resultado_abiertas$letras,
  "10_Tukey_Flores_abiertas_por_planta.csv",
  row.names = FALSE
)


# 43. GUARDAR RESULTADOS DE PISTILOS ---------------------

write.csv(
  resultado_largo$letras,
  "11_Tukey_Pistilo_largo.csv",
  row.names = FALSE
)

write.csv(
  resultado_medio$letras,
  "12_Tukey_Pistilo_medio.csv",
  row.names = FALSE
)

write.csv(
  resultado_corto$letras,
  "13_Tukey_Pistilo_corto.csv",
  row.names = FALSE
)

write.csv(
  resultado_ausente$letras,
  "14_Tukey_Pistilo_ausente.csv",
  row.names = FALSE
)

if(!is.null(resultado_atrofiado)){
  write.csv(
    resultado_atrofiado$letras,
    "15_Tukey_Pistilo_atrofiado.csv",
    row.names = FALSE
  )
}


# 44. GRÁFICO DESCRIPTIVO DE BOTONES ---------------------

grafico_botones <- ggplot(
  df_parcela,
  aes(
    x = genotipo,
    y = botones_promedio
  )
) +
  geom_boxplot(
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    height = 0
  ) +
  labs(
    x = "Genotipo",
    y = "Botones florales por planta",
    title = "Botones florales por genotipo"
  ) +
  theme_classic()

print(grafico_botones)


# 45. GRÁFICO DESCRIPTIVO DE INFLORESCENCIAS ---------------------

grafico_inflo <- ggplot(
  df_parcela,
  aes(
    x = genotipo,
    y = inflorescencias_promedio
  )
) +
  geom_boxplot(
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    height = 0
  ) +
  labs(
    x = "Genotipo",
    y = "Inflorescencias por planta",
    title = "Inflorescencias por genotipo"
  ) +
  theme_classic()

print(grafico_inflo)


# 46. GRÁFICO DESCRIPTIVO DE FLORES ABIERTAS ---------------------

grafico_abiertas <- ggplot(
  df_parcela,
  aes(
    x = genotipo,
    y = abiertas_promedio
  )
) +
  geom_boxplot(
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    height = 0
  ) +
  labs(
    x = "Genotipo",
    y = "Flores abiertas por planta",
    title = "Flores abiertas por genotipo"
  ) +
  theme_classic()

print(grafico_abiertas)


# 47. INFORMACIÓN DE LA SESIÓN ---------------------

sessionInfo()


# 48. DIRECTORIO DE RESULTADOS ---------------------

cat("\nLos archivos fueron guardados en:\n")
print(getwd())


# 49. FIN DEL ANÁLISIS ---------------------

cat(
  "\n============================================================\n",
  "ANÁLISIS ESTADÍSTICO FINALIZADO\n",
  "============================================================\n"
)

# ================================================================
# 50. ADJUSTED MEANS PLOT - FLORAL BUDS
# ================================================================

# 50. ADJUSTED MEANS PLOT - FLORAL BUDS------------------

library(dplyr)
library(ggplot2)


# 50.1. PREPARE DATA FOR THE PLOT------------------

floral_buds_plot_df <- resultado_botones$letras %>%
  as.data.frame() %>%
  mutate(
    genotipo = as.character(genotipo),
    .group = trimws(.group)
  )


# 50.2. IDENTIFY NUMERIC AND TEXT GENOTYPES------------------

numeric_levels <- floral_buds_plot_df$genotipo[
  grepl(
    "^[0-9]+$",
    floral_buds_plot_df$genotipo
  )
]

text_levels <- floral_buds_plot_df$genotipo[
  !grepl(
    "^[0-9]+$",
    floral_buds_plot_df$genotipo
  )
]


# 50.3. DEFINE GENOTYPE ORDER------------------

genotype_order <- c(
  
  as.character(
    sort(
      as.numeric(
        numeric_levels
      )
    )
  ),
  
  text_levels
)


# 50.4. APPLY GENOTYPE ORDER------------------

# The order is reversed because coord_flip()
# places the first desired genotype at the top.

floral_buds_plot_df <- floral_buds_plot_df %>%
  mutate(
    
    genotipo = factor(
      genotipo,
      levels = rev(genotype_order)
    )
    
  )


# 50.5. CREATE GENOTYPE LABELS------------------

# Examples:
# 1  -> UNR001
# 3  -> UNR003
# 11 -> UNR011
# 20 -> UNR020
# selva -> La Selva

genotype_labels <- function(x) {
  
  ifelse(
    
    grepl(
      "^[0-9]+$",
      x
    ),
    
    paste0(
      "UNR",
      sprintf(
        "%03d",
        as.numeric(x)
      )
    ),
    
    ifelse(
      tolower(x) == "selva",
      "La Selva",
      x
    )
  )
}


# 50.6. DEFINE POSITION OF TUKEY LETTERS------------------

# Letters are placed slightly to the right
# of the upper 95% confidence interval.

floral_buds_plot_df <- floral_buds_plot_df %>%
  mutate(
    
    letter_position =
      asymp.UCL +
      0.05 *
      max(
        asymp.UCL,
        na.rm = TRUE
      )
    
  )


# 50.7. BUILD HORIZONTAL PLOT------------------

floral_buds_plot <- ggplot(
  
  floral_buds_plot_df,
  
  aes(
    x = genotipo,
    y = response
  )
  
) +
  
  geom_errorbar(
    
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    
    width = 0.20,
    linewidth = 0.6
    
  ) +
  
  geom_point(
    size = 3
  ) +
  
  geom_text(
    
    aes(
      y = letter_position,
      label = .group
    ),
    
    size = 4,
    fontface = "plain"
    
  ) +
  
  scale_x_discrete(
    labels = genotype_labels
  ) +
  
  coord_flip() +
  
  labs(
    x = "Genotype",
    y = "Number of floral buds per plant"
  ) +
  
  scale_y_continuous(
    
    expand = expansion(
      
      mult = c(
        0.02,
        0.15
      )
      
    )
    
  ) +
  
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    
    axis.text.y = element_text(
      size = 10,
      face = "plain"
    ),
    
    axis.text.x = element_text(
      size = 10,
      face = "plain"
    ),
    
    axis.title.x = element_text(
      margin = margin(
        t = 8
      ),
      face = "plain"
    ),
    
    axis.title.y = element_text(
      margin = margin(
        r = 8
      ),
      face = "plain"
    ),
    
    panel.grid = element_blank()
    
  )


# 50.8. DISPLAY PLOT------------------

print(
  floral_buds_plot
)

# floral_buds_plot---------

# 51. ADJUSTED MEANS PLOT - INFLORESCENCES------------------

library(dplyr)
library(ggplot2)


# 51.1. PREPARE DATA FOR THE PLOT------------------

inflorescences_plot_df <- resultado_inflo$letras %>%
  as.data.frame() %>%
  mutate(
    genotipo = as.character(genotipo),
    .group = trimws(.group)
  )


# 51.2. IDENTIFY NUMERIC AND TEXT GENOTYPES------------------

numeric_levels <- inflorescences_plot_df$genotipo[
  grepl(
    "^[0-9]+$",
    inflorescences_plot_df$genotipo
  )
]

text_levels <- inflorescences_plot_df$genotipo[
  !grepl(
    "^[0-9]+$",
    inflorescences_plot_df$genotipo
  )
]


# 51.3. DEFINE GENOTYPE ORDER------------------

genotype_order <- c(
  
  as.character(
    sort(
      as.numeric(
        numeric_levels
      )
    )
  ),
  
  text_levels
)


# 51.4. APPLY GENOTYPE ORDER------------------

# The order is reversed because coord_flip()
# places the first desired genotype at the top.

inflorescences_plot_df <- inflorescences_plot_df %>%
  mutate(
    
    genotipo = factor(
      genotipo,
      levels = rev(genotype_order)
    )
    
  )


# 51.5. CREATE GENOTYPE LABELS------------------

# Examples:
# 1  -> UNR001
# 3  -> UNR003
# 11 -> UNR011
# 20 -> UNR020
# selva -> La Selva

genotype_labels <- function(x) {
  
  ifelse(
    
    grepl(
      "^[0-9]+$",
      x
    ),
    
    paste0(
      "UNR",
      sprintf(
        "%03d",
        as.numeric(x)
      )
    ),
    
    ifelse(
      tolower(x) == "selva",
      "La Selva",
      x
    )
  )
}


# 51.6. DEFINE POSITION OF TUKEY LETTERS------------------

# Letters are placed slightly to the right
# of the upper 95% confidence interval.

inflorescences_plot_df <- inflorescences_plot_df %>%
  mutate(
    
    letter_position =
      asymp.UCL +
      0.05 *
      max(
        asymp.UCL,
        na.rm = TRUE
      )
    
  )


# 51.7. BUILD HORIZONTAL PLOT------------------

inflorescences_plot <- ggplot(
  
  inflorescences_plot_df,
  
  aes(
    x = genotipo,
    y = rate
  )
  
) +
  
  # 95% confidence intervals
  geom_errorbar(
    
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    
    width = 0.20,
    linewidth = 0.6
    
  ) +
  
  # Adjusted marginal means
  geom_point(
    size = 3
  ) +
  
  # Tukey grouping letters
  geom_text(
    
    aes(
      y = letter_position,
      label = .group
    ),
    
    size = 4,
    fontface = "plain"
    
  ) +
  
  # Genotype labels
  scale_x_discrete(
    labels = genotype_labels
  ) +
  
  # Horizontal orientation
  coord_flip() +
  
  # Axis labels
  labs(
    x = "Genotype",
    y = "Number of inflorescences per plant"
  ) +
  
  # Additional space for confidence intervals and letters
  scale_y_continuous(
    
    expand = expansion(
      
      mult = c(
        0.02,
        0.15
      )
      
    )
    
  ) +
  
  # Publication-style theme
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    
    axis.text.y = element_text(
      size = 10,
      face = "plain"
    ),
    
    axis.text.x = element_text(
      size = 10,
      face = "plain"
    ),
    
    axis.title.x = element_text(
      margin = margin(
        t = 8
      ),
      face = "plain"
    ),
    
    axis.title.y = element_text(
      margin = margin(
        r = 8
      ),
      face = "plain"
    ),
    
    panel.grid = element_blank()
    
  )


# 51.8. DISPLAY PLOT------------------

print(
  inflorescences_plot
)

# inflorescences_plot ----------------

# 52. ADJUSTED MEANS PLOT - OPEN FLOWERS------------------

library(dplyr)
library(ggplot2)


# 52.1. PREPARE DATA FOR THE PLOT------------------

open_flowers_plot_df <- resultado_abiertas$letras %>%
  as.data.frame() %>%
  mutate(
    genotipo = as.character(genotipo),
    .group = trimws(.group)
  )


# 52.2. IDENTIFY NUMERIC AND TEXT GENOTYPES------------------

numeric_levels <- open_flowers_plot_df$genotipo[
  grepl(
    "^[0-9]+$",
    open_flowers_plot_df$genotipo
  )
]

text_levels <- open_flowers_plot_df$genotipo[
  !grepl(
    "^[0-9]+$",
    open_flowers_plot_df$genotipo
  )
]


# 52.3. DEFINE GENOTYPE ORDER------------------

genotype_order <- c(
  
  as.character(
    sort(
      as.numeric(
        numeric_levels
      )
    )
  ),
  
  text_levels
)


# 52.4. APPLY GENOTYPE ORDER------------------

# The order is reversed because coord_flip()
# places the first desired genotype at the top.

open_flowers_plot_df <- open_flowers_plot_df %>%
  mutate(
    
    genotipo = factor(
      genotipo,
      levels = rev(genotype_order)
    )
    
  )


# 52.5. CREATE GENOTYPE LABELS------------------

# Examples:
# 1  -> UNR001
# 3  -> UNR003
# 11 -> UNR011
# 20 -> UNR020
# selva -> La Selva

genotype_labels <- function(x) {
  
  ifelse(
    
    grepl(
      "^[0-9]+$",
      x
    ),
    
    paste0(
      "UNR",
      sprintf(
        "%03d",
        as.numeric(x)
      )
    ),
    
    ifelse(
      tolower(x) == "selva",
      "La Selva",
      x
    )
  )
}


# 52.6. DEFINE POSITION OF TUKEY LETTERS------------------

# Letters are placed slightly to the right
# of the upper 95% confidence interval.

open_flowers_plot_df <- open_flowers_plot_df %>%
  mutate(
    
    letter_position =
      asymp.UCL +
      0.05 *
      max(
        asymp.UCL,
        na.rm = TRUE
      )
    
  )


# 52.7. BUILD HORIZONTAL PLOT------------------

open_flowers_plot <- ggplot(
  
  open_flowers_plot_df,
  
  aes(
    x = genotipo,
    y = response
  )
  
) +
  
  # 95% confidence intervals
  geom_errorbar(
    
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    
    width = 0.20,
    linewidth = 0.6
    
  ) +
  
  # Adjusted marginal means
  geom_point(
    size = 3
  ) +
  
  # Tukey grouping letters
  geom_text(
    
    aes(
      y = letter_position,
      label = .group
    ),
    
    size = 4,
    fontface = "plain"
    
  ) +
  
  # Genotype labels
  scale_x_discrete(
    labels = genotype_labels
  ) +
  
  # Horizontal orientation
  coord_flip() +
  
  # Axis labels
  labs(
    x = "",
    y = "Number of flowers at anthesis per plant"
  ) +
  
  # Additional space for confidence intervals and letters
  scale_y_continuous(
    breaks = c(
      0,
      2.5,
      5,
      7.5,
      10,
      12.5
    ),
    labels = c(
      "0",
      "2.5",
      "5",
      "7.5",
      "10",
      "12.5"
    ),
    expand = expansion(
      mult = c(
        0.02,
        0.15
      )
    )
  ) +
  
  # Publication-style theme
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    
    axis.text.y = element_text(
      size = 10,
      face = "plain"
    ),
    
    axis.text.x = element_text(
      size = 10,
      face = "plain"
    ),
    
    axis.title.x = element_text(
      margin = margin(
        t = 8
      ),
      face = "plain"
    ),
    
    axis.title.y = element_text(
      margin = margin(
        r = 8
      ),
      face = "plain"
    ),
    
    panel.grid = element_blank()
    
  )


# 52.8. DISPLAY PLOT------------------

print(
  open_flowers_plot
)

# open_flowers_plot ------------------

# 53. ADJUSTED PROPORTIONS PLOT - PISTIL CATEGORIES------------------

library(dplyr)
library(ggplot2)


# 53.1. FUNCTION TO CREATE GENOTYPE LABELS------------------

genotype_labels <- function(x) {
  
  ifelse(
    
    grepl(
      "^[0-9]+$",
      x
    ),
    
    paste0(
      "UNR",
      sprintf(
        "%03d",
        as.numeric(x)
      )
    ),
    
    ifelse(
      tolower(x) == "selva",
      "La Selva",
      x
    )
  )
}


# 53.2. FUNCTION TO PREPARE PISTIL DATA------------------

prepare_pistil_plot_data <- function(result_object) {
  
  plot_df <- result_object$letras %>%
    as.data.frame()
  
  
  # Identify the column containing the adjusted proportion
  
  estimate_column <- intersect(
    c(
      "prob",
      "response",
      "rate",
      "emmean"
    ),
    names(plot_df)
  )[1]
  
  
  if(is.na(estimate_column)){
    stop(
      "No adjusted proportion column was found."
    )
  }
  
  
  # Identify confidence interval columns
  
  lower_column <- intersect(
    c(
      "asymp.LCL",
      "lower.CL"
    ),
    names(plot_df)
  )[1]
  
  
  upper_column <- intersect(
    c(
      "asymp.UCL",
      "upper.CL"
    ),
    names(plot_df)
  )[1]
  
  
  if(
    is.na(lower_column) ||
    is.na(upper_column)
  ){
    stop(
      "Confidence interval columns were not found."
    )
  }
  
  
  # Standardize column names
  
  plot_df <- plot_df %>%
    mutate(
      
      genotipo = as.character(genotipo),
      
      adjusted_proportion =
        .data[[estimate_column]],
      
      lower_ci =
        .data[[lower_column]],
      
      upper_ci =
        .data[[upper_column]],
      
      .group =
        trimws(.group)
      
    )
  
  
  # Convert proportions to percentages
  
  plot_df <- plot_df %>%
    mutate(
      
      adjusted_percentage =
        adjusted_proportion * 100,
      
      lower_percentage =
        lower_ci * 100,
      
      upper_percentage =
        upper_ci * 100
      
    )
  
  
  # Identify numeric and text genotypes
  
  numeric_levels <- plot_df$genotipo[
    grepl(
      "^[0-9]+$",
      plot_df$genotipo
    )
  ]
  
  
  text_levels <- plot_df$genotipo[
    !grepl(
      "^[0-9]+$",
      plot_df$genotipo
    )
  ]
  
  
  # Define genotype order
  
  genotype_order <- c(
    
    as.character(
      sort(
        as.numeric(
          numeric_levels
        )
      )
    ),
    
    text_levels
  )
  
  
  # Reverse order for horizontal plot
  
  plot_df <- plot_df %>%
    mutate(
      
      genotipo = factor(
        genotipo,
        levels = rev(genotype_order)
      )
      
    )
  
  
  # Position grouping letters
  
  plot_df <- plot_df %>%
    mutate(
      
      letter_position =
        upper_percentage +
        0.04 *
        max(
          upper_percentage,
          na.rm = TRUE
        )
      
    )
  
  
  return(
    plot_df
  )
}


# 53.3. FUNCTION TO CREATE HORIZONTAL PISTIL PLOT------------------

create_pistil_plot <- function(
    plot_df,
    x_axis_title
){
  
  ggplot(
    
    plot_df,
    
    aes(
      x = genotipo,
      y = adjusted_percentage
    )
    
  ) +
    
    # 95% confidence intervals
    
    geom_errorbar(
      
      aes(
        ymin = lower_percentage,
        ymax = upper_percentage
      ),
      
      width = 0.20,
      linewidth = 0.6
      
    ) +
    
    
    # Adjusted percentage
    
    geom_point(
      size = 3
    ) +
    
    
    # Multiple-comparison grouping letters
    
    geom_text(
      
      aes(
        y = letter_position,
        label = .group
      ),
      
      size = 4,
      fontface = "plain"
      
    ) +
    
    
    # Genotype labels
    
    scale_x_discrete(
      labels = genotype_labels
    ) +
    
    
    # Horizontal orientation
    
    coord_flip() +
    
    
    # Axis labels
    
    labs(
      x = "Genotype",
      y = x_axis_title
    ) +
    
    
    # Percentage scale
    
    scale_y_continuous(
      
      labels = function(x){
        paste0(
          x,
          "%"
        )
      },
      
      expand = expansion(
        mult = c(
          0.02,
          0.15
        )
      )
      
    ) +
    
    
    # Publication-style theme
    
    theme_classic(
      base_size = 12
    ) +
    
    
    theme(
      
      axis.text.y = element_text(
        size = 10,
        face = "plain"
      ),
      
      axis.text.x = element_text(
        size = 10,
        face = "plain"
      ),
      
      axis.title.x = element_text(
        margin = margin(
          t = 8
        ),
        face = "plain"
      ),
      
      axis.title.y = element_text(
        margin = margin(
          r = 8
        ),
        face = "plain"
      ),
      
      panel.grid = element_blank()
      
    )
}


# 53.4. LONG-STYLED FLOWERS------------------

long_style_plot_df <- prepare_pistil_plot_data(
  resultado_largo
)


long_style_plot <- create_pistil_plot(
  plot_df = long_style_plot_df,
  x_axis_title = "Long-styled flowers (%)"
)


print(
  long_style_plot
)

# long_style_plot ----------

# 53.5. MEDIUM-STYLED FLOWERS------------------

medium_style_plot_df <- prepare_pistil_plot_data(
  resultado_medio
)


medium_style_plot <- create_pistil_plot(
  plot_df = medium_style_plot_df,
  x_axis_title = "Medium-styled flowers (%)"
)


print(
  medium_style_plot
)

# 53.6. SHORT-STYLED FLOWERS------------------

short_style_plot_df <- prepare_pistil_plot_data(
  resultado_corto
)


short_style_plot <- create_pistil_plot(
  plot_df = short_style_plot_df,
  x_axis_title = "Short-styled flowers (%)"
)


print(
  short_style_plot
)


# 53.7. PISTIL-ABSENT FLOWERS------------------

absent_pistil_plot_df <- prepare_pistil_plot_data(
  resultado_ausente
)


absent_pistil_plot <- create_pistil_plot(
  plot_df = absent_pistil_plot_df,
  x_axis_title = "Pistil-absent flowers (%)"
)


print(
  absent_pistil_plot
)

# 53. MULTIPANEL FIGURE - REPRODUCTIVE TRAITS------------------

library(ggplot2)
library(patchwork)


# 53.1. COMMON TAG STYLE------------------

tag_style <- theme(
  
  plot.tag.location = "panel",
  
  plot.tag.position = c(
    0.02,
    1.03
  ),
  
  plot.tag = element_text(
    size = 14,
    face = "bold",
    hjust = 0,
    vjust = 1
  )
  
)


# 53.2. PREPARE PANEL A - FLORAL BUDS------------------

panel_A <- floral_buds_plot +
  
  labs(
    x = "Genotype",
    y = "Number of floral buds per plant",
    tag = "A"
  ) +
  
  tag_style +
  
  theme(
    plot.margin = margin(
      t = 14,
      r = 8,
      b = 8,
      l = 8
    )
  )


# 53.3. PREPARE PANEL B - INFLORESCENCES------------------

panel_B <- inflorescences_plot +
  
  labs(
    x = "",
    y = "Number of inflorescences per plant",
    tag = "B"
  ) +
  
  tag_style +
  
  theme(
    plot.margin = margin(
      t = 14,
      r = 8,
      b = 8,
      l = 8
    )
  )


# 53.4. PREPARE PANEL C - FLOWERS AT ANTHESIS------------------

panel_C <- open_flowers_plot +
  
  labs(
    x = "",
    y = "Number of flowers at anthesis per plant",
    tag = "C"
  ) +
  
  tag_style +
  
  theme(
    plot.margin = margin(
      t = 14,
      r = 8,
      b = 8,
      l = 8
    )
  )


# 53.5. BUILD HORIZONTAL MULTIPANEL FIGURE------------------

reproductive_traits_figure <- (
  panel_A |
    panel_B |
    panel_C
) +
  
  plot_layout(
    ncol = 3,
    widths = c(
      1,
      1,
      1
    )
  )


# 53.6. DISPLAY MULTIPANEL FIGURE------------------

print(
  reproductive_traits_figure
)


# 53.7. SAVE AS HIGH-RESOLUTION TIFF------------------

ggsave(
  
  filename = "Figure_Reproductive_traits_multipanel.tiff",
  
  plot = reproductive_traits_figure,
  
  width = 36,
  height = 15,
  units = "cm",
  
  dpi = 600,
  compression = "lzw",
  bg = "white"
  
)


# 53.8. SAVE AS VECTOR PDF------------------

ggsave(
  
  filename = "Figure_Reproductive_traits_multipanel.pdf",
  
  plot = reproductive_traits_figure,
  
  width = 36,
  height = 15,
  units = "cm",
  
  bg = "white"
  
)

