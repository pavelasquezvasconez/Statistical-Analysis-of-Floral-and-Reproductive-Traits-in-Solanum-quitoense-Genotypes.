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

# 82. LIBRARIES FOR SPAD ANALYSIS------------------
paquetes_spad <- c("dplyr", "ggplot2", "nlme", "emmeans", "multcomp", "multcompView", "missForest")
faltantes_spad <- paquetes_spad[!vapply(paquetes_spad, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes_spad) > 0) {
  install.packages(faltantes_spad, repos = "https://cloud.r-project.org")
}
invisible(lapply(paquetes_spad, library, character.only = TRUE))

# 83. VERIFY INPUT DATA------------------
if (!exists("datos")) stop("No existe el objeto 'datos'.")
columnas_spad <- c("fecha", "evaluacion", "genotipo", "bloque", "planta", "clorofilometro_spad")
if (!all(columnas_spad %in% names(datos))) {
  stop(paste("Faltan columnas:", paste(setdiff(columnas_spad, names(datos)), collapse = ", ")))
}

# 84. GENOTYPE ORDER AND LABELS------------------
codigos_spad <- unique(as.character(datos$genotipo))
numericos_spad <- codigos_spad[grepl("^[0-9]+$", codigos_spad)]
numericos_spad <- numericos_spad[order(as.integer(numericos_spad))]
selva_spad <- codigos_spad[tolower(codigos_spad) == "selva"]
niveles_spad <- c(numericos_spad, selva_spad)
if (length(niveles_spad) != length(codigos_spad)) {
  stop("Existen nombres de genotipos no numéricos diferentes de 'selva'.")
}
etiquetas_genotipo_spad <- function(x) {
  x <- as.character(x)
  i <- grepl("^[0-9]+$", x)
  x[i] <- sprintf("UNR%03d", as.integer(x[i]))
  x[tolower(x) == "selva"] <- "La Selva"
  x
}
etiquetas_fecha_spad <- c("1" = "16 DAT", "2" = "44 DAT", "3" = "79 DAT", "4" = "119 DAT")

# 85. PREPARE PLANT-LEVEL DATA------------------
spad_planta <- datos %>%
  dplyr::mutate(
    fecha = as.Date(fecha),
    evaluacion = factor(as.character(evaluacion), levels = as.character(1:4)),
    bloque = factor(bloque),
    genotipo = factor(as.character(genotipo), levels = niveles_spad),
    planta = factor(planta),
    clorofilometro_spad = as.numeric(clorofilometro_spad)
  ) %>%
  dplyr::mutate(
    parcela = interaction(bloque, genotipo, sep = "_", drop = TRUE),
    dias = as.numeric(fecha - min(fecha, na.rm = TRUE)),
    dat = dias + 16,
    tiempo_30 = dias / 30
  ) %>%
  dplyr::arrange(parcela, dias, planta)
if (anyNA(spad_planta[, c("fecha", "evaluacion", "bloque", "genotipo", "planta", "dias")])) {
  stop("Hay identificadores o fechas faltantes.")
}
if (anyDuplicated(spad_planta[, c("parcela", "evaluacion", "planta")]) > 0) {
  stop("Se encontraron observaciones duplicadas por planta, parcela y evaluación.")
}
tabla_fechas_spad <- spad_planta %>%
  dplyr::distinct(evaluacion, fecha, dias, dat) %>%
  dplyr::arrange(evaluacion)
print(tabla_fechas_spad)
if (nrow(tabla_fechas_spad) != 4 || !identical(as.numeric(tabla_fechas_spad$dat), c(16, 44, 79, 119))) {
  stop("Las fechas no corresponden a las etiquetas DAT definidas. Revise los días antes de continuar.")
}
cat("\nGenotipos:", dplyr::n_distinct(spad_planta$genotipo),
    "\nBloques:", dplyr::n_distinct(spad_planta$bloque),
    "\nParcelas:", dplyr::n_distinct(spad_planta$parcela),
    "\nObservaciones:", nrow(spad_planta), "\n")

# 86. CREATE OUTPUT FOLDER------------------
carpeta_spad <- "Resultados_Clorofila_SPAD_LMM"
dir.create(carpeta_spad, recursive = TRUE, showWarnings = FALSE)

# 87. CHECK MISSING VALUES AND REVIEW EXTREMES------------------
print(colSums(is.na(spad_planta)))
print(spad_planta %>%
        dplyr::arrange(dplyr::desc(clorofilometro_spad)) %>%
        dplyr::select(fecha, evaluacion, bloque, genotipo, planta, clorofilometro_spad) %>%
        head(20))
spad_planta$spad_original <- spad_planta$clorofilometro_spad
spad_planta$spad_imputado <- is.na(spad_planta$clorofilometro_spad)

# 88. CONDITIONAL MISSFOREST IMPUTATION------------------
if (anyNA(spad_planta$clorofilometro_spad)) {
  if (all(is.na(spad_planta$clorofilometro_spad))) stop("SPAD no tiene datos observados.")
  datos_mf_spad <- spad_planta %>%
    dplyr::transmute(
      evaluacion = factor(evaluacion), genotipo = factor(genotipo),
      bloque = factor(bloque), planta = factor(planta),
      clorofilometro_spad = clorofilometro_spad
    ) %>% as.data.frame()
  set.seed(12345)
  imp_mf_spad <- missForest::missForest(
    xmis = datos_mf_spad, ntree = 1000, maxiter = 10, verbose = TRUE
  )
  print(imp_mf_spad$OOBerror)
  spad_planta$clorofilometro_spad <- as.numeric(
    imp_mf_spad$ximp$clorofilometro_spad
  )
} else {
  message("SPAD: no hay valores faltantes; no se requiere imputación.")
}
if (anyNA(spad_planta$clorofilometro_spad)) stop("Quedan valores SPAD sin resolver.")
imputaciones_spad <- spad_planta %>%
  dplyr::filter(spad_imputado) %>%
  dplyr::select(parcela, evaluacion, planta, spad_original, clorofilometro_spad)
print(imputaciones_spad, n = Inf)
write.csv(imputaciones_spad, file.path(carpeta_spad, "01_Imputaciones_SPAD.csv"), row.names = FALSE)

# 89. CONSTRUCT PLOT-LEVEL MEANS------------------
spad_parcela <- spad_planta %>%
  dplyr::group_by(bloque, genotipo, parcela, fecha, evaluacion, dias, dat, tiempo_30) %>%
  dplyr::summarise(
    n_plantas = dplyr::n(),
    n_imputados = sum(spad_imputado),
    clorofilometro_spad = mean(clorofilometro_spad),
    .groups = "drop"
  ) %>%
  dplyr::arrange(parcela, dias) %>%
  droplevels() %>% as.data.frame()
if (anyDuplicated(spad_parcela[, c("parcela", "evaluacion")]) > 0) {
  stop("Hay datos duplicados por parcela y fecha.")
}
print(table(spad_parcela$n_plantas))
print(spad_parcela %>% dplyr::count(parcela) %>% dplyr::count(n))
write.csv(spad_parcela, file.path(carpeta_spad, "02_SPAD_por_parcela.csv"), row.names = FALSE)

# 90. DESCRIPTIVE STATISTICS------------------
descriptivos_spad <- spad_parcela %>%
  dplyr::group_by(genotipo, evaluacion, dat) %>%
  dplyr::summarise(
    n = dplyr::n(), media = mean(clorofilometro_spad),
    sd = sd(clorofilometro_spad),
    se = sd / sqrt(n),
    minimo = min(clorofilometro_spad),
    maximo = max(clorofilometro_spad),
    .groups = "drop"
  )
print(descriptivos_spad, n = Inf)
write.csv(descriptivos_spad, file.path(carpeta_spad, "03_Estadisticos_descriptivos.csv"), row.names = FALSE)

# 91. LINEAR REGRESSIONS BY PLOT (DESCRIPTIVE)------------------
tasas_spad <- spad_parcela %>%
  dplyr::group_by(bloque, genotipo, parcela) %>%
  dplyr::filter(dplyr::n_distinct(dias) >= 3) %>%
  dplyr::group_modify(~ {
    m <- lm(clorofilometro_spad ~ dias, data = .x)
    s <- summary(m)
    data.frame(
      beta0 = unname(coef(m)[1]),
      beta1 = unname(coef(m)[2]),
      se_beta1 = s$coefficients["dias", "Std. Error"],
      t_beta1 = s$coefficients["dias", "t value"],
      p_beta1 = s$coefficients["dias", "Pr(>|t|)"],
      r2 = s$r.squared, r2_ajustado = s$adj.r.squared,
      rmse = sqrt(mean(residuals(m)^2)), n_fechas = nobs(m)
    )
  }) %>% dplyr::ungroup()
print(tasas_spad %>% dplyr::arrange(r2), n = Inf)
print(summary(tasas_spad$r2))
write.csv(tasas_spad, file.path(carpeta_spad, "04_Regresiones_parcela_SPAD.csv"), row.names = FALSE)
grafico_r2_spad <- ggplot(tasas_spad, aes(x = r2)) +
  geom_histogram(bins = 15, color = "black", fill = "grey80") +
  labs(x = expression(R^2), y = "Frequency", title = "R-squared distribution: SPAD") +
  theme_bw(base_size = 12)
print(grafico_r2_spad)
ggsave(file.path(carpeta_spad, "05_Distribucion_R2.pdf"), grafico_r2_spad, width = 7, height = 5)

# 92. PREPARE MIXED MODELS------------------
control_spad <- nlme::lmeControl(opt = "optim", msMaxIter = 300, msMaxEval = 500, niterEM = 50)
modelo_base_spad <- nlme::lme(
  fixed = clorofilometro_spad ~ bloque + genotipo * evaluacion,
  random = ~ 1 | parcela,
  data = spad_parcela, method = "REML", control = control_spad
)

# 93. FIT ALTERNATIVE COVARIANCE STRUCTURES------------------
modelo_car1_spad <- try(update(
  modelo_base_spad,
  correlation = nlme::corCAR1(form = ~ tiempo_30 | parcela)
), silent = TRUE)
modelo_hetero_spad <- try(update(
  modelo_base_spad,
  weights = nlme::varIdent(form = ~ 1 | evaluacion)
), silent = TRUE)
modelo_car1_hetero_spad <- try(update(
  modelo_base_spad,
  correlation = nlme::corCAR1(form = ~ tiempo_30 | parcela),
  weights = nlme::varIdent(form = ~ 1 | evaluacion)
), silent = TRUE)
modelos_spad <- list(Intercepto_aleatorio = modelo_base_spad)
if (inherits(modelo_car1_spad, "lme")) modelos_spad$CAR1 <- modelo_car1_spad
if (inherits(modelo_hetero_spad, "lme")) modelos_spad$Heterogeneo <- modelo_hetero_spad
if (inherits(modelo_car1_hetero_spad, "lme")) modelos_spad$CAR1_Heterogeneo <- modelo_car1_hetero_spad

# 94. SELECT COVARIANCE STRUCTURE BY REML AIC------------------
tabla_aic_spad <- data.frame(
  Modelo = names(modelos_spad),
  AIC = unname(vapply(modelos_spad, AIC, numeric(1))),
  BIC = unname(vapply(modelos_spad, BIC, numeric(1))),
  logLik = unname(vapply(modelos_spad, function(m) as.numeric(logLik(m)), numeric(1)))
) %>% dplyr::arrange(AIC) %>%
  dplyr::mutate(Delta_AIC = AIC - min(AIC))
print(tabla_aic_spad, row.names = FALSE)
nombre_seleccionado_spad <- tabla_aic_spad$Modelo[1]
modelo_completo_spad <- modelos_spad[[nombre_seleccionado_spad]]
cat("\nSelected covariance structure:", nombre_seleccionado_spad, "\n")
write.csv(tabla_aic_spad, file.path(carpeta_spad, "06_Comparacion_AIC.csv"), row.names = FALSE)

# 95. TEST GENOTYPE BY ASSESSMENT INTERACTION------------------
modelo_completo_ML_spad <- update(modelo_completo_spad, method = "ML")
modelo_aditivo_ML_spad <- update(
  modelo_completo_ML_spad,
  fixed = clorofilometro_spad ~ bloque + genotipo + evaluacion
)
prueba_interaccion_spad <- anova(modelo_aditivo_ML_spad, modelo_completo_ML_spad)
print(prueba_interaccion_spad)
p_interaccion_spad <- as.numeric(prueba_interaccion_spad[["p-value"]][2])
if (length(p_interaccion_spad) != 1 || !is.finite(p_interaccion_spad)) {
  stop("No se pudo obtener el p-value de interacción.")
}
alfa_spad <- 0.05
interaccion_significativa_spad <- p_interaccion_spad < alfa_spad
cat("\nGenotype x assessment p =", p_interaccion_spad, "\n")

# 96. FINAL MODEL AND MARGINAL ANOVA------------------
if (interaccion_significativa_spad) {
  modelo_final_spad <- modelo_completo_spad
  cat("\nSignificant interaction: Tukey comparisons by assessment.\n")
} else {
  modelo_final_spad <- update(modelo_aditivo_ML_spad, method = "REML")
  cat("\nNon-significant interaction: Tukey comparisons for overall genotype means.\n")
}
anova_final_spad <- anova(modelo_final_spad, type = "marginal")
print(anova_final_spad)
tabla_anova_spad <- data.frame(
  Efecto = rownames(anova_final_spad),
  as.data.frame(anova_final_spad), check.names = FALSE, row.names = NULL
)
write.csv(tabla_anova_spad, file.path(carpeta_spad, "07_ANOVA_modelo_mixto.csv"), row.names = FALSE)
capture.output(summary(modelo_final_spad), file = file.path(carpeta_spad, "08_Resumen_modelo.txt"))

# 97. RESIDUAL DIAGNOSTICS------------------
diagnosticos_spad <- spad_parcela %>%
  dplyr::mutate(
    ajustado = as.numeric(fitted(modelo_final_spad)),
    residuo = as.numeric(resid(modelo_final_spad, type = "response")),
    residuo_normalizado = as.numeric(resid(modelo_final_spad, type = "normalized"))
  )
prueba_shapiro_spad <- shapiro.test(diagnosticos_spad$residuo_normalizado)
print(prueba_shapiro_spad)
resumen_varianzas_spad <- diagnosticos_spad %>%
  dplyr::group_by(evaluacion) %>%
  dplyr::summarise(n = dplyr::n(), sd_residual = sd(residuo_normalizado),
                   var_residual = var(residuo_normalizado), .groups = "drop")
print(resumen_varianzas_spad)
acf_modelo_spad <- nlme::ACF(modelo_final_spad, resType = "normalized", maxLag = 3)
print(acf_modelo_spad)
write.csv(diagnosticos_spad, file.path(carpeta_spad, "09_Residuos_modelo.csv"), row.names = FALSE)

grafico_qq_spad <- ggplot(diagnosticos_spad, aes(sample = residuo_normalizado)) +
  stat_qq() + stat_qq_line(color = "red") +
  labs(x = "Theoretical quantiles", y = "Observed quantiles", title = "Normal Q-Q plot: SPAD") +
  theme_bw(base_size = 12)
grafico_residuos_spad <- ggplot(diagnosticos_spad, aes(x = ajustado, y = residuo_normalizado)) +
  geom_point(alpha = 0.65) + geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "Fitted values", y = "Normalized residuals", title = "Residuals vs fitted: SPAD") +
  theme_bw(base_size = 12)
grafico_varianzas_spad <- ggplot(
  diagnosticos_spad, aes(x = evaluacion, y = residuo_normalizado)
) + geom_boxplot(fill = "grey85") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_discrete(labels = etiquetas_fecha_spad) +
  labs(x = "Days after transplanting", y = "Normalized residuals") +
  theme_bw(base_size = 12)
print(grafico_qq_spad)
print(grafico_residuos_spad)
print(grafico_varianzas_spad)
ggsave(file.path(carpeta_spad, "10_QQ_residuos.pdf"), grafico_qq_spad, width = 7, height = 6)
ggsave(file.path(carpeta_spad, "11_Residuos_vs_ajustados.pdf"), grafico_residuos_spad, width = 7, height = 6)
ggsave(file.path(carpeta_spad, "12_Varianza_por_fecha.pdf"), grafico_varianzas_spad, width = 7, height = 6)
pdf(file.path(carpeta_spad, "13_ACF_residuos.pdf"), width = 7, height = 6)
plot(acf_modelo_spad)
dev.off()
residuos_extremos_spad <- diagnosticos_spad %>%
  dplyr::filter(abs(residuo_normalizado) > 3) %>%
  dplyr::select(bloque, genotipo, parcela, evaluacion,
                clorofilometro_spad, ajustado, residuo_normalizado)
print(residuos_extremos_spad)
write.csv(residuos_extremos_spad, file.path(carpeta_spad, "14_Residuos_a_revisar.csv"), row.names = FALSE)

# 98. ESTIMATED MARGINAL MEANS BY DATE------------------
medias_fecha_spad <- emmeans::emmeans(modelo_final_spad, ~ genotipo | evaluacion)
tabla_medias_fecha_spad <- as.data.frame(summary(medias_fecha_spad, infer = c(TRUE, TRUE)))
print(tabla_medias_fecha_spad, row.names = FALSE)
write.csv(tabla_medias_fecha_spad, file.path(carpeta_spad, "15_Medias_por_evaluacion.csv"), row.names = FALSE)

# 99. TUKEY PAIRWISE COMPARISONS------------------
if (interaccion_significativa_spad) {
  medias_comparacion_spad <- emmeans::emmeans(modelo_final_spad, ~ genotipo | evaluacion)
} else {
  medias_comparacion_spad <- emmeans::emmeans(modelo_final_spad, ~ genotipo)
}
comparaciones_tukey_spad <- pairs(medias_comparacion_spad, adjust = "tukey")
tabla_tukey_spad <- as.data.frame(summary(comparaciones_tukey_spad, infer = c(TRUE, TRUE)))
print(tabla_tukey_spad, row.names = FALSE)
write.csv(tabla_tukey_spad, file.path(carpeta_spad, "16_Comparaciones_Tukey.csv"), row.names = FALSE)

# 100. TUKEY LETTER GROUPS------------------
grupos_tukey_spad <- multcomp::cld(
  medias_comparacion_spad, adjust = "tukey", alpha = alfa_spad,
  Letters = letters, sort = FALSE
)
tabla_grupos_spad <- as.data.frame(grupos_tukey_spad) %>%
  dplyr::mutate(
    genotipo = factor(as.character(genotipo), levels = niveles_spad),
    .group = trimws(.group)
  )
if ("evaluacion" %in% names(tabla_grupos_spad)) {
  tabla_grupos_spad <- tabla_grupos_spad %>% dplyr::arrange(evaluacion, genotipo)
} else {
  tabla_grupos_spad <- tabla_grupos_spad %>% dplyr::arrange(genotipo)
}
print(tabla_grupos_spad, row.names = FALSE)
write.csv(tabla_grupos_spad, file.path(carpeta_spad, "17_Grupos_Tukey_letras.csv"), row.names = FALSE)

# 101. DUNNETT COMPARISONS AGAINST LA SELVA------------------
control_selva_spad <- match("selva", levels(spad_parcela$genotipo))
if (!is.na(control_selva_spad)) {
  comparaciones_selva_spad <- emmeans::contrast(
    medias_comparacion_spad, method = "trt.vs.ctrl",
    ref = control_selva_spad, adjust = "dunnett"
  )
  tabla_selva_spad <- as.data.frame(summary(comparaciones_selva_spad, infer = c(TRUE, TRUE)))
  print(tabla_selva_spad, row.names = FALSE)
  write.csv(tabla_selva_spad, file.path(carpeta_spad, "18_Dunnett_vs_La_Selva.csv"), row.names = FALSE)
}

# 102. ESTIMATED CHANGE BETWEEN FIRST AND LAST DATE------------------
medias_tiempo_spad <- emmeans::emmeans(modelo_final_spad, ~ evaluacion | genotipo)
cambio_spad <- emmeans::contrast(
  medias_tiempo_spad,
  method = list("119 DAT - 16 DAT" = c(-1, 0, 0, 1))
)
tabla_cambio_spad <- as.data.frame(summary(cambio_spad, infer = c(TRUE, TRUE)))
print(tabla_cambio_spad, row.names = FALSE)
write.csv(tabla_cambio_spad, file.path(carpeta_spad, "19_Cambio_SPAD_16_a_119_DAT.csv"), row.names = FALSE)

# 103. PREPARE TRAJECTORY FIGURE DATA------------------
tabla_grafico_spad <- as.data.frame(medias_fecha_spad) %>%
  dplyr::left_join(
    tabla_fechas_spad %>% dplyr::select(evaluacion, dat),
    by = "evaluacion"
  )

# 104. SPAD TEMPORAL TRAJECTORIES------------------
grafico_trayectorias_spad <- ggplot() +
  geom_line(
    data = spad_parcela,
    aes(x = dat, y = clorofilometro_spad, group = parcela),
    color = "grey75", alpha = 0.4
  ) +
  geom_point(
    data = spad_parcela,
    aes(x = dat, y = clorofilometro_spad),
    color = "grey65", alpha = 0.4, size = 1
  ) +
  geom_line(
    data = tabla_grafico_spad,
    aes(x = dat, y = emmean, group = genotipo),
    color = "blue", linewidth = 1
  ) +
  geom_point(
    data = tabla_grafico_spad,
    aes(x = dat, y = emmean),
    color = "blue", size = 1.8
  ) +
  geom_errorbar(
    data = tabla_grafico_spad,
    aes(x = dat, ymin = lower.CL, ymax = upper.CL),
    color = "blue", width = 3
  ) +
  facet_wrap(
    ~ genotipo, ncol = 6, scales = "free_y",
    labeller = labeller(genotipo = etiquetas_genotipo_spad)
  ) +
  scale_x_continuous(breaks = c(16, 44, 79, 119)) +
  labs(
    x = "Days after transplanting (DAT)",
    y = "SPAD chlorophyll index (SPAD units)",
    title = ""
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(size = 11),
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    panel.grid.minor = element_blank()
  )
print(grafico_trayectorias_spad)
ggsave(file.path(carpeta_spad, "20_Trayectorias_SPAD.pdf"), grafico_trayectorias_spad, width = 9, height = 5)
ggsave(file.path(carpeta_spad, "20_Trayectorias_SPAD.png"), grafico_trayectorias_spad, width = 8, height = 5, dpi = 600, bg = "white")

# 105. TUKEY GROUPING FIGURE------------------
grafico_grupos_spad <- ggplot(
  tabla_grupos_spad,
  aes(x = genotipo, y = emmean)
) +
  geom_point(color = "black", size = 2.4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,
                color = "grey35", linewidth = 0.5) +
  geom_text(aes(y = upper.CL, label = .group), vjust = -0.6, size = 3.2) +
  scale_x_discrete(
    limits = niveles_spad,
    labels = etiquetas_genotipo_spad,
    drop = FALSE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.22))) +
  labs(
    x = "Genotype",
    y = "Estimated SPAD chlorophyll index (SPAD units)",
    title = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    strip.text = element_text(size = 12, color = "black"),
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
if (interaccion_significativa_spad) {
  grafico_grupos_spad <- grafico_grupos_spad + facet_wrap(
    ~ evaluacion, ncol = 2, scales = "free_y",
    labeller = labeller(evaluacion = as_labeller(etiquetas_fecha_spad))
  )
}
print(grafico_grupos_spad)
ggsave(file.path(carpeta_spad, "21_Tukey_SPAD.pdf"), grafico_grupos_spad, width = 10, height = 6)
ggsave(file.path(carpeta_spad, "21_Tukey_SPAD.png"), grafico_grupos_spad, width = 10, height = 6, dpi = 600, bg = "white")

# 106. FINAL SUMMARY AND SAVE OBJECTS------------------
cat("\nFINAL LONGITUDINAL SPAD ANALYSIS\n")
cat("Covariance structure:", nombre_seleccionado_spad, "\n")
cat("Genotype x assessment p-value:", p_interaccion_spad, "\n")
cat("Shapiro-Wilk p-value:", prueba_shapiro_spad$p.value, "\n")
print(anova_final_spad)
print(tabla_grupos_spad, row.names = FALSE)
saveRDS(
  list(
    spad_planta = spad_planta, spad_parcela = spad_parcela,
    tasas_spad = tasas_spad, modelos = modelos_spad, aic = tabla_aic_spad,
    modelo_final = modelo_final_spad, anova = anova_final_spad,
    prueba_interaccion = prueba_interaccion_spad,
    diagnosticos = diagnosticos_spad, medias = tabla_medias_fecha_spad,
    tukey = tabla_tukey_spad, letras = tabla_grupos_spad,
    cambio = tabla_cambio_spad
  ),
  file = file.path(carpeta_spad, "22_Objetos_analisis_SPAD.rds")
)
cat("\nResults saved in:", normalizePath(carpeta_spad), "\n")

