# ============================================================
# preTools — Script de completado de series de precipitación
# por regresión lineal simple entre estaciones
# Versión v8 — sept. 2026
# ============================================================
#
# ------------------------------------------------------------
# INSTRUCCIONES DE USO (léelas antes de tocar nada más)
# ------------------------------------------------------------
#
# Este archivo NO ejecuta nada por sí solo: solo DEFINE funciones.
# Para usarlo necesitas dos pasos siempre, en este orden:
#
#   PASO 1 — Cargar las funciones en tu sesión de R
#   ------------------------------------------------
#   Con este archivo abierto y activo en el editor de RStudio,
#   pulsa Ctrl+Shift+S (Windows/Linux) o Cmd+Shift+S (macOS).
#   Esto define todas las funciones del script en tu entorno, pero
#   no procesa ningún Excel todavía.
#
#   PASO 2 — Ejecutar el proceso sobre tu archivo
#   ------------------------------------------------
#   Elige UNA de estas dos formas (no hace falta hacer las dos):
#
#   Opción A (recomendada mientras estés haciendo pruebas):
#   escribe esto directamente en la CONSOLA de RStudio (no en este
#   archivo) y pulsa Enter:
#
#       resultado <- completar_primera_estacion(
#         ruta_excel = "RUTA/A/TU/ARCHIVO.xlsx",
#         ruta_salida = "NOMBRE_SALIDA.xlsx"
#       )
#
#   Opción B (si quieres dejarlo guardado en el script): ve al
#   final de este archivo, a la sección "Ejemplo de uso", quita
#   las almohadillas (#) de esas líneas y sustituye la ruta de
#   ejemplo por la tuya. Guarda el archivo (Ctrl+S) y repite el
#   PASO 1 (Ctrl+Shift+S): al no estar ya comentada, esa llamada
#   se ejecutará automáticamente al cargar el script.
#
#   Parámetros opcionales de completar_primera_estacion() (si no
#   los indicas, se usan estos valores por defecto):
#     hoja               = 1     (número de hoja del Excel a leer)
#     min_n              = 100   (mínimo de datos coincidentes)
#     min_r2             = 0.5   (R² mínimo para aceptar un modelo)
#     limitar_negativos  = TRUE  (recorta a 0 estimaciones negativas)
#
#   Qué esperar en la consola al ejecutar: mensajes informativos
#   (message()) sobre qué estación auxiliar se ha seleccionado y
#   cuántos valores se han completado, y avisos (warning()) si se
#   detectan filas con fecha inválida, columnas vacías o valores
#   negativos en el Excel de origen. Ningún warning detiene el
#   proceso; un error (stop()) sí lo detendría, y solo ocurre si
#   el Excel no tiene la estructura mínima esperada (al menos 3
#   columnas: fecha, objetivo, auxiliar).
#
#   El resultado se guarda en el archivo indicado en ruta_salida,
#   con las hojas Datos (datos originales, sin completar), Datos_
#   completados, Registro_completados, Modelos_regresion y una hoja
#   por cada par estación objetivo - estación auxiliar
#   (DEP_<objetivo>_<auxiliar>).
#
#   En Datos_completados, junto a la columna de la estación objetivo,
#   se añade una columna "<objetivo>_estado" con una de estas
#   etiquetas por fila: "Original", "Completado (regresión)",
#   "Completado (auxiliar=0)" o "SIN COMPLETAR". Esta última señala
#   las filas que no se han podido rellenar (la estación auxiliar
#   seleccionada tampoco tenía dato esa fecha, o no hubo ningún
#   modelo válido). Se usa esta columna de texto en lugar de un
#   valor numérico centinela (p. ej. 999) para no introducir en la
#   columna de precipitación un número que pueda confundirse con un
#   dato real en cálculos posteriores.
# ------------------------------------------------------------
#
# NOTA METODOLÓGICA (vigente desde esta versión):
# Para cada par (estación objetivo, estación auxiliar) se utilizan
# TODOS los registros coincidentes disponibles entre ambas, sin
# exigir que el resto de estaciones auxiliares tengan dato ese día
# (filtrado POR PARES, apdo. 3 del planteamiento original). Esto
# sustituye al criterio de "excel depurado global" usado en la
# versión anterior (v6).
#
# CONSECUENCIA A TENER EN CUENTA (no se corrige automáticamente,
# por instrucción expresa de no complicar la metodología): el
# número de datos coincidentes (n) es distinto para cada estación
# auxiliar evaluada, ya que depende únicamente de su solape temporal
# con la estación objetivo. La selección del mejor modelo compara
# R² entre auxiliares con n potencialmente muy distintos entre sí.
# Esto es una práctica habitual y aceptada, pero conviene tenerlo
# presente: un R² ligeramente mayor con un n mucho menor no es
# necesariamente "mejor" en sentido estadístico estricto que un R²
# ligeramente menor con un n mucho mayor. No se introduce ningún
# criterio adicional (ponderación por n, RMSE, etc.) para corregir
# esto, tal como se ha solicitado explícitamente.

# Paquetes necesarios
library(readxl)
library(dplyr)
library(purrr)
library(writexl)
library(stringr)
library(tibble)

# ============================================================
# 1. LECTURA
# ============================================================
#
# Lee el Excel de origen. Se asume:
#   - primera columna = fecha
#   - segunda columna = estación objetivo (a completar)
#   - resto de columnas = estaciones auxiliares
#   - primera fila = encabezados
#
# Se fuerza explícitamente el tipo de cada columna (fecha / numeric)
# en lugar de dejar que read_excel() lo adivine. Se detectó en
# versiones anteriores que, para determinados archivos, readxl puede
# malinterpretar el formato interno de una columna numérica y
# devolver solo su parte fraccionaria (p. ej. 3.9 -> 0.9), sin
# lanzar error ni aviso. Forzar col_types evita ese problema.

leer_datos_precipitacion <- function(ruta_excel,
                                     hoja = 1,
                                     col_fecha = 1) {

  cabecera <- read_excel(ruta_excel, sheet = hoja, n_max = 0)
  n_cols <- ncol(cabecera)

  if (n_cols < 3) {
    stop("El archivo debe tener al menos 3 columnas: fecha, estación objetivo y al menos una estación auxiliar.")
  }

  tipos_columnas <- rep("numeric", n_cols)
  tipos_columnas[col_fecha] <- "date"

  df <- read_excel(ruta_excel, sheet = hoja, col_types = tipos_columnas)

  # --- Fechas no interpretables: se eliminan solo esas filas ---
  idx_fecha_invalida <- which(is.na(df[[col_fecha]]))
  if (length(idx_fecha_invalida) > 0) {
    warning(
      length(idx_fecha_invalida),
      " fila(s) con fecha vacía o no interpretable han sido eliminadas antes del análisis ",
      "(filas de datos: ", paste(head(idx_fecha_invalida, 10), collapse = ", "),
      if (length(idx_fecha_invalida) > 10) ", ..." else "",
      "). Revisa si son filas espurias (totales, fórmulas residuales, celdas vacías, etc.) en el Excel de origen."
    )
    df <- df[-idx_fecha_invalida, ]
  }

  return(df)
}

# ============================================================
# 2. VALIDACIÓN
# ============================================================
#
# Controles objetivamente detectables sobre los datos ya leídos:
#   - columnas completamente vacías
#   - valores negativos de precipitación (físicamente inválidos)
#
# Deliberadamente NO se incluyen heurísticas basadas en el valor
# máximo de una columna (p. ej. "máximo sospechosamente bajo"):
# una estación con precipitaciones reales bajas es perfectamente
# posible y no debe marcarse como anómala solo por eso.
#
# Esta función solo informa mediante warning(); no modifica ni
# elimina datos.

validar_datos_precipitacion <- function(df, col_fecha = 1) {
  cols_datos <- setdiff(colnames(df), colnames(df)[col_fecha])

  for (cn in cols_datos) {
    valores <- df[[cn]]

    if (all(is.na(valores))) {
      warning("La columna '", cn, "' está completamente vacía (todos los valores son NA).")
      next
    }

    idx_negativos <- which(valores < 0)
    if (length(idx_negativos) > 0) {
      warning(
        "La columna '", cn, "' contiene ", length(idx_negativos),
        " valor(es) negativo(s), físicamente inválidos para precipitación ",
        "(filas de datos: ", paste(head(idx_negativos, 10), collapse = ", "),
        if (length(idx_negativos) > 10) ", ..." else "",
        "). Revisa el Excel de origen; el script NO modifica estos valores automáticamente."
      )
    }
  }

  invisible(df)
}

# ============================================================
# 3. PREPARACIÓN DE DATOS POR PARES
# ============================================================
#
# Para un par (estación objetivo, estación auxiliar), conserva
# únicamente las filas en las que AMBAS estaciones tienen dato.
# La ausencia de dato en otras estaciones auxiliares no afecta
# a este subconjunto.

preparar_datos_par <- function(df, col_fecha, col_target, col_aux) {
  df_par <- df %>%
    select(all_of(c(col_fecha, col_target, col_aux))) %>%
    filter(!is.na(.data[[col_target]]),
           !is.na(.data[[col_aux]]))

  return(df_par)
}

# ============================================================
# 4. AJUSTE DE REGRESIONES (con control de validez)
# ============================================================
#
# Ajusta Y = a + bX (estación objetivo ~ estación auxiliar) sobre
# el subconjunto por pares. Antes de aceptar el modelo comprueba:
#   - número mínimo de observaciones para poder ajustar una recta
#   - variabilidad en la auxiliar y en la objetivo
#   - que el ajuste no produzca error
#   - que los coeficientes sean finitos (no NA/NaN/Inf)
#   - que R² sea calculable y finito
#
# Si cualquiera de estos controles falla, el modelo se marca como
# NO VÁLIDO (valido = FALSE) con un motivo textual, y la función
# NUNCA detiene la ejecución: simplemente informa mediante el
# campo 'motivo' para que quede reflejado en la tabla resumen y se
# continúe con el resto de estaciones auxiliares.
#
# NOTA: el mínimo técnico de observaciones para ajustar una recta
# se fija internamente en 3 (para tener al menos 1 grado de libertad
# tras estimar 2 coeficientes). Esto es independiente del parámetro
# de usuario `min_n`, que se aplica más adelante, en la selección
# del mejor modelo — un modelo puede ser técnicamente VÁLIDO aquí y
# aun así no ser SELECCIONADO por no alcanzar min_n o min_r2.

N_MINIMO_TECNICO <- 3

ajustar_modelo_par <- function(df_par, col_target, col_aux) {

  n_datos <- nrow(df_par)

  resultado_base <- list(
    target = col_target,
    aux = col_aux,
    intercept = NA_real_,
    slope = NA_real_,
    r2 = NA_real_,
    n = n_datos,
    valido = FALSE,
    motivo = NA_character_,
    modelo = NULL
  )

  if (n_datos < N_MINIMO_TECNICO) {
    resultado_base$motivo <- paste0("n insuficiente para ajustar (n=", n_datos,
                                     ", mínimo técnico=", N_MINIMO_TECNICO, ")")
    return(resultado_base)
  }

  x <- df_par[[col_aux]]
  y <- df_par[[col_target]]

  if (stats::var(x, na.rm = TRUE) == 0) {
    resultado_base$motivo <- "la estación auxiliar no tiene variabilidad (varianza = 0)"
    return(resultado_base)
  }

  if (stats::var(y, na.rm = TRUE) == 0) {
    resultado_base$motivo <- "la estación objetivo no tiene variabilidad (varianza = 0)"
    return(resultado_base)
  }

  ajuste <- tryCatch({
    # reformulate() evita fallos cuando los nombres de columna
    # contienen espacios, tildes u otros caracteres no válidos en
    # sintaxis de fórmula.
    formula_modelo <- reformulate(col_aux, response = col_target)
    modelo <- lm(formula_modelo, data = df_par)
    resumen <- summary(modelo)
    list(
      intercept = unname(coef(modelo)[1]),
      slope     = unname(coef(modelo)[2]),
      r2        = resumen$r.squared,
      modelo    = modelo,
      error     = NULL
    )
  }, error = function(e) {
    list(intercept = NA_real_, slope = NA_real_, r2 = NA_real_,
         modelo = NULL, error = conditionMessage(e))
  })

  if (!is.null(ajuste$error)) {
    resultado_base$motivo <- paste0("error al ajustar el modelo: ", ajuste$error)
    return(resultado_base)
  }

  coeficientes_validos <- is.finite(ajuste$intercept) && is.finite(ajuste$slope)
  r2_valido <- is.finite(ajuste$r2)

  if (!coeficientes_validos) {
    resultado_base$motivo <- "coeficientes no finitos (NA/NaN/Inf)"
    return(resultado_base)
  }

  if (!r2_valido) {
    resultado_base$motivo <- "R² no calculable (NA/NaN/Inf)"
    return(resultado_base)
  }

  list(
    target = col_target,
    aux = col_aux,
    intercept = ajuste$intercept,
    slope = ajuste$slope,
    r2 = ajuste$r2,
    n = n_datos,
    valido = TRUE,
    motivo = NA_character_,
    modelo = ajuste$modelo
  )
}

# ============================================================
# 5. EVALUACIÓN DE MODELOS PARA UNA ESTACIÓN OBJETIVO
# ============================================================
#
# Construye el subconjunto por pares y ajusta el modelo para CADA
# estación auxiliar disponible. Devuelve:
#   - tabla_modelos: una fila por auxiliar evaluada (válida o no)
#   - datos_pares: lista con el data.frame usado en cada par
#     (necesario para exportar la hoja individual de cada par)

evaluar_modelos <- function(df, col_fecha, col_target, cols_aux) {

  datos_pares <- map(cols_aux, ~ preparar_datos_par(df, col_fecha, col_target, .x))
  names(datos_pares) <- cols_aux

  resultados <- map2(datos_pares, cols_aux,
                      ~ ajustar_modelo_par(.x, col_target, .y))

  tabla_modelos <- map_dfr(resultados, ~ tibble(
    estacion_objetivo = .x$target,
    estacion_auxiliar = .x$aux,
    intercept = .x$intercept,
    slope = .x$slope,
    r2 = .x$r2,
    n = .x$n,
    valido = .x$valido,
    motivo = .x$motivo
  ))

  message("Estación '", col_target, "': ", nrow(tabla_modelos),
          " estación(es) auxiliar(es) evaluada(s), ",
          sum(tabla_modelos$valido), " modelo(s) válido(s).")

  list(
    tabla_modelos = tabla_modelos,
    datos_pares = datos_pares
  )
}

# ============================================================
# 6. SELECCIÓN DEL MEJOR MODELO
# ============================================================
#
# Entre los modelos VÁLIDOS que cumplen n >= min_n y R² >= min_r2,
# selecciona el de mayor R². Si ninguno cumple, no se completa esa
# estación (se informa mediante message(), no se detiene el script).

seleccionar_mejor_modelo <- function(tabla_modelos, min_n = 100, min_r2 = 0.5) {

  tabla_filtrada <- tabla_modelos %>%
    filter(valido,
           n >= min_n,
           r2 >= min_r2) %>%
    arrange(desc(r2))

  if (nrow(tabla_filtrada) == 0) {
    return(NULL)
  }

  tabla_filtrada[1, ]
}

# ============================================================
# 7. COMPLETADO DE LA ESTACIÓN OBJETIVO
# ============================================================
#
# Aplica el modelo seleccionado sobre el DATASET ORIGINAL COMPLETO
# (no sobre los subconjuntos por pares usados para el ajuste).
#
# Reglas:
#   - auxiliar == 0        -> completado = 0 (no se aplica regresión)
#   - auxiliar != 0 y != NA -> completado = a + b * auxiliar
#   - auxiliar == NA        -> no se puede completar; permanece NA
#
# `limitar_negativos` (por defecto TRUE) controla si un valor
# estimado por regresión que resulte negativo se recorta a 0.
# La precipitación no puede ser físicamente negativa, pero se deja
# como parámetro explícito para que la decisión sea visible y
# configurable, en lugar de una regla oculta en el código.
#
# Devuelve una lista con:
#   - datos: el data.frame completado
#   - registro: tibble con una fila por cada valor efectivamente
#     completado (trazabilidad), o un tibble vacío si no hubo
#     completado.

completar_estacion <- function(df,
                               col_fecha,
                               col_target,
                               mejor_modelo,
                               limitar_negativos = TRUE) {

  # NOTA: en lugar de rellenar las filas sin dato y sin posibilidad de
  # completado con un valor centinela (p. ej. 999), se añade una
  # columna de trazabilidad "<objetivo>_estado" con las etiquetas
  # "Original", "Completado (regresión)", "Completado (auxiliar=0)" y
  # "SIN COMPLETAR". Esto permite identificar de un vistazo (filtro de
  # Excel) las filas sin completar, sin introducir en la columna de
  # precipitación un valor numérico que pueda confundirse con un dato
  # real en cálculos posteriores.
  col_estado <- paste0(col_target, "_estado")

  columnas_registro <- c("fecha", "estacion_objetivo", "valor_original",
                          "valor_completado", "estacion_auxiliar_utilizada",
                          "valor_auxiliar", "metodo_completado",
                          "intercepto", "pendiente", "r2_modelo", "n_modelo")

  registro_vacio <- setNames(
    as.list(rep(list(character(0)), length(columnas_registro))),
    columnas_registro
  ) %>% as_tibble()

  df_out <- df
  idx_na_objetivo <- which(is.na(df_out[[col_target]]))

  estado <- rep("Original", nrow(df_out))
  estado[idx_na_objetivo] <- "SIN COMPLETAR"

  if (is.null(mejor_modelo)) {
    message("No se completa la estación '", col_target,
            "': ningún modelo cumple los criterios mínimos (n, R²).")
    df_out[[col_estado]] <- estado
    df_out <- df_out %>% relocate(all_of(col_estado), .after = all_of(col_target))
    return(list(datos = df_out, registro = registro_vacio))
  }

  aux_col <- mejor_modelo$estacion_auxiliar
  a       <- mejor_modelo$intercept
  b       <- mejor_modelo$slope

  message("Estación '", col_target, "' completada con auxiliar '", aux_col,
          "' (R2 = ", round(mejor_modelo$r2, 3), ", n = ", mejor_modelo$n, ").")

  # Caso 3 (auxiliar NA): se excluyen de idx_completable y permanecen NA
  # (quedarán marcadas como "SIN COMPLETAR" en la columna de estado)
  idx_completable <- idx_na_objetivo[!is.na(df_out[[aux_col]][idx_na_objetivo])]

  registro <- vector("list", length(idx_completable))

  for (k in seq_along(idx_completable)) {
    i <- idx_completable[k]
    x_aux <- df_out[[aux_col]][i]

    if (x_aux == 0) {
      valor_completado <- 0
      metodo <- "Auxiliar = 0"
      estado[i] <- "Completado (auxiliar=0)"
    } else {
      valor_estimado <- a + b * x_aux
      if (limitar_negativos && valor_estimado < 0) {
        valor_completado <- 0
      } else {
        valor_completado <- valor_estimado
      }
      metodo <- "Regresión lineal"
      estado[i] <- "Completado (regresión)"
    }

    df_out[[col_target]][i] <- valor_completado

    registro[[k]] <- tibble(
      fecha = df_out[[col_fecha]][i],
      estacion_objetivo = col_target,
      valor_original = NA_real_,
      valor_completado = valor_completado,
      estacion_auxiliar_utilizada = aux_col,
      valor_auxiliar = x_aux,
      metodo_completado = metodo,
      intercepto = a,
      pendiente = b,
      r2_modelo = mejor_modelo$r2,
      n_modelo = mejor_modelo$n
    )
  }

  registro_df <- if (length(registro) > 0) bind_rows(registro) else registro_vacio

  df_out[[col_estado]] <- estado
  df_out <- df_out %>% relocate(all_of(col_estado), .after = all_of(col_target))

  n_na_restantes <- sum(is.na(df_out[[col_target]]))
  message("  -> ", length(idx_completable), " valores completados. ",
          n_na_restantes, " permanecen NA / SIN COMPLETAR (auxiliar también NA en esas fechas).")

  list(datos = df_out, registro = registro_df)
}

# ============================================================
# 8. NOMBRES DE HOJA VÁLIDOS Y ÚNICOS
# ============================================================
#
# Excel exige nombres de hoja de máximo 31 caracteres y prohíbe los
# caracteres: [ ] : * ? / \
# Esta función sanea el nombre y garantiza unicidad frente a los
# nombres ya usados, reservando espacio para un sufijo numérico si
# hiciera falta.

sanear_nombre_hoja <- function(nombre) {
  nombre <- gsub("[\\[\\]:*?/\\\\]", "_", nombre)
  substr(nombre, 1, 31)
}

generar_nombre_hoja_par <- function(prefijo, col_target, col_aux, nombres_usados) {
  base <- sanear_nombre_hoja(paste0(prefijo, "_", col_target, "_", col_aux))

  if (!(base %in% nombres_usados)) {
    return(base)
  }

  sufijo <- 2
  repeat {
    sufijo_str <- paste0("_", sufijo)
    corte <- 31 - nchar(sufijo_str)
    candidato <- paste0(substr(base, 1, corte), sufijo_str)
    if (!(candidato %in% nombres_usados)) {
      return(candidato)
    }
    sufijo <- sufijo + 1
  }
}

# ============================================================
# 9. PIPELINE PARA UNA ESTACIÓN OBJETIVO (núcleo reutilizable)
# ============================================================
#
# Encapsula: evaluación de modelos -> selección -> completado.
# No lee ni escribe ficheros: recibe un data.frame ya leído y
# devuelve resultados en memoria. Esta separación es intencionada
# para permitir, en el futuro, iterar esta misma función sobre
# varias columnas objetivo sin duplicar lógica.

completar_estacion_objetivo <- function(df,
                                        col_fecha,
                                        col_target,
                                        cols_aux,
                                        min_n = 100,
                                        min_r2 = 0.5,
                                        limitar_negativos = TRUE) {

  evaluacion <- evaluar_modelos(df, col_fecha, col_target, cols_aux)
  mejor_modelo <- seleccionar_mejor_modelo(evaluacion$tabla_modelos, min_n, min_r2)
  resultado_completado <- completar_estacion(df, col_fecha, col_target,
                                             mejor_modelo, limitar_negativos)

  tabla_modelos_final <- evaluacion$tabla_modelos %>%
    mutate(
      ecuacion = if_else(
        valido,
        paste0(estacion_objetivo, " = ", round(intercept, 4),
               " + ", round(slope, 4), " * ", estacion_auxiliar),
        NA_character_
      ),
      seleccionado = if (is.null(mejor_modelo)) {
        FALSE
      } else {
        valido &
          (estacion_objetivo == mejor_modelo$estacion_objetivo) &
          (estacion_auxiliar == mejor_modelo$estacion_auxiliar)
      }
    ) %>%
    select(estacion_objetivo, estacion_auxiliar, intercept, slope,
           ecuacion, r2, n, valido, motivo, seleccionado) %>%
    rename(intercepto = intercept, pendiente = slope) %>%
    arrange(desc(seleccionado), desc(valido), desc(r2))

  list(
    datos_completados = resultado_completado$datos,
    registro_completados = resultado_completado$registro,
    tabla_modelos = tabla_modelos_final,
    datos_pares = evaluacion$datos_pares,
    mejor_modelo = mejor_modelo
  )
}

# ============================================================
# 10. PIPELINE COMPLETO: LECTURA -> ... -> EXPORTACIÓN
# ============================================================
#
# Orquesta todo el proceso para la PRIMERA estación (segunda
# columna del Excel) como objetivo, y escribe el Excel de salida.
#
# Preparado para ampliarse a varias estaciones objetivo: bastaría
# con iterar la llamada a completar_estacion_objetivo() para cada
# columna candidata y combinar los resultados antes de exportar.

completar_primera_estacion <- function(ruta_excel,
                                       hoja = 1,
                                       min_n = 100,
                                       min_r2 = 0.5,
                                       limitar_negativos = TRUE,
                                       ruta_salida = "salida_completado.xlsx") {

  # --- Lectura ---
  df <- leer_datos_precipitacion(ruta_excel, hoja = hoja)

  # --- Validación ---
  validar_datos_precipitacion(df, col_fecha = 1)

  nombres <- colnames(df)
  col_fecha  <- nombres[1]
  col_target <- nombres[2]
  cols_aux   <- nombres[-c(1, 2)]

  # --- Preparación por pares -> Ajuste -> Evaluación -> Selección -> Completado ---
  resultado <- completar_estacion_objetivo(df, col_fecha, col_target, cols_aux,
                                           min_n = min_n, min_r2 = min_r2,
                                           limitar_negativos = limitar_negativos)

  # --- Registro de completados ---
  # (ya incluido en resultado$registro_completados)

  # --- Exportación ---
  # "Datos" es el data.frame original tal como se leyó y validó, ANTES
  # de completar nada. Se coloca como primera hoja para poder comparar
  # de un vistazo, dentro del mismo Excel, el dato original frente al
  # completado, sin tener que abrir el Excel de origen aparte.
  nombres_usados <- c("Datos", "Datos_completados", "Registro_completados", "Modelos_regresion")
  hojas_pares <- list()

  for (col_aux in cols_aux) {
    nombre_hoja <- generar_nombre_hoja_par("DEP", col_target, col_aux, nombres_usados)
    nombres_usados <- c(nombres_usados, nombre_hoja)
    hojas_pares[[nombre_hoja]] <- resultado$datos_pares[[col_aux]]
  }

  lista_hojas <- c(
    list(
      "Datos" = df,
      "Datos_completados" = resultado$datos_completados,
      "Registro_completados" = resultado$registro_completados,
      "Modelos_regresion" = resultado$tabla_modelos
    ),
    hojas_pares
  )

  write_xlsx(lista_hojas, path = ruta_salida)

  message("Archivo de salida escrito en: ", ruta_salida)
  message("Hojas generadas: ", paste(names(lista_hojas), collapse = ", "))

  invisible(list(
    datos_originales = df,
    datos_completados = resultado$datos_completados,
    registro_completados = resultado$registro_completados,
    tabla_modelos = resultado$tabla_modelos,
    datos_pares = resultado$datos_pares
  ))
}

# ------------------------------------------------------------
# 11. Ejemplo de uso
# ------------------------------------------------------------

# resultado <- completar_primera_estacion(
#   ruta_excel = "precipitacion.xlsx",
#   hoja = 1,
#   min_n = 100,
#   min_r2 = 0.5,
#   limitar_negativos = TRUE,
#   ruta_salida = "precipitacion_completada.xlsx"
# )
