library(dplyr)
library(lubridate)
library(jsonlite)

# INPUTS!!!
# Actualizar para ajustar pesos según índice de salarios
mes_i = dmy("1/8/2025") # Mitad del trimestre
path_eph = "data/usu_individual_T325.txt" # Archivo con los datos
path_ind_salarios = "data/indice_salarios.csv" # Archivo con el índice de salarios

# Leemos los datos
df = read.table(path_eph, sep=";", dec=",", header=T)
df_indice_salarios = read.table(path_ind_salarios, sep=";", dec=",", header=T)

# String trimestre
periodo_raw = gsub(".*_(T\\d)(\\d{2})\\.txt", "\\1\\2", path_eph)  # "T325"
periodo = paste0(substr(periodo_raw, 2, 2), "T", substr(periodo_raw, 3, 4))  # "3T25"

# Nos quedamos solo con las columnas que necesitamos
df = df %>% select(
  REGION,
  CH04,        # Sexo
  CH06,        # Edad (por si la necesitás después)
  NIVEL_ED,    # Nivel educativo
  ESTADO,      # Condición de actividad
  PP3E_TOT,    # Horas trabajadas semana - ocupación principal
  P21,         # Ingreso mensual ocupación principal
  PONDERA,     # Ponderador general
  PONDIIO      # Ponderador de ingresos
)
df = df %>% filter(CH06 >= 18) # Solo mayores de edad
df$PP3E_TOT[df$PP3E_TOT >= 112] = NA # Horas razonables

# Agrupamos educación para tener menos categorías, y grupos con más datos
df = df %>% mutate(
  NIVEL_ED_GRUPO = case_when(
    NIVEL_ED %in% c(1, 7) ~ "Primario incompleto",
    NIVEL_ED %in% c(2, 3) ~ "Secundario incompleto",
    NIVEL_ED == 4          ~ "Secundario completo",
    NIVEL_ED %in% c(5, 6) ~ "Universitario en curso/completo"
  ),
  SEXO = case_when(
    CH04 == 1 ~ "Hombres",
    CH04 == 2 ~ "Mujeres"
  ),
  GRUPO_EDAD = case_when(
    CH06 >= 18 & CH06 <= 29 ~ "18-29",
    CH06 >= 30 & CH06 <= 44 ~ "30-44",
    CH06 >= 45 & CH06 <= 59 ~ "45-59",
    CH06 >= 60              ~ "60+"
  )
)

df$NIVEL_ED_GRUPO = factor(df$NIVEL_ED_GRUPO, levels = c(
  "Primario incompleto",
  "Secundario incompleto",
  "Secundario completo",
  "Universitario en curso/completo"
), ordered = TRUE)

# Ingreso mensual ajustado por índice de salarios

# Convertimos periodos a formato fecha
# Agregamos Multiplicador mensual: variación respecto al mes anterior
df_indice_salarios = df_indice_salarios %>%
  mutate(
    fecha = dmy(periodo),
    multiplicador = IS_indice_total / lag(IS_indice_total)
  )

acumulado = df_indice_salarios %>%
  filter(fecha >= mes_i) %>%
  pull(multiplicador) %>%
  prod(na.rm = TRUE)
df$INGRESO = df$P21*acumulado
print(paste0("Ingresos ajustados por índice de salarios (INDEC) a la fecha: ", max(df_indice_salarios$fecha)))

# Calculamos el ingreso por hora:
# multiplicamos por 4.345 porque es la cantidad de semanas que hay en un mes
df$INGRESO_POR_HORA = df$INGRESO / (df$PP3E_TOT * 4.345)


# GENERAMOS .JSON
# Tasa de ocupación por sexo y educación
tasa_ocupacion = df %>%
  filter(ESTADO %in% c(1, 2, 3)) %>%
  group_by(sexo = SEXO, educacion = NIVEL_ED_GRUPO) %>%
  summarise(
    tasa_ocupacion = sum(PONDERA * (ESTADO == 1)) / sum(PONDERA),
    .groups = "drop"
  )

# Indicadores de ocupados: ingreso, horas, ingreso por hora
indicadores_ocupados = df %>%
  filter(ESTADO == 1, P21 > 0, PP3E_TOT > 0) %>%
  group_by(sexo = SEXO, educacion = NIVEL_ED_GRUPO) %>%
  summarise(
    ingreso_mensual = weighted.mean(INGRESO, PONDIIO),
    horas_semanales = weighted.mean(PP3E_TOT, PONDIIO),
    ingreso_por_hora = weighted.mean(INGRESO_POR_HORA, PONDIIO),
    .groups = "drop"
  )

# Unir ambas tablas
datos = tasa_ocupacion %>%
  left_join(indicadores_ocupados, by = c("sexo", "educacion"))

# Calcular brecha por nivel educativo
datos = datos %>%
  group_by(educacion) %>%
  mutate(
    brecha = (ingreso_por_hora[sexo == "Hombres"] - ingreso_por_hora[sexo == "Mujeres"]) / 
      ingreso_por_hora[sexo == "Hombres"]
  ) %>%
  ungroup()

# Exportar
datos = datos %>% arrange(educacion, sexo)
write_json(datos, paste0("datos_brecha_", periodo, ".json"))

# Datos para webapp:

# JSON 2: datos completos para webapp interactiva
tasa_ocupacion_full = df %>%
  filter(ESTADO %in% c(1, 2, 3)) %>%
  group_by(sexo = SEXO, educacion = NIVEL_ED_GRUPO, region = REGION, grupo_edad = GRUPO_EDAD) %>%
  summarise(
    tasa_ocupacion = sum(PONDERA * (ESTADO == 1)) / sum(PONDERA),
    .groups = "drop"
  )

indicadores_full = df %>%
  filter(ESTADO == 1, P21 > 0, PP3E_TOT > 0) %>%
  group_by(sexo = SEXO, educacion = NIVEL_ED_GRUPO, region = REGION, grupo_edad = GRUPO_EDAD) %>%
  summarise(
    ingreso_mensual = weighted.mean(INGRESO, PONDIIO),
    horas_semanales = weighted.mean(PP3E_TOT, PONDIIO),
    ingreso_por_hora = weighted.mean(INGRESO_POR_HORA, PONDIIO),
    .groups = "drop"
  )

datos_full = tasa_ocupacion_full %>%
  left_join(indicadores_full, by = c("sexo", "educacion", "region", "grupo_edad"))

datos_full = datos_full %>%
  group_by(educacion, region, grupo_edad) %>%
  mutate(
    brecha = (ingreso_por_hora[sexo == "Hombres"] - ingreso_por_hora[sexo == "Mujeres"]) / 
      ingreso_por_hora[sexo == "Hombres"]
  ) %>%
  ungroup() %>%
  arrange(educacion, region, grupo_edad, sexo)

write_json(datos_full, paste0("datos_brecha_full_", periodo, ".json"))







table(df$PP3E_TOT)
