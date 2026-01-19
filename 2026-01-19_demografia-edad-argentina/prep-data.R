library(readxl)
library(tidyverse)

data <- read_excel('data/proyecciones_nacionales_2022_2040_c1_c2.xlsx',
                 sheet = "Cuadro 2.1",
                 skip = 2)[-c(1, 2, 104, 105), ]
df <- data
df$Edad <- gsub("100 y más", "100", df$Edad)
df$Edad <- as.numeric(df$Edad)
df$Edad
df <- df %>%
  mutate(
    EtapaVida = case_when(
      Edad >= 0  & Edad <= 12 ~ "Infancia (0 a 12)",
      Edad >= 13 & Edad <= 18 ~ "Adolescencia (13 a 18)",
      Edad >= 19 & Edad <= 39 ~ "Adultez joven (19 a 39)",
      Edad >= 40 & Edad <= 59 ~ "Edad media (40 a 59)",
      Edad >= 60 & Edad <= 79 ~ "Adultez tardía (60 a 79)",
      Edad >= 80               ~ "Vejez (80 y más)",
      TRUE                     ~ NA_character_
    )
  )

df <- df %>%
  mutate(
    EtapaVida = factor(
      EtapaVida,
      levels = c(
        "Infancia (0 a 12)",
        "Adolescencia (13 a 18)",
        "Adultez joven (19 a 39)",
        "Edad media (40 a 59)",
        "Adultez tardía (60 a 79)",
        "Vejez (80 y más)"
      )
    )
  )

df$GrupoEdad <- cut(
  df$Edad,
  breaks = c(seq(0, 90, by = 10), Inf),
  right  = FALSE,
  labels = c(paste0(seq(0, 80, 10), " a ", seq(9, 89, 10)), "90 y más")
)

df_grupos_pct <- df %>%
  select(-Edad) %>%
  group_by(GrupoEdad) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ .x / sum(.x, na.rm = TRUE)))
write.csv(df_grupos_pct, "datos-poblacion-pct.csv", row.names = FALSE)

df_grupos_pob <- df %>%
  select(-Edad) %>%
  group_by(GrupoEdad) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
write.csv(df_grupos_pob, "datos-poblacion.csv", row.names = FALSE)

# Grupos OMS https://ccfprosario.com/etapas-de-la-vida-por-edades-segun-la-oms/#conoce_las_12_etapas_del_desarrollo_humano_y_su_importancia_en_la_vida

df_grupos_pct_2 <- df %>%
  select(-Edad) %>%
  group_by(EtapaVida) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ .x / sum(.x, na.rm = TRUE)))
write.csv(df_grupos_pct_2, "datos-poblacion-etapas-pct.csv", row.names = FALSE)

df_grupos_pob_2 <- df %>%
  select(-Edad) %>%
  group_by(EtapaVida) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
write.csv(df_grupos_pob_2, "datos-poblacion-etapas.csv", row.names = FALSE)
