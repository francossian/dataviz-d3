library(readxl)
library(tidyverse)

data <- read_excel('data/proyecciones_nacionales_2022_2040_c1_c2.xlsx',
                 sheet = "Cuadro 2.1",
                 skip = 2)[-c(1, 2, 104, 105), ]
df <- data
df$Edad <- gsub("100 y más", "100", df$Edad)
df$Edad <- as.numeric(df$Edad)
df$Edad

df$GrupoEdad <- cut(
  df$Edad,
  breaks = c(seq(0, 90, by = 10), Inf),
  right  = FALSE,
  labels = c(paste0(seq(0, 80, 10), " a ", seq(9, 89, 10)), "90 y más")
)

df_grupos <- df %>%
  select(-Edad) %>%
  group_by(GrupoEdad) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ .x / sum(.x, na.rm = TRUE)))
write.csv(df_grupos, "datos-poblacion.csv", row.names = FALSE)

plot(df_grupos$GrupoEdad, df_grupos[[2]])
plot(df_grupos$GrupoEdad, df_grupos[[20]])