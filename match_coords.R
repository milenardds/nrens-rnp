library(readxl)
library(stringdist)
library(dplyr)
library(writexl)

# Ler dados
nrens <- read_excel("nrens.xlsx")
worldcities <- read_excel("worldcities.xlsx")

# Para cada pais em worldcities, pegar um representante (capital ou cidade mais populosa) # nolint
wc_countries <- worldcities %>%
  mutate(is_capital = ifelse(!is.na(capital) & capital == "primary", 1, 0)) %>%
  arrange(desc(is_capital), desc(population)) %>%
  group_by(country) %>%
  slice(1) %>%
  ungroup() %>%
  select(country, lat, lng)

# Correcoes manuais para paises com nomes muito diferentes
correcoes <- c(
  "South Korea"    = "Korea, South",
  "Czech Republic" = "Czechia",
  "Amsterdam"      = "Netherlands"
)

# Paises unicos de nrens
nrens_unicos <- unique(nrens$Country)
wc_lista <- wc_countries$country

# Para cada pais em nrens, encontrar o melhor match em worldcities com similaridade >= 90% # nolint
resultado <- data.frame(Country = nrens_unicos, matched_country = NA_character_,
                        lat = NA_real_, lng = NA_real_, stringsAsFactors = FALSE) # nolint

for (i in seq_along(nrens_unicos)) {
  pais_busca <- nrens_unicos[i]
  
  # Aplicar correcao manual se existir
  if (pais_busca %in% names(correcoes)) {
    pais_busca <- correcoes[[pais_busca]]
  }
  
  sims <- stringsim(tolower(pais_busca), tolower(wc_lista), method = "jw")
  best_idx <- which.max(sims)
  best_sim <- sims[best_idx]
  
  if (best_sim >= 0.90) {
    resultado$matched_country[i] <- wc_lista[best_idx]
    resultado$lat[i] <- wc_countries$lat[best_idx]
    resultado$lng[i] <- wc_countries$lng[best_idx]
  }
}

# Mostrar correspondencias encontradas e nao encontradas
cat("=== Correspondencias encontradas ===\n")
encontrados <- resultado %>% filter(!is.na(matched_country))
print(as.data.frame(encontrados[, c("Country", "matched_country")]), row.names = FALSE) # nolint: line_length_linter.

cat("\n=== Paises SEM correspondencia (similaridade < 90%) ===\n")
nao_encontrados <- resultado %>% filter(is.na(matched_country))
if (nrow(nao_encontrados) > 0) {
  print(nao_encontrados$Country)
} else {
  cat("Todos os paises foram correspondidos!\n")
}

# Juntar lat e lng ao nrens original pela coluna Country
nrens <- nrens %>%
  left_join(resultado %>% select(Country, lat, lng), by = "Country")

# Salvar resultado
write_xlsx(nrens, "nrens_com_coords.xlsx")
cat("\nArquivo salvo: nrens_com_coords.xlsx\n")
cat("Dimensao final:", nrow(nrens), "x", ncol(nrens), "\n")
