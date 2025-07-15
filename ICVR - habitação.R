install.packages(c(
  "tidyverse",    # dplyr, ggplot2, etc
  "readr",        # para importar CSV
  "broom",        # para extrair coeficientes do modelo
  "srvyr",        # se quiser aplicar pesos corretamente
  "modelsummary"  # para exibir tabelas de regressão
))

library(tidyverse)
library(broom)
library(modelsummary)

# Definir diretório e importar os dados
df <- read_csv("M:/Maria Eduarda/Downloads/ipc_gastos pof 2017-2018 aula.csv")
# Filtrar apenas o grupo "Habitação"
df <- df %>%
  filter(str_to_lower(grupo) == "habitacao")

glimpse(df)

# Criar log do preço
df <- df %>%
  mutate(lnp = log(precom))

# Criar dummies para grupos (produto) e estados (UF)
df <- df %>%
  mutate(across(c(cod, uf), as.factor)) %>%
  mutate(across(c(cod, uf), ~ factor(.))) %>%
  mutate(peso = as.numeric(peso))

# Rodar regressão CPD (Country-Product Dummy)
modelo <- lm(lnp ~ cod + uf, data = df, weights = peso)

# Ver coeficientes por UF
coefs <- tidy(modelo) %>%
  filter(str_detect(term, "uf")) %>%
  mutate(uf = str_remove(term, "uf")) %>%
  mutate(uf = as.numeric(uf)) %>%
  rename(coef = estimate) %>%
  mutate(ecoef = exp(coef)) %>%
  mutate(m_ecoef = mean(ecoef),
         icvr = (ecoef / m_ecoef) - 1)

# Reduzir para tabela final por UF
tabela_icvr <- coefs %>%
  select(uf, icvr) %>%
  arrange(desc(icvr))

# Visualização opcional
ggplot(tabela_icvr, aes(x = reorder(as.factor(uf), icvr), y = icvr)) +
  geom_col(fill = "purple", alpha = 0.7) +
  coord_flip() +
  labs(x = "UF", y = "ICVR (Índice de Custo de Vida Relativo)",
       title = "Custo de Vida com base nos Gastos com Habitação (POF 2017-18)") +
  theme_minimal()

# Visualizações para facilitar a escrita do trabalho

# correspondência UF x Estado
ufs_nomes <- tibble(
  uf = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
  estado = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins",
             "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco",
             "Alagoas", "Sergipe", "Bahia", "Minas Gerais", "Espírito Santo", "Rio de Janeiro",
             "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul",
             "Mato Grosso", "Goiás", "Distrito Federal")
)

# juntar com a tabela_icvr
tabela_icvr_nomeada <- tabela_icvr %>%
  left_join(ufs_nomes, by = "uf") %>%
  arrange(desc(icvr))

library(ggplot2)

ggplot(tabela_icvr_nomeada, aes(x = reorder(estado, icvr), y = icvr)) +
  geom_col(fill = "mediumorchid", alpha = 0.7) +
  coord_flip() +
  labs(
    x = "Estado",
    y = "ICVR (Índice de Custo de Vida Relativo)",
    title = "Ranking do Custo de Vida com Gastos em Habitação (POF 2017-18)"
  ) +
  theme_minimal(base_size = 13)


# salvar CSV final
write_csv(tabela_icvr_nomeada, "icvr_habitacao_por_estado.csv")


#Nova forma de visualização - mapa do Brasil

install.packages(c("geobr", "sf", "ggplot2", "dplyr", "readr"))  # caso não tenha
library(geobr)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)

# Extração dos coeficientes
tabela_icvr <- tidy(modelo) %>%
  filter(str_detect(term, "uf")) %>%
  mutate(
    uf = str_remove(term, "uf"),
    uf = as.integer(uf),
    coef = estimate,
    ecoef = exp(coef)
  ) %>%
  mutate(
    m_ecoef = mean(ecoef),
    icvr = (ecoef / m_ecoef) - 1
  ) %>%
  select(uf, icvr) %>%
  arrange(desc(icvr))

# Obter mapa
mapa_uf <- read_state(year = 2020)

# Juntar mapa com dados
mapa_icvr_dados <- mapa_uf %>%
  left_join(tabela_icvr, by = c("code_state" = "uf"))

# Visualizar para checar
summary(mapa_icvr_dados$icvr)  # Não deve ser tudo NA

# Mapa
ggplot(mapa_icvr_dados) +
  geom_sf(aes(fill = icvr), color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0, name = "ICVR"
  ) +
  labs(
    title = "Custo de Vida Relativo com Gastos em Habitação (POF 2017-18)",
    subtitle = "Índice de Custo de Vida Relativo por UF",
    caption = "Fonte: POF 2017-18, IBGE"
  ) +
  theme_minimal(base_size = 13)
