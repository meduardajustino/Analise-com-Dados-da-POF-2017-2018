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

# correspondência UF x Estado
ufs_nomes <- tibble(
  uf = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
  estado = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins",
             "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco",
             "Alagoas", "Sergipe", "Bahia", "Minas Gerais", "Espírito Santo", "Rio de Janeiro",
             "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul",
             "Mato Grosso", "Goiás", "Distrito Federal")
)

# Criar log do preço
df <- df %>%
  mutate(lnp = log(precom))

# Criar dummies para grupos (produto) e estados (UF)
df <- df %>%
  mutate(across(c(cod, uf), as.factor)) %>%
  mutate(across(c(cod, uf), ~ factor(.))) %>%
  mutate(peso = as.numeric(peso))

# 1. Rodar regressão CPD (Country-Product Dummy)
modelo <- lm(lnp ~ cod + uf, data = df, weights = peso)

codigo_base <- as.numeric(levels(df$uf)[1])
print(paste("O código da UF base é:", codigo_base))

# 2. Extrair coeficientes dos 12 estados (não-base)
coefs_sem_base <- tidy(modelo) %>%
  filter(str_detect(term, "uf")) %>%
  mutate(
    uf = as.numeric(str_remove(term, "uf")),
    coef = estimate,
    ecoef = exp(coef)
  ) %>%
  select(uf, coef, std.error, p.value, ecoef)

# 3. Criar a linha para a UF base (Pará)
# O coeficiente é 0 por definição, e o ecoef (exp(0)) é 1.
# p.value e std.error são NA, pois não são testados.
base_row <- tibble(
  uf = codigo_base,
  coef = 0,
  std.error = NA,
  p.value = NA,
  ecoef = 1
)

coefs <- bind_rows(coefs_sem_base, base_row)

# 5. Calcular o ICVR usando a MÉDIA CORRETA (com todos os 13 estados)
tabela_icvr_estados <- coefs %>%
  mutate(
    m_ecoef_correta = mean(ecoef), # A média agora é calculada sobre todos os 13 valores de 'ecoef'
    icvr = (ecoef / m_ecoef_correta) - 1
  ) %>%
  # Adicionar nomes dos estados para os gráficos
  left_join(ufs_nomes, by = "uf") %>%
  # Organizar para visualização
  select(uf, estado, coef, std.error, ecoef, p.value, icvr) %>%
  arrange(desc(icvr))

# Exibir a tabela final e correta
print("Tabela Final do ICVR com todos os 13 estados:")
print(tabela_icvr_estados)

# --- CÓDIGO PARA A TABELA DE ESTATÍSTICAS DESCRITIVAS ---

# Agrupa os dados por UF para calcular as estatísticas
tabela_descritiva <- df %>%
  # Agrupa por cada estado
  group_by(uf, estado = ufs_nomes$estado[match(uf, ufs_nomes$uf)]) %>%
  # Calcula as métricas resumidas
  summarise(
    `Nº de Itens Observados` = n(),
    `Preço Médio (R$)` = mean(precom, na.rm = TRUE),
    `Peso Médio no Orçamento` = mean(peso, na.rm = TRUE),
    .groups = 'drop' # Desagrupa ao final
  ) %>%
  # Ordena pela quantidade de observações
  arrange(desc(`Nº de Itens Observados`))

# Exibir a tabela descritiva
print("Tabela 1: Estatísticas Descritivas da Amostra por Estado")
print(tabela_descritiva)

# Carregue o pacote scales se ainda não o fez
# library(scales)

# Use o seu código para criar a 'tabela_descritiva'
# Depois, crie uma versão formatada para apresentar no seu trabalho
tabela_descritiva_formatada <- tabela_descritiva %>%
  mutate(
    # Formata o preço com duas casas decimais
    `Preço Médio (R$)` = scales::number(`Preço Médio (R$)`, accuracy = 0.01),
    # Formata o peso como porcentagem com duas casas decimais
    `Peso Médio no Orçamento` = scales::percent(`Peso Médio no Orçamento`, accuracy = 0.01)
  )

# Exibir a tabela formatada e pronta para o artigo
print(tabela_descritiva_formatada)

# --- CÓDIGO PARA GERAR A TABELA NO FORMATO SOLICITADO ---

tabela_artigo <- tabela_icvr_estados %>%
  # Renomeia a coluna 'ecoef' para 'exp(coef)' para maior clareza
  rename(`exp(coef)` = ecoef) %>%
  # Seleciona as colunas na ordem exata solicitada
  select(
    uf,
    estado,
    coef,
    std.error,
    p.value,
    `exp(coef)`,
    icvr
  )

# Exibir a tabela final no formato correto
print("Tabela de Resultados:")
print(tabela_estado)


# Visualização opcional
ggplot(tabela_icvr_estados, aes(x = reorder(as.factor(uf), icvr), y = icvr)) +
  geom_col(fill = "purple", alpha = 0.7) +
  coord_flip() +
  labs(x = "UF", y = "ICVR (Índice de Custo de Vida Relativo)",
       title = "Custo de Vida com base nos Gastos com Habitação (POF 2017-18)") +
  theme_minimal()

summary(modelo)


# Visualizações para facilitar a escrita do trabalho
# Instalando novos pacotes para ajudar na visualização
install.packages(c("geobr", "sf", "ggplot2", "dplyr", "readr"))  # caso não tenha
library(geobr)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(ggplot2)
library(broom)
library(stringr)

plot_icvr_estados <- ggplot(tabela_icvr_estados, aes(x = reorder(estado, icvr), y = icvr)) +
  geom_col(fill = "#9b3d5c", color = "#9b3d5c", width = 0.7) +
  coord_flip() +
  labs(
    title = "Ranking do Custo de Vida com Gastos em Habitação (POF 2017-2018)",
    x = "Estado",
    y = "ICVR (Índice de Custo de Vida Relativo)"
  ) +
  theme_minimal(base_family = "serif", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "black"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(color = "black", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot_icvr_estados)

# Salvar o gráfico com dimensões ampliadas
ggsave(
  "ranking_icvr_habitacao.png",
  plot = plot_icvr_estados,
  width = 12,        # aumentada a largura
  height = 6,       # altura proporcional
  dpi = 300,
  units = "in",
  bg = "white"
)


# salvar CSV final
write_csv(tabela_icvr_estados, "icvr_habitacao_por_estado.csv")


#Nova forma de visualização - mapa do Brasil


# Coeficientes e cálculo do ICVR
tabela_icvr <- tidy(modelo) %>%
  filter(str_detect(term, "uf")) %>%
  mutate(
    uf = str_remove(term, "uf"),
    uf = as.integer(uf),
    coef = estimate,
    ecoef = exp(coef),
    m_ecoef = mean(ecoef),
    icvr = (ecoef / m_ecoef) - 1
  ) %>%
  select(uf, icvr)

# Mapa das UFs
mapa_uf <- read_state(year = 2020)

# Merge: mapa + dados
mapa_icvr_dados <- mapa_uf %>%
  left_join(tabela_icvr, by = c("code_state" = "uf"))

# Plot do mapa com estilo pastel + serif
mapa_icvr <- ggplot(mapa_icvr_dados) +
  geom_sf(aes(fill = icvr), color = "white", size = 0.2) +
  scale_fill_gradient2(
    low = "#9b3d5c", mid = "#f4ebe9", high = "#4a6785",
    midpoint = 0,
    name = "ICVR",
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Índice de Custo de Vida Relativo (ICVR) por UF",
    subtitle = "Componente de habitação – POF 2017-2018",
  ) +
  theme_light(base_size = 13, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right"
  )

print(mapa_icvr)

ggsave("mapa_icvr_habitacao.png", plot = mapa_icvr, width = 8, height = 8, units = "in", dpi = 300)
