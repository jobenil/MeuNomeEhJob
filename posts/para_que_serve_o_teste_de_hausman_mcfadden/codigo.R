# Pacotes
if(!require(mlogit)) install.packages("mlogit", dependencies = TRUE)
if(!require(dfidx)) install.packages("dfidx", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if(!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if(!require(tidyr)) install.packages("tidyr", dependencies = TRUE)

library(mlogit); library(dfidx); library(ggplot2); library(dplyr); library(tidyr)

set.seed(123)

# Parâmetros
n <- 500
alternativas <- c("carro", "onibus", "bicicleta", "ape")

# Simulação (mesma ideia de antes)
dados <- expand.grid(id = 1:n, modo = alternativas, stringsAsFactors = FALSE)
dados$renda <- rep(runif(n, 1000, 8000), each = length(alternativas))

dados$custo <- with(dados,
                    ifelse(modo == "carro",     5 + 0.0002 * renda + rnorm(n*4, 0, 1),
                           ifelse(modo == "onibus",    3 + 0.0001 * renda + rnorm(n*4, 0, 1),
                                  ifelse(modo == "bicicleta", 0.5 + rnorm(n*4, 0, 0.5),
                                         0.1 + rnorm(n*4, 0, 0.2))))
)

dados$tempo <- with(dados,
                    ifelse(modo == "carro",     25 + rnorm(n*4, 0, 5),
                           ifelse(modo == "onibus",    40 + rnorm(n*4, 0, 8),
                                  ifelse(modo == "bicicleta", 35 + rnorm(n*4, 0, 6),
                                         50 + rnorm(n*4, 0, 10))))
)

dados$utilidade <- with(dados, 
                        -0.05 * custo - 0.03 * tempo +
                          ifelse(modo == "carro", 0.8,
                                 ifelse(modo == "onibus", 0.5,
                                        ifelse(modo == "bicicleta", 0.3, 0)))
)

dados <- dados %>%
  group_by(id) %>%
  mutate(prob = exp(utilidade) / sum(exp(utilidade))) %>%
  ungroup()

escolha <- dados %>%
  group_by(id) %>%
  slice(which.max(prob)) %>%
  ungroup() %>%
  select(id, escolha = modo)

dados <- merge(dados, escolha, by = "id")

# --- CORREÇÃO CRUCIAL: definir factors e indicador binário "chosen" ---
dados$modo <- factor(dados$modo, levels = alternativas)
dados$escolha <- factor(dados$escolha, levels = alternativas)
dados$chosen <- as.integer(dados$modo == dados$escolha)  # 1/0

# diagnósticos rápidos
cat("Contagem de escolhas por alternativa:\n")
print(table(dados$escolha))
cat("\nContagem de linhas e primeiras observações:\n")
print(dim(dados))
print(head(dados))

# Converter para dfidx: idx = c("id","modo"), choice = "chosen"
dados_dfidx <- dfidx(data = dados, idx = c("id", "modo"), choice = "chosen")

# conferir índices criados
cat("\nÍndices do objeto dfidx:\n")
print(attr(dados_dfidx, "idx"))

# Estimar modelo logit multinomial (note: left-hand side é 'chosen')
modelo_completo <- mlogit(chosen ~ custo + tempo | renda, data = dados_dfidx)
summary(modelo_completo)

# --- PREDIÇÃO ---
renda_seq <- seq(1000, 8000, by = 500)
pred_dados <- expand.grid(id = 1:length(renda_seq), modo = alternativas, stringsAsFactors = FALSE)
pred_dados$renda <- rep(renda_seq, each = length(alternativas))

# usar médias de custo/tempo por modo (como antes)
valores_medios <- dados %>%
  group_by(modo) %>%
  summarise(custo = mean(custo), tempo = mean(tempo), .groups = "drop")

pred_dados <- left_join(pred_dados, valores_medios, by = "modo")
pred_dados$modo <- factor(pred_dados$modo, levels = alternativas)

# dfidx para predição (aqui não passamos choice)
pred_dfidx <- dfidx(pred_dados, idx = c("id", "modo"))

# predict retorna matriz com probabilidades por alternativa
pred_matrix <- predict(modelo_completo, newdata = pred_dfidx)

pred_df <- as.data.frame(pred_matrix)
pred_df$id <- 1:nrow(pred_df)

pred_long <- pred_df %>%
  pivot_longer(cols = -id, names_to = "modo", values_to = "prob")

renda_map <- data.frame(id = rep(1:length(renda_seq), each = length(alternativas)),
                        modo = rep(alternativas, times = length(renda_seq)),
                        renda = rep(renda_seq, each = length(alternativas)),
                        stringsAsFactors = FALSE)

pred_long <- left_join(pred_long, renda_map, by = c("id", "modo"))

# Plot
ggplot(pred_long, aes(x = renda, y = prob, color = modo)) +
  geom_line(size = 1.1) +
  labs(title = "Probabilidade predita de escolha do modo de transporte",
       subtitle = "Em função da renda mensal",
       x = "Renda mensal (R$)",
       y = "Probabilidade predita",
       color = "Modo de transporte") +
  theme_minimal(base_size = 14)


###########################################################################
# Pacotes necessários
if(!require(mlogit)) install.packages("mlogit", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if(!require(dplyr)) install.packages("dplyr", dependencies = TRUE)

library(mlogit)
library(ggplot2)
library(dplyr)

set.seed(123)

# Número de indivíduos
n <- 500
alternativas <- c("carro", "onibus", "bicicleta", "ape")

# Simulação dos dados
dados <- expand.grid(id = 1:n, modo = alternativas)
dados$renda <- rep(runif(n, 1000, 8000), each = length(alternativas))

dados$custo <- with(dados,
                    ifelse(modo == "carro",     5 + 0.0002 * renda + rnorm(n*4, 0, 1),
                           ifelse(modo == "onibus",    3 + 0.0001 * renda + rnorm(n*4, 0, 1),
                                  ifelse(modo == "bicicleta", 0.5 + rnorm(n*4, 0, 0.5),
                                         0.1 + rnorm(n*4, 0, 0.2))))
)

dados$tempo <- with(dados,
                    ifelse(modo == "carro",     25 + rnorm(n*4, 0, 5),
                           ifelse(modo == "onibus",    40 + rnorm(n*4, 0, 8),
                                  ifelse(modo == "bicicleta", 35 + rnorm(n*4, 0, 6),
                                         50 + rnorm(n*4, 0, 10))))
)

# Utilidade verdadeira (modelo teórico)
dados$utilidade <- with(dados, 
                        -0.05 * custo - 0.03 * tempo +
                          ifelse(modo == "carro", 0.8,
                                 ifelse(modo == "onibus", 0.5,
                                        ifelse(modo == "bicicleta", 0.3, 0)))
)

# Probabilidades reais e escolha simulada
dados <- dados %>%
  group_by(id) %>%
  mutate(prob = exp(utilidade) / sum(exp(utilidade))) %>%
  ungroup()

escolha <- dados %>%
  group_by(id) %>%
  slice(which.max(prob)) %>%
  ungroup() %>%
  select(id, escolha = modo)

dados <- merge(dados, escolha, by = "id")

# Converter para formato mlogit
dados_mlogit <- mlogit.data(dados, choice = "escolha", shape = "long",
                            alt.var = "modo", id.var = "id")

# Estimar modelo logit multinomial
modelo_completo <- mlogit(escolha ~ custo + tempo | renda, data = dados_mlogit)
summary(modelo_completo)

# Gerar predições por nível de renda
renda_seq <- seq(1000, 8000, by = 500)
pred_dados <- expand.grid(
  renda = renda_seq,
  modo = alternativas
)

# Fixar valores médios de custo e tempo para cada modo
valores_medios <- dados %>%
  group_by(modo) %>%
  summarise(custo = mean(custo), tempo = mean(tempo))

pred_dados <- left_join(pred_dados, valores_medios, by = "modo")

# Converter para formato mlogit
pred_mlogit <- mlogit.data(pred_dados, choice = "chosen",
                           shape = "long", alt.var = "modo", id.var = NULL)

# Predizer probabilidades
pred_probs <- predict(modelo_completo, newdata = pred_mlogit)
pred_dados$prob <- as.numeric(pred_probs)

# Gráfico: Probabilidade predita vs. Renda
ggplot(dados, aes(x = renda, y = prob, color = modo)) +
  geom_smooth(size = 0.5) +
  labs(title = "Probabilidade predita de escolha do modo de transporte",
       subtitle = "Em função da renda mensal",
       x = "Renda mensal (R$)",
       y = "Probabilidade predita",
       color = "Modo de transporte") +
  theme_minimal(base_size = 14)
