# Carregando pacotes
library(ggplot2)
library(tidyr)
library(dplyr)

# Gerando os dados
set.seed(123)  # Para reprodutibilidade
x <- rnorm(1001, 15, 2.5)

# Função para calcular quantis para todos os tipos
quantAll <- function(x, prob, ...) {
  t(vapply(1:9, function(typ) quantile(x, probs = prob, type = typ, ...),
           quantile(x, prob, type=1, ...)))
}

# Percentis
p <- c(0.1, 0.5, 1, 2, 5, 10, 50)/100

# Calculando quantis
qmat <- quantAll(x, p)

# Transformando para data frame longo (long format)
df <- as.data.frame(qmat)
df$Type <- 1:9
df_long <- pivot_longer(df, -Type, names_to = "Percentile", values_to = "Quantile")

# Ajustando nomes dos percentis (para exibição como %)
df_long$Percentile <- as.numeric(gsub("V", "", gsub("\\.", "", df_long$Percentile)))
df_long$Percentile <- factor(p[order(p)], levels = sort(p), labels = paste0(p[order(p)]*100, "%"))

# Criando o gráfico com ggplot2
ggplot(df_long, aes(x = Percentile, y = Quantile, color = factor(Type))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  labs(title = "Comparação de Quantis por Tipo (1 a 9)",
       x = "Percentil",
       y = "Valor do Quantil",
       color = "Tipo de Quantile") +
  theme_minimal(base_size = 14)

### Segunda tentativa

# Carregando pacotes
library(ggplot2)
library(tidyr)
library(dplyr)

# Gerando os dados
set.seed(123)
x <- rnorm(1001, 15, 2.5)

# Definindo os percentis
p <- c(0.01, 0.05, 0.1,0.2, 0.3, 0.4, 0.5)

# Função para calcular quantis usando os 9 tipos
quantAll <- function(x, prob, ...) {
  t(vapply(1:9, function(typ) quantile(x, probs = prob, type = typ, ...),
           numeric(length(prob))))
}

# Calculando quantis
qmat <- quantAll(x, p)

# Convertendo para data frame e organizando no formato longo
df <- as.data.frame(qmat)
colnames(df) <- paste0(p * 100, "%")  # Nomeando colunas como "0.1%", "0.5%", etc.
df$Type <- factor(1:9)

# Transformando para formato longo (tidy)
df_long <- pivot_longer(df, -Type, names_to = "Percentile", values_to = "Quantile")

# Plotando com ggplot2
ggplot(df_long, aes(x = Percentile, y = Quantile, color = Type)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  labs(title = "Comparação de Quantis por Tipo (1 a 9)",
       x = "Percentil",
       y = "Valor do Quantil",
       color = "Tipo de Quantile") +
  theme_minimal(base_size = 14)


# Terceira tentativa

# Carregando pacotes
library(ggplot2)
library(tidyr)
library(dplyr)

# Gerando os dados
set.seed(123)
x <- rnorm(1001, 15, 2.5)
y <- rgamma(1001, shape = 2, rate = 1)

# Definindo os percentis e os rótulos
p <- c(1, 5, 10, 20, 30, 40, 50)/100
percent_labels <- paste0(p * 100, "%")

# Função para calcular quantis usando os 9 tipos
quantAll <- function(x, prob, ...) {
  t(vapply(1:9, function(typ) quantile(x, probs = prob, type = typ, ...),
           numeric(length(prob))))
}

# Calculando quantis com os 9 tipos
qmat_x <- quantAll(x, p)
qmat_y <- quantAll(y, p)

# Convertendo para data frame
df_x <- as.data.frame(qmat_x)
colnames(df_x) <- percent_labels
df_x$Type <- factor(1:9)

df_y <- as.data.frame(qmat_y)
colnames(df_y) <- percent_labels
df_y$Type <- factor(1:9)

# Transformando em formato longo
df_x_long <- pivot_longer(df_x, -Type, names_to = "Percentil", values_to = "Quantile")

df_y_long <- pivot_longer(df_y, -Type, names_to = "Percentil", values_to = "Quantile")

# Reordenando os níveis do fator Percentile explicitamente
df_x_long$Percentil <- factor(df_x_long$Percentil, levels = percent_labels)

df_y_long$Percentil <- factor(df_y_long$Percentil, levels = percent_labels)

# Gerando o gráfico
p_x <- ggplot(df_x_long, aes(x = Percentil, y = Quantile, color = Type)) +
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +
  labs(title = "Comparação de Quantis por Tipo (1 a 9) - Normal",
       x = "Percentil",
       y = "Valor do Quantil",
       color = "Tipo de Quantile") +
  theme_minimal(base_size = 14)

p_y <- ggplot(df_y_long, aes(x = Percentil, y = Quantile, color = Type)) +
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +
  labs(title = "Comparação de Quantis por Tipo (1 a 9) - Assimétrica",
       x = "Percentil",
       y = "Valor do Quantil",
       color = "Tipo de Quantile") +
  theme_minimal(base_size = 14)

par(mfrow = c(1,2))
p_x
p_y

