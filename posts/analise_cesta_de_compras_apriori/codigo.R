# =============================================================================
# Análise de Cesta de Compras com Apriori no R
# Post: "Quem Leva Pão Também Leva Manteiga?" - Blog Meu Nome é Job
# Prof. Jobenil Júnior
# =============================================================================

library(arules)
library(arulesViz)
library(dplyr)
library(ggplot2)
library(gt)

# ---- 1. Simulação de 5.000 cupons do Supermercado Bom Preço -----------------
set.seed(2026)

n <- 5000
base <- c("pão" = 0.25, "leite" = 0.40, "café" = 0.20, "açúcar" = 0.12,
          "arroz" = 0.25, "feijão" = 0.22, "ovos" = 0.18, "banana" = 0.20,
          "tomate" = 0.18, "queijo" = 0.10, "salgadinho" = 0.12,
          "refrigerante" = 0.15, "fralda" = 0.06, "carvão" = 0.05,
          "carne" = 0.15, "cerveja" = 0.14, "detergente" = 0.10,
          "papel higiênico" = 0.12, "lenço umedecido" = 0.02, "mamadeira" = 0.01)

cupons <- lapply(seq_len(n), function(i) {
  itens <- names(base)[runif(length(base)) < base]
  # associações plantadas (o "gabarito" que o Apriori deve reencontrar)
  if ("pão" %in% itens        && runif(1) < 0.55) itens <- c(itens, "manteiga")
  if (!"pão" %in% itens       && runif(1) < 0.02) itens <- c(itens, "manteiga")
  if ("fralda" %in% itens     && runif(1) < 0.70) itens <- c(itens, "lenço umedecido")
  if ("fralda" %in% itens     && runif(1) < 0.40) itens <- c(itens, "mamadeira")
  if ("fralda" %in% itens     && runif(1) < 0.10) itens <- c(itens, "cerveja")  # a "lenda"
  if ("salgadinho" %in% itens && runif(1) < 0.45) itens <- c(itens, "refrigerante")
  if ("carvão" %in% itens     && runif(1) < 0.75) itens <- c(itens, "carne")
  if ("carvão" %in% itens     && runif(1) < 0.65) itens <- c(itens, "cerveja")
  if (length(itens) == 0) itens <- sample(names(base), 1)  # nenhum cupom vazio
  unique(itens)
})

trans <- as(cupons, "transactions")
summary(trans)

# ---- 2. Itens mais vendidos --------------------------------------------------
freq <- tibble(item = itemLabels(trans), suporte = itemFrequency(trans)) |>
  arrange(desc(suporte))

ggplot(freq, aes(x = suporte, y = reorder(item, suporte), fill = item == "leite")) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("grey70", "#1f4e79")) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Suporte (% dos cupons)", y = NULL,
       title = "O que mais sai no Supermercado Bom Preço") +
  theme_minimal(base_size = 14)

# ---- 3. Apriori -------------------------------------------------------------
regras <- apriori(trans,
                  parameter = list(supp = 0.01, conf = 0.30, minlen = 2),
                  control = list(verbose = FALSE))
length(regras)
regras <- regras[!is.redundant(regras)]
length(regras)

# ---- 4. Confiança x lift ----------------------------------------------------
inspect(head(sort(regras, by = "lift"), 10))
pares <- subset(regras, size(regras) == 2)
inspect(head(sort(pares, by = "confidence"), 10))
inspect(subset(regras, rhs %in% "leite"))

r_baixo <- apriori(trans, parameter = list(supp = 0.003, conf = 0.05, minlen = 2),
                   control = list(verbose = FALSE))
inspect(subset(r_baixo, (lhs %oin% "fralda" & rhs %in% "cerveja") |
                        (lhs %oin% "leite"  & rhs %in% "pão") |
                        (lhs %oin% "pão"    & rhs %in% "manteiga")))

# ---- 5. Tabela das regras de 2 itens com lift > 1,2 -------------------------
DATAFRAME(sort(subset(pares, lift > 1.2), by = "lift")) |>
  select(LHS, RHS, support, confidence, lift, count) |>
  gt() |>
  fmt_percent(columns = c(support, confidence), decimals = 1) |>
  fmt_number(columns = lift, decimals = 2)

# ---- 6. Mapa suporte x confiança --------------------------------------------
ggplot(DATAFRAME(regras), aes(support, confidence, colour = lift)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_colour_gradient(low = "grey80", high = "#c0392b", trans = "log10",
                        breaks = c(1, 2, 5, 10)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Suporte", y = "Confiança", colour = "Lift",
       title = "Nem toda regra confiante é interessante") +
  theme_minimal(base_size = 14)

# ---- 7. Grafo de regras -----------------------------------------------------
grafo <- subset(regras, lift > 1.2 &
                  (size(regras) == 2 |
                   lhs %ain% c("carne", "cerveja") |
                   lhs %ain% c("carvão", "carne")))
grafo <- sort(grafo[!is.redundant(grafo, measure = "lift")], by = "lift")
length(grafo)

set.seed(2026)
plot(grafo, method = "graph", engine = "ggplot2",
     control = list(
       layout = "stress",
       edges  = ggraph::geom_edge_link(
         colour = "grey55", alpha = 0.7,
         start_cap = ggraph::circle(4, "mm"), end_cap = ggraph::circle(4, "mm"),
         arrow = grid::arrow(length = grid::unit(2.5, "mm"), type = "closed")),
       colors = c("#c0392b", "#f6e3a1"))) +
  labs(title = "As 4 'ilhas' do carrinho no Supermercado Bom Preço",
       size = "Suporte", colour = "Lift") +
  theme(plot.title = element_text(size = 16, face = "bold"))

# versão interativa (abre no Viewer / navegador)
plot(grafo, method = "graph", engine = "htmlwidget")
