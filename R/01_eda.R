## Regressão Logística
# Dividir o desfecho em 4 grupos seguindo um esquema parecido com o
# estudo anterior, ou seja, serão baseados na presença ou não do
# desfecho em W1 e W4 (e W3 para W4 se possível):
# 1) Ausentes, 2) Incidentes, 3) Persistentes e 4) Remitentes
# O grupo 1 será o grupo controle para o grupo 2 e o grupo 3 será o
# grupo controle para o grupo 4
# Preditores principais e variáveis de ajustes serão do W1
# Limitação: não conseguimos avaliar o TEPT em W1 e W2,
# então essa variável ficaria fora do modelo

# Ler os dados processados gerados no script de limpeza
dados_w1_w4 <- readr::read_rds("data/processed/dados_ucla_w1_w4.rds")

# The scores for each individual question can be added together to
# give you a possible range of scores from 3 to 9.
# Researchers in the past have grouped people who score 3 – 5 as
# "not lonely" and people with the score 6 – 9 as "lonely".

trajetoria_ucla_w1_w4 <- dados_w1_w4 |>
  # Selecionar itens da UCLA
  dplyr::select(dplyr::matches("^ucla_(1|2|3).*")) |>
  # Agrupar de forma a calcular em linhas
  dplyr::rowwise() |>
  # Criar variáveis dos escores no W1 e W4 e trajetórias
  dplyr::transmute(
    # Escore UCLA no W1
    ucla_total_w1 = ucla_1 + ucla_2 + ucla_3,
    # Escore UCLA no W4
    ucla_total_w4 = ucla_1_w4 + ucla_2_w4 + ucla_3_w4
  ) |>
  # Desagrupar
  dplyr::ungroup() |>
  dplyr::mutate(
    # Dicotomizar solidão baseado no ponto de corte do scoring guide
    solidao_w1 = dplyr::case_when(
      ucla_total_w1 %in% 3:5 ~ "Não",
      ucla_total_w1 %in% 6:9 ~ "Sim"
    ),
    solidao_w4 = dplyr::case_when(
      ucla_total_w4 %in% 3:5 ~ "Não",
      ucla_total_w4 %in% 6:9 ~ "Sim"
    ),
    # Criar variável da trajetória (quatro categorias)
    solidao_w1_w4 = dplyr::case_when(
      solidao_w1 == "Não" & solidao_w4 == "Não" ~ "Ausente",
      solidao_w1 == "Não" & solidao_w4 == "Sim" ~ "Incidente",
      solidao_w1 == "Sim" & solidao_w4 == "Não" ~ "Remitente",
      solidao_w1 == "Sim" & solidao_w4 == "Sim" ~ "Persistente"
    )
  )

# Gráfico de barras com trajetórias
trajetoria_ucla_w1_w4 |>
  dplyr::count(solidao_w1_w4) |>
  dplyr::mutate(solidao_w1_w4 = forcats::fct_reorder(solidao_w1_w4, n)) |>
  ggplot2::ggplot(ggplot2::aes(
    x = n,
    y = solidao_w1_w4,
    label = paste0(as.character(n), " (", as.character((n / sum(
      n
    ) * 100) |> format(digits = 4)), "%)")
  )) +
  ggplot2::geom_col(fill = "steelblue") +
  ggplot2::geom_text(
    position = ggplot2::position_stack(vjust = 0.5),
    size = 8,
    family = "Charter"
  ) +
  ggplot2::theme_classic(18, "Charter") +
  ggplot2::labs(
    y = NULL,
    x = "# de sujeitos",
    title = "Na onda 1 e 4, temos dados da UCLA de 944 sujeitos.",
    subtitle = "O gráfico mostra o número de sujeitos com cada trajetória de solidão."
  )
