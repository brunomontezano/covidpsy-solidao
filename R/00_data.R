# Carregar dados e limpar nomes
dados <-
  haven::read_sav("data/raw/alliance_covid-psy_four_waves_weights.sav") |>
  janitor::clean_names()

dados |>
  readr::write_rds("data/processed/dados_brutos.rds")

# Contar linhas válidas para os itens da UCLA em cada wave
dados |>
  dplyr::select(dplyr::starts_with("ucla")) |>
  dplyr::summarise(dplyr::across(dplyr::everything(),
                                 \(x) sum(!is.na(x)))) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "onda",
    values_to = "linhas_validas"
  ) |>
  dplyr::filter(onda %in% c(
    "ucla_1", paste0("ucla_1_w", 1:4)
  )) |>
  dplyr::mutate(
    onda = dplyr::case_match(
      onda,
      "ucla_1" ~ "w1",
      "ucla_1_w2" ~ "w2",
      "ucla_1_w3" ~ "w3",
      "ucla_1_w4" ~ "w4"
    )
  )

# Ok, nós temos 8k na onda 1, uns 1.5k na onda 2,
# uns 1.4k na onda 3 e 0.9k na onda 4

# Quantos sujeitos responderam as quatro ondas?
dados_ucla_all_waves <- dados |>
  dplyr::filter(!is.na(ucla_1),
                !is.na(ucla_1_w2),
                !is.na(ucla_1_w3),
                !is.na(ucla_1_w4))

dados_ucla_all_waves |>
  readr::write_rds(file = "data/processed/dados_ucla_all_waves.rds")

# Ao que parece, 390 sujeitos responderam as quatro ondas

# Quantos responderam a onda 1 e 4?
dados_ucla_w1_w4 <- dados |>
  dplyr::filter(!is.na(ucla_1),
                !is.na(ucla_1_w4))

dados_ucla_w1_w4 |>
  readr::write_rds(file = "data/processed/dados_ucla_w1_w4.rds")

# 944 sujeitos

# E para onda 1, 3 e 4?

dados_ucla_w1_w3_w4 <- dados |>
  dplyr::filter(!is.na(ucla_1),
                !is.na(ucla_1_w3),
                !is.na(ucla_1_w4))

dados_ucla_w1_w3_w4 |>
  readr::write_rds(file = "data/processed/dados_ucla_w1_w3_w4.rds")

# 547 sujeitos
