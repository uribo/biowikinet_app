library(targets)
source("R/gbif.R")
controller <-
  crew::crew_controller_local(
    name = "default",
    workers = future::availableCores() - 1L,
    seconds_idle = 5L)
targets::tar_option_set(controller = crew::crew_controller_group(controller),
                        resources = tar_resources(crew = tar_resources_crew(controller = "default")),
                        error = "continue")

list(
  targets::tar_target(
    focus_lang,
    c(`アラビア語`     = "ar",
      `ドイツ語`       = "de",
      `英語`           = "en",
      `スペイン語`     = "es",
      `フランス語`     = "fr",
      `ヒンディー語`   = "hi",
      `インドネシア語`  = "id",
      `日本語`         = "ja",
      `ポルトガル語`    = "pt",
      `ロシア語`       = "ru",
      `中国語`         = "zh"),
    deployment = "main",
  ),
  targets::tar_target(
    df_gbif,
    read_gbif_backbone_taxon_tsv(
    here::here("data-raw/Taxon.tsv"),
    .col_select = FALSE,
    .filter_canonical_name = TRUE) |>
      dplyr::rename(gbif_id = taxonID) |>
      dplyr::select(
        gbif_id,
        kingdom, phylum, class, order, family, genus),
    format = "rds"),
    tar_target(
      nodes,
      jsonlite::fromJSON(here::here("data-raw/nodes.json")) |>
        dplyr::filter(lang == focus_lang) |>
        tibble::as_tibble(),
      pattern = map(focus_lang),
      deployment = "worker"
    ),
    tar_target(
      edges,
      jsonlite::fromJSON(here::here("data-raw/edges.json")) |>
        dplyr::filter(stringr::str_detect(from, paste0("^", focus_lang))) |>
        tibble::as_tibble() |>
        tidyr::unnest(cols = c(to, to_label)),
      pattern = map(focus_lang),
      deployment = "worker"
    )
)
