library(targets)
source("R/gbif.R")

list(
  targets::tar_target(
    df_gbif,
    read_gbif_backbone_taxon_tsv(
    here::here("data-raw/Taxon.tsv"),
    .col_select = FALSE,
    .filter_canonical_name = TRUE) |>
      dplyr::rename(gbif_id = taxonID) |>
      dplyr::select(
        gbif_id,
        kingdom, phylum, class, order, family, genus)
    ),
    tar_target(
      nodes,
      jsonlite::fromJSON(here::here("data-raw/nodes.json")) |>
        tibble::as_tibble()
    ),
    tar_target(
      edges,
      jsonlite::fromJSON(here::here("data-raw/edges.json")) |>
        tibble::as_tibble() |> 
        tidyr::unnest(cols = c(to, to_label))
    )
)


 
