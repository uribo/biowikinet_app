# canonical_nameが与えられているものが対象
# taxonomic_status=accepet以外も含める
read_gbif_backbone_taxon_tsv <- function(file, .col_select = TRUE, .filter_canonical_name = TRUE) {
  if (.col_select) {
    d <-
      data.table::fread(
        file, 
        select = c(1, 8, 12, 15), 
        quote  = "") |>
      dplyr::select(taxonID,
                    canonical_name = canonicalName,
                    taxon_rank = taxonRank,
                    taxonomic_status = taxonomicStatus)
  } else {
    d <-
      data.table::fread(
        file, 
        quote  = "") |> 
      dplyr::rename(
        canonical_name = canonicalName,
        taxon_rank = taxonRank,
        taxonomic_status = taxonomicStatus) |> 
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(is.character), 
          .fns =  \(x) dplyr::na_if(x, "")))
  }
  d <-
    d |>
    pointblank::row_count_match(7746724L) |> 
    tibble::as_tibble() |> 
    dplyr::mutate(canonical_name = dplyr::na_if(canonical_name, ""))

  if (.filter_canonical_name) {
    d <-
      d |>
      dplyr::filter(!is.na(canonical_name)) |>
      pointblank::row_count_match(6456758L)
  }
  d
}

taxonomy_count <- function(list_count) {
  list_count |>
    purrr::map(
      \(x) dplyr::select(x, !kingdom)
    ) |>
    purrr::map_dbl(nrow) |>
    tibble::as_tibble() |>
    tibble::add_column(taxon_rank = stringr::str_remove(names(list_count), "^([a-z])_")) |>
    dplyr::select(taxon_rank, n = value)
}

taxon_absent_count <- function(df_gbif, df_target) {
  df_gbif |>
    dplyr::left_join(df_target, by = "taxon_rank") |>
    dplyr::mutate(absent_n = n - present_n) |>
    dplyr::mutate(prop = present_n / n * 100) |>
    dplyr::mutate(taxon_rank = forcats::fct_inorder(taxon_rank))
}
