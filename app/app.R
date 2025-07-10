library(shiny)
library(bslib)
library(visNetwork)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(shiny.i18n)
library(DT)
library(dplyr)

targets::tar_load(names = c(df_gbif, edges, nodes))

# 翻訳機能の初期化 (明確にjsonファイルを指定する)
i18n <- shiny.i18n::Translator$new(translation_json_path = here::here("app/translations/translation_ja.json"))
i18n$set_translation_language("en")

ui <- bslib::page_fluid(
  fillable = TRUE,
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  shinyjs::useShinyjs(),
  shiny.i18n::usei18n(i18n),
  tags$head(
    tags$style(HTML("
      html, body, .container-fluid {
        height: 100%;
        padding: 0;
        margin: 0;
      }
      .navbar {
        box-shadow: 0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06);
        border-bottom: 1px solid #dee2e6;
      }
      .card-header {
        font-weight: 600;
      }
      .irs-bar, .irs-bar-edge, .irs-single, .irs-grid-pol {
        background: #18bc9c;
        border-color: #18bc9c;
      }
      .bslib-layout-columns {
        height: calc(100vh - 58px - 50px);
        padding: 0.5rem;
        gap: 0.5rem;
      }
      .footer {
        position: fixed;
        bottom: 0;
        left: 0;
        right: 0;
        height: 50px;
        background-color: #2c3e50;
        color: white;
        display: flex;
        align-items: center;
        justify-content: center;
        border-top: 1px solid #34495e;
        z-index: 1000;
        font-size: 0.9rem;
      }
      .footer a {
        color: #18bc9c;
        text-decoration: none;
        margin: 0 10px;
      }
      .footer a:hover {
        color: #1dd9a8;
        text-decoration: underline;
      }
      .footer-divider {
        margin: 0 15px;
        color: #7f8c8d;
      }
      .metric-card {
        background-color: #f77e7e;
        color: white;
        border-radius: 0.5rem;
        padding: 0.5rem;
        text-align: center;
        margin-bottom: 0.5rem;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .metric-icon {
        font-size: 1.2rem;
        margin-bottom: 0.2rem;
      }
      .metric-value {
        font-size: 1.2rem;
        font-weight: 700;
        margin: 0.2rem 0;
      }
      .metric-label {
        font-size: 0.8rem;
        opacity: 0.9;
      }
      .node-header {
        margin-bottom: 1.5rem;
      }
      .node-taxonomy {
        display: inline-block;
        background-color: #18bc9c;
        color: white;
        padding: 0.25rem 0.75rem;
        border-radius: 1rem;
        font-size: 0.75rem;
        font-weight: 600;
        text-transform: uppercase;
        margin-bottom: 0.75rem;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .node-taxonomy.kingdom {
        background-color: #8e44ad;
      }
      .node-taxonomy.phylum {
        background-color: #3498db;
      }
      .node-taxonomy.class {
        background-color: #2ecc71;
      }
      .node-taxonomy.order {
        background-color: #f39c12;
      }
      .node-taxonomy.family {
        background-color: #e74c3c;
      }
      .node-taxonomy.genus {
        background-color: #e67e22;
      }
      .node-taxonomy.species {
        background-color: #1abc9c;
      }
      .node-taxonomy.other {
        background-color: #95a5a6;
      }
      .node-description {
        color: #666;
        font-size: 0.9rem;
        margin-top: 1rem;
        line-height: 1.5;
      }
      .node-image-container {
        width: 120px;
        height: 120px;
        border-radius: 0.5rem;
        overflow: hidden;
        background-color: #f8f9fa;
        display: flex;
        align-items: center;
        justify-content: center;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-right: 1.5rem;
        flex-shrink: 0;
      }
      .node-image {
        width: 100%;
        height: 100%;
        object-fit: cover;
      }
      .node-image-placeholder {
        font-size: 3rem;
        color: #dee2e6;
      }
      .search-results {
        position: absolute;
        top: 100%;
        left: 0;
        right: 0;
        z-index: 1000;
        background: white;
        border: 1px solid #dee2e6;
        border-radius: 0.25rem;
        max-height: 300px;
        overflow-y: auto;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .search-result-item {
        padding: 0.5rem;
        cursor: pointer;
        border-bottom: 1px solid #f0f0f0;
      }
      .search-result-item:hover {
        background-color: #f8f9fa;
      }
      .search-result-item:last-child {
        border-bottom: none;
      }
    "))
  ),

  tags$header(
    class = "navbar navbar-expand-lg navbar-light bg-white",
    div(
      class = "container-fluid px-3",
      a(class = "navbar-brand d-flex align-items-center", href = "#",
        icon("link", class="text-primary me-2"),
        tags$h1(i18n$t("BioWikiNet"), class="h5 mb-0 fw-bold"),
        tags$span(i18n$t("Explorer"), class="text-muted fw-normal ms-2")
      ),
      div(
        class="d-flex align-items-center",
        selectInput("ui_language",
                    label = NULL,
                    choices = c(
                      "English" = "en",
                      "日本語" = "ja",
                      "العربية" = "ar",
                      "Deutsch" = "de",
                      "Español" = "es",
                      "Français" = "fr",
                      "हिन्दी" = "hi",
                      "Bahasa Indonesia" = "id",
                      "Português" = "pt",
                      "中文" = "zh",
                      "Русский" = "ru"),
                    selected = "en",
                    width = "150px"),
        shiny::actionButton(
          "help_btn",
          label = NULL,
          icon = icon("question-circle"),
          class = "btn-light ms-2")
      )
    )
  ),

  bslib::layout_columns(
    col_widths = c(3, 5, 4),

    bslib::card(
      class = "h-100 shadow-sm",
      bslib::card_header(i18n$t("Search & Filter")),
      bslib::card_body(
        selectInput("language_version",
                    i18n$t("Language Edition"),
                    choices = list(
                      "العربية" = "ar",
                      "Deutsch (German)" = "de",
                      "English" = "en",
                      "Español (Spanish)" = "es",
                      "Français (French)" = "fr",
                      "हिन्दी" = "hi",
                      "Bahasa Indonesia" = "id",
                      "日本語 (Japanese)" = "ja",
                      "中文 (Chinese)" = "zh",
                      "Português (Portuguese)" = "pt",
                      "Русский (Russian)" = "ru"
                    ),
                    selected = "en",
                    multiple = TRUE),

        div(
          class = "position-relative",
          uiOutput("search_input_ui"),
          uiOutput("search_results")
        ),

        tags$hr(),

        div(class = "alert alert-info mb-3",
            icon("calendar-alt", class = "me-2"),
            tags$strong(i18n$t("Last updated: ")),
            format(as.Date("2024-12-31"), "%Y-%m-%d-"),
            tags$br(),
            tags$small(i18n$t("The information displayed here is based on the version of this date."))
        ),

        tags$h6(i18n$t("Selected Taxa"), class = "mb-2"),
        uiOutput("selected_taxa_list"),

        tags$hr(),

        actionButton("update_network",
                     i18n$t("Update Network"),
                     class = "btn-primary w-100 mt-3",
                     icon = icon("refresh"))
      )
    ),

    card(
      full_screen = TRUE,
      class = "h-100 shadow-sm",
      card_header(
        class="d-flex justify-content-between align-items-center",
        tags$div(
          id = "network_title",
          class = "h6 mb-0",
          i18n$t("Network Visualization")
        )
      ),
      card_body(
        padding = 0,
        visNetworkOutput("network_view", height = "100%")
      )
    ),

    card(
      class = "h-100 shadow-sm",
      card_body(
        padding=0,
        tabsetPanel(
          id = "details_tabs",
          tabPanel(i18n$t("Details"), value = "details",
                   div(class="p-3",
                       uiOutput("selected_node_info")
                   )
          ),
          tabPanel(i18n$t("Statistics"), value = "metrics",
                   div(class="p-3",
                       uiOutput("network_stats")
                   )
          ),
          tabPanel(i18n$t("GBIF Information"), value = "gbif",
                   div(class="p-3",
                       uiOutput("gbif_summary"),
                       DT::dataTableOutput("gbif_table")
                   )
          )
        )
      )
    )
  ),

  tags$footer(
    class = "footer",
    div(
      tags$span("© 2025 BioWikiNet"),
      tags$span(class = "footer-divider", "|"),
      tags$a(href = "https://github.com/uribo/biowikinet_app", target = "_blank",
             icon("github"), " GitHub"),
      tags$span(class = "footer-divider", "|"),
      tags$a(href = "#", onclick = "alert('BioWikiNet v1.0.0')",
             icon("info-circle"), " About"),
      tags$span(class = "footer-divider", "|"),
      tags$a(href = "https://x.com/u_ribo",
             icon("x-twitter"), " Contact"),
      tags$span(class = "footer-divider", "|"),
      tags$span(icon("database"), " Data: "),
      tags$a(href = "https://doi.org/10.6084/m9.figshare.29431577.v2", 
             target = "_blank",
             "Uryu (2025)")
    )
  )
)

server <- function(input, output, session) {

  selected_taxa <- shiny::reactiveVal(list())
  selected_node_id <- shiny::reactiveVal(NULL)

  # 言語切り替えの処理
  shiny::observeEvent(input$ui_language, {
    shiny.i18n::update_lang(input$ui_language)

    # 設定をリセット: Language Editionを現在のUI言語のみに設定
    shiny::updateSelectInput(session, "language_version", selected = input$ui_language)

    # 選択された分類群をクリア
    selected_taxa(list())

    # 選択されたノードIDもクリア
    selected_node_id(NULL)

    # 検索ボックスをクリア
    shiny::updateTextInput(session, "species_search", value = "")
  })

  # 検索入力UIを動的に生成
  output$search_input_ui <- shiny::renderUI({
    shiny::textInput("species_search",
              label = i18n$t("Search taxon"),
              placeholder = i18n$t("e.g. Human, American bison, Sequoia sempervirens ..."))
  })

  .filter_nodes <- function(nodes, langs) {
    nodes |>
      dplyr::filter(
        stringr::str_detect(id, paste0("^(", paste(langs, collapse = "|"), "):"))
      ) |>
      dplyr::filter(!is.na(label))
  }

  filtered_nodes <- shiny::reactive({
    req(input$language_version)
    .filter_nodes(nodes, input$language_version)
  })

  output$search_results <- shiny::renderUI({
    req(input$species_search)

    if (nchar(input$species_search) < 2) {
      return(NULL)
    }

    search_term <- input$species_search

    matching_nodes <-
      filtered_nodes() |>
      dplyr::filter(
        stringr::str_detect(label,
          stringr::str_c("^",
          stringr::regex(search_term, ignore_case = TRUE),
          "$", sep = ""
        )
          )
      )

    if (nrow(matching_nodes) == 0) {
      return(
        div(class = "search-results",
            div(class = "search-result-item text-muted",
                i18n$t("No search results"))
        )
      )
    }

    div(
      class = "search-results",
      lapply(seq_len(nrow(matching_nodes)), function(i) {
        node <- matching_nodes[i, ]
        div(
          class = "search-result-item",
          onclick = sprintf("Shiny.setInputValue('add_taxon', '%s', {priority: 'event'})", node$id),
          tags$strong(node$label),
          tags$small(class = "text-muted ms-2",
                     sprintf("(%s%s)",
                             stringr::str_extract(node$id, "^[^:]+"),
                             if(!is.na(node$core_index) && node$core_index > 0)
                               paste0(", Core: ", round(node$core_index, 1))
                             else ""))
        )
      })
    )
  })

  shiny::observeEvent(input$add_taxon, {
    node_info <- nodes |>
      dplyr::filter(id == input$add_taxon) |>
      dplyr::slice_head(n = 1)

    if (nrow(node_info) > 0) {
      current_taxa <- selected_taxa()

      if (!input$add_taxon %in% names(current_taxa)) {
        current_taxa[[input$add_taxon]] <- list(
          id = node_info$id,
          label = node_info$label
        )
        selected_taxa(current_taxa)
      }

      # 選択したノードの詳細情報を表示
      selected_node_id(input$add_taxon)

      # ネットワーク内のノードを選択状態にする
      # 少し遅延を入れてネットワークが更新されるのを待つ
      shinyjs::delay(500, {
        visNetwork::visNetworkProxy("network_view") |>
          visNetwork::visSelectNodes(id = input$add_taxon)
      })
    }

    shiny::updateTextInput(session, "species_search", value = "")
  })

  output$selected_taxa_list <- shiny::renderUI({
    taxa <- selected_taxa()

    if (length(taxa) == 0) {
      return(
        div(class = "text-muted small", i18n$t("No taxa selected"))
      )
    }

    div(
      lapply(names(taxa), function(taxon_id) {
        div(
          class = "d-flex justify-content-between align-items-center mb-2 p-2 bg-light rounded",
          tags$span(taxa[[taxon_id]]$label, class = "small"),
          actionButton(
            inputId = paste0("remove_", gsub(":", "_", taxon_id)),
            label = NULL,
            icon = icon("times"),
            class = "btn-sm btn-light",
            onclick = sprintf("Shiny.setInputValue('remove_taxon', '%s', {priority: 'event'})", taxon_id)
          )
        )
      })
    )
  })

  shiny::observeEvent(input$remove_taxon, {
    current_taxa <- selected_taxa()
    current_taxa[[input$remove_taxon]] <- NULL
    selected_taxa(current_taxa)
  })

  network_data <- shiny::reactive({
    taxa <- selected_taxa()

    if (length(taxa) == 0) {
      return(list(nodes = data.frame(), edges = data.frame()))
    }

    target_ids <- names(taxa)

    selected_edges <-
      edges |>
      dplyr::filter(
        from %in% target_ids | to %in% target_ids,
        stringr::str_detect(from, paste0("^(", paste(input$language_version, collapse = "|"), "):")),
        stringr::str_detect(to, paste0("^(", paste(input$language_version, collapse = "|"), "):"))
      )

    all_node_ids <-
      unique(c(selected_edges$from, selected_edges$to, target_ids))

    selected_nodes <- nodes |>
      dplyr::filter(id %in% all_node_ids)

    selected_nodes <- selected_nodes |>
      dplyr::mutate(
        color = dplyr::case_when(
          id %in% target_ids ~ "#e74c3c",
          TRUE ~ "#3498db"
        ),
        size = dplyr::case_when(
          id %in% target_ids ~ 30,
          TRUE ~ 20
        )
      )

    list(nodes = selected_nodes, edges = selected_edges)
  })

  output$network_view <- visNetwork::renderVisNetwork({
    data <- network_data()

    if (nrow(data$nodes) == 0) {
      return(
        visNetwork(data.frame(id = 1, label = i18n$t("Please select taxa")),
                   data.frame()) |>
          visOptions(manipulation = FALSE)
      )
    }

    visNetwork(data$nodes, data$edges) |>
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;')
      ) |>
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -50,
          centralGravity = 0.01,
          springLength = 100,
          springConstant = 0.08
        )
      ) |>
      visEvents(select = "function(nodes) {
        Shiny.setInputValue('selected_node', nodes.nodes[0]);
      }")
  })

  shiny::observeEvent(input$selected_node, {
    selected_node_id(input$selected_node)
  })

  shiny::observeEvent(input$update_network, {
    visNetworkProxy("network_view") |>
      visRedraw()
  })

  output$selected_node_info <- shiny::renderUI({
    req(selected_node_id())

    node_info <-
      nodes |>
      dplyr::filter(id == selected_node_id()) |>
      dplyr::slice_head(n = 1)

    if (nrow(node_info) == 0) {
      return(div(i18n$t("Node information not found")))
    }

    node_info <-
      node_info |>
      tidyr::unnest(cols = gbif) |>
      dplyr::left_join(df_gbif,
                        by = dplyr::join_by(gbif_id))

    # 実際の値を取得（NAの場合はデフォルト値を使用）
    pageviews <- if(!is.na(node_info$pageviews)) node_info$pageviews else 0
    edit_count <- if(!is.na(node_info$edit_count)) node_info$edit_count else 0
    core_index <- if(!is.na(node_info$core_index)) node_info$core_index else 0
    sci_value <- if(!is.na(node_info$sci)) node_info$sci else 0

    div(
      div(class = "d-flex mb-3",
        div(class = "node-image-container",
          # プレースホルダー画像 - 実際の画像URLがある場合はここに設定
          if (FALSE) {  # 画像URLが利用可能な場合の条件
            tags$img(src = "placeholder.png", class = "node-image", alt = node_info$label)
          } else {
            icon("image", class = "node-image-placeholder")
          }
        ),
        div(class = "flex-grow-1",
          div(class = "node-header",
            if(!is.na(node_info$taxon_rank[1]) && nchar(node_info$taxon_rank[1]) > 0) {
              rank_class <- if(node_info$taxon_rank[1] %in% c("kingdom", "phylum", "class", "order", "family", "genus", "species")) {
                paste("node-taxonomy", node_info$taxon_rank[1])
              } else {
                "node-taxonomy other"
              }
              div(class = rank_class, node_info$taxon_rank[1])
            },
            tags$h4(node_info$label, class = "mb-1"),
            tags$div(class = "text-muted",
                     if(!is.na(node_info$canonical_name[1]) && nchar(node_info$canonical_name[1]) > 0) {
                       tags$em(node_info$canonical_name[1])
                     } else {
                       node_info$label
                     },
                     if(!is.na(node_info$taxonomic_status[1]) && nchar(node_info$taxonomic_status[1]) > 0) {
                       tags$span(class = "ms-2", paste0("(", node_info$taxonomic_status[1], ")"))
                     })
          )
        )
      ),

      # 分類階層の表示
      if(!is.na(node_info$kingdom) || !is.na(node_info$phylum) || !is.na(node_info$class) ||
         !is.na(node_info$order) || !is.na(node_info$family) || !is.na(node_info$genus)) {
        div(class = "mb-3",
          tags$h6(i18n$t("Taxonomic Rank"), class = "text-muted mb-2"),
          tags$div(
            class = "text-primary",
            paste(
              c(
                if(!is.na(node_info$kingdom)) node_info$kingdom else NULL,
                if(!is.na(node_info$phylum)) node_info$phylum else NULL,
                if(!is.na(node_info$class)) node_info$class else NULL,
                if(!is.na(node_info$order)) node_info$order else NULL,
                if(!is.na(node_info$family)) node_info$family else NULL,
                if(!is.na(node_info$genus)) node_info$genus else NULL
              ),
              collapse = " > "
            )
          )
        )
      },

      div(class = "node-description",
        if(!is.na(node_info$abstract) && nchar(node_info$abstract) > 0) {
          div(
          if(nchar(node_info$abstract) > 200) {
            tags$span(
              substr(node_info$abstract, 1, 200),
                "..."
            )
          } else {
            node_info$abstract
            },
            tags$br(),
            if(!is.na(node_info$url) && nchar(node_info$url) > 0) {
              tags$a(href = node_info$url, target = "_blank", class = "text-primary ms-1", i18n$t("Read more"))
          }
          )
        } else {
          div(
            i18n$t("Description for this taxon is not available."),
            tags$br(),
            if(!is.na(node_info$url) && nchar(node_info$url) > 0) {
              tags$a(href = node_info$url, target = "_blank", class = "text-primary ms-1", i18n$t("Read more"))
            }
          )
        }
      ),

      tags$hr(),

      div(class = "row g-3 mt-3",
        div(class = "col-6",
          div(class = "metric-card",
            div(class = "metric-icon", icon("eye")),
            div(class = "metric-label", "Pageviews"),
            div(class = "metric-value",
                if (pageviews == 0) {
                  "N/A"
                } else if (pageviews > 1000000) {
                  paste0(round(pageviews/1000000, 1), "M")
                } else if (pageviews > 1000) {
                  paste0(round(pageviews/1000), "K")
                } else {
                  format(pageviews, big.mark = ",")
                })
          )
        ),
        div(class = "col-6",
          div(class = "metric-card",
            div(class = "metric-icon", icon("edit")),
            div(class = "metric-label", "Edit Count"),
            div(class = "metric-value",
                if (edit_count == 0) "N/A" else format(edit_count, big.mark = ","))
          )
        ),
        div(class = "col-6",
          div(class = "metric-card",
            div(class = "metric-icon", icon("star")),
            div(class = "metric-label", "Core Index"),
            div(class = "metric-value",
                if (core_index == 0) "N/A" else round(core_index, 1))
          )
        ),
        div(class = "col-6",
          div(class = "metric-card",
            div(class = "metric-icon", icon("sitemap")),
            div(class = "metric-label", "SCI"),
            div(class = "metric-value",
                if (sci_value == 0) "N/A" else sprintf("%.2f", sci_value))
          )
        ),
        div(class = "col-6",
          div(class = "metric-card",
            div(class = "metric-icon", icon("file-alt")),
            div(class = "metric-label", "Page Length"),
            div(class = "metric-value",
                if (!is.na(node_info$page_length_bytes) && node_info$page_length_bytes > 0) {
                  if (node_info$page_length_bytes > 1024*1024) {
                    paste0(round(node_info$page_length_bytes/(1024*1024), 1), " MB")
                  } else if (node_info$page_length_bytes > 1024) {
                    paste0(round(node_info$page_length_bytes/1024), " KB")
                  } else {
                    paste0(node_info$page_length_bytes, " B")
                  }
                } else {
                  "N/A"
                })
          )
        ),
        div(class = "col-6",
          div(class = "metric-card",
            div(class = "metric-icon", icon("chart-line")),
            div(class = "metric-label", "Excess Focus"),
            div(class = "metric-value",
                if (!is.na(node_info$excess_focus) && node_info$excess_focus != 0) {
                  sprintf("%.2f", node_info$excess_focus)
                } else {
                  "N/A"
                })
          )
        ),
        div(class = "col-6",
          div(class = "metric-card",
            div(class = "metric-icon", icon("calendar")),
            div(class = "metric-label", "Last Modified"),
            div(class = "metric-value",
                if (!is.na(node_info$date_modified) && nchar(node_info$date_modified) > 0) {
                  format(as.Date(node_info$date_modified), "%Y-%m-%d")
                } else {
                  "N/A"
                })
          )
        )
      )
    )
  })

  output$network_stats <- shiny::renderUI({
    data <- network_data()

    div(
      tags$h5(i18n$t("Network Statistics"), class = "mb-3"),
      tags$table(class = "table table-sm",
        tags$tbody(
          tags$tr(
            tags$td(i18n$t("Node count:")),
            tags$td(nrow(data$nodes))
          ),
          tags$tr(
            tags$td(i18n$t("Edge count:")),
            tags$td(nrow(data$edges))
          ),
          tags$tr(
            tags$td(i18n$t("Selected taxa:")),
            tags$td(length(selected_taxa()))
          ),
          tags$tr(
            tags$td(i18n$t("Display languages:")),
            tags$td(length(input$language_version))
          )
        )
      )
    )
  })

  # GBIF情報のサマリー表示
  output$gbif_summary <- shiny::renderUI({
    req(selected_node_id())

    node_info <- nodes |>
      dplyr::filter(id == selected_node_id()) |>
      dplyr::slice_head(n = 1)

    if (nrow(node_info) == 0) {
      return(div(i18n$t("Node information not found")))
    }

    # GBIF情報を取得
    gbif_data <- node_info$gbif[[1]]

    if (is.null(gbif_data)) {
      return(
        div(
          class = "alert alert-info text-center",
          icon("info-circle", class = "me-2"),
          i18n$t("No GBIF information available")
        )
      )
    }

    # データの形式を確認
    record_count <- nrow(gbif_data)

    div(
      tags$h5(i18n$t("GBIF Information"), class = "mb-3"),
      div(
        class = "alert alert-success",
        icon("check-circle", class = "me-2"),
        sprintf("この分類群には %d 件のGBIF情報があります", record_count)
      )
    )
  })

  # GBIF情報のテーブル表示
  output$gbif_table <-
    DT::renderDataTable({
    req(selected_node_id())

    node_info <-
      nodes |>
      dplyr::filter(id == selected_node_id()) |>
      dplyr::slice_head(n = 1)

    if (nrow(node_info) == 0) {
      return(data.frame())
    }

    # GBIF情報を取得
    gbif_data <- node_info$gbif[[1]]

    if (is.null(gbif_data)) {
      return(data.frame())
    }

    # データフレームでない場合やレコードがない場合の処理
    if (!is.data.frame(gbif_data) && !is.list(gbif_data)) {
      return(data.frame())
    }

    # データフレームの列名を日本語と英語で表示するためのマッピング
    col_mapping <- list(
      "gbif_id" = "GBIF ID",
      "canonical_name" = i18n$t("Canonical Name"),
      "taxon_rank" = i18n$t("Taxonomic Rank"),
      "taxonomic_status" = i18n$t("Taxonomic Status"),
      "kingdom" = "Kingdom",
      "phylum" = "Phylum",
      "class" = "Class",
      "order" = "Order",
      "family" = "Family",
      "genus" = "Genus",
      "species" = "Species"
    )

    # GBIF データの構造を確認してデータフレームに変換
    if (is.data.frame(gbif_data)) {
      # 既にデータフレームの場合
      display_data <- gbif_data |>
        dplyr::select(dplyr::any_of(names(col_mapping)))
    } else {
      # リスト形式の場合、データフレームに変換
      # 利用可能な列のみを抽出してデータフレームを作成
      available_cols <- names(col_mapping)[names(col_mapping) %in% names(gbif_data)]
      if (length(available_cols) > 0) {
        # 各列を個別に抽出してデータフレームを作成
        display_list <- list()
        for (col in available_cols) {
          if (!is.null(gbif_data[[col]]) && !all(is.na(gbif_data[[col]]))) {
            display_list[[col_mapping[[col]]]] <- gbif_data[[col]]
          }
        }

        if (length(display_list) > 0) {
          # 長さを統一（最長の要素に合わせる）
          max_length <- max(sapply(display_list, length))
          display_list <- lapply(display_list, function(x) {
            if (length(x) < max_length) {
              c(x, rep(NA, max_length - length(x)))
            } else {
              x[1:max_length]
            }
          })
          display_data <- data.frame(display_list, stringsAsFactors = FALSE)
        } else {
          display_data <- data.frame()
        }
      } else {
        display_data <- data.frame()
      }
    }

    # GBIF IDがある場合はリンクを作成
    if ("gbif_id" %in% names(display_data)) {
      display_data$`GBIF Link` <- sprintf(
        '<a href="https://www.gbif.org/species/%s" target="_blank">View on GBIF</a>',
        display_data$gbif_id
      )
    }

    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tp',
        language = list(
          search = i18n$t("Search"),
          info = "_START_ から _END_ まで（全 _TOTAL_ 件）",
          paginate = list(
            previous = "Previous",
            `next` = "Next"
          )
        )
      ),
      escape = FALSE,
      rownames = FALSE
    )
  })
}

shiny::shinyApp(ui, server)
