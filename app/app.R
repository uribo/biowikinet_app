library(shiny)
library(bslib)
library(visNetwork)
library(plotly)
library(shinyjs)
library(shinyWidgets)

###############
# input <- NULL
# input$language_version <- "ja"
# input$species_search <- "アカガシ"
# d <- .filter_nodes(nodes, lang = input$language_version)
# d |> 
#   dplyr::filter(
#         stringr::str_detect(label, stringr::regex(input$species_search, ignore_case = TRUE)))
###############

nodes <-
  readr::read_rds(here::here("data-raw/nodes.rds"))

edges <-
  readr::read_rds(here::here("data-raw/edges.rds"))

ui <- page_fluid(
  fillable = TRUE,
  theme = bs_theme(version = 5, bootswatch = "minty"),
  shinyjs::useShinyjs(),
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
        height: calc(100vh - 58px);
        padding: 0.5rem;
        gap: 0.5rem;
      }
      .metric-card {
        background-color: #f77e7e;
        color: white;
        border-radius: 0.5rem;
        padding: 1.5rem;
        text-align: center;
        margin-bottom: 1rem;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .metric-icon {
        font-size: 2rem;
        margin-bottom: 0.5rem;
      }
      .metric-value {
        font-size: 2rem;
        font-weight: 700;
        margin: 0.5rem 0;
      }
      .metric-label {
        font-size: 1rem;
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
        tags$h1("BioWikiNet", class="h5 mb-0 fw-bold"),
        tags$span("Explorer", class="text-muted fw-normal ms-2")
      ),
      div(
        class="d-flex align-items-center",
        actionButton("help_btn", label=NULL, icon=icon("question-circle"), class="btn-light ms-2")
      )
    )
  ),

  layout_columns(
    col_widths = c(3, 5, 4),

    card(
      class = "h-100 shadow-sm",
      card_header("検索 & フィルター"),
      card_body(
        selectInput("language_version", 
                    "言語版 (Language Edition)",
                    choices = list(
                      "English" = "en",
                      "日本語 (Japanese)" = "ja", 
                      "Español (Spanish)" = "es",
                      "Français (French)" = "fr",
                      "Deutsch (German)" = "de",
                      "中文 (Chinese)" = "zh",
                      "Português (Portuguese)" = "pt",
                      "Русский (Russian)" = "ru"
                    ),
                    selected = "ja",
                    multiple = TRUE),
        
        div(
          class = "position-relative",
          textInput("species_search", 
                    label = "分類群名を検索 (Search taxon)",
                    placeholder = "例: アカガシ、ブナ、ニジマス..."),
          uiOutput("search_results")
        ),
        
        tags$hr(),
        
        tags$h6("選択された分類群", class = "mb-2"),
        uiOutput("selected_taxa_list"),
        
        tags$hr(),
        
        sliderInput("core_index", 
                    "Core Index (最小値)", 
                    min = 0, max = 100, value = 25),
        
        sliderInput("sci", 
                    "SCI (構造的中心性)", 
                    min = 0, max = 1, value = 0.5, step = 0.01),
        
        actionButton("update_network", 
                     "ネットワークを更新", 
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
          "ネットワーク可視化"
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
          tabPanel("詳細情報", value = "details",
                   div(class="p-3",
                       uiOutput("selected_node_info")
                   )
          ),
          tabPanel("統計", value = "metrics",
                   div(class="p-3",
                       uiOutput("network_stats")
                   )
          ),
          tabPanel("言語比較", value = "comparison",
                   div(class="p-3",
                       plotlyOutput("lang_comparison_plot", height = "400px")
                   )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_taxa <- reactiveVal(list())
 
  .filter_nodes <- function(nodes, langs) {
    nodes |> 
      dplyr::filter(
        stringr::str_detect(id, paste0("^(", paste(langs, collapse = "|"), "):"))
      )
  }

  filtered_nodes <- reactive({
    req(input$language_version)
    .filter_nodes(nodes, input$language_version)
  })
  
  output$search_results <- renderUI({
    req(input$species_search)
    
    if (nchar(input$species_search) < 2) {
      return(NULL)
    }
    
    search_term <- input$species_search
    
    matching_nodes <- 
      filtered_nodes() |> 
      dplyr::filter(
        stringr::str_detect(label, stringr::regex(search_term, ignore_case = TRUE))
      ) |> 
      dplyr::slice_head(n = 20)
    
    if (nrow(matching_nodes) == 0) {
      return(
        div(class = "search-results",
            div(class = "search-result-item text-muted",
                "検索結果がありません")
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
  
  observeEvent(input$add_taxon, {
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
      
      # ネットワーク内のノードを選択状態にする
      # 少し遅延を入れてネットワークが更新されるのを待つ
      shinyjs::delay(500, {
        visNetworkProxy("network_view") |> 
          visSelectNodes(id = input$add_taxon)
      })
    }
    
    updateTextInput(session, "species_search", value = "")
  })
  
  output$selected_taxa_list <- renderUI({
    taxa <- selected_taxa()
    
    if (length(taxa) == 0) {
      return(
        div(class = "text-muted small", "分類群が選択されていません")
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
  
  observeEvent(input$remove_taxon, {
    current_taxa <- selected_taxa()
    current_taxa[[input$remove_taxon]] <- NULL
    selected_taxa(current_taxa)
  })
  
  network_data <- reactive({
    taxa <- selected_taxa()
    
    if (length(taxa) == 0) {
      return(list(nodes = data.frame(), edges = data.frame()))
    }
    
    target_ids <- names(taxa)
    
    selected_edges <- edges |> 
      dplyr::filter(
        from %in% target_ids | to %in% target_ids,
        stringr::str_detect(from, paste0("^(", paste(input$language_version, collapse = "|"), "):")),
        stringr::str_detect(to, paste0("^(", paste(input$language_version, collapse = "|"), "):"))
      )
    
    all_node_ids <- unique(c(selected_edges$from, selected_edges$to, target_ids))
    
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
  
  output$network_view <- renderVisNetwork({
    data <- network_data()
    
    if (nrow(data$nodes) == 0) {
      return(
        visNetwork(data.frame(id = 1, label = "分類群を選択してください"), 
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
  
  observeEvent(input$update_network, {
    visNetworkProxy("network_view") |> 
      visRedraw()
  })
  
  output$selected_node_info <- renderUI({
    req(input$selected_node)
    
    node_info <- nodes |> 
      dplyr::filter(id == input$selected_node) |> 
      dplyr::slice_head(n = 1)
    
    if (nrow(node_info) == 0) {
      return(div("ノード情報が見つかりません"))
    }
    
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
            if(!is.na(node_info$taxon_rank) && nchar(node_info$taxon_rank) > 0) {
              div(class = "node-taxonomy", node_info$taxon_rank)
            },
            tags$h4(node_info$label, class = "mb-1"),
            tags$div(class = "text-muted", 
                     if(!is.na(node_info$canonical_name) && nchar(node_info$canonical_name) > 0) {
                       tags$em(node_info$canonical_name)
                     } else {
                       node_info$label
                     },
                     if(!is.na(node_info$taxonomic_status) && nchar(node_info$taxonomic_status) > 0) {
                       tags$span(class = "ms-2", paste0("(", node_info$taxonomic_status, ")"))
                     })
          )
        )
      ),
      
      div(class = "node-description",
        if(!is.na(node_info$abstract) && nchar(node_info$abstract) > 0) {
          if(nchar(node_info$abstract) > 200) {
            tags$span(
              substr(node_info$abstract, 1, 200),
              "...",
              tags$a(href = "#", class = "text-primary ms-1", "Read more")
            )
          } else {
            node_info$abstract
          }
        } else {
          "この分類群についての説明文は利用できません。"
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
        )
      )
    )
  })
  
  output$network_stats <- renderUI({
    data <- network_data()
    
    div(
      tags$h5("ネットワーク統計", class = "mb-3"),
      tags$table(class = "table table-sm",
        tags$tbody(
          tags$tr(
            tags$td("ノード数:"),
            tags$td(nrow(data$nodes))
          ),
          tags$tr(
            tags$td("エッジ数:"),
            tags$td(nrow(data$edges))
          ),
          tags$tr(
            tags$td("選択された分類群:"),
            tags$td(length(selected_taxa()))
          ),
          tags$tr(
            tags$td("表示言語数:"),
            tags$td(length(input$language_version))
          )
        )
      )
    )
  })
  
  output$lang_comparison_plot <- renderPlotly({
    taxa <- selected_taxa()
    
    if (length(taxa) == 0) {
      return(
        plot_ly() |> 
          layout(
            title = "分類群を選択してください",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    lang_stats <- nodes |> 
      dplyr::filter(
        stringr::str_extract(id, "(?<=:).*") %in% 
          stringr::str_extract(names(taxa), "(?<=:).*")
      ) |> 
      dplyr::group_by(lang = stringr::str_extract(id, "^[^:]+")) |> 
      dplyr::summarise(
        count = dplyr::n(),
        avg_core = mean(core_index, na.rm = TRUE),
        avg_sci = mean(sci, na.rm = TRUE),
        .groups = "drop"
      ) |> 
      dplyr::arrange(dplyr::desc(count))
    
    plot_ly(lang_stats, 
            y = ~lang, 
            x = ~count, 
            type = 'bar', 
            orientation = 'h',
            text = ~paste("Count:", count, "<br>Avg Core:", round(avg_core, 2)),
            textposition = 'none',
            hoverinfo = 'text',
            marker = list(color = '#18bc9c')) |> 
      layout(
        title = "言語版別の分類群数",
        xaxis = list(title = "分類群数"),
        yaxis = list(title = "言語", categoryorder = "total ascending"),
        showlegend = FALSE,
        margin = list(l = 50, r = 20, t = 40, b = 40)
      ) |> 
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)