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
###############

nodes <-
  readr::read_rds(here::here("data-raw/nodes.rds"))

edges <-
  readr::read_rds(here::here("data-raw/edges.rds"))

ui <- page_fluid(
  fillable = TRUE,
  theme = bs_theme(version = 5, bootswatch = "minty"),
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
                    placeholder = "例: ネコ, イヌ, サクラ..."),
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
    
    matching_nodes <- filtered_nodes() |> 
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
                     sprintf("(%s, Core: %.1f)", 
                             stringr::str_extract(node$id, "^[^:]+"), 
                             node$core_index))
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
    
    div(
      tags$h5(node_info$label, class = "mb-3"),
      tags$table(class = "table table-sm",
        tags$tbody(
          tags$tr(
            tags$td("Node ID:"),
            tags$td(tags$code(node_info$id))
          ),
          tags$tr(
            tags$td("言語:"),
            tags$td(stringr::str_extract(node_info$id, "^[^:]+"))
          ),
          tags$tr(
            tags$td("Core Index:"),
            tags$td(sprintf("%.2f", node_info$core_index))
          ),
          tags$tr(
            tags$td("SCI:"),
            tags$td(sprintf("%.3f", node_info$sci))
          ),
          if (!is.na(node_info$size)) {
            tags$tr(
              tags$td("サイズ:"),
              tags$td(format(node_info$size, big.mark = ","))
            )
          }
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