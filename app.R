library(shiny)
library(tidyverse)
library(tidymodels)
library(janitor)
library(DT)

# ------------------------------------------------------------
# GitHub locations
# ------------------------------------------------------------
REPO_WEB_BASE <- "https://github.com/AbdullahSallam/customer_churn"
BRANCH <- "main"  # change to "master" if needed

DEMO_WEB_URL  <- paste0(REPO_WEB_BASE, "/blob/", BRANCH, "/demo.csv")
DEMO_RAW_URL  <- paste0("https://raw.githubusercontent.com/AbdullahSallam/customer_churn/", BRANCH, "/demo.csv")
MODEL_RAW_URL <- paste0("https://raw.githubusercontent.com/AbdullahSallam/customer_churn/", BRANCH, "/churn_model.rds")

THRESHOLD <- 0.50

download_to_temp <- function(url) {
  tmp <- tempfile()
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  tmp
}

ui <- fluidPage(
  tags$head(
    # “Zoom out” effect: smaller font + tighter cells
    tags$style(HTML("
      table.dataTable tbody td { font-size: 11px; padding: 4px 6px; }
      table.dataTable thead th { font-size: 11px; padding: 4px 6px; }
      .btn { margin-right: 6px; }
    "))
  ),
  
  titlePanel("Churn Labeling Tool (Simple)"),
  
  fluidRow(
    column(
      6,
      tags$a("1) Open demo.csv and edit", href = DEMO_WEB_URL, target = "_blank",
             class = "btn btn-default")
    ),
    column(
      6,
      actionButton("score", "2) Do labeling", class = "btn btn-primary")
    )
  ),
  
  br(),
  verbatimTextOutput("status"),
  br(),
  
  h4("Labeled records (Churn / NoChurn)"),
  DTOutput("labeled_table"),
  br(),
  downloadButton("download_labeled", "Download labeled CSV")
)

server <- function(input, output, session) {
  
  status_msg <- reactiveVal("Ready.")
  
  # Load model once at app start
  model_wf <- reactiveVal(NULL)
  observe({
    tryCatch({
      status_msg("Loading model…")
      model_path <- download_to_temp(MODEL_RAW_URL)
      model_wf(readRDS(model_path))
      status_msg("Model loaded. Edit demo.csv then click 'Do labeling'.")
    }, error = function(e) {
      status_msg(paste("ERROR loading model:", e$message))
      model_wf(NULL)
    })
  })
  
  labeled_data <- eventReactive(input$score, {
    req(model_wf())
    
    tryCatch({
      status_msg("Downloading latest demo.csv…")
      df_raw <- readr::read_csv(DEMO_RAW_URL, show_col_types = FALSE) %>%
        drop_na()%>% #logistic reg. fails with NA values
        janitor::clean_names()
      
      # Remove churn_label if user included it by mistake
      df <- df_raw %>% select(-any_of("churn_label"))
      
      status_msg("Labelling in progress…")
      
      
      # Predict probabilities
      pred_prob <- predict(model_wf(), df, type = "prob")
      
      # Identify the churn probability column
      # Prefer .pred_Yes if present; otherwise use the last .pred_* column
      prob_col <- if (".pred_Yes" %in% names(pred_prob)) ".pred_Yes" else tail(names(pred_prob), 1)
      
      # Create ONLY the label (simple output)
      out <- df_raw %>%
        mutate(
          churn_label = if_else(pred_prob[[prob_col]] >= THRESHOLD, "Churn", "NoChurn")
        ) %>%
        # Put label first for visibility
        relocate(churn_label, .before = 1)
      
      status_msg("Done.")
      out
      
    }, error = function(e) {
      status_msg(paste("ERROR:", e$message))
      NULL
    })
  })
  
  output$status <- renderText(status_msg())
  
  output$labeled_table <- renderDT({
    req(labeled_data())
    
    datatable(
      labeled_data(),
      rownames = FALSE,
      options = list(
        dom = "tip",          # table + search + paging info (keeps it simple)
        pageLength = 25,      # show 25 rows (change to 50 if you like)
        autoWidth = TRUE
        # Note: no scrollX; we try to fit columns naturally
      )
    )
  })
  
  output$download_labeled <- downloadHandler(
    filename = function() paste0("labeled_demo_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(labeled_data())
      readr::write_csv(labeled_data(), file)
    }
  )
}

shinyApp(ui, server)
