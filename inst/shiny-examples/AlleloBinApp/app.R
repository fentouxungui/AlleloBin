library(shiny)
library(readxl)

# Helper: detect header automatically
detect_header <- function(filepath, ext) {
  if (ext %in% c("csv", "txt")) {
    first_row <- readLines(filepath, n = 1)
    return(any(grepl("[A-Za-z]", unlist(strsplit(first_row, "[,\\t; ]")))))
  } else if (ext %in% c("xls", "xlsx")) {
    return(TRUE)
  }
  return(FALSE)
}

# Unified reader
read_input_file <- function(filepath, header_detected = TRUE) {
  ext <- tools::file_ext(filepath)
  if (ext == "csv") {
    return(read.csv(filepath, header = header_detected))
  } else if (ext == "txt") {
    return(read.table(filepath, header = header_detected, sep = "", fill = TRUE))
  } else if (ext %in% c("xls", "xlsx")) {
    return(readxl::read_excel(filepath, col_names = header_detected))
  } else {
    stop("Unsupported file type")
  }
}

ui <- fluidPage(
  titlePanel("AlleloBin - Microsatellite Allele Binning"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload Data File", 
                accept = c(".txt", ".csv", ".xls", ".xlsx")),
      fileInput("infofile", "Upload Info File", 
                accept = c(".txt", ".csv", ".xls", ".xlsx")),
      checkboxInput("header", "Input file contains headers", value = FALSE),
      numericInput("ploidy", "Ploidy (alleles per genotype)", value = 2, min = 1),
      selectInput("marker", "Select Marker", choices = NULL),
      actionButton("run", "Run Binning"),
      # tags$br(),   # <-- adds a vertical space
      # downloadButton("downloadExcel", "Download Excel Results")
      div(style="margin-top:15px;",
          downloadButton("downloadExcel", "Download Excel Results")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", tableOutput("datapreview")),
        tabPanel("Summary", tableOutput("summary")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Bins", tableOutput("bins")),
        tabPanel("Calls", tableOutput("calls")),
        tabPanel("Help", htmlOutput("helptext"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Auto-detect header when file is uploaded
  observeEvent(input$datafile, {
    ext <- tools::file_ext(input$datafile$name)
    auto_header <- detect_header(input$datafile$datapath, ext)
    updateCheckboxInput(session, "header", value = auto_header)
  })
  
  # Reactive expression to read the uploaded data
  raw_data <- reactive({
    req(input$datafile)
    df <- read_input_file(input$datafile$datapath, input$header)
    colnames(df) <- make.names(colnames(df), unique = TRUE)
    if (!input$header) {
      ncols <- ncol(df)
      colnames(df) <- c("Marker", "Genotype", paste0("Allele", seq_len(ncols-2)))
    }
    df
  })
  
  # Show data preview BEFORE binning
  output$datapreview <- renderTable({
    head(raw_data(), 20)
  })
  
  # Help tab
  output$helptext <- renderUI({
    HTML("
    <p><b>AlleloBin</b> is an interactive R Shiny application designed to classify observed SSR allele sizes into representative discrete biological allele sizes. 
    The application utilizes a modified least-squares minimization algorithm based on Idury & Cardon (1997).</p>
    
    <h4>Key Features</h4>
    <ul>
      <li><b>Flexible Ploidy Options:</b> Supports selection of multiple ploidy levels (diploid, triploid, tetraploid, etc.).</li>
      <li><b>Automated Binning:</b> Accurately determines representative discrete allele sizes from continuous data.</li>
      <li><b>Comprehensive Analytics:</b> Provides detailed summary statistics of all inferred alleles.</li>
      <li><b>Quality Control:</b> Calculates a dedicated Quality Index and detects allelic drift for each marker.</li>
      <li><b>Interactive Visualizations:</b> Generates overlapping histograms comparing observed vs. inferred allele sizes.</li>
      <li><b>Data Export:</b> Stores and exports complete results directly into Excel file(s).</li>
    </ul>
    
    <h4>Input Data File Requirements</h4>
    <p>1. <b>data.txt</b>: A tab- or comma-separated file containing fields: <code>Marker</code>, <code>Genotype</code>, <code>Allele1</code>, <code>Allele2</code>, ... <code>Allele K</code></p>
    <p>2. <b>info.txt</b>: A single-column file containing the repeat lengths of the markers in the same order.</p>
    
    <hr>
    <p><b>Feedback & Support:</b><br>
      Contact <a href='mailto:kamsubh@gmail.com'>Subhash Chandra</a> or 
      <a href='mailto:prasanth.stat@gmail.com'>Prasanth V.P.</a>
    </p>
    ")
  })
  
  # Reactive value container to safely store results across outputs
  binned_results <- reactiveVal(NULL)
  
  observeEvent(input$run, {
    req(input$datafile, input$infofile)
    
    # 1. FIX: Retrieve the data frame correctly from the reactive expression
    working_data <- raw_data()
    
    # 2. Parse the info file
    info <- read_input_file(input$infofile$datapath, FALSE)
    repeat_lengths <- as.numeric(unlist(info))
    
    # 3. Wrap allele_binning in withProgress cleanly
    results <- withProgress(message = "Running allele binning...", {
      allele_binning(working_data, repeat_lengths, input$ploidy, progress = TRUE)
    })
    
    # Save results to the reactive value
    binned_results(results)
    
    # Update choices for marker selection dropdown menu
    updateSelectInput(session, "marker", choices = names(results))
  }) # 🔹 FIX: Safely closed the calculation event loop here
  
  # --- RENDER OUTPUTS OUTSIDE THE OBSERVE EVENT ---
  
  output$summary <- renderTable({
    results <- binned_results()
    req(results)
    do.call(rbind, lapply(results, function(x) {
      data.frame(Min=x$min, Max=x$max, Bins=length(x$bins))
    }))
  })
  
  output$histogram <- renderPlot({
    results <- binned_results()
    req(results, input$marker)
    barplot(results[[input$marker]]$freq, main=paste("Histogram for", input$marker))
  })
  
  output$bins <- renderTable({
    results <- binned_results()
    req(results, input$marker)
    as.data.frame(results[[input$marker]]$freq)
  })
  
  output$calls <- renderTable({
    results <- binned_results()
    req(results, input$marker)
    results[[input$marker]]$calls
  })
  
  # Excel download handler
  output$downloadExcel <- downloadHandler(
    filename = function() paste0("AlleloBin_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      results <- binned_results()
      req(results)
      
      wb <- openxlsx::createWorkbook()
      
      # Sheet 1: BinSummary (UPDATED WITH EXPLICIT DECIMAL ROUNDING FOR ALL COLUMNS)
      df_bins <- do.call(rbind, lapply(names(results), function(marker) {
        bs <- results[[marker]]$bin_summary
        bs$Marker <- marker
        
        # Enforce exact decimal string layouts to handle trailing zeros cleanly
        formatted_lb         <- sprintf("%.2f", bs$LB)
        formatted_ub         <- sprintf("%.2f", bs$UB)
        formatted_median     <- sprintf("%.2f", bs$Median)
        formatted_mean       <- sprintf("%.2f", bs$Mean)
        formatted_sd         <- sprintf("%.4f", bs$SD)
        formatted_median_obs <- sprintf("%.2f", bs$MedianObs)
        formatted_mad        <- sprintf("%.4f", bs$MAD)
        
        # Re-assign back to data frame with strict formatting constraints
        bs$LB        <- as.numeric(formatted_lb)
        bs$UB        <- as.numeric(formatted_ub)
        bs$Median    <- as.numeric(formatted_median)     # 🔹 Forced 2 Decimals
        bs$Mean      <- as.numeric(formatted_mean)       # 🔹 Forced 2 Decimals
        bs$SD        <- as.numeric(formatted_sd)         # 🔹 Forced 4 Decimals
        bs$MedianObs <- as.numeric(formatted_median_obs) # 🔹 Forced 2 Decimals
        bs$MAD       <- as.numeric(formatted_mad)        # 🔹 Forced 4 Decimals
        
        # Reorder columns slightly so Marker shows up cleanly at the front
        bs <- bs[, c("Marker", setdiff(names(bs), "Marker"))]
        bs
      }))
      openxlsx::addWorksheet(wb, "BinSummary")
      openxlsx::writeData(wb, "BinSummary", df_bins)
      
      # Sheet 2: AlleleCalls
      df_calls <- do.call(rbind, lapply(names(results), function(marker) {
        results[[marker]]$calls
      }))
      openxlsx::addWorksheet(wb, "AlleleCalls")
      openxlsx::writeData(wb, "AlleleCalls", df_calls)
      
      # Sheet 3: MarkerSummary (CALIBRATED FOR EXACT C MATH MATCH)
      df_summary <- do.call(rbind, lapply(names(results), function(marker) {
        
        min_val     <- results[[marker]]$min
        max_val     <- results[[marker]]$max
        true_repeat <- results[[marker]]$true_input_repeat 
        
        # Exact equation verified by your C execution file:
        max_poss_alleles <- ((max_val - min_val) / true_repeat) + 1
        
        # Force strict layout mappings to protect precision boundaries
        formatted_max_poss     <- sprintf("%.2f", max_poss_alleles)
        formatted_quality_idx  <- sprintf("%.4f", results[[marker]]$quality_index)
        formatted_optimal_rep  <- sprintf("%.2f", results[[marker]]$optimal_repeat)
        formatted_drift        <- sprintf("%.2f", results[[marker]]$allelic_drift)
        
        data.frame(
          Marker              = marker,
          MinAllele           = min_val,
          MaxAllele           = max_val,
          Individuals         = nrow(results[[marker]]$calls),
          MaxPossibleAlleles  = as.numeric(formatted_max_poss),    # 🔹 Forced 2 Decimals
          RepeatLength        = true_repeat,                        # 🔹 True reading from info.txt
          QualityIndex        = as.numeric(formatted_quality_idx), # 🔹 Forced 4 Decimals
          OptimalRepeatLength = as.numeric(formatted_optimal_rep), # 🔹 Forced 2 Decimals
          AllelicDrift        = as.numeric(formatted_drift)        # 🔹 Forced 2 Decimals
        )
      }))
      openxlsx::addWorksheet(wb, "MarkerSummary")
      openxlsx::writeData(wb, "MarkerSummary", df_summary)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
