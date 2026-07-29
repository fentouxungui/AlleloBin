
library(shiny)
library(DT)
library(readxl)
library(data.table)
library(openxlsx)

server <- function(input, output, session) {

  # Conditional display of menu
  # output$results_tabs <- shiny::renderUI({
  #   tabsetPanel(
  #     tabPanel("Data Preview", DTOutput("datapreview")),
  #     tabPanel("Repeat Lengths", DTOutput("repeatpreview")),
  #     # Conditionally add results tabs
  #     if (!is.null(binned_results())) tabPanel("Summary", tableOutput("summary")),
  #     if (!is.null(binned_results())) tabPanel("All Markers Summary", DTOutput("allsummary")),
  #     if (!is.null(binned_results())) tabPanel("Histogram", plotOutput("histogram")),
  #     if (!is.null(binned_results())) tabPanel("Bins", tableOutput("bins")),
  #     if (!is.null(binned_results())) tabPanel("Calls", tableOutput("calls")),
  #     tabPanel("Help", htmlOutput("helptext"))
  #   )
  # })

  # --- Auto-detect header when file is uploaded ---
  observeEvent(input$datafile, {
    ext <- tools::file_ext(input$datafile$name)
    auto_header <- detect_header(input$datafile$datapath, ext)
    updateCheckboxInput(session, "header", value = auto_header)
  })

  # --- Reactive dataset (data file) ---
  detect_delimiter <- function(filepath) {
    first_line <- readLines(filepath, n = 1)
    counts <- sapply(c("\t", ",", ";", ":"), function(d) {
      length(gregexpr(d, first_line, fixed = TRUE)[[1]])
    })
    names(which.max(counts))
  }


  dataset <- reactive({
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    header_flag <- as.logical(input$header)

    if (ext %in% c("csv", "txt")) {
      delim <- detect_delimiter(input$datafile$datapath)
      df <- fread(input$datafile$datapath, header = header_flag,
                  sep = delim, fill = TRUE, blank.lines.skip = TRUE)
    } else if (ext == "xlsx") {
      df <- read_excel(input$datafile$datapath, col_names = header_flag)
    }

    if (!header_flag) {
      ncols <- ncol(df)
      colnames(df) <- c("Marker", "Genotype",
                        paste0("Allele_", seq_len(ncols - 2)))
    }
    df
  })

  # Preview raw data
  output$datapreview <- renderDT(dataset(), options = list(pageLength = 10, scrollX = TRUE))

  # --- Reactive repeat length file ---
  repeat_lengths <- reactive({
    req(input$infofile)
    ext <- tools::file_ext(input$infofile$name)
    header_flag <- as.logical(input$header)

    if (ext %in% c("csv", "txt")) {
      df <- fread(input$infofile$datapath, header = header_flag)
    } else if (ext == "xlsx") {
      df <- read_excel(input$infofile$datapath, col_names = header_flag)
    }

    # If no header, assign a default column name
    if (!header_flag) {
      colnames(df) <- "Repeat_Length"
    }

    df
  })

  # Preview repeat length
  output$repeatpreview <- renderDT(repeat_lengths(), options = list(pageLength = 10, scrollX = TRUE))

  # --- Help tab ---
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


  # --- Event-driven binning ---
  binned_results <- eventReactive(input$run, {
    working_data <- dataset()
    info <- repeat_lengths()
    repeat_vec <- as.numeric(unlist(info))

    if (length(repeat_vec) != length(unique(working_data$Marker))) {
      showNotification("Mismatch: number of repeat lengths does not equal number of markers",
                       type = "error")
      return(NULL)
    }

    withProgress(message = "Running allele binning...", {
      AlleloBin::allele_binning(working_data, repeat_vec, input$ploidy, progress = TRUE)
    })
  })

  # --- Dynamic marker selector ---
  output$marker_ui <- renderUI({
    req(binned_results())
    selectInput("marker", "Select Marker:", choices = names(binned_results()))
  })

  # --- Outputs ---

      output$histogram <- renderPlot({
        results <- binned_results()
        req(results, input$marker)
        barplot(results[[input$marker]]$freq, main = paste("Histogram for", input$marker))
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

      # Marker Summary: bin-level stats for selected marker
      output$summary <- renderTable({
        results <- binned_results()
        req(results, input$marker)

        res <- results[[input$marker]]

        # bin_summary should already contain the columns you listed
        df <- res$bin_summary

        # Add marker name column
        df$Marker <- input$marker

        # Apply formatting for neatness
        df$LB        <- sprintf("%.2f", df$LB)
        df$UB        <- sprintf("%.2f", df$UB)
        df$Median    <- sprintf("%.2f", df$Median)
        df$Frequency <- df$Frequency
        df$Mean      <- sprintf("%.2f", df$Mean)
        df$SD        <- sprintf("%.4f", df$SD)
        df$MedianObs <- sprintf("%.2f", df$MedianObs)
        df$MAD       <- sprintf("%.2f", df$MAD)

        # Reorder columns so Marker appears first
        df <- df[, c("Marker", setdiff(names(df), "Marker"))]

        df
      })

      # Global Summary: all markers
      output$allsummary <- renderDT({
        results <- binned_results()
        req(results)

        df <- do.call(rbind, lapply(names(results), function(marker) {
          res <- results[[marker]]
          data.frame(
            Marker              = marker,
            MinAllele           = sprintf("%.2f", res$min),
            MaxAllele           = sprintf("%.2f", res$max),
            Individuals         = nrow(res$calls),
            MaxPossibleAlleles  = sprintf("%.2f", ((res$max - res$min) / res$true_input_repeat) + 1),
            RepeatLength        = res$true_input_repeat,
            QualityIndex        = sprintf("%.4f", res$quality_index),
            OptimalRepeatLength = sprintf("%.2f", res$optimal_repeat),
            AllelicDrift        = sprintf("%.2f", res$allelic_drift)
          )
        }))
        datatable(df, options = list(pageLength = 10, scrollX = TRUE))
      })


      # --- Excel download handler ---
      output$downloadExcel <- downloadHandler(
        filename = function() paste0("AlleloBin_results_", Sys.Date(), ".xlsx"),
        content = function(file) {
          results <- binned_results()
          req(results)

          wb <- createWorkbook()

          # --- BinSummary (stacked across markers)
          df_bins <- do.call(rbind, lapply(names(results), function(marker) {
            bs <- results[[marker]]$bin_summary
            bs$Marker <- marker
            bs <- bs[, c("Marker", setdiff(names(bs), "Marker"))]
            bs
          }))
          addWorksheet(wb, "BinSummary")
          writeData(wb, "BinSummary", df_bins)

          # Apply styles: bold header + numeric precision
          headerStyle <- createStyle(textDecoration = "bold")
          numStyle2   <- createStyle(numFmt = "0.00")   # 2 decimals
          numStyle4   <- createStyle(numFmt = "0.0000") # 4 decimals

          # Bold headers
          addStyle(wb, "BinSummary", headerStyle,
                   rows = 1, cols = 1:ncol(df_bins), gridExpand = TRUE)

          # Apply numeric formats
          addStyle(wb, "BinSummary", numStyle2,
                   rows = 2:(nrow(df_bins)+1), cols = c(3,4,5,7,9,10), gridExpand = TRUE) # LB, UB, Median, Mean, MedianObs, MAD
          addStyle(wb, "BinSummary", numStyle4,
                   rows = 2:(nrow(df_bins)+1), cols = c(8), gridExpand = TRUE) # SD

          # --- AlleleCalls (all markers together)
          df_calls <- do.call(rbind, lapply(names(results), function(marker) {
            results[[marker]]$calls
          }))
          addWorksheet(wb, "AlleleCalls")
          writeData(wb, "AlleleCalls", df_calls)

          # --- Global Summary
          df_summary <- do.call(rbind, lapply(names(results), function(marker) {
            res <- results[[marker]]
            data.frame(
              Marker              = marker,
              MinAllele           = res$min,
              MaxAllele           = res$max,
              Individuals         = nrow(res$calls),
              MaxPossibleAlleles  = ((res$max - res$min) / res$true_input_repeat) + 1,
              RepeatLength        = res$true_input_repeat,
              QualityIndex        = res$quality_index,
              OptimalRepeatLength = res$optimal_repeat,
              AllelicDrift        = res$allelic_drift
            )
          }))
          addWorksheet(wb, "GlobalSummary")
          writeData(wb, "GlobalSummary", df_summary)

          saveWorkbook(wb, file, overwrite = TRUE)
        }
      )
}
