library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("AlleloBin - Microsatellite Allele Binning"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload Data File",
                accept = c(".txt", ".csv", ".xls", ".xlsx")),
      fileInput("infofile", "Upload Repeat Length File",
                accept = c(".txt", ".csv", ".xls", ".xlsx")),
      checkboxInput("header", "Data contains column headers", value = FALSE),
      numericInput("ploidy", "Ploidy (alleles per genotype)", value = 2, min = 1),
      actionButton("run", "Run Binning"),
      br(), br(),
      uiOutput("marker_ui"),   # marker dropdown rendered dynamically
      downloadButton("downloadExcel", "Download Excel Results")
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Data Preview", DTOutput("datapreview")),
        tabPanel("Repeat Lengths", DTOutput("repeatpreview")),
        tabPanel("Marker Summary", tableOutput("summary")),       # single marker
        tabPanel("Global Summary", DTOutput("allsummary")),       # all markers
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Bins", tableOutput("bins")),
        tabPanel("Calls", tableOutput("calls")),
        tabPanel("Help", htmlOutput("helptext"))
      )
    )
  )
)
