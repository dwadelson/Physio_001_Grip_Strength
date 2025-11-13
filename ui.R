
library(shiny)

shinyUI(fluidPage(
  titlePanel("Interactive Histogram Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput("file", "Choose CSV File:",
                  choices = list.files("data", pattern = "\\.(csv)$")),


      uiOutput("filter_controls"),

      checkboxInput("relative_freq", "Show Relative Frequency (instead of Count)", value = FALSE),
      checkboxInput("show_summary", "Show Mean ± SEM Summary Plot", value = TRUE),

      sliderInput("bin_min", "Bin Min", min = 0, max = 100, value = 0),
      sliderInput("bin_max", "Bin Max", min = 0, max = 100, value = 100),
      numericInput("bin_width", "Bin Width", value = 5, min = 1),

      actionButton("update_plot", "Update Plot"),
      br(), br(),
      downloadButton("download_pdf", "Download Plots as PDF"),
      downloadButton("download_png", "Download Plots as PNG")
    ),

    mainPanel(
      plotly::plotlyOutput("hist_plot"),
      br(), br(),
      plotly::plotlyOutput("summary_plot"),
      br(),
      textOutput("summary_line")
    )
  )
))
