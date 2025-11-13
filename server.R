
library(shiny)
library(tidyverse)
library(plotly)

shinyServer(function(input, output, session) {
  # Load raw data
  raw_data <- reactive({
    req(input$file)
    read_csv(file.path("data", input$file))
  })

  # Detect categorical variables (character or factor, excluding id/value)
  cat_vars <- reactive({
    df <- raw_data()
    vars <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    setdiff(vars, c("id", "value"))
  })


  # Render filter controls
  observeEvent(raw_data(), {
    df <- raw_data()
    vars <- cat_vars()
    
    output$filter_controls <- renderUI({
      if (length(vars) == 0) return(NULL)
      
      tagList(
        lapply(vars, function(var) {
          selectInput(
            inputId = paste0("filter_", var),
            label = paste("Filter", var),
            choices = sort(unique(df[[var]])),
            selected = NULL,  # None selected by default
            multiple = TRUE
          )
        })
      )
    })
  })

  # Reactive filtered data based on selected filters
  filtered_data <- reactive({
    df <- raw_data()
    vars <- cat_vars()
    
    for (var in vars) {
      input_id <- paste0("filter_", var)
      selected_vals <- input[[input_id]]
      if (!is.null(selected_vals) && length(selected_vals) > 0) {
        df <- df[df[[var]] %in% selected_vals, , drop = FALSE]
      }
    }
    
    df
  })
  

  output$hist_plot <- renderPlotly({
    req(input$update_plot)
    
    df <- filtered_data()
    
    # Determine grouping variables: any filtered categorical variable
    cat_vars <- names(df)[sapply(df, is.character) & names(df) != "gender"]
    group_vars <- c()
    
    for (var in cat_vars) {
      filter_vals <- input[[paste0("filter_", var)]]
      if (!is.null(filter_vals) && length(filter_vals) > 0) {
        group_vars <- c(group_vars, var)
      }
    }
    
    if (length(group_vars) == 0) {
      df$group <- "All"
    } else {
      df <- df %>% unite("group", all_of(group_vars), sep = " | ", remove = FALSE)
    }
    
    df <- df %>%
      filter(!is.na(value),
             value >= input$bin_min,
             value <= input$bin_max)
    
    p <- ggplot(df, aes(x = value, fill = group)) +
      scale_x_continuous(limits = c(input$bin_min, input$bin_max)) +
      labs(
        x = "Value",
        y = if (input$relative_freq) "Relative Frequency" else "Count",
        title = paste("Distribution",
                      if (group_vars[1] != "All")
                        paste("grouped by", paste(group_vars, collapse = ", "))
                      else "")
      ) +
      theme_minimal()
    
    if (input$relative_freq) {
      p <- p + geom_histogram(aes(y = after_stat(count / sum(count))),
                              binwidth = input$bin_width,
                              position = "identity", alpha = 0.6)
    } else {
      p <- p + geom_histogram(binwidth = input$bin_width,
                              position = "identity", alpha = 0.6)
    }
    
    if ("group" %in% names(df) && length(unique(df$group)) > 1) {
      p <- p + facet_wrap(~group, scales = "fixed")
    }
    
    ggplotly(p)
  })

  output$summary_plot <- renderPlotly({
    req(input$update_plot)
    if (!input$show_summary) return(NULL)
    
    df <- filtered_data()
    
    # Determine grouping variables: any categorical variable with a non-empty filter
    cat_vars <- names(df)[sapply(df, is.character) & names(df) != "gender"]
    group_vars <- c()
    
    for (var in cat_vars) {
      filter_vals <- input[[paste0("filter_", var)]]
      if (!is.null(filter_vals) && length(filter_vals) > 0) {
        group_vars <- c(group_vars, var)
      }
    }
    
    if (length(group_vars) == 0) {
      df$group <- "All"
    } else {
      df <- df %>% unite("group", all_of(group_vars), sep = " | ", remove = FALSE)
    }
    
    df <- df %>%
      filter(!is.na(value),
             value >= input$bin_min,
             value <= input$bin_max)
    
    summary_df <- df %>%
      group_by(group) %>%
      summarise(
        mean = mean(value),
        sem = sd(value) / sqrt(n()),
        .groups = "drop"
      )
    
    p <- ggplot(summary_df, aes(x = group, y = mean, fill = group)) +
      geom_col(width = 0.6) +
      geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.2) +
      labs(x = "Group", y = "Mean ± SEM", title = "Group Means with SEM") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p)
  })

  generate_static_plots <- function() {
    req(input$group_vars)

    df <- dataset()

    for (var in input$group_vars) {
      filter_vals <- input[[paste0("filter_", var)]]
      if (!is.null(filter_vals)) {
        df <- df %>% filter(.data[[var]] %in% filter_vals)
      }
    }

    df <- df %>%
      filter(!is.na(value),
             value >= input$bin_min,
             value <= input$bin_max) %>%
      unite("group", all_of(input$group_vars), sep = " | ", remove = FALSE)

    hist <- ggplot(df, aes(x = value, fill = group)) +
      geom_histogram(binwidth = input$bin_width,
                     position = "identity",
                     alpha = 0.6) +
      facet_wrap(~group, scales = "fixed") +
      scale_x_continuous(limits = c(input$bin_min, input$bin_max)) +
      labs(x = "Value",
           y = if (input$relative_freq) "Relative Frequency" else "Count",
           title = paste("Distribution grouped by", paste(input$group_vars, collapse = ", "))) +
      theme_minimal()

    summary_df <- df %>%
      group_by(group) %>%
      summarise(mean = mean(value), sem = sd(value) / sqrt(n()), .groups = "drop")

    summary <- ggplot(summary_df, aes(x = group, y = mean, fill = group)) +
      geom_col(width = 0.6) +
      geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.2) +
      labs(x = "Group", y = "Mean ± SEM", title = "Group Means with SEM") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    list(hist = hist, summary = summary)
  }

  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("plots_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      plots <- generate_static_plots()
      pdf(file, width = 10, height = 8)
      print(plots$hist)
      if (input$show_summary) print(plots$summary)
      dev.off()
    }
  )

  output$download_png <- downloadHandler(
    filename = function() {
      paste0("plots_", Sys.Date(), ".png")
    },
    content = function(file) {
      plots <- generate_static_plots()
      ggsave(file, plot = plots$hist, width = 10, height = 5, dpi = 300)
      if (input$show_summary) {
        ggsave(sub(".png", "_summary.png", file), plot = plots$summary, width = 10, height = 5, dpi = 300)
      }
    }
  )
})
