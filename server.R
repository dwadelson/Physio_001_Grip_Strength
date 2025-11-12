
library(shiny)
library(tidyverse)
library(plotly)

shinyServer(function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    read_csv(file.path("data", input$file))
  })
  
  # Dynamically detect categorical variables for grouping
  output$group_var_ui <- renderUI({
    req(dataset())  # Ensure the dataset is loaded
    
    # Identify categorical variables (character or factor)
    cat_vars <- names(dataset())[sapply(dataset(), function(col) is.character(col) || is.factor(col))]
    cat_vars <- setdiff(cat_vars, c("id", "value"))  # Exclude non-grouping fields
    
    # Render UI
    selectizeInput("group_vars", "Group by (one or more):",
                   choices = cat_vars,
                   selected = head(cat_vars, 1),
                   multiple = TRUE)
  })

  output$filter_controls <- renderUI({
    req(input$group_vars)
    df <- dataset()
    controls <- lapply(input$group_vars, function(var) {
      vals <- unique(df[[var]])
      selectInput(
        inputId = paste0("filter_", var),
        label = paste("Filter values for", var),
        choices = vals,
        selected = vals,
        multiple = TRUE
      )
    })
    do.call(tagList, controls)
  })

  output$hist_plot <- renderPlotly({
    req(input$update_plot)
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

    p <- ggplot(df, aes(x = value, fill = group)) +
      scale_x_continuous(limits = c(input$bin_min, input$bin_max)) +
      labs(
        x = "Value",
        y = if (input$relative_freq) "Relative Frequency" else "Count",
        title = paste("Distribution grouped by", paste(input$group_vars, collapse = ", "))
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

    p <- p + facet_wrap(~group, scales = "fixed")
    ggplotly(p)
  })

  output$summary_plot <- renderPlotly({
    req(input$update_plot)
    req(input$group_vars)

    if (!input$show_summary) return(NULL)

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
