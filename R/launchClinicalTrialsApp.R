#' Launch Clinical Trials Shiny Application
#'
#' This function encapsulates the UI and server logic for a Shiny application
#' that enables users to interactively explore clinical trials data.
#'
#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny textInput selectInput numericInput tabsetPanel tabPanel plotOutput dataTableOutput
#' @importFrom shiny renderPlot renderDataTable reactive
#' @importFrom dplyr filter left_join select arrange summarise group_by rename
#' @importFrom tidyr complete
#' @importFrom DT DTOutput renderDT
#' @importFrom ggplot2 ggplot aes geom_col coord_polar theme_void theme_bw theme xlab ylab geom_bar
#' @importFrom DBI dbConnect dbListTables
#' @export

launchClinicalTrialsApp <- function() {
  max_num_studies = 1000

  external_db_path <- file.path("..", "ctgov.duckdb")
  db_path <- if (file.exists(external_db_path)) {
    external_db_path
  } else {
    # Fallback to the internal package path
    system.file("extdata", "ctgov.duckdb", package = "bis620.2023")
  }

  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)

  # Check the connection
  if (length(DBI::dbListTables(con)) == 0) {
    stop("Problem reading from the database connection at ", db_path)
  }

  # Define UI
  ui <- fluidPage(

    # Application title
    titlePanel("Clinical Trials Query"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        textInput("brief_title_kw", "Brief title keywords"),
        selectInput("source_class",
                    label = h3("Sponsor Type"),
                    choices = c("Federal" = "FED",
                                "Individual" = "INDIV",
                                "Industry" = "INDUSTRY",
                                "Network" = "NETWORK",
                                "NIH" = "NIH",
                                "Other" = "OTHER",
                                "Other Government" = "OTHER_GOV",
                                "Unknown" = "UNKNOWN"),
                    multiple = TRUE),
        numericInput("num_conditions", "Number of Conditions to Display", value = 10, min = 1)
      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Phase", plotOutput("phase_plot")),
          tabPanel("Concurrent", plotOutput("concurrent_plot")),
          tabPanel("Conditions", plotOutput("conditions_plot")),
          tabPanel("Sponsor Performance",
                   conditionalPanel(
                     condition = "input.source_class.length === 0",
                     wellPanel(
                       h4("Please select a Sponsor Type to view performance analysis."),
                       tags$p("Use the sidebar to select one or more sponsor types to analyze their performance metrics.")
                     )
                   ),
                   DTOutput("sponsor_performance_table")
          ),
          tabPanel("Status Summary", plotOutput("status_summary_plot"))
        ),
        dataTableOutput("trial_table")
      )
    )
  )

  # Define Server
  server <- function(input, output, session) {

    get_studies = reactive({
      if (input$brief_title_kw != "") {
        si = input$brief_title_kw |>
          strsplit(",") |>
          unlist() |>
          trimws()
        ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
      } else {
        ret = studies
      }
      if (!is.null(input$source_class)) {
        ret = ret |>
          filter(source_class %in% !!input$source_class)
      }
      ret = ret %>%
        left_join(tbl(con, "conditions"), by = "nct_id") %>%
        select(-id, -downcase_name) # Exclude unnecessary columns

      #print(str(ret))

      ret |>
        head(max_num_studies) |>
        collect()
    })

    output$sponsor_performance_table = renderDT({
      if (is.null(input$source_class) || length(input$source_class) == 0) {
        return(NULL)
      }

      sponsor_performance_data <- get_studies() %>%
        filter(source_class %in% input$source_class) %>%
        group_by(source_class) %>%
        summarize(
          total_studies = n(),
          average_duration = mean(as.numeric(completion_date - start_date), na.rm = TRUE),
          completed_studies = sum(overall_status == "Completed"),
          terminated_studies = sum(overall_status == "Terminated")
        ) %>%
        arrange(desc(total_studies))

      datatable(sponsor_performance_data,
                options = list(
                  pageLength = 5,
                  search = list(search = ""),
                  order = list(list(1, 'desc')),
                  columnDefs = list(list(className = 'dt-center', targets = '_all'))
                ),
                filter = 'top')
    })

    output$phase_plot = renderPlot({
      get_studies() |>
        plot_phase_histogram()
    })

    output$concurrent_plot = renderPlot({
      get_studies() |>
        select(start_date, completion_date) |>
        get_concurrent_trials() |>
        ggplot(aes(x = date, y = count)) +
        geom_line() +
        xlab("Date") +
        ylab("Count") +
        theme_bw()
    })

    output$conditions_plot = renderPlot({
      data_with_conditions <- get_studies()

      num_conditions <- as.numeric(input$num_conditions)

      if (!is.na(num_conditions) && num_conditions > 0) {
        data_with_conditions <- data_with_conditions %>%
          count(name, sort = TRUE) %>%
          rename(count = n) %>%
          slice_head(n = num_conditions)
      } else {
        data_with_conditions <- data_with_conditions %>%
          count(name, sort = TRUE) %>%
          rename(count = n) %>%
          slice_head(n = 10)
      }

      plot_conditions_histogram(data_with_conditions)
    })



    output$status_summary_plot = renderPlot({
      study_data <- get_studies() %>%
        count(overall_status) %>%
        collect()

      ggplot(study_data, aes(x = "", y = n, fill = overall_status)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(legend.title = element_blank()) +
        labs(fill = "Overall Status")
    })


    output$trial_table = renderDataTable({
      get_studies() |>
        head(max_num_studies) |>
        select(nct_id, brief_title, start_date, completion_date) |>
        rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
               `Start Date` = start_date, `Completion Date` = completion_date)
    })

  }

  # Run the application
  shinyApp(ui, server)
}
