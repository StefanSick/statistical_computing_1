library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(jsonlite)
library(DT)
library(countrycode)

# Load data
data_cia <- fromJSON("data_cia2.json")

# Prepare world map
world_map <- map_data("world")
suppressWarnings(
  world_map$ISO3 <- countrycode::countrycode(
    sourcevar   = world_map$region,
    origin      = "country.name",
    destination = "iso3c",
    nomatch     = NA
  )
)

# Map variables
var_choices <- c(
  "Expenditure on education"      = "expenditure",
  "Youth unemployment rate"       = "youth_unempl_rate",
  "Net migration rate"            = "net_migr_rate",
  "Population growth rate"        = "pop_growth_rate",
  "Electricity from fossil fuels" = "electricity_fossil_fuel",
  "Life expectancy"               = "life_expectancy"
)

var_labels <- c(
  "expenditure"             = "Expenditure on Education (% GDP)",
  "youth_unempl_rate"       = "Youth Unemployment Rate (%)",
  "net_migr_rate"           = "Net Migration Rate (per 1,000)",
  "pop_growth_rate"         = "Population Growth Rate (%)",
  "electricity_fossil_fuel" = "Electricity from Fossil Fuels (%)",
  "life_expectancy"         = "Life Expectancy (years)"
)

size_choices <- c("Population" = "population", "Area" = "area")

# UI 
ui <- fluidPage(
  titlePanel("CIA World Factbook 2020"),
  p(em(
    "Welcome to this Shiny app, which allows you to visualize variables from the CIA 2020 factbook",
    "on the world map, generate descriptive statistics and statistical graphics."
  )),

  tabsetPanel(

    # Univariate
    tabPanel("Univariate analysis",
      sidebarLayout(
        sidebarPanel(
          strong("Select a variable:"),
          selectInput("uni_var", label = NULL, choices = var_choices, selected = "expenditure"),
          actionButton("view_raw", "View raw data"),
          br(), br(),
          DTOutput("raw_table")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Map",
              p(em("The map contains values of the selected variable. Countries with gray areas have a missing value for the visualized variable.")),
              plotlyOutput("map_plot", height = "500px")
            ),
            tabPanel("Global analysis",
              fluidRow(
                column(6, plotlyOutput("hist_density_plot", height = "380px")),
                column(6, plotlyOutput("boxplot_global",    height = "380px"))
              )
            ),
            tabPanel("Analysis per continent",
              fluidRow(
                column(6, plotlyOutput("density_continent", height = "420px")),
                column(6, plotlyOutput("boxplot_continent", height = "420px"))
              )
            )
          )
        )
      )
    ),

    # Multivariate 
    tabPanel("Multivariate analysis",
      sidebarLayout(
        sidebarPanel(
          strong("Select variable 1:"),
          selectInput("mv_var1", label = NULL, choices = var_choices, selected = "expenditure"),
          strong("Select variable 2:"),
          selectInput("mv_var2", label = NULL, choices = var_choices, selected = "youth_unempl_rate"),
          strong("Scale points by:"),
          selectInput("size_var", label = NULL, choices = size_choices, selected = "area")
        ),
        mainPanel(
          h4("Scatterplot"),
          plotlyOutput("scatter_plot", height = "580px")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  uni_var  <- reactive(input$uni_var)
  mv_var1  <- reactive(input$mv_var1)
  mv_var2  <- reactive(input$mv_var2)
  size_var <- reactive(input$size_var)

  # Raw-data table visibility
  show_table <- reactiveVal(FALSE)
  observeEvent(input$view_raw, show_table(!show_table()))

  output$raw_table <- renderDT({
    req(show_table())
    df <- data_cia %>%
      select(country, continent, all_of(uni_var())) %>%
      arrange(country)
    names(df) <- c("Country", "Continent", var_labels[uni_var()])
    datatable(df,options  = list(pageLength = 15, scrollX = TRUE),rownames = FALSE)})

  #  Map
  output$map_plot <- renderPlotly({
    var <- uni_var()
    lbl <- var_labels[var]

    cia_sub <- data_cia %>% select(ISO3, country, all_of(var))
    plot_df <- left_join(world_map, cia_sub, by = "ISO3") %>%
      mutate(display_name = ifelse(is.na(country), region, country),
             val_label     = ifelse(is.na(.data[[var]]),
                                    paste0(display_name, ": NA"),
                                    paste0(display_name, ": ", round(.data[[var]], 2))))

    p <- ggplot(plot_df,
                aes(x = long, y = lat, group = group,
                    fill = .data[[var]], text = val_label)) +
      geom_polygon(colour = "white", linewidth = 0.1) +
      scale_fill_viridis_c(name = lbl, na.value = "grey") +
      labs(x = "long", y = "lat") +
      theme_void()

    ggplotly(p, tooltip = "text")
  })

  # Global analysis
  output$hist_density_plot <- renderPlotly({
    var <- uni_var()
    lbl <- var_labels[var]
    df  <- data_cia %>% filter(!is.na(.data[[var]]))

    p <- ggplot(df, aes(x = .data[[var]])) +
      geom_histogram(aes(y = after_stat(density)),
                     fill = "steelblue", alpha = 0.5, bins = 30, colour = "white") +
      geom_density(fill = "steelblue", alpha = 0.25, colour = "steelblue4") +
      labs(x = lbl, y = "Density") +
      theme_minimal()

    ggplotly(p)
  })

  output$boxplot_global <- renderPlotly({
    var <- uni_var()
    lbl <- var_labels[var]
    df  <- data_cia %>% filter(!is.na(.data[[var]]))

    p <- ggplot(df, aes(x = "", y = .data[[var]])) +
      geom_boxplot(fill = "steelblue", alpha = 0.5, width = 0.5) +
      labs(x = "", y = lbl) +
      theme_minimal()

    ggplotly(p)
  })

  # Analysis per continent 
  output$density_continent <- renderPlotly({
    var <- uni_var()
    lbl <- var_labels[var]
    df  <- data_cia %>% filter(!is.na(.data[[var]]), !is.na(continent))

    p <- ggplot(df, aes(x = .data[[var]], fill = continent, colour = continent)) +
      geom_density(alpha = 0.3) +
      labs(x = lbl, y = "Density", fill = "Continent", colour = "Continent") +
      theme_minimal()

    ggplotly(p)
  })

  output$boxplot_continent <- renderPlotly({
    var <- uni_var()
    lbl <- var_labels[var]
    df  <- data_cia %>% filter(!is.na(.data[[var]]), !is.na(continent))

    p <- ggplot(df, aes(x = continent, y = .data[[var]], fill = continent)) +
      geom_boxplot(alpha = 0.5) +
      labs(x = "Continent", y = lbl) +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Scatterplot
  output$scatter_plot <- renderPlotly({
    v1 <- mv_var1()
    v2 <- mv_var2()
    sz <- size_var()
    l1 <- var_labels[v1]
    l2 <- var_labels[v2]
    sz_lbl <- names(size_choices)[size_choices == sz]

    df <- data_cia %>%
      filter(!is.na(.data[[v1]]), !is.na(.data[[v2]]),
             !is.na(.data[[sz]]),  !is.na(continent))

    p <- ggplot(df, aes(x = .data[[v1]], y = .data[[v2]], colour = continent)) +
      geom_point(aes(size = .data[[sz]], text = country), alpha = 0.6) +
      geom_smooth(aes(group = continent),
                  method = "loess", se = FALSE, linewidth = 0.8,
                  formula = y ~ x) +
      scale_size_continuous(name = sz_lbl, range = c(1, 12)) +
      labs(x = l1, y = l2, colour = "Continent") +
      theme_minimal()

    ggplotly(p, tooltip = c("text", "x", "y"))
  })
}

shinyApp(ui = ui, server = server)
