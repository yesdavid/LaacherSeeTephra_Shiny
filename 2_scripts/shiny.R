library(magrittr)
library(shiny)
library(shinyWidgets)
library(bslib)
library(ggplot2)
library(leaflet)
library(plotly)
library(DT)


LSTbase <- readr::read_csv(file.path("..", "1_data", "LSTbase.csv"))
LSTbase$`Mean thickness (cm)` <- as.double(LSTbase$`Mean thickness (cm)`)
LSTbase$`Distance from vent (km)` <- as.double(LSTbase$`Distance from vent (km)`)

phases <- c("LLST", "MLST", "MLST-A", "MLST-B", "MLST-C", "MLST-C1", "MLST-C2", "MLST-C3", "ULST", "Various")

LSTbase <- 
tidyr::pivot_longer(data = LSTbase,
                    cols = phases,
                    names_to = "Phase") %>% 
  subset(value == 1) %>% 
  dplyr::select(-value)

countries <- unique(LSTbase$Country)


# set.seed(1)
# ggpal <- randomcoloR::distinctColorPalette(length(phases))
# names(ggpal) <- phases

ggpal <- c("LLST" = "#FF0000",
           "MLST" = "#DF7850", 
           "MLST-A" = "#CA5B85", 
           "MLST-B" = "#BF4CA0", 
           "MLST-C" = "#B53DBA", 
           "MLST-C1" = "#AA2ED5", 
           "MLST-C2" = "#A020F0", 
           "MLST-C3" = "#5010BD", 
           "ULST" = "darkblue", 
           "Various" = "yellow")

pal <- leaflet::colorFactor(ggpal,
                            domain = phases
)



###########################################
ui <- page_sidebar(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  title = "Laacher See Tephra",
  sidebar = sidebar(
    helpText(
      "Select and submit"
    ),
    shiny::checkboxInput(inputId = "archaeology", 
                         label = "Associated archaeology", 
                         value = FALSE),
    shiny::checkboxGroupInput(
      inputId = "Country",
      label = "Country",
      choices = countries,
      selected = countries
    ),
    shiny::checkboxGroupInput(
      inputId = "Phase",
      label = "Phase",
      choices = phases,
      selected = phases
    ),
    
    # Add a submit button
    actionButton(inputId = "submit_btn", label = "Submit")
  ),
  layout_columns(
    # map
    card(
      card_header("Map"),
      leafletOutput("map")
    ),

    # tephra
    card(
      card_header("Tephra thickness"),
      plotlyOutput("tephra")
    ),

    # dataframe
    card(
      card_header("Data"),
      dataTableOutput("table")
    ),
    col_widths = c(6, 6, 12),
    row_heights = c(2, 1)
  )
)



###########################################

server <-
  function(input, output, session) {
    # When the submit button is pressed, update the reactive data
    selected_Countries <- eventReactive(input$submit_btn, {
      # Only update this when the submit button is pressed
      req(input$Country) # make sure input$Country is not NULL
      if(input$archaeology == FALSE) {
        dplyr::filter(LSTbase, Country %in% input$Country & Phase %in% input$Phase)
      } else {
        dplyr::filter(LSTbase, Country %in% input$Country & Phase %in% input$Phase & !(`Associated archaeology` %in% NA))
      }
    })

    # 1. Render DataTable
    output$table <- DT::renderDT(
      {
        req(selected_Countries())
        datatable(selected_Countries(), 
                  selection = "single")
      },
      server = FALSE
    )

    # 2. Render leaflet map
      output$map <- renderLeaflet({
        req(selected_Countries())
        map_data <- selected_Countries()
        leaflet(map_data) %>%
          addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
          addMarkers(lat = 50.410278, lng = 7.269722, label = "Laacher See Volcano") %>%
          addCircleMarkers(~Longitude, ~Latitude,
                           popup = ~ as.character(paste0("Site: ", `Site name`, " | ",
                                                         "Phase: ", `Phase`, " | ",
                                                         "Mean thickness (cm): ", `Mean thickness (cm)`, " | ",
                                                         "Archaeology: ", `Associated archaeology`)),
                           color = ~ pal(Phase), layerId = ~`Site name`,
                           opacity = 1,
                           radius = ~ scales::rescale(map_data$`Mean thickness (cm)`, to = c(10, 30), from = range(map_data$`Mean thickness (cm)`, na.rm = TRUE, finite = TRUE))
          ) %>%
          fitBounds(
            min(map_data$Longitude), min(map_data$Latitude),
            max(map_data$Longitude), max(map_data$Latitude)
          ) %>%
          addLegend("bottomright", pal = pal, values = map_data$Phase, title = "Phase")
      })


    # 3. Render Plotly plot
    output$tephra <- renderPlotly({
      req(selected_Countries())
      plot_data <- selected_Countries()

      tephra_plot <- 
        ggplot(
          data = plot_data,
          aes(
            x = `Distance from vent (km)`,
            # y = log(`Mean thickness (cm)`),
            y = `Mean thickness (cm)`,
            fill = Phase,
            key = `Site name`#,
            # shape = `Fan direction...23`
          ),
          color = "black"
        ) +
        geom_jitter(size = 4) +
        theme_bw() +
        scale_fill_manual(values = ggpal) +
        theme(legend.position = "none") + 
        scale_y_log10() +
        ylab("log(Mean thickness (cm))")#+
        # scale_shape_manual(values = c("N" = "\u2b06",
        #                               "S" = "\u2b07",
        #                               "W" = "\u2b05", 
        #                               "E" = "\u2b62",
        #                               "NE" = "\u2b08",
        #                               "Near-vent" = "\u2b24"))

      ggplotly(tephra_plot) %>%
        layout(dragmode = "select")
    })
  }


# Run the app ----
shinyApp(ui = ui, server = server)
