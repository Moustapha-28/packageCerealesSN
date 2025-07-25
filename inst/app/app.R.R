library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(viridis)

ui <- fluidPage(
  titlePanel("Analyse spatiale au Sénégal"),
  sidebarLayout(
    sidebarPanel(
      fileInput("shp", "Charger les fichiers du shapefile (.shp + .dbf + .shx + .prj)",
                multiple = TRUE,
                accept = c(".shp", ".dbf", ".shx", ".prj")),
      fileInput("csv", "Charger le fichier CSV avec indicateurs", accept = ".csv"),
      uiOutput("var_select"),
      uiOutput("region_select"),
      actionButton("go", "Afficher la carte")
    ),
    mainPanel(
      leafletOutput("map", height = 600),
      verbatimTextOutput("region_info")
    )
  )
)

server <- function(input, output, session) {
  shapefile <- reactiveVal(NULL)
  datafile <- reactiveVal(NULL)
  merged_data <- reactiveVal(NULL)
  selected_region <- reactiveVal(NULL)

  observeEvent(input$go, {
    req(input$shp, input$csv)

    tmpdir <- tempdir()
    for (i in 1:nrow(input$shp)) {
      file.copy(from = input$shp$datapath[i],
                to = file.path(tmpdir, input$shp$name[i]),
                overwrite = TRUE)
    }

    shp_name <- input$shp$name[grep(".shp$", input$shp$name)]
    shp_path <- file.path(tmpdir, shp_name)

    # Lire et transformer le shapefile
    shp <- st_read(shp_path, quiet = TRUE)
    shp <- st_transform(shp, crs = 4326)
    shp$NOMREG <- toupper(shp$NOMREG)

    # Lire les données CSV et harmoniser les noms
    data <- read.csv(input$csv$datapath)
    data$NOMREG <- toupper(data$NOMREG)

    shapefile(shp)
    datafile(data)

    updateSelectInput(session, "var", choices = names(data)[sapply(data, is.numeric)])
    updateSelectInput(session, "region", choices = c("Toutes", unique(data$NOMREG)))
  })

  output$var_select <- renderUI({
    req(datafile())
    selectInput("var", "Choisir l’indicateur à cartographier :",
                choices = names(datafile())[sapply(datafile(), is.numeric)])
  })

  output$region_select <- renderUI({
    req(datafile())
    selectInput("region", "Filtrer par région (facultatif) :",
                choices = c("Toutes", unique(datafile()$NOMREG)), selected = "Toutes")
  })

  observe({
    req(shapefile(), datafile(), input$var)
    merged <- left_join(shapefile(), datafile(), by = "NOMREG")
    if (input$region != "Toutes") {
      merged <- merged[merged$NOMREG == input$region, ]
    }
    merged_data(merged)
  })

  output$map <- renderLeaflet({
    req(merged_data(), input$var)

    # Créer une palette verte (clair -> foncé)
    pal <- colorNumeric(
      palette = colorRampPalette(c("#d9f0d3", "#74c476", "#006d2c"))(100),
      domain = merged_data()[[input$var]],
      na.color = "lightgray"
    )

    leaflet(merged_data()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -14.5, lat = 14.5, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(get(input$var)),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        label = ~paste0(NOMREG, ": ", round(get(input$var), 2)),
        highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE),
        layerId = ~NOMREG
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~get(input$var),
        title = input$var,
        opacity = 0.8
      )
  })


  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    selected_region(click$id)
  })

  output$region_info <- renderPrint({
    req(selected_region(), datafile())
    df <- datafile()
    info <- df[df$NOMREG == selected_region(), ]
    if (nrow(info) > 0) return(info)
    else return("Pas d'information disponible.")
  })
}

shinyApp(ui, server)
