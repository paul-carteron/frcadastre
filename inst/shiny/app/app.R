library(shiny)
library(leaflet)
library(sf)
library(openxlsx)
library(DT)

if (!requireNamespace("rcadastre", quietly = TRUE)) {
  remotes::install_github("mucau/rcadastre")
}
library(rcadastre)

# ui ----
ui <- fluidPage(
  titlePanel("Rcadastre Shiny App"),

  # Forest and Owner
  fluidRow(
    h4("Forest & Owner"),
    column(
      width = 6,
      textInput("forest", "Forest name")
    ),
    column(
      width = 6,
      textInput("owner", "Forest owner")
    )
  ),

  hr(),

  # Plots selection
  fluidRow(
    column(
      width = 4,
      h4("Plots selection"),

      # Region
      h5("Region"),
      fluidRow(
        column(6, verbatimTextOutput("region_code")),
        column(6, verbatimTextOutput("region_name"))
      ),

      # Department
      h5("Department"),
      fluidRow(
        column(6, selectizeInput("departement", label = NULL, choices = rcadastre::departement_2025$DEP)),
        column(6, verbatimTextOutput("departement_name"))
      ),

      # City
      h5("City"),
      fluidRow(
        column(6, selectizeInput("commune", label = NULL, choices = NULL)),
        column(6, verbatimTextOutput("commune_name"))
      ),

      # Section & numero
      h5("Parcelle"),
      fluidRow(
        column(6, selectizeInput("section", label = "Section", choices = NULL)),
        column(6, selectizeInput("numero", label = "Numero", choices = NULL))
      ),

      # Lieudit
      h5("Lieudit"),
      fluidRow(
        column(6, verbatimTextOutput("lieudit")),
        column(6, uiOutput("manual_lieudit_ui"))
      ),

      # Contenance
      h5("Contenance"),
      verbatimTextOutput("contenance"),

      # Add button
      actionButton("add_parcelle", "Add selected plot")
    ),

    column(
      width = 8,
      h4("Displaying plots"),
      leafletOutput("map", height = 500)
    )
  ),

  hr(),

  # Troisième espace : affichage du dataframe
  fluidRow(
    h4("Selected plots"),
    DTOutput("table_parcelles")
  ),

  hr(),

  # Quatrième espace : boutons d'export
  uiOutput("export_ui")
)

# server ----
server <- function(input, output, session) {
  # Dataset
  regs <- rcadastre::region_2025
  deps <- rcadastre::departement_2025
  coms <- rcadastre::commune_2025

  # Reactives values
  parcelles_sf <- reactiveVal(
    st_sf(
      forest = character(0),
      owner = character(0),
      code_reg = character(0),
      reg_name = character(0),
      code_dep = character(0),
      dep_name = character(0),
      code_com = character(0),
      com_name = character(0),
      idu = character(0),
      prefixe = character(0),
      section = character(0),
      numero = character(0),
      lieudit = character(0),
      contenance = numeric(0),
      geometry = st_sfc(crs = 4326) # CRS EPSG:4326
    )
  )

  commune_parcelles <- reactiveVal(NULL)
  reg_code_rv <- reactiveVal()
  reg_name_rv <- reactiveVal()
  dep_name_rv <- reactiveVal()
  com_name_rv <- reactiveVal()
  idu_rv <- reactiveVal()
  surface_rv <- reactiveVal()
  lieudit_rv <- reactiveVal()
  parcelle_rv <- reactiveVal()

  # Department & Region
  observeEvent(input$departement, {
    # Departement
    dep_name <- deps$NCC[deps$DEP == input$departement]
    dep_name_rv(dep_name)
    output$departement_name <- renderText(dep_name)

    # Region
    reg_code <- deps$REG[deps$DEP == input$departement]
    reg_code_rv(reg_code)
    output$region_code <- renderText(reg_code)

    reg_name <- regs$NCC[regs$REG == reg_code]
    reg_name_rv(reg_name)
    output$region_name <- renderText(reg_name)

    # Communes filtrées en base R
    communes <- coms$COM[coms$DEP == input$departement]
    updateSelectizeInput(session, "commune",
                         choices = communes,
                         server = TRUE)
  })

  # Commune
  observeEvent(input$commune, {
    req(input$commune)

    com_name <- coms$NCC[coms$COM == input$commune]
    com_name_rv(com_name)
    output$commune_name <- renderText(com_name)

    # Download
    parcelles <- rcadastre::get_quick_etalab(input$commune) |>
      rcadastre::idu_rename_in_df("idu")
    parcelles$section <- toupper(parcelles$section)
    parcelles$section <- ifelse(nchar(parcelles$section) < 2,
                                paste0(parcelles$prefixe, "0", parcelles$section),
                                paste0(parcelles$prefixe, parcelles$section))

    parcelles$numero <- as.character(parcelles$numero)
    parcelles$numero <- ifelse(nchar(parcelles$numero) < 4,
                               paste0(strrep("0", 4 - nchar(parcelles$numero)), parcelles$numero),
                               parcelles$numero)

    # Reactive value
    commune_parcelles(parcelles)

    # Leaflet
    bbox <- st_bbox(commune_parcelles())
    leafletProxy("map") %>%
      clearGroup("Commune") %>%
      addPolygons(
        data = commune_parcelles(),
        color = "blue",
        weight = 1,
        fillOpacity = 0.2,
        label = ~paste0("Section: ", section, " - N°: ", numero),
        group = "Commune"
      ) %>%
      fitBounds(
        lng1 = as.numeric(bbox["xmin"]),
        lat1 = as.numeric(bbox["ymin"]),
        lng2 = as.numeric(bbox["xmax"]),
        lat2 = as.numeric(bbox["ymax"])
      )

    # Sections
    sections <- sort(unique(parcelles$section))
    updateSelectizeInput(session, "section", choices = sections, selected = NULL)
  })

  # Numero
  observeEvent(input$section, {
    parcelles <- commune_parcelles()

    if (!is.null(parcelles) && !is.null(input$section)) {
      numeros <- sort(unique(parcelles$numero[parcelles$section == input$section]))
      updateSelectizeInput(session, "numero", choices = numeros, selected = NULL)
    }
  })

  # Plot
  observeEvent(input$numero, {
    parcelles <- commune_parcelles()
    req(parcelles, input$commune, input$section, input$numero)

    .idu <- idu_build( substr(input$commune, 1, 2),
                       substr(input$commune, 3, 5),
                       substr(input$section, 1, 3),
                       substr(input$section, 4, 5),
                       input$numero )

    idu_rv(.idu)

    observe({
      cat(as.character(.idu), "\n")
    })

    # IDU check
    if (.idu %in% parcelles$idu) {
      parcelle <- parcelles[parcelles$idu == .idu, ]
      req(nrow(parcelle) == 1)

      parcelle_rv(parcelle)

      # Leaflet
      leafletProxy("map") %>%
        clearGroup("selected") %>%
        addPolygons(
          data = parcelle,
          color = "red",
          weight = 2,
          fillOpacity = 0,
          label = ~paste0("Section: ", section, " - N°: ", numero),
          group = "selected"
        )

      # Lieudit
      lieudit_res <- idu_get_lieudit(.idu)

      lieudit_name <- if (!is.null(lieudit_res) && !is.na(lieudit_res$lieudit)) {
        lieudit_res$lieudit
      } else {
        NA_character_
      }

      if (is.na(lieudit_name) || lieudit_name == "") {
        output$lieudit <- renderText("NA")
        output$manual_lieudit_ui <- renderUI({
          textInput("manual_lieudit", label = NULL, value = "Enter the 'lieudit'")
        })
      } else {
        output$lieudit <- renderText(lieudit_name)
        output$manual_lieudit_ui <- renderUI(NULL)  # cache l'UI
        lieudit_rv(lieudit_name)
      }

      # Contenance
      surface <- parcelle$contenance / 10000
      surface_rv (surface)
      output$contenance <- renderText(surface)
    }
  })

  observeEvent(input$manual_lieudit, {
    lieudit_rv(input$manual_lieudit)
  })

  # Leaflet
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 2.35, lat = 48.85, zoom = 6) %>%

      # Fonds de carte
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

      # LayersControl pour fonds et overlays
      addLayersControl(
        baseGroups = c("OpenStreetMap", "CartoDB Positron", "Satellite"),
        overlayGroups = c("Commune", "selected", "Plots"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  # Add plot
  observeEvent(input$add_parcelle, {
    # Checks
    if (is.null(input$forest) || input$forest == "" ||
        is.null(input$owner) || input$owner == "") {

      showNotification(
        "You have to complete 'Forest' et 'Owner' before add plot.",
        type = "error",
        duration = 5  # en secondes
      )
      return()  # stoppe l'exécution
    }

    parcelle <- parcelle_rv()
    req(parcelle, input$forest, input$owner)

    # Lieudit
    lieudit_value <- if (!is.null(lieudit_rv()) && lieudit_rv() != "") {
      lieudit_rv()
    } else {
      "NA"
    }

    # New row
    new_row <- st_sf(
      forest   = input$forest,
      owner    = input$owner,
      code_reg = reg_code_rv(),
      reg_name = reg_name_rv(),
      code_dep = substr(input$commune, 1, 2),
      dep_name = dep_name_rv(),
      code_com = substr(input$commune, 3, 5),
      com_name = com_name_rv(),
      idu = idu_rv(),
      prefixe = substr(input$section, 1, 3),
      section = substr(input$section, 4, 5),
      numero = input$numero,
      lieudit = lieudit_value,
      contenance = surface_rv(),
      geometry = st_geometry(parcelle)
    )

    # Check plot geometry
    existing <- parcelles_sf()
    if (nrow(existing) > 0) {
      same_geom <- sapply(st_equals(new_row, existing), any)
      same_attrs <- with(existing, forest == input$forest & owner == input$owner &
                           section == substr(input$section, 4, 5) &
                           numero == input$numero)
      if (any(same_geom & same_attrs)) {
        showNotification(
          "This plot is already selected !",
          type = "error",
          duration = 5
        )
        return()
      }
    }

    # rbind
    parcelles_sf(rbind(parcelles_sf(),
                       new_row |> st_transform(4326)))

    # Leaflet
    leafletProxy("map") %>%
      clearGroup("Plots") %>%
      addPolygons(
        data = parcelles_sf(),
        color = "green",
        weight = 2,
        fillOpacity = 1,
        popup  = ~paste0("Forest: ", forest, "<br>Owner: ", owner, "<br>Plot: ", section, "-", numero),
        group = "Plots"
      )

    leafletProxy("map") %>%
      addTiles(group = "Fond") %>%
      addLayersControl(
        baseGroups = c("Fond"),
        overlayGroups = c("Commune", "selected", "Plots"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  # Data.Frame
  output$table_parcelles <- renderDT({
    datatable(parcelles_sf())
  })

  # UI
  output$format_ui <- renderUI({
    if (input$data_type == "df") {
      selectInput("format", "Format de fichier", choices = c("csv", "xlsx"))
    } else {
      selectInput("format", "Format de fichier", choices = c("shp", "geojson"))
    }
  })

  # Export
  output$export_ui <- renderUI({
    req(parcelles_sf())
    if (nrow(parcelles_sf()) > 0) {
      fluidRow(
        h4("Exports"),
        column(
          width = 12,
          # Aligner les inputs horizontalement
          div(style = "display: flex; align-items: center; gap: 10px;",
              radioButtons("data_type", "Type d'export",
                           choices = c("dataframe" = "df", "sf" = "sf"),
                           inline = TRUE),
              uiOutput("format_ui"),
              actionButton("export_btn", "Exporter")
          )
        )
      )
    }
  })

  observeEvent(input$export_btn, {
    df <- parcelles_sf()
    file_path <- NULL

    if (input$data_type == "df") {
      if (input$format == "csv") {
        file_path <- "parcelles.csv"
        write.csv(df, file_path, row.names = FALSE)
      } else if (input$format == "xlsx") {
        file_path <- "parcelles.xlsx"
        openxlsx2::write_xlsx(df, file_path)
      }
    } else if (input$data_type == "sf") {
      if (input$format == "shp") {
        file_path <- "parcelles.shp"
        st_write(df, file_path, delete_dsn = TRUE)
      } else if (input$format == "geojson") {
        file_path <- "parcelles.geojson"
        st_write(df, file_path, delete_dsn = TRUE)
      }
    }

    showNotification(
      paste0("Export done ! File written in : ", normalizePath(file_path)),
      type = "message",
      duration = 5
    )
  })

}

# shinyApp ----
shinyApp(ui, server)
