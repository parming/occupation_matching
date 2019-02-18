library(shiny)
library(shinyWidgets)

library(DT)
library(leaflet)

library(rgdal)

source("helpers/esco_skills.R")
source("helpers/geo.R")

# Load map and polygons
m <- readOGR("maps/shp/bm5mv20sh0tc1_20180701_0/bm5mv20sh0tpc1_20180701_0.shp")
m <- spTransform(m, CRS("+proj=longlat +datum=WGS84"))

contracts_final <- read.table("data/processed_contracts_esco.csv", sep=";", comment.char = "", quote='"', header = TRUE)
educations_final <- read.table("data/processed_educations_esco.csv", sep=";", comment.char = "", quote='"', header = TRUE)
jobs_final <- read.table("data/processed_ads_esco.csv", sep=";", comment.char = "", quote='"', header = TRUE)

# Correct polygon ids
m$municipality_id <- as.numeric(m$CODICOMAR)
m$contracts <- unlist(lapply(m$municipality_id, function(id) {
  sum(contracts_final[contracts_final$municipality_id == id,]$count)
}))
m$educations <- unlist(lapply(m$municipality_id, function(id) {
  sum(educations_final[educations_final$municipality_id == id,]$count)
}))
m$supply <- mapply(function(contracts, educations) {
  supply <- educations-contracts
  if (supply < 0) {
    0-(0-supply)^(1/3)
  } else {
    (supply)^(1/3)
  }
}, m$contracts, m$educations)

#jobs_final[jobs_final$related_occupation == "2",]$related_occupation <- "http://data.europa.eu/esco/occupation/9e2e6e1e-363b-4e1b-a673-7bc0f7343300; administrador de sistemas de TIC/administradora de sistemas de TIC"
#jobs_final[jobs_final$related_occupation == "3",]$related_occupation <- "http://data.europa.eu/esco/occupation/8b6388a4-4904-471b-9331-d3b1211f5525; gestor de proyectos de TIC/gestora de proyectos de TIC"
#jobs_final[jobs_final$related_occupation == "4",]$related_occupation <- "http://data.europa.eu/esco/occupation/9e2e6e1e-363b-4e1b-a673-7bc0f7343300; administrador de sistemas de TIC/administradora de sistemas de TIC"
#jobs_final[jobs_final$related_occupation == "5",]$related_occupation <- "http://data.europa.eu/esco/occupation/3e7bf729-4442-4b9f-ad5e-83111963795c; técnico de TIC/técnica de TIC"
#jobs_final[jobs_final$related_occupation == "6",]$related_occupation <- "http://data.europa.eu/esco/occupation/8c57af09-719c-42b3-be40-6ed4946236cc; administrador de bases de datos/administradora de bases de datos"
#jobs_final[jobs_final$related_occupation == "7",]$related_occupation <- "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software"
#jobs_final[jobs_final$related_occupation == "8",]$related_occupation <- "http://data.europa.eu/esco/occupation/f2b15a0e-e65a-438a-affb-29b9d50b77d1; desarrollador de software/desarrolladora de software"

# Get active occupations from dataset
unique_occupations <- unique(c(
  as.character(unique(educations_final$related_occupation)),
  as.character(unique(contracts_final$related_occupation)),
  as.character(unique(jobs_final$related_occupation))
))

unique_occupations_names <- unlist(lapply(unique_occupations, esco_occupation_name))
occupations <- setNames(unique_occupations, unique_occupations_names)

years <- 2013:2017

# Set default values
municipalities <- get_municipalities()

ui <- fluidPage(
  tabPanel("Interactive map",
    div(class="outer",

      # Include assets
      tags$head(
        includeCSS("assets/css/styles.css")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("Map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Occupacions"),

        pickerInput("year", "Any", choices=years, selected=years, options=list(`actions-box` = TRUE), multiple=T),
        pickerInput("occupation", "Occupació ESCO", choices=occupations, selected=occupations, options=list(`actions-box` = TRUE), multiple=T),

        h3(textOutput("municipality_name")),
        dataTableOutput("info"),

        tags$div(class="right",
          tags$br(),
          actionLink("clear_municipality", textOutput("clear_municipality"))
        )

      )
    )
  ),

  conditionalPanel("false", icon("crosshair"))
)

server <- function(input, output, session) {

  output$Map <- renderLeaflet({

    pal <- colorQuantile(
      palette = "YlOrRd",
      domain = m$supply,
      reverse = TRUE
    )
    legend_labels <- c("Contractes >> Matricules", "Contractes > Matricules", "Contractes ~= Matricules", "Contractes < Matricules")

    popup <- paste0(m$NOMCOMAR)

    leaflet() %>%
      addTiles() %>%
      addPolygons(data=m, weight = 1, label=popup, layerId=~m$municipality_id, fillOpacity = 0.8,
                  fillColor = ~colorNumeric("YlOrRd", supply, reverse = TRUE)(supply),
                  highlightOptions = highlightOptions(fillColor = "white", weight = 2, bringToFront = TRUE),
                  labelOptions = labelOptions(direction = "bottom")) %>%
      addLegend(pal = pal, values = m$supply, position = "bottomright", title = "Balanç laboral",
                labFormat = function(type, cuts, p) { legend_labels[(p*4)+1] }) %>%
      setView(lng = 2.6589, lat = 41.4887, zoom = 8)
  })

  # Nullable map click
  data <- reactiveValues(map_id=NULL)
  observeEvent(input$Map_shape_click, {data$map_id <- input$Map_shape_click$id})

  # Update information window if anything changes
  generate_info <- eventReactive({
    data$map_id
    input$year
    input$occupation
  }, {

    id <- if(!is.null(data$map_id)) {
      input$Map_shape_click$id
    } else {
      m$municipality_id
    }
    year <- input$year
    occupation <- input$occupation

    job_count <- sum(jobs_final[which(
      jobs_final$municipality_id %in% id &
      jobs_final$year %in% year &
      jobs_final$related_occupation %in% occupation
    ),]$count)

    contract_count <- sum(contracts_final[which(
      contracts_final$municipality_id %in% id &
      contracts_final$year %in% year &
      contracts_final$related_occupation %in% occupation)
    ,]$count)

    student_count <- sum(educations_final[which(
      educations_final$municipality_id %in% id &
      educations_final$year %in% year &
      educations_final$related_occupation %in% occupation
    ),]$count)

    data.frame(
      "Data" = c("Contractes", "Ofertes", "Matricules", "Coverage"),
      "Valor" = c(contract_count, job_count, student_count, student_count-contract_count)
    )
  })

  municipality_name <- eventReactive(data$map_id, {
    id <- input$Map_shape_click$id
    as.character(municipalities[municipalities$municipality_id == id,]$municipality)
  })

  observeEvent(input$clear_municipality, {
    data$map_id <- NULL
  })
  observeEvent(input$clear_everything, {
    data$map_id <- NULL
    input$year <- years
    input$occupation <- occupations
  })

  output$municipality_name <- renderText(municipality_name())
  output$clear_municipality <- renderText(
    if (!is.null(municipality_name())) {
      "[ Anul·la la comarca ]"
    }
  )

  output$info <- renderDataTable(
    generate_info(),
    options = list(
      ordering = FALSE,
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    )
  )
}

shinyApp(ui, server)
