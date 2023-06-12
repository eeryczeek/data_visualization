import("shiny")
import("tidyselect")
import("lubridate")
import("dplyr")
import("utils")
import("tidyr")
import("ggplot2")
import("DT")

export("ui")
export("init_server")

ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "panel-header breakdown-header",
      div(class = "item", "Datatable"),
    ),
    div(
      class = "chart-container",
      DTOutput(ns("datatable"))
    )
  )
}

init_server <- function(id, df, y, state) {
  callModule(server, id, df, y, state)
}

server <- function(input, output, session, df, y, state) {
  output$show_pokemon_data <- DT::renderDataTable({
        DT::datatable(data.frame(df$Name),
            options = list(scrollY = 200,
                pageLength = 10,
                lengthChange = FALSE,
                info = FALSE,
                colnames = "Pokemon Name",
                columnDefs = list(list(className = "dt-center", targets = "_all"))),
            selection = list(mode = "single", target = "row"),
            rownames = FALSE)
    })
}