library(shiny)
library(shinydashboard)
library(DT)

dashboardPage(
    dashboardHeader(title = "Pokemon Dashboard"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            box(
                width = 3,
                DT::dataTableOutput("show_pokemon_data")
            ),
            box(
                status = "warning", width = 6,
                plotOutput("treemap")
            ),
            box(
                width = 3,
                plotOutput("pokemon_plot")
            )
        ),
        fluidRow(
            column(
                width = 4,
                box(
                    width = NULL, solidHeader = TRUE, status = "primary",
                    imageOutput("pokemon_image")
                ),
            ),
            column(
                width = 4,
                box(
                    width = NULL, solidHeader = TRUE, status = "warning",
                    "Box content"
                ),
            ),
            column(
                width = 4,
                box(
                    width = NULL, solidHeader = TRUE,
                    plotOutput("violin_plot")
                ),
            )
        )
    )
)
