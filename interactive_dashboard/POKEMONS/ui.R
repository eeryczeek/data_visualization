library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

dashboardPage(
    dashboardHeader(title = "Pokemon Dashboard"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            box(
                width = 3,
                DT::dataTableOutput("show_pokemon_data")
            ),
            box(
                status = "warning", width = 6,
                imageOutput("pokemon_image")
            ),
            box(
                width = 3,
                plotOutput("pokemon_plot")
            )
        ),
        fluidRow(
            column(
                width = 7,
                box(
                    width = NULL, solidHeader = TRUE, status = "primary",
                    plotlyOutput("interactive_treemap")
                ),
            ),
            column(
                width = 5,
                box(
                    width = NULL, solidHeader = TRUE,
                    plotOutput("violin_plot")
                ),
            )
        )
    )
)
