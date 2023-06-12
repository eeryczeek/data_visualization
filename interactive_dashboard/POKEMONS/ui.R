library(shiny)
library(shinydashboard)
library(plotly)

dashboardPage(
    dashboardHeader(
        title = "Pokemon Dashboard",
        titleWidth = 300,
        tags$li(
            class = "dropdown",
            tags$a(
                href = "#",
                class = "dropdown-toggle",
                `data-toggle` = "dropdown",
                tags$i(class = "fa fa-question-circle"),
                style = "color: #fff;"
            ),
            tags$ul(
                class = "dropdown-menu",
                style = "min-width: 300px;",
                tags$li(
                    tags$div(
                        style = "padding: 10px;",
                        "This is a Pokemon Dashboard where you can explore various information about Pokemon.",
                        tags$p("Here are some features of this dashboard:"),
                        tags$ul(
                            tags$li("The 'Pokemon Name' table displays the list of Pokemon names. Selecting a row highlights the corresponding Pokemon."),
                            tags$li("The 'Radar Chart' shows the stats (HP, Defense, Attack, Sp..Atk, Sp..Def, Speed) of the selected Pokemon."),
                            tags$li("The 'Interactive Treemap' provides a hierarchical view of the Pokemon types. Click on a type to filter the data."),
                            tags$li("The 'Pokemon Image' displays the image of the selected Pokemon."),
                            tags$li("The 'Violin Plot' visualizes the distribution of stats for all Pokemon or the selected type."),
                            tags$li("The 'Contour Plot' shows the relationship between Attack and Defense stats for all Pokemon or the selected type.")
                        )
                    )
                )
            )
        )
    ),
    dashboardSidebar(
        disable = TRUE),
    dashboardBody(
        fluidRow(
            box(
                width = 2,
                DT::dataTableOutput("show_pokemon_data")
            ),
            # make a column with logo image and them imageOutput of pokemon
            column(
                width = 3,
                box(
                    width = NULL,
                    height = "120px",
                    plotOutput("put_image")
                ),
                box(
                    height = "280px",
                    width = NULL,
                    plotOutput("pokemon_image")
                )
            ),
            box(
                width = 4,
                plotOutput("radar_chart")
            ),
            box(
                width = 3,
                plotlyOutput("contour_plot")
            )
        ),
        fluidRow(
            column(
                width = 7,
                box(
                    width = NULL,
                    plotlyOutput("interactive_treemap")
                ),
            ),
            column(
                width = 5,
                box(
                    width = NULL,
                    plotlyOutput("violin_plot")
                ),
            )
        )
    )
)
