library(forcats)
library(fmsb)
library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(tidyr)
library(treemap)

function(input, output, session) {
    set.seed(23)
    
    pokemon_data <- read.csv("~/GitHub/data_visualization/interactive_dashboard/Pokemon.csv")

    # find the min and max values for each stat
    min_max <- pokemon_data %>%
        select(Attack, Defense, Sp..Atk, Sp..Def, Speed) %>%
        gather(key = "stat", value = "value") %>%
        group_by(stat) %>%
        summarise(min = min(value), max = max(value))
    
    # Create a reactive value to store the selected row index
    selected_pokemons <- reactiveVal(NULL)
    
    # Show datatable of pokemon_names with vertical scrolling get rid of the bottom description
    output$show_pokemon_data <- DT::renderDataTable({
        DT::datatable(data.frame(pokemon_data$Name),
            options = list(scrollY = 200,
                pageLength = 10,
                lengthChange = FALSE,
                info = FALSE,
                colnames = "Pokemon Name",
                columnDefs = list(list(className = 'dt-center', targets = "_all"))),
            selection = list(mode = 'multiple', target = 'row'),
            rownames = FALSE)
    })
    
    # Update the selected row when a row is clicked
    observeEvent(input$show_pokemon_data_rows_selected, {
        selected_pokemons(input$show_pokemon_data_rows_selected)
    })
    
    # Update the plot title dynamically based on the selected Pokemon
    output$plot_title <- renderUI({
        HTML(paste0("<h3>", pokemon_data[selected_pokemons(), ]$Name, "</h3>"))
    })
    
    # Plot the stats of selected pokemons on a radar chart fill inside add dynamic title and legend
    output$pokemon_plot <- renderPlot({
        
        if (is.null(selected_pokemons())) {
            return(NULL)
        }
        
        row <- pokemon_data[selected_pokemons(), ] %>%
            select(HP, Defense, Attack, Sp..Atk, Sp..Def, Speed)
        row <- rbind(rep(250, 6), rep(0, 6), row)
        
        radarchart(
            row, axistype = 1,
            cglcol = "grey", cglty = 1, axislabcol = "grey",
            caxislabels = seq(0, 250, 6), cglwd = 0.8, vlcex = 0.8,
            title = pokemon_data[selected_pokemons(), ]$Name,
        )
    })

    # treemap of pokemon types
    output$treemap <- renderPlot({
        # custom color palette
        pokemon_colors <- c(
            "Bug" = "#A6B91A",
            "Dark" = "#705746",
            "Dragon" = "#6F35FC",
            "Electric" = "#F7D02C",
            "Fairy" = "#D685AD",
            "Fighting" = "#C22E28",
            "Fire" = "#EE8130",
            "Flying" = "#A98FF3",
            "Ghost" = "#735797",
            "Grass" = "#7AC74C",
            "Ground" = "#E2BF65",
            "Ice" = "#96D9D6",
            "Normal" = "#A8A77A",
            "Poison" = "#A33EA1",
            "Psychic" = "#F95587",
            "Rock" = "#B6A136",
            "Steel" = "#B7B7CE",
            "Water" = "#6390F0"
        )
        pokemon_data %>%
            group_by(Type.1, Type.2) %>%
            summarise(count = n()) %>%
            mutate(
                Type.1 = fct_reorder(Type.1, count),
                Type.2 = fct_reorder(Type.2, count)
            ) %>%
            treemap(index = c("Type.1", "Type.2"), vSize = "count", title = "pokemon type distribution", palette = pokemon_colors)
    })

    output$compare2pokemons <- renderPlotly({
        if (is.null(selected_pokemons())) {
            return(NULL)
        }

        row <- pokemon_data[selected_pokemons(), ] %>%
            select(Name, Type.1, HP, Defense, Attack, Sp..Atk, Sp..Def, Speed)
        
        trace0 <- list(
            type = "scatterpolar",
            r = c(row$HP[1], row$Attack, row$Defense, row$`Sp. Atk`, row$`Sp. Def`, row$Speed, row$HP),
            theta = c("HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed", "HP"),
            fill = "toself",
            name = row$Name
        )
        
        trace1 <- list(
            type = "scatterpolar",
            r = c(row$HP, row$Attack, row$Defense, row$`Sp. Atk`, row$`Sp. Def`, row$Speed, row$HP),
            theta = c("HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed", "HP"),
            fill = "toself",
            name = row$Name
        )
        
        data <- list(trace0, trace1)
        
        layout <- list(
            polar = list(
            radialaxis = list(
                visible = TRUE,
                range = c(0, 200)
            )
            ),
            showlegend = TRUE,
            title = paste(trace0$Name, "vs", trace1$Name)
        )
        
        plotly::plot_ly(data = data.frame(data), layout = layout) %>%
            plotly::layout(title = layout$title) %>%
            plotly::config(displayModeBar = FALSE)
        }
    )

    # pokemon_image
    output$pokemon_image <- renderImage({
            if (is.null(selected_pokemons())) {
                return(NULL)
            }
            selected_pokemon_data <- pokemon_data[selected_pokemons(), ]
            name <- selected_pokemon_data$Name
            print(name)
            # return the image from archive/images/images directory
            pokemon_image <- list(src = paste0("archive/images/images/", tolower(name), ".png"),
                                  contentType = "image/png",
                                  width = 256,
                                  height = 256,
                                  alt = "Pokemon Image")
            
            View(pokemon_image)

            return(pokemon_image)
        },
        deleteFile = FALSE
    )

    # violin plot of pokemon stats
    output$violin_plot <- renderPlot({
        if (is.null(selected_pokemons())) {
            return(
                # violin plot for all the pokemons for all the stats
                pokemon_data %>%
                    select(HP, Defense, Attack, Sp..Atk, Sp..Def, Speed) %>%
                    gather(key = "stat", value = "value") %>%
                    ggplot(aes(x = stat, y = value, fill = stat)) +
                    geom_violin() +
                    geom_boxplot(width = 0.1, fill = "white", color = "black") +
                    theme_minimal() +
                    theme(legend.position = "none") +
                    labs(x = "Stat", y = "Value", title = "Pokemon Stats")
            )
        }
        else {
            pokemon_data %>%
                select(HP, Defense, Attack, Sp..Atk, Sp..Def, Speed) %>%
                gather(key = "stat", value = "value") %>%
                ggplot(aes(x = stat, y = value, fill = stat)) +
                geom_violin() +
                geom_boxplot(width = 0.1, fill = "white", color = "black") +
                theme_minimal() +
                theme(legend.position = "none") +
                labs(x = "Stat", y = "Value", title = "Pokemon Stats")
        }
    })
}