library(forcats)
library(fmsb)
library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(tidyr)
library(treemap)

function(input, output, session) {
    pokemon_data <- read.csv("data/Pokemon.csv")
    
    selected_pokemons <- reactiveVal(pokemon_data[1, ]$Name)
    selected_type <- reactiveVal(NULL)
    
    View(selected_pokemons())
    
    observeEvent(input$show_pokemon_data_rows_selected, {
        selected_pokemons(input$show_pokemon_data_rows_selected)
    })

    #custom colors for each pokemon type
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

    output$show_pokemon_data <- DT::renderDataTable({
        DT::datatable(data.frame(pokemon_data$Name),
            options = list(scrollY = 200,
                pageLength = 80,
                lengthChange = FALSE,
                info = FALSE,
                colnames = "Pokemon Name",
                columnDefs = list(list(className = "dt-center", targets = "_all"))),
            selection = list(mode = "single", target = "row"),
            rownames = FALSE)
    })
    
    output$put_image <- renderImage({
        put_image <- list(src = "logo.png",
                              contentType = "image/png",
                              width = 300,
                              height = 80,
                              alt = "Pokemon Image")
    })

    output$pokemon_image <- renderImage({
        pokemon_image <- list(src = paste0("data/images/", tolower(pokemon_data[selected_pokemons(), ]$Name), ".png"),
                              contentType = "image/png",
                              width = 256,
                              height = 256,
                              alt = "Pokemon Image")
    })

    output$plot_title <- renderUI({
        HTML(paste0("<h3>", pokemon_data[selected_pokemons(), ]$Name, "</h3>"))
    })

    output$radar_chart <- renderPlot({
        row <- pokemon_data[selected_pokemons(), ] %>%
            select(HP, Defense, Attack, Sp..Atk, Sp..Def, Speed)
        row <- rbind(rep(255, 6), rep(0, 6), row)
        radarchart(
            row, axistype = 1,
            cglcol = "grey", cglty = 1, axislabcol = "grey", plwd = 3, plty = 1,
            caxislabels = c(0, 50, 100, 150, 200, 250), cglwd = 0.8, vlcex = 0.8,
            pfcol = scales::alpha(pokemon_colors[pokemon_data[selected_pokemons(), ]$Type.1], 0.5),
            pcol = pokemon_colors[pokemon_data[selected_pokemons(), ]$Type.1],
            title = pokemon_data[selected_pokemons(), ]$Name,
        )
    })

    output$interactive_treemap <- renderPlotly({
        pokemon_data_treemap <- pokemon_data %>%
            group_by(Type.1, Type.2) %>%
            summarise(count = n()) %>%
            mutate(Type.2 = ifelse(Type.2 == "", Type.1, Type.2)) %>%
            mutate(Type.1 = ifelse(Type.2 == Type.1, "", Type.1)) %>%
            rename(label = Type.2, parent = Type.1) %>%
            mutate(label = ifelse(parent == "", label, paste(parent, label, sep = " - ")))
        
        plot <- pokemon_data_treemap %>%
            plot_ly(
                labels = ~label,
                parents = ~parent,
                values = ~count,
                type = "treemap",
                colors = pokemon_colors,
                domain = list(column = 0),
                hovertemplate = paste(
                    "<b>%{label}</b><br>",
                    "Count: %{value}<br>"
                )
            )
        plot <- event_register(plot, "plotly_click")
        plot
    })
    
    observeEvent(event_data("plotly_click", source = "interactive_treemap"), {
        selected_label <- event_data("plotly_click", source = "interactive_treemap")$label
        if (selected_label == selected_type()) {
            selected_type(NULL)
        } else {
            selected_type(selected_label)
        }
    })

    output$violin_plot <- renderPlotly({
        if (!is.null(selected_type())) {
            filtered_data <- pokemon_data %>%
                filter(Type.1 == selected_type() | Type.2 == selected_type())
        } else {
            filtered_data <- pokemon_data
        }
        filtered_data %>%
            select(HP, Defense, Attack, Sp..Atk, Sp..Def, Speed) %>%
            gather(key = "stat", value = "value") %>%
            ggplot(aes(x = stat, y = value, fill = stat)) +
            geom_violin() +
            geom_boxplot(width = 0.1, fill = "white", color = "black") +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Stat", y = "Value", title = "Pokemon Stats")
    })

    # contour plot for selected type of pokemon
    output$contour_plot <- renderPlotly({
        if (!is.null(selected_type())) {
            filtered_data <- pokemon_data %>%
                filter(Type.1 == selected_type() | Type.2 == selected_type())
        } else {
            filtered_data <- pokemon_data
        }
        filtered_data %>%
            ggplot(aes(x = Attack, y = Defense)) +
            geom_density2d() +
            geom_point(alpha = 0.1) +
            theme_minimal() +
            labs(x = "Attack", y = "Defense", title = "Pokemon Attack vs Defense")
    })
}