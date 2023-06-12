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

    # load pokemon data and fill in missing type 2 with type 1
    pokemon_data <- read.csv("~/GitHub/data_visualization/interactive_dashboard/Pokemon.csv")

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

    # find the min and max values for each stat
    min_max <- pokemon_data %>%
        select(Attack, Defense, Sp..Atk, Sp..Def, Speed) %>%
        gather(key = "stat", value = "value") %>%
        group_by(stat) %>%
        summarise(min = min(value), max = max(value))

    # Create a reactive values to store the selected pokemon and selected type
    selected_pokemons <- reactiveVal(NULL)
    selected_type <- reactiveVal(NULL)

    # Show datatable of pokemon_names with vertical scrolling get rid of the bottom description
    output$show_pokemon_data <- DT::renderDataTable({
        DT::datatable(data.frame(pokemon_data$Name),
            options = list(scrollY = 200,
                pageLength = 10,
                lengthChange = FALSE,
                info = FALSE,
                colnames = "Pokemon Name",
                columnDefs = list(list(className = "dt-center", targets = "_all"))),
            selection = list(mode = "single", target = "row"),
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
        pokemon_data %>%
            group_by(Type.1, Type.2) %>%
            summarise(count = n()) %>%
            mutate(
                Type.1 = fct_reorder(Type.1, count),
                Type.2 = fct_reorder(Type.2, count)
            ) %>%
            treemap(index = c("Type.1", "Type.2"), vSize = "count", title = "pokemon type distribution", palette = pokemon_colors)
    })

    # interactive treemap of pokemon types without using treemap package
    output$interactive_treemap <- renderPlotly({
        pokemon_data_treemap <- pokemon_data %>%
            group_by(Type.1, Type.2) %>%
            summarise(count = n())
        
        # if Type.2 is blank, then swap Type.1 places of Type.2
        pokemon_data_treemap <- pokemon_data_treemap %>%
            mutate(Type.2 = ifelse(Type.2 == "", Type.1, Type.2)) %>%
            mutate(Type.1 = ifelse(Type.2 == Type.1, "", Type.1))
        
        # change name of Type.1 and Type.2 to label and parent
        pokemon_data_treemap <- pokemon_data_treemap %>%
            rename(label = Type.2, parent = Type.1)
        
        # change label to parent + label if parent is not blank
        pokemon_data_treemap <- pokemon_data_treemap %>%
            mutate(label = ifelse(parent == "", label, paste(parent, label, sep = " - ")))

        # Create an event handler for treemap click event
        event_data <- event_data("plotly_click", source = "interactive_treemap")

        if (!is.null(event_data)) {
            # Get the selected label (primary_pokemon_type) from the clicked part
            selected_label <- event_data[["x"]]
            selected_type(selected_label)
        }

        pokemon_data_treemap %>%
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
    })

    # pokemon_image
    output$pokemon_image <- renderImage({
            if (is.null(selected_pokemons())) {
                return(NULL)
            }
            selected_pokemon_data <- pokemon_data[selected_pokemons(), ]
            name <- selected_pokemon_data$Name
            # return the image from archive/images/images directory
            pokemon_image <- list(src = paste0("archive/images/images/", tolower(name), ".png"),
                                  contentType = "image/png",
                                  width = 256,
                                  height = 256,
                                  alt = "Pokemon Image")
            return(pokemon_image)
        },
        deleteFile = FALSE
    )

    # violin plot of pokemon stats
    output$violin_plot <- renderPlotly({
    if (is.null(selected_type())) {
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
    } else {
        filtered_data <- pokemon_data %>%
            filter(Type.1 == selected_type() | Type.2 == selected_type())

        filtered_data %>%
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