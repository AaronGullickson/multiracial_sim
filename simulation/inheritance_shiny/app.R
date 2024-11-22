#
# Shiny app to visualize how group inheritance parameters lead to different
# scenarios

library(shiny)
library(bslib)
library(markdown)
library(ggplot2)
library(dplyr)
library(tidyr)

# chosen from Color Buddy
my_palette <- c("#a91610", "#005864", "#fdcc00", "#4d8900", "#ce6a00")

# Define UI for application
ui <- page_fillable(
  card(
    card_header("Parameters for Group Inheritance"),
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("group1_intercept",
                    "Group 1 Intercept:",
                    min = -20,
                    max = 20,
                    value = 3),
        sliderInput("group1_slope",
                    "Group 1 Slope:",
                    min = 0,
                    max = 15,
                    value = 3),
        sliderInput("group2_intercept",
                    "Group 2 Intercept:",
                    min = -20,
                    max = 20,
                    value = 10),
        sliderInput("group2_slope",
                    "Group 2 Slope:",#
                    min = -15,
                    max = 0,
                    value = -3)
      ),
        accordion(
          accordion_panel(
            "Probability",
            plotOutput("probPlot")
          ),
          accordion_panel(
            "Cumulative Probability",
            plotOutput("cumProbPlot")
          )
        )
    )
  ),
  
  card(
    card_header("Instructions"),
    includeMarkdown("text/instructions.md")
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$probPlot <- renderPlot({
      
      tibble(
        prop_group1 = seq(from = 0, to = 1, by = .05),
        logodds1 = input$group1_intercept+input$group1_slope*prop_group1,
        logodds2 = input$group2_intercept+input$group2_slope*prop_group1,
        logodds3 = 0,
        group1 = round(exp(logodds1)/(exp(logodds1)+exp(logodds2)+exp(logodds3)), 4),
        group2 = round(exp(logodds2)/(exp(logodds1)+exp(logodds2)+exp(logodds3)), 4),
        group3 = round(exp(logodds3)/(exp(logodds1)+exp(logodds2)+exp(logodds3)), 4)
      ) |>
        select(prop_group1, starts_with("group")) |>
        pivot_longer(cols = starts_with("group"), 
                     names_to = "group", values_to = "prob") |>
        mutate(group = factor(group, 
                              levels = c("group1", "group2", "group3"),
                              labels =c ("Group 1", "Group 2", "Mixed Group"))) |>
      ggplot(aes(x = prop_group1, y = prob, color = group, group = group))+
        geom_line(size=1.5, alpha = 0.6)+
        scale_y_continuous(labels = scales::percent)+
        scale_color_manual(values = my_palette)+
        labs(x = "proportion of ancestry from group 1",
             y = "probability of being classified with given group")+
        theme_bw()
      
      
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #     xlab = 'Waiting time to next eruption (in mins)',
        #     main = 'Histogram of waiting times')
    })
    
    output$cumProbPlot <- renderPlot({
      
      tibble(
        prop_group1 = seq(from = 0, to = 1, by = .05),
        logodds1 = input$group1_intercept+input$group1_slope*prop_group1,
        logodds2 = input$group2_intercept+input$group2_slope*prop_group1,
        logodds3 = 0,
        group1 = round(exp(logodds1)/(exp(logodds1)+exp(logodds2)+exp(logodds3)), 4),
        group2 = round(exp(logodds2)/(exp(logodds1)+exp(logodds2)+exp(logodds3)), 4),
        group3 = round(exp(logodds3)/(exp(logodds1)+exp(logodds2)+exp(logodds3)), 4)
      ) |>
        select(prop_group1, starts_with("group")) |>
        pivot_longer(cols = starts_with("group"), 
                     names_to = "group", values_to = "prob") |>
        mutate(group = factor(group, 
                              levels = c("group1", "group2", "group3"),
                              labels =c ("Group 1", "Group 2", "Mixed Group"))) |>
        ggplot(aes(x = prop_group1, y = prob, fill = group, group = group))+
        geom_area()+
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual(values = my_palette)+
        labs(x = "proportion of ancestry from group 1",
             y = "probability of being classified with given group")+
        theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
