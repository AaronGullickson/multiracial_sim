#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(markdown)
library(ggplot2)
library(dplyr)
library(tidyr)
library(nnet)

# Define UI for application that draws a histogram
ui <- page_fillable(
  card(
    card_header("Parameters for Group Inheritance"),
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("first_gen_peak",
                    "First Generation Peak:",
                    min = 0.05,
                    max = 0.95,
                    value = 0.75,
                    step = 0.05),
        sliderInput("second_gen_loss",
                    "Second Generation Loss:",
                    min = 0,
                    max = 1,
                    value = 0.25,
                    step = 0.05),
        sliderInput("second_gen_ratio",
                    "Second Generation Ratio:",
                    min = 1,
                    max = 50,
                    value = 4, 
                    step = 1),
        sliderInput("third_gen_loss",
                    "Third Generation Loss:",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.05),
        sliderInput("third_gen_ratio",
                    "Third Generation Ratio:",
                    min = 1,
                    max = 50,
                    value = 20, 
                    step = 1),
        actionButton("generate", "Generate")
      ),
      accordion(
        accordion_panel(
          "Probability",
          plotOutput("plot")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  get_model <- eventReactive(input$generate, {
    
    # generate data and model and return model
    
    # set up some parameters
    first_gen_peak <- input$first_gen_peak
    second_gen_loss <- input$second_gen_loss
    second_gen_ratio <- input$second_gen_ratio
    third_gen_loss <- input$third_gen_loss
    third_gen_ratio <- input$third_gen_ratio
    second_gen_peak <- first_gen_peak * (1- second_gen_loss)
    second_gen_pref <- (1-second_gen_peak)*second_gen_ratio/(second_gen_ratio+1)
    second_gen_dis <- (1-second_gen_peak)*1/(second_gen_ratio+1)
    third_gen_peak <- second_gen_peak * (1 - third_gen_loss)
    third_gen_pref <- (1-third_gen_peak)*third_gen_ratio/(third_gen_ratio+1)
    third_gen_dis <- (1-third_gen_peak)*1/(third_gen_ratio+1)
    
    # first generation
    race <- sample(c("MR","A","B"), 10000, replace = TRUE, 
                   prob = c(first_gen_peak, (1-first_gen_peak)/2, (1-first_gen_peak)/2))
    sim_data <- tibble(prop_a = 0.5, race)
    
    # second generation
    # prop_a = 0.75
    race <- sample(c("MR","A","B"), 5000, replace = TRUE, 
                   prob = c(second_gen_peak, second_gen_pref, second_gen_dis))
    
    sim_data <- tibble(prop_a = 0.75, race) |>
      bind_rows(sim_data)
   
    # prop_a = 0.25
    race <- sample(c("MR","A","B"), 5000, replace = TRUE, 
                   prob = c(second_gen_peak, second_gen_dis, second_gen_pref))
    
    sim_data <- tibble(prop_a = 0.25, race) |>
      bind_rows(sim_data)
    
    # third generation
    # prop_a = 0.875
    race <- sample(c("MR","A","B"), 5000, replace = TRUE, 
                   prob = c(third_gen_peak, third_gen_pref, third_gen_dis))
    
    sim_data <- tibble(prop_a = 0.875, race) |>
      bind_rows(sim_data)
    
    # prop_a = 0.125
    race <- sample(c("MR","A","B"), 5000, replace = TRUE, 
                   prob = c(third_gen_peak, third_gen_dis, third_gen_pref))
    
    sim_data <- tibble(prop_a = 0.125, race) |>
      bind_rows(sim_data)
    
    sim_data <- sim_data |>
      mutate(race = factor(race, levels = c("MR", "A", "B")))
    
    model <- multinom(race ~ prop_a+I(prop_a^2)+I(prop_a^3), data = sim_data)
    
    
    
    return(model)
    
  })
  
  output$plot <- renderPlot({
    
    model <- get_model()
    
    pred_data <- tibble(prop_a = seq(from = 0.01, to = 0.99, by =0.005))
    df_pred <- predict(model, newdata = pred_data,
                       type = "probs") |>
      as_tibble() |> 
      mutate(prop_a = seq(from = 0.01, to = 0.99, by =0.005)) |> 
      pivot_longer(cols = c(MR, A, B), names_to = "group", values_to = "prop")
    
    # set up some parameters
    first_gen_peak <- input$first_gen_peak
    second_gen_loss <- input$second_gen_loss
    second_gen_ratio <- input$second_gen_ratio
    third_gen_loss <- input$third_gen_loss
    third_gen_ratio <- input$third_gen_ratio
    second_gen_peak <- first_gen_peak * (1- second_gen_loss)
    second_gen_pref <- (1-second_gen_peak)*second_gen_ratio/(second_gen_ratio+1)
    second_gen_dis <- (1-second_gen_peak)*1/(second_gen_ratio+1)
    third_gen_peak <- second_gen_peak * (1 - third_gen_loss)
    third_gen_pref <- (1-third_gen_peak)*third_gen_ratio/(third_gen_ratio+1)
    third_gen_dis <- (1-third_gen_peak)*1/(third_gen_ratio+1)
    
    point_pred <- tibble(prop_a = rep(c(0.125, 0.25, 0.5, 0.75, 0.875), 3),
                         group = rep(c("MR", "A", "B"), each = 5),
                         prop = c(third_gen_peak, second_gen_peak, first_gen_peak,
                                  second_gen_peak, third_gen_peak,
                                  third_gen_dis, second_gen_dis, 
                                  (1-first_gen_peak)/2, second_gen_pref, 
                                  third_gen_pref,
                                  third_gen_pref, second_gen_pref, 
                                  (1-first_gen_peak)/2, second_gen_dis, 
                                  third_gen_dis))
    
    ggplot(df_pred, aes(x = prop_a, y = prop, group = group, color = group))+
      geom_line(size = 1.5, alpha = 0.6)+
      geom_point(data = point_pred)+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(x = "proportion of ancestry from group A",
           y = "probability of being classified with given group")+
      theme_bw()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
