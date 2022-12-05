library(shiny)
library(tidyverse)
library(ggplot2)
library(viridis)
library(shinythemes)

footy = read.csv("project_3_footy_dataset.csv") %>%
  select(Age, Goals, Nation, TotalMinutes, AnnualWages, Continent)

parameter_tabs = tabsetPanel(
  id = "params",
  type = "hidden",
  
  tabPanel("numeric", 
           selectInput("nvar", 
                       label = "Select numeric variable:", 
                       choices = c("Age", "Goals", "TotalMinutes", "AnnualWages"),           
                       selected = "Age"), 
           sliderInput("bins",
                       label = "Select number of bins:",
                       min = 1, 
                       max = 50, 
                       value = 25), 
           selectInput("binColor",
                       label = "Select bin color:", 
                       choices = colors(), 
                       selected = "darkolivegreen"), 
           checkboxInput("showMean", 
                         label = "Display mean on plot:", 
                         value = FALSE), 
           checkboxInput("showMedian", 
                         label = "Display median on plot:", 
                         value = FALSE)), 
  tabPanel("categorical", 
           selectInput("cvar", 
                       label = "Select categorical variable:", 
                       choices = c("Continent", "Nation"), 
                       selected = "Continent"))
)

ui = fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel(title = strong("Premier League Player Statistics"),
             windowTitle = "Player Statistics"), 
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualising Premier League statistics from the 2022/23 season"),
      selectInput("variable", 
                  label = "Select variable type:", 
                  choices = c("numeric", "categorical")), 
      parameter_tabs,
      hr(),
      helpText(strong("Variable descriptions")),
      tags$style(HTML("table, th, td { border: 1px solid black; }")),
      tags$style(HTML("th, td { padding: 5px; }")),
      tags$table(
        tags$tr(
          tags$th("Variable"),
          tags$th("Description")
        ),
        tags$tr(
          tags$td("Age"),
          tags$td("Player age in years")
        ),
        tags$tr(
          tags$td("Goals"),
          tags$td("Goals scored by player")
        ),
        tags$tr(
          tags$td("TotalMinutes"),
          tags$td("Total number of minutes played")
        ),
        tags$tr(
          tags$td("AnnualWages"),
          tags$td("Player's annual salary in pounds")
        ),
        tags$tr(
          tags$td("Continent"),
          tags$td("Continent player is from")
        ),
        tags$tr(
          tags$td("Nation"),
          tags$td("Player Nationality")
        )
      ),
    ),
    mainPanel(
      h3(strong("Your Output")),
      plotOutput("plot"), 
      h4(strong("Summary Statistics")),
      tableOutput("stats")
    )
  )
)

server = function(input, output) {
  observeEvent(input$variable, {
    updateTabsetPanel(inputId = "params", 
                      selected = input$variable)
  })
  
  sample = reactive({
    switch(input$variable,
           numeric = ggplot(footy) + 
             geom_histogram(aes(x = eval(parse(text = input$nvar))), 
                            bins = input$bins, 
                            fill = input$binColor, 
                            color = "black") + 
             labs(title = paste("Distribution of", input$nvar), 
                  x = input$nvar, 
                  y = "Frequency") + 
             theme(plot.title = element_text(size = 18)) +
             {if (input$showMean) {
               geom_vline(aes(xintercept = mean(eval(parse(text = input$nvar)), na.rm = T)),
                          color = "red", 
                          linetype = "dashed")}} + 
             {if (input$showMedian) {
               geom_vline(aes(xintercept = median(eval(parse(text = input$nvar)), na.rm = T)),
                          color = "blue", 
                          linetype = "dashed")}},
           
           categorical = ggplot(footy) + 
             geom_bar(aes(x = eval(parse(text = input$cvar)), 
                          fill = eval(parse(text = input$cvar))), 
                      color = "black") + 
             labs(title = paste("Barplot of", input$cvar), 
                  x = input$cvar, 
                  y = "Frequency") + 
             scale_fill_viridis(discrete = TRUE) + 
             theme(legend.position = "none") + 
             theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
             theme(plot.title = element_text(size = 19)))
  })
    output$plot = renderPlot(sample())
    
    output$stats = renderTable({
      if (input$params == "numeric") {
        footy %>% 
          summarise(Mean = mean(eval(parse(text = input$nvar)), na.rm = T), 
                    Median = median(eval(parse(text = input$nvar)), na.rm = T),
                    `Standard Deviation` = sd(eval(parse(text = input$nvar)), na.rm = T), 
                    `First Quartile` = quantile(eval(parse(text = input$nvar)), na.rm = T)[1], 
                    `Third Quartile` = quantile(eval(parse(text = input$nvar)), na.rm = T)[3])
      }
      else if (input$params == "categorical") {
        as.data.frame(prop.table(table(select(footy, input$cvar)))) %>%
          rename(Proportion = 2) 
      }
    })
}

shinyApp(ui = ui, server = server)
