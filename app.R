#Load Packages
library(shiny)
library(tidyverse)

#load Data
salary = read_csv(file = "data/salary.csv")

ui =   navbarPage(
  title = "Data Scientist skills and salaries",
  # Bar Graph
  tabPanel(title = "Bar View",
           titlePanel(title = "Most Demanded Skills"),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "sect",
                           label = "Sectors",
                           choices = sort(unique(salary$Sector)),
                           selected = "Information Technology"),
               sliderInput(inputId = "sal",
                           label = "Salary Range",
                           min = 50.0,
                           max = 255.0,
                           value = c(80.0, 120.0)
                           )
             ),
             mainPanel(plotOutput("plot"))
           )),
  # Table
  tabPanel(title = "Table View", 
           titlePanel(title = "Sector filtered table view"),
           dataTableOutput("table")),
  tabPanel(title = "Insights", includeMarkdown("insights.Rmd")),
  tabPanel(title = "About", includeMarkdown("about.Rmd"))
)

server = function(input, output) {
  
  salary_sect = reactive({
    salary |>
      filter(Sector == input$sect & Sector != -1)
  })

  observeEvent(
    eventExpr = input$sect,
    handlerExpr = {
      updateSelectInput(inputId = "loc",
                        choices = sort(unique(salary_sect()$location)))
    }
  )
  
  salary_sect_loc = reactive({
    salary_sect() |>
      filter(location == input$loc)
  })
  
  observeEvent(
    eventExpr = input$loc,
    handlerExpr = {
      updateSliderInput(inputId = "sal",
                        min = min(salary_sect_loc()$avgsalary),
                        max = max(salary_sect_loc()$avgsalary),
                        value = c(min, max))
    }
  )  
  
  output$plot = renderPlot(
    {
      salary |>
        filter(Sector != -1) |>
        filter(Sector == input$sect) |>
        filter(avgsalary >= input$sal[1]) |>
        filter(avgsalary <= input$sal[2]) |>
        pivot_longer(Python:google_an, names_to = "Skills", values_to = "Count") |>
        group_by(Skills) |>
        summarise(Count = sum(Count)) |>
        ggplot() +
        aes(x = Count, y = Skills, fill = Skills) |>
        geom_bar(stat = "identity") + theme_bw()
    }
  )
   
  output$table = renderDataTable(
    {
      salary_sect() |>
        rename(company = company_txt) |>
        select(-(Python:google_an)) |>
        sort_function()
    }
  )
   
}

shinyApp(ui = ui, server = server)
