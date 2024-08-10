#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(googlesheets4)

df_raw = read_sheet("1VLyoaRy_jZ0qVGK2dh6DbAulbIhtFJDrjuGdlKG1-Kw", sheet = 'Sheet1')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Seating Chart"),
  
    # Show a plot of the generated distribution
    mainPanel(
      selectInput(
        "block",
        label = "Block",
        choices = c("b3", "b9")
      ),
      textInput(
        "keyword1",
        "Keyword 1: ",
        value = "hi",
        placeholder = "hi"
      ),
      textInput(
        "keyword2",
        "Keyword 2: ",
        value = "bye",
        placeholder = "bye"
      ),
      tableOutput("groups")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$groups <- renderTable({
    
    keywords = c(input$keyword1, input$keyword2)
    block = input$block
    # keywords = c("hello", "zebra")
    # block = 'b9'
    seed = strtoi(keywords, 36L) |> sum()
    seed
    set.seed(seed)
    
    blocks = df_raw
    
    group_size = 3
    students = blocks[[block]]
    students = students[!is.na(students)]
    n = length(students)
    n_groups = floor(n / group_size)
    max_group_size = group_size + (n > n_groups*group_size)
    shuffled_students = sample(students)
    groups = split(
      shuffled_students,
      shuffled_students |> 
        seq_along() |> 
        cut(breaks = n_groups, labels = FALSE)
    ) |> 
      lapply(as.list) |> 
      lapply(`length<-`, max_group_size)
    groups = do.call(tibble, groups) |> 
      unnest(cols = 1:n_groups) |> 
      rename_all(~ str_c("Group ", .))
    
    replacements = rep("", n_groups)
    names(replacements) = str_c("Group ", 1:n_groups)
    replacements = as.list(replacements)
    groups |> 
      replace_na(replacements)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
