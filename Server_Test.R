# Lade die Shiny-Bibliothek
library(shiny)

# Benutzeroberfläche (UI) der App
ui <- fluidPage(
  
  # Titel der App
  titlePanel("mtcars Dataset Visualisierung"),
  
  # Erste Reihe: Auswahlmenüs, Eingabe für die Anzahl der Zeilen und Tabelle
  fluidRow(
    # Linke Spalte: Auswahlmenüs und Eingabe für die Anzahl der Zeilen (50% Breite)
    column(6,
           selectInput("xvar", "X-Achse:", choices = names(mtcars)),
           selectInput("yvar", "Y-Achse:", choices = sample(names(mtcars))),
           numericInput("rows", "Anzahl der anzuzeigenden Zeilen:", value = 10, min = 1, max = nrow(mtcars))
    ),
    # Rechte Spalte: Tabelle (50% Breite)
    column(6,
           tableOutput("table")
    )
  ),
  
  # Zweite Reihe: Streudiagramm
  fluidRow(
    column(12,
           plotOutput("scatterPlot")
    )
  )
)

# Serverlogik der App
server <- function(input, output) {
  
  # Zeige eine bestimmte Anzahl von Zeilen des mtcars Datasets als Tabelle an
  output$table <- renderTable({
    head(mtcars, n = input$rows)
  })
  
  # Erstelle ein Streudiagramm basierend auf den Benutzerauswahlen
  output$scatterPlot <- renderPlot({
    plot(mtcars[[input$xvar]], mtcars[[input$yvar]],
         xlab = input$xvar,
         ylab = input$yvar,
         main = paste("Streudiagramm von", input$xvar, "gegen", input$yvar))
  })
}

# Starte die Shiny-App
shinyApp(ui = ui, server = server)
