library(shiny)
library(bslib)
library(meta)

ui <- page_sidebar(
  theme = bs_theme(version = 5, "darkly"),

  sidebar = sidebar(
    selectInput("dataset", "Dataset", choices =  c("Olkin1995", "woodyplants")),
    sliderInput("width", "Plot width (pixels)", min = 100, max = 1000, value = 500),
    sliderInput("height", "Plot height (pixels)", min = 400, max = 1000, value = 400)
  ),
  plotOutput("plot")
)

server <- function(input, output) {

  output$plot <- renderPlot({
    if (input$dataset == "Olkin1995"){
      data(Olkin1995)
      m <- metabin(ev.exp, n.exp, ev.cont, n.cont,
                    data = Olkin1995, subset = c(41, 47, 51, 59),
                    sm = "RR", method = "I",
                    studlab = paste(author, year))
    }
    if (input$dataset == "woodyplants"){
      data(woodyplants)
      m <- metacont(n.elev, mean.elev, sd.elev, n.amb, mean.amb, sd.amb,
                     data = woodyplants, sm = "ROM")
    }
    forest(m)
    }, 
    width = \() input$width, 
    height = \() input$height
  )
}

shinyApp(ui = ui, server = server)
