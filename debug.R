library(shiny)
library(bslib)

ui <- page_fluid(
  actionButton("add", "Add 'Dynamic' tab"),
  actionButton("remove", "Remove 'Foo' tab"),
  navset_tab(
    id = "tabs",
    nav_panel("Hello", "hello"),
    nav_panel("Foo", "foo"),
    nav_panel("Bar", "bar tab")
  )
)
server <- function(input, output) {
  observeEvent(input$add, {
    nav_insert(
      "tabs", target = "Bar", select = TRUE,
      nav_panel("Dynamic", "Dynamically added content")
    )
  })
  observeEvent(input$remove, {
    nav_remove("tabs", target = "Foo")
  })
}
shinyApp(ui, server)
