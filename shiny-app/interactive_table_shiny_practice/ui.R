ui <- fluidPage(
  mainPanel(
    rHandsontableOutput("myTable"),
    textOutput("test"),
    textOutput("math_check")
  )
)