library(shiny)

ui <- fluidPage(
  
  titlePanel("Body Fat Prediction"),
  
  sidebarLayout(position = "right",
    sidebarPanel(
      h4("Contact Us"),
      p("If you have any questions about the result, please feel free to contact us with email."),
      p("cao234@wisc.edu"),
    ),
    
    mainPanel(
      sliderInput(inputId = "weight",
                  label = "Weight(lbs):",
                  min = 100,
                  max = 300,
                  value = 200),
      sliderInput(inputId = "neck",
                  label = "Neck circumference(cm):",
                  min = 30,
                  max = 55,
                  value = 40),
      sliderInput(inputId = "abdomen",
                  label = "Abdomen 2 circumference(cm):",
                  min = 50,
                  max = 150,
                  value = 90),
      sliderInput(inputId = "forearm",
                  label = "Forearm circumference(cm):",
                  min = 20,
                  max = 40,
                  value = 30),
      sliderInput(inputId = "wrist",
                  label = "Wrist circumference(cm):",
                  min = 12,
                  max = 25,
                  value = 18),
      verbatimTextOutput("value")
    )
  )
)

server<-function(input,output){
  output$value=renderText({
    paste("Your body fat is", 
          as.character(-24.42-0.12*input$weight-0.34*input$neck+0.95*input$abdomen+0.58*input$forearm-1.42*input$wrist))
  })
}

shinyApp(ui=ui,server = server)
