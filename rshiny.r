library(shiny)

ui <- fluidPage(
  
  titlePanel("Body Fat Prediction"),
  
  sidebarLayout(position = "right",
    mainPanel(
      textOutput("value")
    ),
    
    sidebarPanel(
      h3("Enter your information"),
      sliderInput(inputId = "weight",
                  label = "Weight (lbs):",
                  min = 90,
                  max = 400,
                  value = 170),
      sliderInput(inputId = "neck",
                  label = "Neck circumference (cm):",
                  min = 30,
                  max = 60,
                  value = 40),
      sliderInput(inputId = "abdomen",
                  label = "Abdomen 2 circumference (cm):",
                  min = 50,
                  max = 160,
                  value = 90),
      sliderInput(inputId = "forearm",
                  label = "Forearm circumference (cm):",
                  min = 15,
                  max = 40,
                  value = 30),
      sliderInput(inputId = "wrist",
                  label = "Wrist circumference (cm):",
                  min = 12,
                  max = 25,
                  value = 18),
      br(),
      br(),
      h4("Contact Us"),
      p("If you have any questions about the result, please feel free to contact us with email."),
      p("Hao Tong ()"),
      p("Yuan Cao (cao234@wisc.edu)"),
      p("Shushu Zhang ()")
    )
  )
)

server<-function(input,output){
  output$value=renderText({
    paste("Your body fat is ", 
          as.character(-24.42-0.12*input$weight-0.34*input$neck+0.95*input$abdomen+0.58*input$forearm-1.42*input$wrist), "%.", sep = "")
  })
}

shinyApp(ui=ui,server = server)
