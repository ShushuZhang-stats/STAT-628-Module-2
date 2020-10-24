library(shiny)

ui <- fluidPage(
  
  titlePanel("Body Fat Prediction"),
  
  sidebarLayout(position = "right",
                mainPanel(
                  textOutput("value")
                ),
                
                sidebarPanel(
                  h3("Enter your information"),
                  sliderInput(inputId = "age",
                              label = "Age (years):",
                              min = 20,
                              max = 80,
                              value = 35),
                  sliderInput(inputId = "height",
                              label = "Height (inches):",
                              min = 55,
                              max = 80,
                              value = 70),
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
                  sliderInput(inputId = "biceps",
                              label = "Biceps (extended) circumference (cm):",
                              min = 20,
                              max = 40,
                              value = 30),
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
                  p("Hao Tong (htong25@wisc.edu)"),
                  p("Yuan Cao (cao234@wisc.edu)"),
                  p("Shushu Zhang ()")
                )
  )
)

server<-function(input,output){
  output$value=renderText({
    if ((input$weight/input$height)>4 | (input$weight/input$height)<1)
    { 
      print("Check your data again, you're probably entering erroneous inputs.")
    }
    else {
    print(
      paste("Your body fat is ", 
            as.character(-28.81-0.13*input$weight-0.38*input$neck+0.94*input$abdomen+0.23*input$biceps+0.41*input$forearm-1.14*input$wrist), "%.", sep = "")
      )
    }
  })
}

shinyApp(ui=ui,server = server)
