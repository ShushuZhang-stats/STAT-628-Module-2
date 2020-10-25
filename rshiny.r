library(shiny)
library(shinydashboard)

# user interface design
ui <- dashboardPage(
  # title
  dashboardHeader(title = "Body Fat Calculator"),
  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Contact Us", tabName = "contact", icon = icon("comment")),
      menuItem("Reference", tabName = "ref", icon = icon("book"))
    )
  ),
  # main body
  dashboardBody(
    #Multiple Tabs
    tabItems(
      #First tab is calculator
      tabItem(
        tabName = "calculator",
        fluidRow(
          # A simple readme
          column(
            width = 12,
            box(
              title = "Read me first",
              status = "primary",
              width = NULL,
              p("This body fat calculator is based on 252 men observations, so the result might be suitable for men compared with women."),
              p("Note: The height information won't count in the calculation, it is just to check you enter reasonable height and weight."),
              p("If you use metric (cm and kg) for your height and weight, you can change unit to imperial/U.S. here."),
              numericInput(
                inputId = "cm",
                label = "Height (cm):",
                value = 170),
              numericInput(
                inputId = "kg",
                label = "Weight (kg):",
                value = 60),
              textOutput("inch"),
              textOutput("lbs")
            )
          ),
          # Input box
          box(
            title = "Enter your information",
            status = "primary",
            sliderInput(
              inputId = "height",
              label = "Height (inches):",
              min = 55,
              max = 80,
              value = 70,
              step = 0.01),
            sliderInput(
              inputId = "weight",
              label = "Weight (lbs):",
              min = 90,
              max = 400,
              value = 170,
              step = 0.01),
            sliderInput(
              inputId = "neck",
              label = "Neck circumference (cm):",
              min = 30,
              max = 60,
              value = 40,
              step = 0.1),
            sliderInput(
              inputId = "abdomen",
              label = "Abdomen 2 circumference (cm):",
              min = 50,
              max = 160,
              value = 90,
              step = 0.1),
            sliderInput(
              inputId = "forearm",
              label = "Forearm circumference (cm):",
              min = 15,
              max = 40,
              value = 30,
              step = 0.1),
            sliderInput(
              inputId = "wrist",
              label = "Wrist circumference (cm):",
              min = 12,
              max = 25,
              value = 18,
              step = 0.1),
            sliderInput(
              inputId = "biceps",
              label = "Biceps (extended) circumference (cm):",
              min = 20,
              max = 50,
              value = 32,
              step = 0.1)
          ),
          
          # Result box 
          box(
            title = "Result",
            textOutput("value")
          ),
          
          # Suggestion box
          box(
            title = "Suggestion",
            htmlOutput("sugg")
          )
        )
      ),
      
      # second tab is contact information
      tabItem(
        tabName = "contact",
        h4("Contact Us"),
        p("This calculator is built by Hao Tong, Yuan Cao and Shushu Zhang, Hao created original app, Yuan updated UI and Shushu updated server."),
        p("If you have any problem when using this calculator, please 
                  feel free to contact us with email."),
        p("Hao Tong (htong25@wisc.edu)"),
        p("Yuan Cao (cao234@wisc.edu)"),
        p("Shushu Zhang (szhang695@wisc.edu)")
      ),
      
      # third tab contains some references
      tabItem(
        tabName = "ref",
        h4("Reference"),
        p("Siri W. E. The gross composition of the body. Advances in Biological and Medical Physics. 1956; 4:239–280."),
        uiOutput("url")
      )
    )
  )
)

# server function
server<-function(input,output){
  # body fat result
  re=reactive({
    -28.81-0.13*input$weight-0.38*input$neck+0.94*input$abdomen+0.23*input$biceps+0.41*input$forearm-1.14*input$wrist
  })
  
  # convert unit
  output$inch=renderText(paste("Your height is ", round(input$cm/2.54,2), "inches.", sep = ""))
  output$lbs=renderText(paste("Your weight is ", round(input$kg*2.2046,2), "lbs.", sep = ""))
  
  # output body fat to user
  output$value=renderText({
    if ((input$weight/input$height)>4 | (input$weight/input$height)<1){ 
      if (re()>0) {
        paste("If you are sure you do not enter wrong information, your body fat is ", re(), "%. But that's abnormal, please pay attention.", sep = "")
      }
      else {
        "Please check your data again, you're probably entering erroneous inputs as the result is not positive."
      }
    }
    else if (re()<=0) {
      "Please check your data again, you're probably entering erroneous inputs as the result is not positive."
    }
    else {
      paste("Your body fat is ", re(), "%.", sep = "")
    }
  })
  
  # judgment of bodyfat value
  ideal=reactive({
    if (re()<6) {
      "Your body even do not have essential fat for basic physical and physiological health!"
    } 
    else if (re()<14){
      "Your body fat is similar as an athlete, that's great!"
    }
    else if (re()<18){
      "Your body fat is at fitness level, that's healthy and please keep your healthy lifestyle."
    }
    else if (re()<25){
      "Your body fat is at an average level and it's good. If you want to be healthier, you can go to gym for training and try to change your eating habit."
    }
    else {
      "Your body fat is higher than average and your body is obese now. Please take some time for exercise, decrease the intake for high-energy or high-fat food."
    }
  })
  
  # suggestion output
  output$sugg=renderUI({
    HTML(paste("The Judgement of body fat percentage below is based on ideal body fat percentage chart from American Council on Exercise (ACE).", ideal(), sep = "<br/><br/>"))
  })
  
  # some reference
  wikiurl=a("Body fat percentage.", href="https://en.wikipedia.org/wiki/Body_fat_percentage")
  output$url=renderUI({
    tagList("Wikipedia page for body fat:", wikiurl)
  })
}

shinyApp(ui=ui,server = server)
