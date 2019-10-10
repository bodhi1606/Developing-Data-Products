if(!("shiny" %in% rownames(installed.packages()))) {
    install.packages("shiny")
}
library(shiny)

if(!("markdown" %in% rownames(installed.packages()))) {
    install.packages("markdown")
}
library(markdown)

shinyUI(
    navbarPage("Iris Species Predictor",
               tabPanel("Use the App",
                        sidebarPanel(
                            width = 8,
                            
                            h3("Your choices (red lines) against iris dataset values."),
                            
                            fluidRow(
                                splitLayout(cellWidths = c("50%", "50%"),
                                            plotOutput("plotSepalWidth", height = "180px"),
                                            plotOutput("plotSepalLength", height = "180px")
                                )),
                            
                            fluidRow(
                                splitLayout(cellWidths = c("50%", "50%"),
                                            sliderInput("sepalWidth", 
                                                        "Sepal Width:", 
                                                        min = round(min(iris$Sepal.Width) / 2, 1),
                                                        max = round(max(iris$Sepal.Width) * 1.25, 1),
                                                        value = round(mean(iris$Sepal.Width), 1)),
                                            sliderInput("sepalLength", 
                                                        "Sepal Length:",
                                                        min = round(min(iris$Sepal.Length) / 2, 1),
                                                        max = round(max(iris$Sepal.Length) * 1.25, 1),
                                                        value = round(mean(iris$Sepal.Length), 1))
                                )),
                            
                            fluidRow(
                                splitLayout(cellWidths = c("50%", "50%"),
                                            plotOutput("plotPetalWidth", height = "180px"),
                                            plotOutput("plotPetalLength", height = "180px")
                                )),
                            
                            fluidRow(
                                splitLayout(cellWidths = c("50%", "50%"),
                                            sliderInput("petalWidth", 
                                                        "Petal Width:", 
                                                        min = round(min(iris$Petal.Width) / 2, 1),
                                                        max = round(max(iris$Petal.Width) * 1.25, 1),
                                                        value = round(mean(iris$Petal.Width), 1)),
                                            sliderInput("petalLength", 
                                                        "Petal Length:",
                                                        min = round(min(iris$Petal.Length) / 2, 1),
                                                        max = round(max(iris$Petal.Length) * 1.25, 1),
                                                        value = round(mean(iris$Petal.Length ), 1))
                                )),
                            
                            actionButton(
                                inputId = "submitBtn",
                                label = "Classify My Iris"
                            ),
                            
                            actionButton(
                                inputId = "resetBtn",
                                label = "Reset"
                            )
                        ),
                        
                        mainPanel(
                            width = 4,
                            tabsetPanel(
                                tabPanel(p(icon("table"), "Classify Your Iris"),                             
                                         h1("Prediction"),
                                         h3("Shown as probabilities for each species."),
                                         p("Choose your measurements and hit 'Classify My Iris'."),
                                         tableOutput("prediction")
                                ),
                                
                                tabPanel(p(icon("table"), "Prediction Model"),
                                         
                                         h1("Prediction Model"),
                                         p("Predictions are based on a random forests model trained against the complete iris dataset. For more details view source on GitHub:"),
                                         a(href = "http://github.com",
                                           "http://github.com",
                                           target = "_blank"),
                                         
                                         h2("Dataset"),
                                         p("The prediction model was trained on the Iris Dataset:"),
                                         a(href = "https://en.wikipedia.org/wiki/Iris_flower_data_set",
                                           "https://en.wikipedia.org/wiki/Iris_flower_data_set",
                                           target = "_blank"),
                                         h3("str(iris)"),
                                         tableOutput("oStr")
                                )
                            )
                        )
               )
               
              
               
    )
)