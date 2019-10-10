if(!("shiny" %in% rownames(installed.packages()))) {
    install.packages("shiny")
}
library(shiny)

if(!("lattice" %in% rownames(installed.packages()))) {
    install.packages("lattice")
}
library(lattice)

if(!("ggplot2" %in% rownames(installed.packages()))) {
    install.packages("ggplot2")
}
library(ggplot2)

if(!("e1071" %in% rownames(installed.packages()))) {
    install.packages("e1071")
}
library(e1071)

if(!("caret" %in% rownames(installed.packages()))) {
    install.packages("caret")
}
library(caret)

if(!("randomForest" %in% rownames(installed.packages()))) {
    install.packages("randomForest")
}
library(randomForest)

#####
# Random Forest classificator
#####
set.seed(777)

buildRFModel <- function() {
    fitControl <- trainControl(method = "cv", number = 5)
    fitRF <- train(Species ~ ., data = iris,
                   method = "rf",
                   trControl = fitControl)
    print(timestamp())
    return(fitRF)
}

predictIris <- function(trainedModel, inputs) {
    prediction <- predict(trainedModel,
                          newdata = inputs,
                          type = "prob",
                          predict.all = TRUE)
    #return(renderText(levels(iris$Species)[prediction]))
    return(renderTable(prediction))
}

shinyServer(
    function(input, output, session) {
        
        data(iris)
        
        myStr <- capture.output(str(iris))
        myStr <- paste(myStr, collapse = "<br/>")
        output$oStr <- renderText(myStr)
        
        output$outputSepalWidth <- renderText(input$sepalWidth)
        output$outputSepalLength <- renderText(input$sepalLength)
        output$outputPetalWidth <- renderText(input$petalWidth)
        output$outputPetalLength <- renderText(input$petalLength)
        
        output$outputSepalWidthSD <- renderText(sd(iris$Sepal.Width))
        output$outputSepalLengthSD <- renderText(sd(iris$Sepal.Length))
        output$outputPetalWidthSD <- renderText(sd(iris$Petal.Width))
        output$outputPetalLengthSD <- renderText(sd(iris$Petal.Length))
        
        output$outputSepalWidthMean <- renderText(mean(iris$Sepal.Width))
        output$outputSepalLengthMean <- renderText(mean(iris$Sepal.Length))
        output$outputPetalWidthMean <- renderText(mean(iris$Petal.Width))
        output$outputPetalLengthMean <- renderText(mean(iris$Petal.Length))
        
        output$plotSepalWidth <- renderPlot({
            ggplot(iris, aes(x = Sepal.Width,
                             group = Species,
                             fill = as.factor(Species))) + 
                geom_density(position = "identity", alpha = 0.5) +
                scale_fill_discrete(name = "Species") +
                theme_bw() +
                xlab("Sepal Width") +
                geom_vline(xintercept = input$sepalWidth,
                           color = "red",
                           size = 2) +
                scale_x_continuous(limits = c(round(min(iris$Sepal.Width) / 2, 1),
                                              round(max(iris$Sepal.Width) * 1.25, 1)))
            
        })
        
        output$plotSepalLength <- renderPlot({
            ggplot(iris, aes(x = Sepal.Length,
                             group = Species,
                             fill = as.factor(Species))) + 
                geom_density(position = "identity", alpha = 0.5) +
                scale_fill_discrete(name = "Species") +
                theme_bw() +
                xlab("Sepal Length") +
                geom_vline(xintercept = input$sepalLength,
                           color = "red",
                           size = 2) +
                scale_x_continuous(limits = c(round(min(iris$Sepal.Length) / 2, 1),
                                              round(max(iris$Sepal.Length) * 1.25, 1)))
            
        })
        
        output$plotPetalWidth <- renderPlot({
            ggplot(iris, aes(x = Petal.Width,
                             group = Species,
                             fill = as.factor(Species))) + 
                geom_density(position = "identity", alpha = 0.5) +
                scale_fill_discrete(name = "Species") +
                theme_bw() +
                xlab("Petal Width") +
                geom_vline(xintercept = input$petalWidth,
                           color = "red",
                           size = 2) +
                scale_x_continuous(limits = c(round(min(iris$Petal.Width) / 2, 1),
                                              round(max(iris$Petal.Width) * 1.25, 1)))
            
        })
        
        output$plotPetalLength <- renderPlot({
            ggplot(iris, aes(x = Petal.Length,
                             group = Species,
                             fill = as.factor(Species))) + 
                geom_density(position = "identity", alpha = 0.5) +
                scale_fill_discrete(name = "Species") +
                theme_bw() +
                xlab("Petal Length") +
                geom_vline(xintercept = input$petalLength,
                           color = "red",
                           size = 2) +
                scale_x_continuous(limits = c(round(min(iris$Petal.Length) / 2, 1),
                                              round(max(iris$Petal.Length) * 1.25, 1)))
            
        })
        
        builtModel <- reactive({
            buildRFModel()
        })
        
        observeEvent(
            eventExpr = input[["submitBtn"]],
            handlerExpr = {
                withProgress(message = 'Just a moment...', value = 0, {
                    myModel <- builtModel()
                })
                Sepal.Length <- input$sepalLength
                Sepal.Width <- input$sepalWidth
                Petal.Length <- input$petalLength
                Petal.Width <- input$petalWidth
                myEntry <- data.frame(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
                
                myPrediction <- predictIris(myModel, myEntry)
                output$prediction <- myPrediction
            })
        
        observeEvent(input[["resetBtn"]], {
            updateNumericInput(session, "sepalWidth", value = round(mean(iris$Sepal.Width), 1))
            updateNumericInput(session, "sepalLength", value = round(mean(iris$Sepal.Length), 1))
            updateNumericInput(session, "petalWidth", value = round(mean(iris$Petal.Width), 1))
            updateNumericInput(session, "petalLength", value = round(mean(iris$Petal.Length), 1))
        })
    }
)
