library(shiny)
library(shinydashboard)
library(ggplot2)
library(survival)
library(corrplot)
library(plotly)
library(caret)
library(rgl)
library(car)
library(survminer)
library(rpart)
library(DT)
library(nortest)
library(e1071)
library(pROC)
library(pwr)
library(stats)
library(lattice)
library(tinytex)
library(dplyr)
# Загрузка данных
getwd()
data <- read.csv('data_8_2(out).csv')

# Добавляем переменную statuso, если её нет
if (!("statuso" %in% colnames(data))) {
  data$statuso <- sample(0:1, nrow(data), replace = TRUE)
}

# Ограничиваем данные для ускорения работы
data <- data %>% sample_n(5000)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "My project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashboard"),
      menuItem("About", tabName = "about"),
      menuItem("Table", tabName = "Table"),
      menuItem("Visualization", tabName = "visualization"),
      menuItem("Test of normality", tabName = "normality"),
      menuItem("Multicollinearity", tabName = "multicollinearity"),
      menuItem("Best model regression", tabName = "regression"),
      menuItem("Best model classification", tabName = "classification"),
      menuItem("Chi Square test", tabName = "chitest"),
      menuItem("Predict new data", tabName = "predict"),
      menuItem("Survival Analysis", tabName = "survival")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h1("Movies",style = "color:blue",align = "center"),
              br(),
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  tags$img(src = "Movies.png", height = "500px", align = "center", style = "display: block; margin: 0 auto;")
                ),
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  h2("Team members:",style = "color:red",align = "left"),
                  h3("1. Arina Nolfina",style = "color:blue",align = "left"),
                  h3("2. Karina Gadzhimagomedova",style = "color:blue",align = "left"),
                  h3("3. Olga Tsoy",style = "color:blue",align = "left"),
                  h3("4. Ranida Koilybayeva",style = "color:blue",align = "left"),
                  h3("5. Sabyr Yermukhanuly",style = "color:blue",align = "left"))
              )
      ),
      tabItem(tabName = "about",
              h1("Movies dataset",style = "color:red"),
              p("The",span("Movies ", style = "color:blue"), 
                "Welcome, movie buffs and data enthusiasts! 🎬 Dive into this Dataset,
                a comprehensive collection of over 960,000 movies, lovingly curated from
                The Movie Database (TMDB). This dataset is inspired by the groundwork laid
                by asaniczka, you can check out his dataset . Whether you're into data analysis,
                building your next big machine learning project, or just love exploring movie data,
                there's something here for everyone."), 
              br(),
              h2("data data set",style = "color:red"),
              p("The dataset consists of various information about movies in IMDB & TMDB. Specifically,
                it contains data that can be used for analysis."),
              
              p("The columns in this dataset are:"),
              p("- Titles & Release Info: From classic hits to the latest blockbusters, get detailed info on movie titles and their release years."),
              p("- Genres & Keywords: Navigate through movies sorted by genres and keywords for targeted analysis or discovery."),
              p("- Ratings & Popularity: Dive into audience ratings, popularity scores, and vote counts to gauge movie success."),
              p("- Production Insights: Explore behind-the-scenes with info on production companies, countries, and budgets."),
              p("- Cast & Crew: Get the scoop on who's who in your favorite movies with detailed cast and crew listings."),
              br(),
              h2("Goal of project",style = "color:red"),
              p("Develop an analysis of film data using the R programming language to explore various aspects of the film industry, such as film popularity, rating, genres, budgets, box office, and identify the relationship between these indicators. The project will include the collection and processing of film data, the creation of visualizations, as well as statistical analysis to obtain insights that can help in making decisions in the field of film production and marketing."),
              br(),
              p("For more information, visit the ",
                a("TMDB", 
                  href = "https://www.themoviedb.org/"),
                a("IMDB", 
                  href = "https://www.imdb.com/"),
                a("Source of dataset", 
                  href = "https://www.kaggle.com/datasets/alanvourch/tmdb-movies-daily-updates"))
              
              
      ),
      tabItem(tabName = "Table",
              h2("Movies table",style = "color:red"),
              DT::dataTableOutput("mytable")),
      tabItem(tabName = "visualization",
              h1("Visualization",style = "color:red",align = "center"),
              selectInput("xvar", "Select X Variable", choices = names(data), selected = "popularity"),
              selectInput("yvar", "Select Y Variable", choices = names(data), selected = "revenue"),
              selectInput("color", "Select Color by", choices = c("status", names(data)), selected = "status"),
              sliderInput("size", "Adjust Point Size", min = 1, max = 5, value = 2),
              fluidRow(
                # First box with a scatter plot
                box(
                  title = "Scatter Plot",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("scatterPlot")
                ),
                
                # Second box with a boxplot
                box(
                  title = "Boxplot",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("boxPlot")
                )
              ),
              fluidRow(
                # Third box with a histogram
                box(
                  title = "Histogram",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("histPlot")
                ),
                
                # Fourth box with a density plot
                box(
                  title = "Density Plot",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("densityPlot")
                )
              )
      ),
      tabItem(tabName = "normality",
              h1("Test of normality",style = "color:red",align = "center"),
              sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                # Widget to select variable for normality tests
                selectInput("variable", "Select Variable", choices = names(data), selected = "revenue")
              ),
              fluidRow(
                # First box: Shapiro-Wilk test result
                box(
                  title = "Shapiro-Wilk Test of Normality",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  verbatimTextOutput("shapiroTest")
                ),
                
                # Second box: Anderson-Darling test result
                box(
                  title = "Anderson-Darling Test of Normality",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  verbatimTextOutput("andersonTest")
                ),
                
                # Third box: Skewness-Kurtosis test result
                box(
                  title = "Skewness-Kurtosis Test of Normality",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  verbatimTextOutput("skewnessTest")
                )
              ),
              fluidRow(
                # Fourth box: Q-Q plot
                box(
                  title = "Q-Q Plot",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("qqPlot")
                ),
                # Fifth box:Boxplot
                box(
                  title = "Boxplot",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("nboxPlot")
                ),
                
              ),
              fluidRow(
                # Third box: Histogram
                box(
                  title = "Histogram",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("histogram")
                ),
                
                # Fourth box: Density plot
                box(
                  title = "Density Plot",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("densityPlot")
                )
              )
      ),
      tabItem(tabName = "multicollinearity",
              h1("Multicollinearity",style = "color:red",align = "center"),
              sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                # Widget to select variables for regression
                selectInput("regression_vars", "Select Variables for Regression", 
                            choices = names(data), selected = c("revenue", "popularity"), multiple = TRUE)
              ),
              fluidRow(
                
                
                # Second box: VIF (Variance Inflation Factor) results
                box(
                  title = "VIF - Multicollinearity Check",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  tableOutput("vifTable")
                )),
              fluidRow(
                # First box: Correlation Matrix plot
                box(
                  title = "Correlation Matrix",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("corrPlot")
                ),
                # Second box:  Pairwise Scatterplots
                box(
                  title = "Pairwise Scatterplots",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("pairs_plot")
                )
                
              )
      ),
      tabItem(tabName = "regression",
              h1("Regression",style = "color:red",align = "center"),
              fluidRow(
                # First box: Decision Tree Regression Model Summary
                box(
                  title = "Decision Tree Model Summary",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("decisionTreeSummary")
                ),
                
                # Second box: Model Performance (R2, Adjusted R2)
                box(
                  title = "Model Performance (R2, Adjusted R2)",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("modelPerformance")
                ),
                
                
                # Third box: Normality of Residuals (Shapiro Test & Q-Q Plot)
                box(
                  title = "Normality of Residuals",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("qqPlotResiduals"),
                  verbatimTextOutput("shapiroTestResiduals")
                )
                
                # # Fourth box: VIF - Multicollinearity Check
                # box(
                #   title = "VIF - multicollinearity among predictors in a regression model",
                #   status = "success",
                #   solidHeader = TRUE,
                #   width = 6,
                #   tableOutput("vifTable1")
                # )
              )
      ),
      tabItem(tabName = "classification",
              h1("Classification",style = "color:red",align = "center"),
              fluidRow(
                # First box: Decision Tree Classification Model Summary
                box(
                  title = "Decision Tree Classification Model Summary",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("decisionTreeSummary2")
                ),
                
                # Second box: Confusion Matrix & AUC-ROC
                box(
                  title = "Confusion Matrix & AUC-ROC",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("confusionMatrixOutput"),
                  plotOutput("rocPlot")
                )
              )
      ),
      tabItem(tabName = "chitest",
              h1("Chi-Square Test: Goodness of Fit and Power Analysis",style = "color:red",align = "center"),
              fluidRow(
                # Third box: Chi-Square Test for Goodness of Fit
                box(
                  title = "Chi-Square Test: Goodness of Fit",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("chiSquareTestOutput"),
                  plotOutput("chiPlot")
                ),
                
                # Fourth box: Power Analysis
                box(
                  title = "Power Analysis",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("powerAnalysisOutput"),
                  plotOutput("powerPlot")
                )
              )
      ),
      tabItem(tabName = "predict",
              h1("Predict new data",style = "color:red",align = "center"),
              fluidRow(
                box(
                  title = "Input Parameters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  sliderInput("budget", "Budget($):", min = 0, max = 400000000, value = 100000),
                  sliderInput("runtime", "Runtime(minutes):", min = 0, max = 1000, value = 90)
                ),
                box(
                  title = "Prediction Result",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("predictionOutput")
                )
              )
      ),
      tabItem(tabName = "survival",
              h1("Survival Analysis",style = "color:red",align = "center"),
              fluidRow(
                box(
                  title = "Kaplan-Meier Survival Curve",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("kmPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Cox Proportional Hazards Model",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("coxModelSummary")
                ),
                box(
                  title = "Cox Proportional Hazards Assumption",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("coxModelAssoumption"),
                  plotOutput("coxModelAssoumptionPlot")
                )
              )
      )
    )
  ))

# Define server logic
server <- function(input, output) {
  output$mytable = DT::renderDataTable({
    data
  })
  
  #visualization
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    ggplot(data, aes_string(x = input$xvar, y = input$yvar, color = input$color)) +
      geom_point(size = input$size) +
      theme_minimal() +
      labs(title = paste("Scatter Plot of", input$yvar, "vs", input$xvar))
  })
  
  # Boxplot
  output$boxPlot <- renderPlot({
    ggplot(data, aes_string(x = input$color, y = input$yvar, color = input$color)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Boxplot of", input$yvar, "by", input$color))
  })
  
  # Histogram
  output$histPlot <- renderPlot({
    ggplot(data, aes_string(x = input$xvar, fill = input$color)) +
      geom_histogram(bins = 15, alpha = 0.7, position = "identity") +
      theme_minimal() +
      labs(title = paste("Histogram of", input$xvar))
  })
  
  # Density Plot
  output$densityPlot <- renderPlot({
    ggplot(data, aes_string(x = input$xvar, fill = input$color)) +
      geom_density(alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Density Plot of", input$xvar))
  })
  
  #normality
  
  # Shapiro-Wilk Test of Normality
  output$shapiroTest <- renderPrint({
    shapiro_test_result <- shapiro.test(data[[input$variable]])
    return(shapiro_test_result)
  })
  
  # Anderson-Darling Test of Normality
  output$andersonTest <- renderPrint({
    anderson_test_result <- ad.test(data[[input$variable]])
    return(anderson_test_result)
  })
  
  # Skewness-Kurtosis Test of Normality
  output$skewnessTest <- renderPrint({
    sk <- skewness(data[[input$variable]])
    kur <- kurtosis(data[[input$variable]])
    cat("Skewness: ", round(sk, 4), "\n")
    cat("Kurtosis: ", round(kur, 4), "\n")
  })
  
  # Q-Q Plot
  output$qqPlot <- renderPlot({
    ggplot(data, aes_string(sample = input$variable)) +
      stat_qq() +
      stat_qq_line() +
      ggtitle(paste("Q-Q Plot for", input$variable)) +
      theme_minimal()
  })
  
  # Box Plot
  output$nboxPlot <- renderPlot({
    ggplot(data, aes_string(x = input$variable)) +
      geom_boxplot(fill = "red") + 
      theme_minimal() +
      labs(title = paste("Box Plot of", input$variable), x = input$variable, y = "Box") + coord_flip()
  })
  
  # Histogram
  output$histogram <- renderPlot({
    ggplot(data, aes_string(x = input$variable)) +
      geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Histogram of", input$variable), x = input$variable, y = "Frequency")
  })
  
  # Density Plot
  output$densityPlot <- renderPlot({
    ggplot(data, aes_string(x = input$variable)) +
      geom_density(fill = "purple", alpha = 0.5) +
      theme_minimal() +
      labs(title = paste("Density Plot of", input$variable), x = input$variable, y = "Density")
  })
  
  # multicollinearity
  
  # Correlation Matrix plot
  output$corrPlot <- renderPlot({
    numeric_data <- data[c(2,3,5,6,7,9)]  # Only the numeric columns
    cor_matrix <- cor(numeric_data)  # Compute correlation matrix
    corrplot(cor_matrix, method = "circle")  # Plot correlation matrix
  })
  
  # Create Pairwise Scatterplots for numeric variables
  output$pairs_plot <- renderPlot({
    numeric_data <- data[c(2,3,5,6,7,9)]  # Only the numeric columns
    pairs(numeric_data, panel = panel.smooth, main = "Pairwise Scatterplots")
  })
  
  # VIF (Variance Inflation Factor) for multicollinearity
  output$vifTable <- renderTable({
    model <- lm(revenue ~ ., data = data[, c(input$regression_vars, "budget")])
    vif_result <- vif(model)
    data.frame(Variable = names(vif_result), VIF = vif_result)
  })
  
  #regression
  
  set.seed(123)
  trainIndex <- createDataPartition(data$revenue, p = 0.8, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  
  # Train Decision Tree Regression model
  decisionTreeModel <- rpart(revenue ~ budget + vote_average + vote_count + popularity + runtime, 
                             data = trainData, method = "anova")
  
  # Model Summary: Render Decision Tree Summary
  output$decisionTreeSummary <- renderPrint({
    summary(decisionTreeModel)
  })
  
  # Model Performance (R-squared and Adjusted R-squared)
  modelPerformance <- reactive({
    predictions <- predict(decisionTreeModel, testData)
    model_residuals <- testData$revenue - predictions
    ss_res <- sum(model_residuals^2)
    ss_tot <- sum((testData$revenue - mean(testData$revenue))^2)
    r2 <- 1 - (ss_res / ss_tot)
    n <- nrow(testData)
    p <- length(decisionTreeModel$variable.importance)
    adj_r2 <- 1 - ((1 - r2) * (n - 1) / (n - p - 1))
    return(list(r2 = r2, adj_r2 = adj_r2))
  })
  
  output$modelPerformance <- renderPrint({
    performance <- modelPerformance()
    cat("R-squared: ", round(performance$r2, 4), "\n")
    cat("Adjusted R-squared: ", round(performance$adj_r2, 4), "\n")
  })
  
  # Calculate residuals for the Decision Tree Regression model
  residualsBestModel <- reactive({
    predictions <- predict(decisionTreeModel, testData)
    residuals <- testData$revenue - predictions
    return(residuals)
  })
  
  # Normality of residuals (Shapiro-Wilk test) and Q-Q plot
  output$shapiroTestResiduals <- renderPrint({
    model_residuals <- residualsBestModel()
    shapiro_test_result <- shapiro.test(model_residuals)
    return(shapiro_test_result)
  })
  
  # Q-Q plot for residuals
  output$qqPlotResiduals <- renderPlot({
    model_residuals <- residualsBestModel()
    ggplot(data.frame(residuals = model_residuals), aes(sample = residuals)) +
      stat_qq() +
      stat_qq_line() +
      ggtitle("Q-Q Plot of Residuals") +
      theme_minimal()
  })
  
  #Classification
  
  # Train Decision Tree Classification model
  decisionTreeModel2 <- rpart(statuso ~ revenue + budget + popularity + vote_average, 
                              data = trainData, method = "class")
  
  # Model Summary: Render Decision Tree Summary
  output$decisionTreeSummary2 <- renderPrint({
    summary(decisionTreeModel2)
  })
  
  # Predictions and Confusion Matrix
  predictions2 <- predict(decisionTreeModel2, testData, type = "class")
  testData$statuso <- as.factor(testData$statuso)
  predictions2 <- as.factor(predictions2)
  levels(predictions2) <- levels(testData$statuso)
  confusionMatrixResult <- confusionMatrix(predictions2, testData$statuso)
  
  # Confusion Matrix Output
  output$confusionMatrixOutput <- renderPrint({
    confusionMatrixResult
  })
  
  # AUC-ROC Curve
  output$rocPlot <- renderPlot({
    prob_predictions <- predict(decisionTreeModel2, testData, type = "prob")
    roc_result <- roc(testData$statuso, prob_predictions[, 1])
    plot(roc_result, col = "blue", main = "AUC-ROC Curve", lwd = 2)
    text(0.5, 0.5, paste("AUC =", round(auc(roc_result), 2)), col = "red", cex = 1.2)
  })
  
  # Chi-Square Test for Goodness of Fit (using observed vs expected counts)
  observed <- table(testData$statuso)
  expected <- rep(sum(observed) / length(observed), length(observed))
  if (length(expected) != length(observed)) {
    stop("Длина expected и observed должна быть одинаковой.")
  }
  output$chiSquareTestOutput <- renderPrint({
    chi_square_test <- chisq.test(observed, p = expected, rescale.p = TRUE)
    chi_square_test
  })
  
  # Goodness of Fit plot
  cont_table <- table(testData$statuso)
  observed <- as.vector(cont_table)
  output$chiPlot <- renderPlot({
    plot_data <- data.frame(
      Category = rep(rownames(cont_table), 2),
      Frequency = c(observed, expected),
      Type = rep(c("Observed", "Expected"), each = nrow(cont_table))
    )
    
    ggplot(plot_data, aes(x = Category, y = Frequency, fill = Type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
      labs(title = "Observed vs. Expected Frequencies",
           y = "Frequency",
           fill = "Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Power Analysis (using pwr package)
  power_analysis <- pwr.chisq.test(w=ES.w1(expected, observed), df=(length(expected)-1), power=0.8, sig.level = 0.05)
  output$powerAnalysisOutput <- renderPrint({
    power_analysis
  })
  
  # Power Analysis Curve
  output$powerPlot <- renderPlot({
    plot(power_analysis)
  })
  modelpred <- lm(revenue ~ poly(budget,2) + poly(runtime,2), data = trainData)
  # Prediction based on user inputs
  output$predictionOutput <- renderPrint({
    new_data <- data.frame(
      budget = input$budget,
      runtime = input$runtime
    )
    
    prediction <- predict(modelpred, newdata = new_data)
    paste("Predicted Revenue: ", prediction)
    print(prediction)
  })
  
  # Survival Analysis
  data$time <- runif(nrow(data), min = 1, max = 100)  # Simulated time-to-event variable
  data$statuso <- sample(0:1, nrow(data), replace = TRUE)  # Simulated event indicator
  
  # Kaplan-Meier Survival Curve
  km_fit <- survfit(Surv(time, statuso) ~ statuso, data = data)
  
  output$kmPlot <- renderPlot({
    ggsurvplot(km_fit, data = data, pval = TRUE, risk.table = TRUE)
  })
  
  # Cox Proportional Hazards Model
  cox_model <- coxph(Surv(time, statuso) ~ revenue + budget + popularity + vote_average, data = data)
  
  # Render the Cox model summary
  output$coxModelSummary <- renderPrint({
    summary(cox_model)
  })
  
  # Cox Proportional Hazards Assumption
  cox_assumptions <- cox.zph(cox_model)
  output$coxModelAssoumption <- renderPrint({
    cox_assumptions
  })
  
  output$coxModelAssoumptionPlot <- renderPlot({
    ggcoxzph(cox_assumptions)
  })
}

# Run the application
shinyApp(ui = ui, server = server)