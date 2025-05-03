library(shiny)
library(tidyverse)
library(patchwork)

ui <- fluidPage(
  titlePanel("Linear Model with Training and Testing Split"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Upload Dataset (CSV)", accept = ".csv"),
      numericInput("train_ratio", "Training Ratio (0-1)", value = 0.5, min = 0, max = 1),
      actionButton("run", "Run Analysis")
    ),
    
    mainPanel(
      plotOutput("plots"),
      verbatimTextOutput("model_summary")
    )
  )
)

server <- function(input, output) {
  # Reactive to load dataset
  dataset <- reactive({
    req(input$dataset) # Ensure dataset is uploaded
    read.csv(input$dataset$datapath)
  })
  
  # Reactive to split dataset and run analysis
analysis <- eventReactive(input$run, {
    # Check if 'caret' is installed, if not, install it
    if (!requireNamespace("caret", quietly = TRUE)) {
        install.packages("caret")
    }

    # Load the 'caret' package
    library(caret)

    req(dataset())  # Ensure that dataset is available
    data <- dataset()  # Load the dataset
    
    # Define the target variable
    TARGET <- 'Price'
    
    # Create a new feature combining 'Brand' and 'Model' (e.g., "Toyota_Corolla")
    # data$Brand_Model_encoded <- paste(data$Brand, data$Model, sep = "_")
    
    # Define selected features for EDA (Exploratory Data Analysis)
    EDA_selected <- c('Brand_Model_encoded', 'Location_encoded', 'Car_Suv_encoded', 'Kilometres_num', 
                        'Cylinders', 'ColourExtInt_encoded', 'FuelType_encoded', 'DriveType_encoded')
    
    # Select relevant features and the target variable, and remove rows with missing values
    selected_data <- data %>% select(all_of(EDA_selected), all_of(TARGET))
    selected_data <- selected_data %>% filter(complete.cases(.))
    
    # Summary of the selected data
    summary(selected_data)
    
    # Split the dataset into training and testing sets (80% train, 20% test)
    train_index <- createDataPartition(selected_data$Price, p = 0.8, list = FALSE)
    train_data <- selected_data[train_index, ]
    test_data  <- selected_data[-train_index, ]
    
    # Option A: Log-transform Price (training set only)
    train_data$log_Price <- log(train_data$Price)
    
    # Define the formula for the model
    model_formula <- reformulate(EDA_selected, response = "log_Price")
    
    # Train the linear model using cross-validation
    lm_model <- train(
        model_formula,
        data = train_data,
        method = "lm",
        trControl = trainControl(method = "cv", number = 5)
    )
    
    # Print model coefficients
    print(summary(lm_model$finalModel))
    
    # Predict on the test data
    test_data$log_Price_pred <- predict(lm_model, newdata = test_data)
    
    # Revert to the original price scale using the exponent function
    test_data$Price_pred <- exp(test_data$log_Price_pred)
    
    # --- 1. Plot: Predicted vs. Actual (log scale) ---
    log_scale_plot <- ggplot(test_data, aes(x = log(Price), y = log_Price_pred)) +
        geom_point(alpha = 0.4, color = "blue") +
        geom_abline(slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Predicted vs. Actual Prices (Log Scale)",
            x = "Actual log(Price)", y = "Predicted log(Price)") +
        theme_minimal()
    
    # --- 2. Plot: Predicted vs. Actual (original price scale) ---
    original_scale_plot <- ggplot(test_data, aes(x = Price, y = Price_pred)) +
        geom_point(alpha = 0.4, color = "darkgreen") +
        geom_abline(slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Predicted vs. Actual Prices (Original Scale)",
            x = "Actual Price", y = "Predicted Price") +
        scale_x_continuous(labels = scales::dollar) +
        scale_y_continuous(labels = scales::dollar) +
        theme_minimal()
    
    # --- 3. Plot: Residuals Distribution ---
    model_data <- data.frame(
        Fitted = fitted(lm_model$finalModel),
        Residuals = residuals(lm_model$finalModel)
    )
    
    residuals_plot <- ggplot(model_data, aes(x = Residuals)) +
        geom_histogram(
        aes(y = after_stat(density)), 
        bins = 50,
        fill = "steelblue",
        color = "white",
        alpha = 0.8
        ) +
        geom_density(color = "red", linewidth = 1) + 
        labs(
        title = "Distribution of Model Residuals",
        x = "Residuals (Actual - Predicted Price)",
        y = "Density"
        ) +
        theme_minimal()
    
    # Residuals vs. Fitted Plot
    homoscedasticity_plot <- ggplot(model_data, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", color = "darkgreen", se = FALSE) +
    labs(
        title = "Residuals vs. Fitted Values",
        subtitle = "Check for homoscedasticity (constant variance)",
        x = "Fitted Values (Predicted Price)",
        y = "Residuals (Actual - Predicted)"
    ) +
    theme_minimal()

    

        # TRAINING performance metrics
train_predictions <- predict(lm_model$finalModel, newdata = train_data)
residuals_train <- train_data$log_Price - train_predictions
rss_train <- sum(residuals_train^2)
n_train <- nrow(train_data)
p <- length(lm_model$finalModel$coefficients)  # Number of predictors incl. intercept
rse_train <- sqrt(rss_train / (n_train - p))

tss_train <- sum((train_data$log_Price - mean(train_data$log_Price))^2)
r_squared_train <- 1 - rss_train / tss_train
adj_r_squared_train <- 1 - (1 - r_squared_train) * ((n_train - 1) / (n_train - p))

# TEST performance metrics
log_Price_test <- log(test_data$Price)
rss_test <- sum((log_Price_test - test_data$log_Price_pred)^2)
tss_test <- sum((log_Price_test - mean(log_Price_test))^2)
r_squared_test <- 1 - rss_test / tss_test
rmse_test <- sqrt(mean((test_data$log_Price_pred - log_Price_test)^2))

# Combine into a printable summary
# Combine into a printable summary
summary_text <- paste0(
  "TRAINING SET:\n",
  "  Residual Std. Error (RSE): ", round(rse_train, 4), "\n",
  "  R-squared: ", round(r_squared_train, 4), "\n",
  "  Adjusted R-squared: ", round(adj_r_squared_train, 4), "\n\n",
  
  "TEST SET:\n",
  "  RMSE (log_Price): ", round(rmse_test, 4), "\n",
  "  R-squared: ", round(r_squared_test, 4), "\n\n",
  
  "INTERPRETATION:\n",
  "  Based on RSE of ", round(rse_train, 4), 
  ", about 68% of predictions fall within ±", round(rse_train, 3), " log-units.\n",
  "  This translates to:\n",
  "    Overestimation up to ~", round((exp(rse_train) - 1) * 100), "%\n",
  "    Underestimation up to ~", round((1 - exp(-rse_train)) * 100), "%\n"
)

    # Return list of outputs (plots and model summary)
    list(
        layout = (log_scale_plot / original_scale_plot) | (residuals_plot / homoscedasticity_plot),  # Combine the plots
        summary = summary_text  # Return model summary
    )
    })

    # Output the layout of the plots
  output$plots <- renderPlot({
    req(analysis())  # Ensure analysis() is available
    analysis()$layout  # Render the layout
  })
  
  # Output the model summary text
  output$model_summary <- renderText({
    req(analysis())
    analysis()$summary
  })
}

shinyApp(ui = ui, server = server)