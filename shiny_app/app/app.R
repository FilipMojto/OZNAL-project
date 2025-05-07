library(shiny)
library(tidyverse)
library(patchwork)
library(caret)

# Ensure caret is properly loaded
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

ui <- fluidPage(
  titlePanel("Linear Model with Training and Testing Split"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Upload Dataset (CSV)", accept = ".csv"),
      numericInput("train_ratio", "Training Ratio (0-1)", value = 0.8, min = 0, max = 1),
      selectInput("model", "Select Model",
                  choices = c("Linear Regression" = "lm",
                            "Polynomial Regression" = "poly"),
                    selected = "lm"),
      # New hyperparameters
      numericInput("cv_folds", "Number of CV Folds", value = 5, min = 2, max = 20, step = 1),
      numericInput("seed", "Random Seed", value = 123, min = 1, step = 1),
    #   selectInput("metric", "Performance Metric", 
    #               choices = c("RMSE" = "RMSE", "R-squared" = "Rsquared", "MAE" = "MAE"),
    #               selected = "RMSE"),

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
    library(car)

    req(dataset())  # Ensure that dataset is available
    data <- dataset()  # Load the dataset
    
    # Define the target variable

    if (input$model == "lm") {

    # Create a new feature combining 'Brand' and 'Model' (e.g., "Toyota_Corolla")
    # data$Brand_Model_encoded <- paste(data$Brand, data$Model, sep = "_")
    
    # Define selected features for EDA (Exploratory Data Analysis)
    EDA_selected <- c('Model_encoded', 'Location_encoded', 'Car_Suv_encoded', 'Kilometres_num', 'Cylinders', 'ColourExtInt_encoded', 'FuelType_encoded','DriveType_encoded', 'UsedOrNew_encoded', 'age')
    
    # Select relevant features and the target variable, and remove rows with missing values
    selected_data <- data %>% select(all_of(EDA_selected), all_of(TARGET))
    # selected_data <- selected_data %>% filter(complete.cases(.))
    
    # Summary of the selected data
    summary(selected_data)

    # Fixing the random number generator to ensure the same data split every time.
    # set.seed(123) 
    set.seed(input$seed)
    
    # Split the dataset into training and testing sets (80% train, 20% test)
    train_index <- createDataPartition(selected_data$Price, p = input$train_ratio, list = FALSE)
    train_data <- selected_data[train_index, ]
    test_data  <- selected_data[-train_index, ]
    
    # Option A: Log-transform Price (training set only)
    # train_data$log_Price <- log(train_data$Price)
    
    # Define the formula for the model
    model_formula <- reformulate(EDA_selected, response = "Price_log")
    ctrl <- trainControl(method = "cv", number = input$cv_folds)
    
    # Train the linear model using cross-validation
    lm_model <- train(
        model_formula,
        data = train_data,
        method = "lm",
        #  metric = input$metric,
        trControl = ctrl,
    )

    train_data$log_Price_pred <- predict(lm_model, newdata = train_data)
    
    # Print model coefficients
    print(summary(lm_model$finalModel))

    vif_values <- car::vif(lm_model$finalModel)
    vif_text <- paste(
      "VIF (Variance Inflation Factor):\n",
      paste0("  ", names(vif_values), ": ", round(vif_values, 2), collapse = "\n")
    )

    
                              # Predict on the test data
    test_data$log_Price_pred <- predict(lm_model, newdata = test_data)
    
    # Revert to the original price scale using the exponent function
    test_data$Price_pred <- 10 ^ test_data$log_Price_pred

    
    # --- 1. Plot: Predicted vs. Actual (log scale) ---
    log_scale_plot <- ggplot(test_data, aes(x = Price_log, y = log_Price_pred)) +
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
    
    # Residuals Histogram with Density
    residuals_plot <- ggplot(model_data, aes(x = Residuals)) +
        geom_histogram(
            aes(y = after_stat(density)), 
            bins = 50,
            fill = "#4C6A92",    # Subtle blue shade for histogram fill
            color = "white",     # White borders
            alpha = 0.7          # Slightly transparent
        ) +
        geom_density(color = "#1F3A59", linewidth = 1) +  # Darker blue for density line
        labs(
            title = "Distribution of Model Residuals",
            x = "Residuals (Actual - Predicted Price)",
            y = "Density"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12, face = "italic")
        )

    # Residuals vs. Fitted Values Plot (Homoscedasticity)
    homoscedasticity_plot <- ggplot(model_data, aes(x = Fitted, y = Residuals)) +
        geom_point(alpha = 0.6, color = "#5B7D9D") +  # Light blue points
        geom_hline(yintercept = 0, color = "#1F3A59", linetype = "dashed") +  # Dark blue horizontal line
        geom_smooth(method = "loess", color = "#3C5066", se = FALSE) +  # Mid-blue smooth line
        labs(
            title = "Residuals vs. Fitted Values",
            subtitle = "Check for homoscedasticity (constant variance)",
            x = "Fitted Values (Predicted Price)",
            y = "Residuals (Actual - Predicted)"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12, face = "italic")
        )

    # Normal Q-Q Plot
    qqplot <- ggplot(model_data, aes(sample = Residuals)) +
        stat_qq(color = "#4C6A92") +  # Subtle blue for QQ points
        stat_qq_line(color = "#1F3A59", linetype = "dashed") +  # Darker blue for QQ line
        labs(
            title = "Normal Q-Q Plot",
            subtitle = "Check for normality of residuals",
            x = "Theoretical Quantiles",
            y = "Sample Quantiles"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12, face = "italic")
        )

    

    # TRAINING performance metrics
    train_predictions <- predict(lm_model$finalModel, newdata = train_data)
    residuals_train <- train_data$Price_log - train_predictions
    rss_train <- sum(residuals_train^2)
    n_train <- nrow(train_data)
    p <- length(lm_model$finalModel$coefficients)  # Number of predictors incl. intercept
    rse_train <- sqrt(rss_train / (n_train - p))
    log_Price_train <- log(train_data$Price)
    rmse_train <- sqrt(mean((train_data$log_Price_pred - log_Price_train)^2))
    
    tss_train <- sum((train_data$Price_log - mean(train_data$Price_log))^2)
    r_squared_train <- 1 - rss_train / tss_train
    adj_r_squared_train <- 1 - (1 - r_squared_train) * ((n_train - 1) / (n_train - p))

    # TEST performance metrics
    log_Price_test <- log(test_data$Price)
    residuals_test <- test_data$Price_log - test_data$log_Price_pred
    rss_test <- sum(residuals_test^2)
    n_test <- nrow(test_data)
    rse_test <- sqrt(rss_test / (n_test - p))  # Same calculation for RSE
    rmse_test <- sqrt(mean((test_data$log_Price_pred - log_Price_test)^2))
    tss_test <- sum((log_Price_test - mean(log_Price_test))^2)
    r_squared_test <- 1 - rss_test / tss_test
    adj_r_squared_test <- 1 - (1 - r_squared_test) * ((n_test - 1) / (n_test - p))

    # Combine into a printable summary
    summary_text <- paste0(
    "TRAINING CONFIGURATION:\n",
    sprintf("  Training ratio: %.2f | CV folds: %d | Seed: %d | Metric: %s", input$train_ratio, input$cv_folds, input$seed, input$metric),
    "\n\n",

    "TRAINING SET:\n",
    "  RMSE (log_Price): ", round(rmse_train, 4), "\n",
    "  Residual Std. Error (RSE): ", round(rse_train, 4), "\n",
    "  R-squared: ", round(r_squared_train, 4), "\n",
    "  Adjusted R-squared: ", round(adj_r_squared_train, 4), "\n\n",

    "TEST SET:\n",
    "  RMSE (log_Price): ", round(rmse_test, 4), "\n",
    "  Residual Std. Error (RSE): ", round(rse_test, 4), "\n",
    "  R-squared: ", round(r_squared_test, 4), "\n",
    "  Adjusted R-squared: ", round(adj_r_squared_test, 4), "\n\n",

    # "INTERPRETATION:\n",
    # "  Based on RSE of ", round(rse_train, 4), 
    # ", about 68% of predictions fall within ±", round(rse_train, 3), " log-units.\n",
    # "  This translates to:\n",
    # "    Overestimation up to ~", round((10^rse_train - 1) * 100, 1), "%\n",
    # "    Underestimation up to ~", round((1 - 10^(-rse_train)) * 100, 1), "%\n",
    # "\n\n",

    "INTERPRETATION (Test Set):\n",
"  Based on test‐set RSE of ", round(rse_test, 4),
", about 68% of predictions fall within ±", round(rse_test, 3), " log₁₀‑units.\n",
"  This translates to roughly a multiplicative factor of 10^RSE:\n",
"    Overestimation up to ~", round((10^rse_test - 1) * 100, 1), "%\n",
"    Underestimation up to ~", round((1 - 10^(-rse_test)) * 100, 1), "%\n",
"\n\n",
    vif_text
    )

    # Return list of outputs (plots and model summary)
    list(
        layout = (log_scale_plot / original_scale_plot) | (residuals_plot / (homoscedasticity_plot | qqplot)),  # Combine the plots
        summary = summary_text  # Return model summary
    )}
    else if (input$model == "poly") {
        # Polynomial regression logic here (if needed)
        # For now, just return a message
        return("Polynomial regression is not implemented yet.")
    }
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