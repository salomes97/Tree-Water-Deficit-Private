library(vars)
library(urca)

df.ts.series2 <- read.csv("df_twd.csv")
summary(ur.df(df.ts.series2$twd, type="drift", lags=2))
summary(ur.df(df.ts.series2$at, type="drift", lags=2))
library(vars)

granger_test <- function(data, x_col, y_col, lags = 2) {
  # Rename and select relevant columns
  df <- data[, c(x_col, y_col)]
  names(df) <- c("x", "y")
  
  # Remove missing values
  df <- df[complete.cases(df), ]
  
  # Fit VAR model
  var_model <- VAR(df, p = lags, type = "const")
  
  # Run Granger causality test: does x cause y?
  causality_result <- causality(var_model, cause = "x")
  
  return(causality_result)
}

granger_test(df.ts.series2, "twd", "at", lags = 2)
granger_test(df.ts.series2, "twd", "dp", lags = 2)


# This tests the null hypothesis: The series has a unit root (i.e., non-stationary).
# If the coefficient on z.lag.1 is significantly different from zero (with a very small p-value), and negative, then you can reject the null hypothesis and conclude the series is stationary.