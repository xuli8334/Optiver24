
arima.pred.dic <- get(load("arima_pred_dic.RData"))
reg.pred.dic <- get(load("reg_pred_dic.RData"))
hav.pred.dic <- get(load("hav_pred_dic.RData"))
garch.pred.dic <- get(load("garch_pred_dic.RData"))

vol.dic <- get(load("vol_dic.RData"))
vol.train.dic <- get(load("vol_train_dic.RData"))
vol.val.dic <- get(load("vol_val_dic.RData"))

len.vol <- length(vol.dic[[1]][[1]]$volatility)
len.train <- length(vol.train.dic[[1]][[1]]$volatility)
len.val <- length(vol.val.dic[[1]][[1]]$volatility)

arima_res <- get(load("arima_res.RData"))
reg_res <- get(load("reg_res.RData"))
hav_res <- get(load("hav_res.RData"))
garch_res <- get(load("garch_res.RData"))

comm_time_IDs <- get(load("comm_time_IDs.RData"))

stocks_to_analyze <- get(load("stocks_to_analyze.RData"))
group_lis <- split(stocks_to_analyze, stocks_to_analyze$cluster)

draw_res_plot <- function(stock_id, time_id, models_to_show){
  actual.train.df <- vol.train.dic[[stock_id]][[time_id]]
  actual.val.df <- vol.val.dic[[stock_id]][[time_id]]
  actual.vol.df <- vol.dic[[stock_id]][[time_id]]
  
  pred_vol_0 <- actual.train.df[len.train,]$volatility # last volatility of the first predicted one
  
  arima.pred.df <- data.frame(
    time_bucket = c(len.train: len.vol),
    pred_vol = c(pred_vol_0, as.numeric(arima.pred.dic[[stock_id]][[time_id]]))
  )
  
  reg.pred.df <- data.frame(
    time_bucket = c(len.train: len.vol),
    pred_vol = c(pred_vol_0, reg.pred.dic[[stock_id]][[time_id]])
  )
  
  hav.pred.df <- data.frame(
    time_bucket = c(len.train: len.vol),
    pred_vol = c(pred_vol_0, hav.pred.dic[[stock_id]][[time_id]])
  )
  
  garch.pred.df <- data.frame(
    time_bucket = c(len.train: len.vol),
    pred_vol = c(pred_vol_0, garch.pred.dic[[stock_id]][[time_id]])
  )
  
  plot <- ggplot(data = actual.vol.df, aes(x = time_bucket,
                                           y = volatility,
                                           color = "Actual Volatility")) +
    geom_line() +
    geom_vline(xintercept = len.train, linetype = "dashed", color = "red") +
    scale_color_manual(values = c(
      "Actual Volatility" = "black", "ARIMA prediction" = "orange", "Regression prediction" = "blue", "HAV prediction" = "green", "GARCH prediction" = "red")) +
    ggtitle(paste0("Actual vs Predicted Volatility for Stock ID ", stock_id, " at time ID ", time_id)) +
    xlab("Time bucket") +
    ylab("Volatility") +
    annotate("text", x = len.train, y = 0, label = len.train)
  
  if(!is.null(models_to_show)) {
    for (k in models_to_show) {
      if (k == "1") {
        plot <- plot + geom_line(data = arima.pred.df, aes(x = time_bucket,y = pred_vol, color = "ARIMA prediction"))
      }
      else if (k == "2") {
        plot <- plot + geom_line(data = reg.pred.df, aes(x = time_bucket,y = pred_vol, color = "Regression prediction"))
      }
      else if (k == "3") {
        plot <- plot + geom_line(data = hav.pred.df, aes(x = time_bucket,y = pred_vol, color = "HAV prediction"))
      }
      else if (k == "4") {
        plot <- plot + geom_line(data = garch.pred.df, aes(x = time_bucket,y = pred_vol, color = "GARCH prediction"))
      }
    }
  }
  print(plot)
}


plot_performance <- function(stock_id, method_type="RMSE") {
  methods <- list("MSE" = 1, "RMSE" = 2, "MAE" = 3, "QLIKE" = 4)
  method_idx <- methods[[method_type]]
  arima_1 <- (unlist(arima_res[[method_idx]][[stock_id]]))
  reg_1 <- (unlist(reg_res[[method_idx]][[stock_id]]))
  hav_1 <- (unlist(hav_res[[method_idx]][[stock_id]]))
  garch_1 <- (unlist(garch_res[[method_idx]][[stock_id]]))
  
  # Combine the vectors into a list
  vectors <- list(arima_1, reg_1, hav_1, garch_1)
  names(vectors) = c("ARIMA", "Linear Regression", "HAV-RV", "GARCH")
  
  # Calculate means for each vector
  vector_means <- sapply(vectors, mean)
  
  # Identify the vector with the lowest mean
  lowest_mean_idx <- which.min(vector_means)
  
  boxplot(vectors, 
          names = names(vectors), 
          col = ifelse(names(vectors) == names(vectors)[lowest_mean_idx], "red", "gray"),
          main = paste0("Models performance at cluster ", stock_id), 
          ylab = method_type,
          outline = FALSE)
}