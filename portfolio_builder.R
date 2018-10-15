installedPackages <- row.names(installed.packages())

packages <- c("shiny", "quantmod", "plotly", "jsonlite", "lubridate", "pso", "QRM", "zoo", "BLCOP", 
              "fBasics", "ghyp", "shinyjs", "rhandsontable", "DT", "gridExtra", "plotly")
packagesForInstallation <- setdiff(packages, installedPackages)
if(length(packagesForInstallation) > 0) {
  for(ipn in packagesForInstallation) {
    install.packages(ipn)
  }
}


library(shiny)
library(quantmod)
library(plotly)
library(jsonlite)
library(lubridate)
library(QRM)
library(zoo)
library(BLCOP)
library(fBasics)
library(ghyp)
library(pso)
library(shinyjs)
library(DT)
library(gridExtra)
library(plotly)
library(rhandsontable)




#################################################
#######
#######         F U N C T I O N S
#######


timeSeriesData <- function(x) {
  na_finger <- is.na(x)
  
  na_start <- 0
  if(na_finger[1]) {
    for(index in 1:length(x)) {
      if(!is.na(x[index])) {
        break
      }
      na_start <- index
    }
  }
  
  x_data <- data.frame(is_na = na_finger, x = c(NA, diff(log(c(rep(NA, na_start), zoo::na.locf(x))))))
  x_data[which(x_data$is_na), "x"] <- NA
  
  x_data[, "x"]
}



#   These method returns the prices' returns, determine the boundaries for the number of 
# potential assets and the last prices
dataPreparation <- function(pricesData, conversion_prices, leverages, fund, direction = c("only long", "only short", "both")) {
  JJ <- NULL
  returnData <- NULL
  
  direction <- match.arg(arg = direction, choices = c("only long", "only short", "both"))
  
  for(colIndex in 1:ncol(pricesData)) {
    returnData <- cbind(returnData, timeSeriesData(pricesData[, colIndex]))
  }
  
  returnData <- as.data.frame(returnData)
  names(returnData) <- names(pricesData)
  
  prices <- pricesData[nrow(pricesData), ]
  
  
  prices[which(is.na(prices))] <- Inf 
  
  lowConstr <- -trunc(fund*leverages/prices/conversion_prices)
  highConstr <- trunc(fund*leverages/prices/conversion_prices)
  
  prices[which(is.infinite(prices))] <- 0 
  
  if(direction == "only long") {  lowConstr <- rep(0, length(prices)) }
  if(direction == "only short") {  highConstr <- rep(0, length(prices)) }
  
  
  JJ$returnData <- returnData
  JJ$highConstr <- highConstr; JJ$lowConstr <- lowConstr
  JJ$prices <- prices
  
  JJ
}



conversion_price_function <- function(assets_currencies, all_currency_pairs, cacn) {
  
  all_currency_pairs <- fromJSON(all_currency_pairs)
  
  
  #   If we have an empty list "all_currency_pairs", then all assets are measured with clients 
  # account currency. So we multiply all with 1
  if(length(all_currency_pairs) == 0) {
    return(rep(1, length(assets_currencies)))
  }
  
  # Remove all duplicated and unused. If we have such pairs
  part1 <- which(!duplicated(names(all_currency_pairs)))
  part2 <- which(grepl(pattern = cacn, x = names(all_currency_pairs)))
  all_currency_pairs = all_currency_pairs[intersect(part1, part2)]
  
  pair_names <- strsplit(x = names(all_currency_pairs), split = "/")
  finger <- unlist(lapply(X = pair_names, FUN = function(x) {which(x %in% cacn)}))
  
  
  all_currency_names <- array(dim = length(all_currency_pairs))
  for(i in 1:length(all_currency_pairs)) {
    all_currency_names[i] <- pair_names[[i]][setdiff(c(1, 2), finger[i])]
  }
  
  if(any(finger == 1)) {
    all_currency_pairs[which(finger == 1)] <- 1/all_currency_pairs[which(finger == 1)]
  }
  
  
  
  result <- array(10^5, length(assets_currencies))
  for(index in 1:length(assets_currencies)) {
    if(assets_currencies[index] == cacn) {
      result[index] <- 1  
    } else if(assets_currencies[index] %in% all_currency_names) {
      result[index] <- all_currency_pairs[which(all_currency_names == assets_currencies[index])][[1]]
    } 
  }
  
  result
}



loadingData <- function(assets, start, end) {
  JJ <- NULL
  returns_data <- NULL
  
  x <- lapply(X = assets, FUN = function(x, start, end) {
    pr <- data.frame(getSymbols(x, auto.assign = FALSE,
                                from = as.Date(start), to = as.Date(end)))
    pr <- data.frame(Date = row.names(pr), Close = pr[, 4])
    names(pr) <- c("Date", x)
    return(pr)
  }, start, end)
  
  
  prices_data <- x[[1]]
  for(asset_index in 2:length(x)) {
    prices_data <- merge(prices_data, x[[asset_index]], all = T)
  }
  
  jsonlite::toJSON(prices_data[, which(names(prices_data) != "Date")])
}



dataForUsing <- function(prices_data_in_JSON_format, parametres_in_JSON_format, conversion_prices = NULL) {
  JJ <- NULL
  
  JJ$asset_names <- names(fromJSON(prices_data_in_JSON_format))
  
  pricesData <- as.matrix(fromJSON(prices_data_in_JSON_format))
  JJ$pricesData <- pricesData
  parametres <- fromJSON(parametres_in_JSON_format)
  
  
  JJ$fund <- parametres$fund
  JJ$period <- parametres$period
  JJ$leverages <- unlist(parametres$leverages)
  JJ$currency_names <- unlist(parametres$currency_names)
  JJ$cacn <- parametres$account_currency
  
  
  # If we haven't conversiont prices we decide that the assets are measured in customer's account currency
  if(!is.null(conversion_prices)) {
    conversion_prices <- conversion_price_function(assets_currencies = JJ$currency_names, 
                                                   all_currency_pairs = conversion_prices, cacn = JJ$cacn)
  } else {
    conversion_prices <- rep(1, ncol(pricesData))
  }
  JJ$conversion_prices = conversion_prices
  
  JJ$minProfit <- parametres$minProfit
  JJ$maxLoss <- min(parametres$maxLoss, JJ$fund)
  JJ$optimization <- parametres$optimization
  JJ$direction <- parametres$direction
  JJ$typeOfMethod <- parametres$typeOfMethod
  
  
  dp <- dataPreparation(pricesData = pricesData, conversion_prices = conversion_prices, 
                        leverages = JJ$leverages, fund = JJ$fund, direction = JJ$direction)
  returnData <- dp$returnData
  #  training_horizon <- min(max(600, 20*JJ$period), 1000)
  training_horizon <- min(max(100, 20*JJ$period), 1000)
  
  #  print(paste("Investment horizon:", training_horizon))
  n <- nrow(returnData)
  JJ$returnData <- returnData[c(max(n - training_horizon + 1, 1):n), ]
  
  JJ$lowConstr <- dp$lowConstr
  JJ$highConstr <- dp$highConstr
  JJ$prices <- dp$prices*JJ$conversion_prices
  
  jsonlite::toJSON(JJ)
}

####
####        END
####
################



seasonalEffect <- function(portfolioValues, freq) {
  if(missing(freq)) {
    acf_v <- acf(x = portfolioValues, lag.max = 50, plot = TRUE)$acf
    
    freq <- NULL
    for(index in 3:(length(acf_v)-2)) {
      if(all(acf_v[index] > max(acf_v[index - c(-2, -1, 1, 2)]),
             min(acf_v[index + seq(-2, 2, by = 1)]) >= 0.05)) {
        freq <- c(freq, index)
      }
    }
    
    if(length(freq) > 0) {
      freq <- freq[which(acf_v[freq] == max(acf_v[freq]))]  
    } else {
      freq <- 1
    }
  }
  
  
  ts_portfolioValues <- ts(diff(portfolioValues), freq = freq)
  armaModel <- forecast::auto.arima(ts_portfolioValues, max.Q = 0, max.D = 0, max.d = 0)
  
  #print(freq)
  #print(armaModel)
  
  result <- 0
  
  estimate <- armaModel$coef["sar1"]
  if(!is.na(estimate)) {
    se <- sqrt(armaModel$var.coef["sar1", "sar1"])
    result <- ifelse(estimate/se > 2.5, min(unname(estimate), 1), 0)
  }
  
  
  result
}



diversificationRatio <- function(weights, crm) {
  
  sigmas <- crm$sigmas; copula <- crm$cop
  
  DR_portfolio <- sum(abs(weights)*sigmas) / sqrt(t(weights*sigmas) %*% copula %*% t(t(weights*sigmas)))
  
  
  ww <- rep(1/length(weights), length(weights))
  middle_DR_portfolio <- sum(abs(ww)*sigmas) / sqrt(t(ww*sigmas) %*% copula %*% t(t(ww*sigmas))) 
  
  data.frame(DR_portfolio, middle_DR_portfolio)
}



randomizeResult <- function(r) {
  n <- length(r)
  rr <- rank(r) %% 2
  part1 <- sort(r[which(rr == 1)][1:trunc(n/2)], decreasing = TRUE)
  part2 <- sort(r[which(rr == 0)][1:trunc(n/2)], decreasing = FALSE)
  
  c(part1, 1, part2)
}



resultPreview <- function(x, fund, period, prices, cp, cop, sigmas, tt = 0) {
  JJ <- NULL
  
  JJ$assets<- names(prices)
  JJ$quantity <- round(x) #   Quantity
  JJ$marketShare <- JJ$quantity*prices
  

  
  pr <- JJ$quantity*prices/(sum(abs(JJ$quantity*prices)) + 1e-5)
  
  risk <- sqrt( ((pr*sigmas) %*% cop) %*% (pr*sigmas) )[1, 1]*sqrt(period) 
  JJ$risk <- 2*risk
  #  95% for portfolio's risk for exact period. That's why we multiply by 2
  
  JJ$riskSize <- JJ$risk*sum(abs(JJ$quantity)*prices)
  JJ$return <- sum((cp$mu - cp$s*tt*sign(pr))*pr)*period  #   Portfolio's extected return for the period
  JJ$profitOrLoss <- JJ$return*sum(abs(JJ$quantity)*prices)
  
  
  JJ$Investment <- sum(abs(JJ$quantity)*prices) # Used resources
  
  q <- c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
  r <- randomizeResult(rnorm(length(q), mean = 1, sd = 0.005))
  # Randomize the result 
  
  PL_table <- round((JJ$return + risk*qnorm(q))*JJ$Investment*r, 2)
  names(PL_table) <- paste0("probab ", q*100)#, "%")
  JJ$PL_table <- PL_table
  
  JJ
}





################
####
####       Measure estimator
####

splitingData <- function(returnData, horizon) {
  JJ <- NULL
  
  horizon <- ifelse(is.character(horizon), fromJSON(horizon), horizon)
  
  n <- nrow(returnData)
  finger <- min(trunc(n/2), max(10, horizon))
  
  # Show us where are the NA values and shift the split point
  if(any(is.na(returnData))) {
    naObservationFinger <- which(apply(returnData, 1, function(x) any(is.na(x))))
    split_point <- tail(setdiff(1:n, naObservationFinger), finger)[1] - 1
  } else {
    split_point <- n - finger  
  }
  
  
  JJ$rd_training <- returnData[1:split_point, ]
  JJ$rd_testing <- returnData[(split_point + 1):nrow(returnData), ]
  
  JJ
}



measureEstimation <- function(dfu) {
  returnData <- jsonlite::fromJSON(dfu)$returnData
  if(!is.data.frame(returnData)) {
    returnData <- as.data.frame(returnData)
  }
  
  horizon <- fromJSON(dfu)$period
  horizon <- ifelse(is.character(horizon), fromJSON(horizon), horizon)
  
  
  JJ <- NULL
  measures <- NULL
  
  
  # Provide training and testing datasets  
  spData <- splitingData(returnData = returnData, horizon = horizon)
  rd_training <- spData$rd_training
  rd_testing <- spData$rd_testing
  
  # Tesing for arch effects
  archTestForAllSeries <- 
    unlist(lapply(rd_training, function(x) {FinTS::ArchTest(x, lags = 2)$p.value[[1]]}))
  
  JJ <- ghdMeasure(rd_training = rd_training)
  JJ$measureEstimationMethod <- "GHD"
  
  
  std_error <- function(x) {
    x1 <- x[which(!is.na(x))]
    sd(x1)/sqrt(length(x1))
  }
  
  mu_ts <- data.frame(mu = apply(rd_testing, 2, mean, na.rm = T), 
                      s = apply(rd_testing, 2, std_error))
  names(mu_ts) <- c("mu", "s")
  
  
  JJ$mu_ts <- mu_ts
  JJ$sigmas_ts <- apply(rd_testing, 2, function(x) {sd(x, na.rm = TRUE)})
  
  JJ1 <- NULL
  JJ1$data_for_using_in_JSON_format <- fromJSON(dfu)
  JJ1$asset_measures_in_JSON_format <- JJ
  
  jsonlite::toJSON(JJ1)
}



ghdMeasure <- function(rd_training) {
  JJ <- NULL
  
  model <- ghyp::stepAIC.ghyp(rd_training, silent = TRUE)
  model <- model$best.model
  
  #   For confidencce interval I have to use a bootstrap method but it is much expensive for 
  # computation :(
  
  crm <- from_covariation_to_correlation(model@sigma)
  
  mu <- data.frame(mu = model@expected.value, s = crm$sigmas/sqrt(nrow(rd_training)))
  
  
  JJ$mu <- mu
  JJ$cop <- crm$cor
  JJ$sigmas <- crm$sigmas
  
  JJ
}



####         END




investmentConstrain <- function(x, prices, leverages, fund) {
  percent_of_real_investment <- sum(abs(x)*prices/leverages)/fund - 1
  10*max(0, percent_of_real_investment, ifelse(percent_of_real_investment < -0.1, 1, 0))
}



estimatedMeasuresStatitionary <- function(pr, cp, sigmas, copula, cp_ts, sigmas_ts, tt,
                                          muMultiplier, sigmaMultiplier) {
  muFunct <- sum((cp$mu - cp$s*tt*sign(pr))*pr)
  muFunct_ts <- sum((cp_ts$mu - cp_ts$s*tt*sign(pr))*pr)
  
  mu_max_v <- max(abs(muFunct), abs(muFunct_ts))
  mu_min_v <- min(abs(muFunct), abs(muFunct_ts))
  mu_constr <- mu_max_v/mu_min_v * (1 + ifelse(min(muFunct, muFunct_ts) > 0, 0, 1))
  
  
  muFunct <- sum((cp$mu - cp$s*tt*sign(pr))*pr)
  muFunct_ts <- sum((cp_ts$mu - cp_ts$s*tt*sign(pr))*pr)
  
  
  
  
  sigmaFunct <- sqrt(((pr*sigmas) %*% copula) %*% (pr*sigmas) + 1e-8)
  sigmaFunct_ts <- sqrt(((pr*sigmas_ts) %*% copula) %*% (pr*sigmas_ts) + 1e-8)
  
  sigma_constr <- max(sigmaFunct, sigmaFunct_ts) / min(sigmaFunct, sigmaFunct_ts)
  
  
  muMultiplier*muFunct_ts + sigmaMultiplier*sigma_constr
}




maxReturnConstantVarF <- function(x, prices, leverages, sigmas, copula, cp, maxRisk, fund, tt, cp_ts, sigmas_ts, period) {
  x <- round(x)
  if(all(x == 0)) { return(10^10) }
  
  pr <- x*prices/sum(abs(x*prices))
  volatConstrain <- sqrt(((pr*sigmas) %*% copula) %*% (pr*sigmas))
  includedAssets <- sum(x != 0)
  
  functVal <- sum((cp$mu - cp$s*tt*sign(pr))*pr)
  
  constr1 <- max(0, 100*(sqrt(volatConstrain)/maxRisk - 1))
  constr2 <- investmentConstrain(x, prices, leverages, fund)*period / 20
  
  constr3 <- estimatedMeasuresStatitionary(pr, cp, sigmas, copula, cp_ts, sigmas_ts, tt, 
                                           muMultiplier = 2, sigmaMultiplier = 10)
  
  
  -5*functVal*ifelse(functVal < 0, 100, 1) + 10*constr1 + constr2 + constr3
}



minRiskConstantRetF <- function(x, prices, leverages, sigmas, copula, cp, minReturn, fund, tt, cp_ts, sigmas_ts, period) {
  x <- round(x)
  if(all(x == 0)) { return(10^10) }
  
  pr <- x*prices/sum(abs(x*prices))
  
  
  
  d <- max(c(0, which(abs(minReturn)*10^seq(1, 6) < 10)))
  #strsplit(as.character(format(minReturn, scientific = FALSE)), split = "\\.")[[1]]
  multiplier <- 10^d
  
  returnConstrain <- sum((cp$mu - cp$s*tt*sign(pr))*pr)
  includedAssets <- sum(x != 0)
  
  functVal <- sqrt(((pr*sigmas) %*% copula) %*% (pr*sigmas)) * 10^(d-2)
  constr1 <- max(0, minReturn - returnConstrain)*multiplier
  constr2 <- investmentConstrain(x, prices, leverages, fund)*period / 20
  constr3 <- estimatedMeasuresStatitionary(pr, cp, sigmas, copula, cp_ts, sigmas_ts, tt, 
                                           muMultiplier = 10, sigmaMultiplier = 2)
  
  #  print(c(functVal, constr2, constr3)) 
  5*functVal + 10*constr1 + constr2 + constr3
}



maxSharpeF <- function(x, prices, leverages, sigmas, copula, cp, fund, tt, cp_ts, sigmas_ts, period) {
  x <- round(x)
  if(all(x == 0)) { return(10^10) }
  
  
  
  pr <- x*prices/sum(abs(x*prices))
  includedAssets <- sum(x != 0)
  
  functVal <- sum((cp$mu - cp$s*tt*sign(pr))*pr)/sqrt(((pr*sigmas) %*% copula) %*% (pr*sigmas) + 1e-8)
  
  
  # Comparing results with data from test set
  f_t <- sum((cp_ts$mu - cp_ts$s*tt*sign(pr))*pr)/sqrt(((pr*sigmas_ts) %*% copula) %*% (pr*sigmas_ts) + 1e-8)
  max_v <- max(abs(functVal), abs(f_t))
  min_v <- min(abs(functVal), abs(f_t))
  const1 <- max_v / min_v + 3*ifelse(min(functVal, f_t) > 0, 0, 1)
  
  
  constr2 <- investmentConstrain(x, prices, leverages, fund)*period / 20
  
  
  #  print(c(functVal, const1, constr2))
  -10*functVal + const1 + constr2 #+ constr3
}





from_correlation_to_covariation <- function(sigmas, cor_matrix) {
  cov <- diag(sigmas^2)
  
  for(row_index in 1:(nrow(cov)-1)) {
    for(col_index in (row_index+1):nrow(cov)) {
      cov_value <- sigmas[col_index]*sigmas[row_index]*cor_matrix[row_index, col_index]
      cov[row_index, col_index] <- cov[col_index, row_index] <- cov_value
    }
  }
  
  cov
}



from_covariation_to_correlation <- function(cov_matrix) {
  JJ <- NULL
  sigmas <- sqrt(diag(cov_matrix))
  
  cor <- diag(ncol(cov_matrix))
  
  for(row_index in 1:(nrow(cor)-1)) {
    for(col_index in (row_index+1):nrow(cor)) {
      cor_value <- cov_matrix[row_index, col_index] / (sigmas[col_index]*sigmas[row_index])
      cor[row_index, col_index] <- cor[col_index, row_index] <- cor_value
    }
  }
  
  JJ$cor <- cor
  JJ$sigmas <- sigmas
  
  
  JJ
}



# assets_currencies - string. Example: '["EUR", "EUR", "USD", "EUR"]'
#   It depends from currency assets is measured -  DAX: EUR, DJI: USD, CAC40: EUR, ...
# all_currency_pairs - JSON with prices. Example: '[{"EUR/USD": 1.1, "USD/JPY": 100, "GBP/USD": 1.2}]'
# cacn == customer_account_currency_name - The currency name of the client's account








portfolioOptimizer <- function(dfu, investor_view = NULL, return_correction_activated = F,
                               hedge_mode = NULL, correction = FALSE) {
#  print(fromJSON(dfu)$data_for_using_in_JSON_format)
  print(investor_view)
  dataForUse <- fromJSON(dfu)$data_for_using_in_JSON_format
  returnData <- dataForUse$returnData
  
  
  asset_measures_in_JSON_format <- fromJSON(dfu)$asset_measures_in_JSON_format
  
  asset_names <- dataForUse$asset_names
  pricesData <- dataForUse$pricesData
  fund <- dataForUse$fund
  period <- dataForUse$period
  print(str(period))
  leverages <- dataForUse$leverages
  
  
  minProfit <- dataForUse$minProfit
  maxLoss <- min(dataForUse$maxLoss, fund)
  optimization <- dataForUse$optimization
  direction <- dataForUse$direction
  typeOfMethod <- dataForUse$typeOfMethod
  prices <- dataForUse$prices
  lowConstr <- dataForUse$lowConstr
  highConstr <- dataForUse$highConstr
  
  crm <- asset_measures_in_JSON_format
  cp <- crm$mu
  cp_ts <- crm$mu_ts
  
  
  if(correction) {
    correction <- abs(cp$mu/cp$s)    
    cp$mu[which(correction <= 2)] <- 0;     cp$s[which(correction <= 2)] <- 1e-8
    
    correction <- abs(cp_ts$mu/cp_ts$s)    
    cp_ts$mu[which(correction <= 2)] <- 0;  cp_ts$s[which(correction <= 2)] <- 1e-8
  }
  
  
  minReturn <- minProfit/fund/period
  maxRisk <- maxLoss/fund/sqrt(period)/2
  # devide by 2 because we want to take 95% from the all risk when we use confidential interval
  
  
  tt <- 0 #  cp$s*tt
  sigmas <- crm$sigmas; copula <- crm$cop
  sigmas_ts <- crm$sigmas_ts
  
  # Whether to use portfolio optimization
  FLAG <- (any(cp$mu > 0) & direction == "only long") |
    (any(cp$mu < 0) & direction == "only short") |
    (any(cp$mu != 0) & direction == "both")
  
  
  
  if(FLAG) {
    
    # Investor view
    if(!is.null(investor_view)) {
      investor_view_par <- fromJSON(investor_view)
      print("Use investor view!")
      
      # q - expected assets' returns
      views <- BLViews(P = diag(ncol(returnData)), q = investor_view_par$mu, 
                       confidences = 1/investor_view_par$s, assetNames = asset_names)
      
      cov <- from_correlation_to_covariation(sigmas = sigmas, cor_matrix = copula)
      
      post <- posteriorEst(views, mu = cp$mu, tau = 1, sigma = cov, kappa = 1)
      cp$mu <- post@posteriorMean
      
      new_crm <- from_covariation_to_correlation(post@posteriorCovar)
      sigmas <- new_crm$sigmas; copula <- new_crm$cor
    }
    
    
    if(is.null(investor_view) & return_correction_activated) {
      lastReturnData <- tail(returnData, max(period, 20))
      auto_BL_result_mu <- colMeans(lastReturnData)
      auto_BL_result_se <- apply(lastReturnData, 2, sd)/sqrt(period)
      
      views <- BLViews(P = diag(ncol(returnData)), q = auto_BL_result_mu, 
                       confidences = 1/auto_BL_result_se, assetNames = asset_names)
      
      cov <- from_correlation_to_covariation(sigmas = sigmas, cor_matrix = copula)
      
      post <- posteriorEst(views, mu = cp$mu, tau = 1, sigma = cov, kappa = 1)
      cp$mu <- post@posteriorMean
      
      new_crm <- from_covariation_to_correlation(post@posteriorCovar)
      sigmas <- new_crm$sigmas; copula <- new_crm$cor
    }
    
    # Hedge mode
    if(!is.null(hedge_mode)) {
      
    }
    
    optimFunct <- function(x) {
      switch(optimization,
             "max return" = maxReturnConstantVarF(x, prices, leverages, sigmas, copula, cp, maxRisk, 
                                                  fund, tt, cp_ts = cp_ts, sigmas_ts = sigmas_ts, period),
             "min risk" = minRiskConstantRetF(x, prices, leverages, sigmas, copula, cp, minReturn, fund, 
                                              tt, cp_ts = cp_ts, sigmas_ts = sigmas_ts, period),
             "sharpe ratio" = maxSharpeF(x, prices, leverages, sigmas, copula, cp, fund, tt,
                                         cp_ts = cp_ts, sigmas_ts = sigmas_ts, period)
             )
    }
    
    x <- NULL
    
    useOptimization <- TRUE
    
    if(optimization == "max return" & min(sigmas) > maxRisk) {
      marketShare <- trunc(fund/prices)*prices
      firstSelection <- which(sigmas == min(sigmas))
      
      optimalTValue <- switch(direction,
                              "only long" = -cp$mu*marketShare/cp$s, 
                              "only short" = cp$mu*marketShare/cp$s,
                              "both" = -abs(cp$mu*marketShare/cp$s))
      optimalTValue <- optimalTValue[firstSelection]
      
      secondSelection <- firstSelection[which(optimalTValue == min(optimalTValue))]
      
      x <- rep(0, length(marketShare))
      x[secondSelection] <- (sign(cp$mu)*trunc(fund/prices))[secondSelection]
      
      useOptimization <- FALSE
    }
    
    optimalReturn <- switch(direction, "only long" = cp$mu, "only short" = -cp$mu, "both" = abs(cp$mu))
    if(optimization == "min risk" & max(optimalReturn) < minReturn) {
      marketShare <- trunc(fund/prices)*prices
      minSigma <- min(sigmas[which(optimalReturn == max(optimalReturn))])
      
      finger <- which(sigmas == minSigma)
      
      x <- rep(0, length(marketShare))
      x[finger] <- (sign(cp$mu)*trunc(fund/prices))[finger]
      
      useOptimization <- FALSE
    }
    
    
    if(useOptimization) {
      if(typeOfMethod == "GA") {
        set.seed(8889)
        ga.port <- genalg::rbga(stringMin = lowConstr, stringMax = highConstr, evalFunc = optimFunct, 
                                popSize = 1500, iters = 200)
        x <- ga.port$population[which.min(ga.port$best), ]
      } else if(typeOfMethod == "PSO") {
        set.seed(8889)
        C <- list(trace = 1, maxit = 3000, s = 50, REPORT = 500, abstol = 1e-3)
        pso.port <- pso::psoptim(par = rep(NA, length(lowConstr)), fn = optimFunct, lower = lowConstr, 
                                 upper = highConstr, control = C)
        x <- pso.port$par 
      }
    }
    
  } else {
    print("All trends are zeroes")
    x <- rep(0, length(sigmas))
    useOptimization <- FALSE
  }
  
  
  x <- round(x)
  print(paste0("Shares: ", paste(x, collapse = ",")))
  pr <- x*prices/sum(abs(x*prices))
  
  mu1 <- sum((cp$mu - cp$s*tt*sign(pr))*pr)
  sigma1 <- sqrt(((pr*sigmas) %*% copula) %*% (pr*sigmas) + 1e-8)
  functVal <- mu1 / sigma1
  print(paste("Predicted MU:", mu1, "Predicted SIGMA:", sigma1))
  print(paste("Predicted SHARPE VALUE:", functVal))
  
  
  # Comparing results with data from test set
  mu2 <- sum((cp_ts$mu - cp_ts$s*tt*sign(pr))*pr)
  sigma2 <- sqrt(((pr*sigmas_ts) %*% copula) %*% (pr*sigmas_ts) + 1e-8)
  f_t <- mu2 / sigma2
  print(paste("Test real MU:", mu2, "Test real SIGMA:", sigma2))
  print(paste("Test real SHARPE VALUE:", f_t))
  
  
  #   After all I have to multiply "minReturn" by "period" and "fund" and 
  # "maxRisk" - by "fund" and "sqrt(period)"
  JJ <- resultPreview(x = x, fund = fund, period = period, prices = prices, 
                      cp = cp, cop = crm$cop, sigmas = sigmas, tt)
  
  JJ$measureEstimationMethod <- crm$measureEstimationMethod
  JJ$usedOptimizationAlgorithm <- useOptimization
  JJ$asset_names <- asset_names
  
  
  #       New
  
  # Historical graphic
  start_position <- min(which(apply(pricesData, 1, function(x) {all(!is.na(x))})))
  returns_data <- pricesData[start_position:nrow(pricesData), ]
  for(col_index in 1:ncol(returns_data)) {
    returns_data[, col_index] <- log(na.locf(returns_data[, col_index])/returns_data[1, col_index])
  }
  historical_graphic_check <- tail(array(t(JJ$marketShare/fund) %*% t(returns_data)), 1)
  
  if(JJ$profitOrLoss <= 0 | historical_graphic_check <= 0) {
    x <- rep(0.0, length(x))
    JJ <- resultPreview(x = x, fund = fund, period = period, prices = prices, 
                        cp = cp, cop = crm$cop, sigmas = sigmas, tt)
    
    JJ$measureEstimationMethod <- crm$measureEstimationMethod
    JJ$usedOptimizationAlgorithm <- useOptimization
    JJ$asset_names <- asset_names
    
    JJ$portfolioValues <- rep(0, nrow(returns_data))
    
    JJ$seasonalEffect <- 0
    JJ$DR <- data.frame(DR_portfolio = 0, middle_DR_portfolio = 100)
    
  }
  
  
  
  
  if(any(x != 0)) {
    start_position <- min(which(apply(pricesData, 1, function(x) {all(!is.na(x))})))
    returns_data <- pricesData[start_position:nrow(pricesData), ]
    for(col_index in 1:ncol(returns_data)) {
      returns_data[, col_index] <- log(na.locf(returns_data[, col_index])/returns_data[1, col_index])
    }
    JJ$portfolioValues <- array(t(JJ$marketShare/fund) %*% t(returns_data))
    
    JJ$seasonalEffect <- seasonalEffect(JJ$portfolioValues)
    JJ$DR <- diversificationRatio(JJ$marketShare/fund, crm)
  }
  print(JJ)
  
  
  toJSON(JJ)
}






#######
#######           E N D
#######
#################################################







###########################################




assets_list <- c("AA", "AXP", "BA", "BAC", "C", "CAT", "CVX", #"DD", 
                 "DIS", "GE", "GM", "HD",
                 "HPQ", "IBM", "INTC", "JNJ", "JPM", "AIG", "KO", "MCD", "MMM", "MRK", "MSFT", 
                 "PFE", "PG", "T", "UTX", "VZ", "WMT", "XOM")



initial_portfolio_cond_FP <- fluidPage(
    numericInput(inputId = "investment", label = h3("Investment size:"), value = 10000, min = 1, max = 10^9),
    
    selectInput(inputId = "assets_list", label = h3("Assets"), choices = assets_list, multiple = TRUE), 
    
    dateRangeInput(inputId = "date_range", label = "From ... to ...", 
                   start = Sys.Date() %m-% months(18), end = Sys.Date()),
    
    sliderInput(inputId = "investment_horizon", label = "Investment horizon: (in days)", 
                min = 10, max = 1000, step = 1, value = 100, animate = animationOptions(interval = 300, loop = TRUE)),
    textOutput(outputId = "recommendation_text"),
    
    radioButtons(inputId = "portfolio_type", "Portfolio type:",
                 choiceNames = list("Maximal return", "Minimal risk", "Optimal ratio"),
                 choiceValues = list("max return", "min risk", "sharpe ratio"), 
                 inline = FALSE), 
    numericInput(inputId = "min_ret_constr", label = "Minimal expected return", value = 1, min = 1, max = 10^9),
    numericInput(inputId = "max_risk_constr", label = "Maximal admissible loss", value = 1, min = 1, max = 10^9),
    
    radioButtons(inputId = "directions", "Directions",
                 choiceNames = list("Long & short", "Only long", "Only short"),
                 choiceValues = list("both", "only long", "only short"), 
                 inline = FALSE)
)



estimation_FP <- fluidPage(
  useShinyjs(),
  titlePanel("Trend measures of assets' returns (in %)"),
  fluidRow(
    column(4, tableOutput("mu_table")),
    #column(4, DT::dataTableOutput("mu_table")),
    column(9, rHandsontableOutput("investor_view_mu_table"))
    
  ),
  hr(),
  actionButton(inputId = "investor_view_button", label = "Investor view"),
  hr(),
  titlePanel("Correlations between assets"),
  tableOutput("cor_table"),
  hr(),
  titlePanel("Risk measures of assets"),
  tableOutput("risk_table"),
  actionButton(inputId = "optimization", "Portfolio builder")
)



result_FP <- fluidPage(
  tableOutput("quantity_table"),
  plotOutput("market_share_pie_chart"),
  tableOutput("measures_table"), 
  tableOutput("PL_table"),
  plotOutput("PV_plot")
)



ui <- navbarPage('Portfolio builder', id = "inTabset",
                 tabPanel(title = "Portfolio conditions", initial_portfolio_cond_FP, 
                          value = "panel1", actionButton('estimations', 'Estimations')),
                 tabPanel(title = "Estimations", estimation_FP,
                          value = "panel2"),
                 tabPanel(title = "Result", result_FP,
                          value = "panel3")
                 
)





server <- function(input, output, session) {
  Flag <- F
  
  
  observeEvent(input$estimations, {
    prices_data_in_JSON_format <- loadingData(input$assets_list, input$date_range[1], input$date_range[2])
    
    leverage_cond <- paste(rep(1, length(input$assets_list)), collapse = ",")
    currency_names <- paste(rep("\"USD\"", length(input$assets_list)), collapse = ",")
    pars <- paste0('[{"fund":', input$investment, ', "period":', input$investment_horizon,
                   ', "leverages": [', leverage_cond, '], "currency_names": [', currency_names, 
                   '], "account_currency": "EUR", "minProfit": ', input$min_ret_constr, 
                   ', "maxLoss": ', input$max_risk_constr, ', "optimization": "', input$portfolio_type,
                   '", "direction": "', input$directions, '", "typeOfMethod": "PSO"',
                   ', "minNumberOfAssets": 3, "maxNumberOfAssets": 7}]')
      
    parametres_in_JSON_format <- pars
    dfu <- dataForUsing(prices_data_in_JSON_format, parametres_in_JSON_format)
      
    ME <<- measureEstimation(dfu)
#    print(fromJSON(ME))
    temporaryME <- fromJSON(ME)
      
    updateTabsetPanel(session, "inTabset", selected = "panel2")
    
    
    
    
    
    trend_coef_matrix <- temporaryME$asset_measures_in_JSON_format$mu
    
    tr_sig <- 2*pnorm(-abs(trend_coef_matrix[, 1]/trend_coef_matrix[, 2]))
    trend_significant <- rep("Weak", length(tr_sig))
    trend_significant[which(tr_sig < 0.05)] <- "Moderate"
    trend_significant[which(tr_sig < 0.01)] <- "Strong"
    
    mu_estimation_table <- data.frame(assets = temporaryME$data_for_using_in_JSON_format$asset_names, 
                                      trend_coef_matrix*100, trend_significant)
    names(mu_estimation_table) <- c("Assets", "Estimate", "Deviation", "")
    output$mu_table <- renderTable({mu_estimation_table})
    #output$mu_table <- DT::renderDataTable({mu_estimation_table})
    
    
    
    output$investor_view_mu_table <- renderRHandsontable({rhandsontable(temporaryME$asset_measures_in_JSON_format$mu*100)})
    mu_table_rows <- nrow(temporaryME$asset_measures_in_JSON_format$mu)
    
    cor_table <- data.frame(temporaryME$asset_measures_in_JSON_format$cop)
    names(cor_table) <- temporaryME$data_for_using_in_JSON_format$asset_names
    row.names(cor_table) <- temporaryME$data_for_using_in_JSON_format$asset_names
    output$cor_table <- renderTable({cor_table*100}, include.rownames = TRUE)
    
    risk_table <- data.frame(#Assets = temporaryME$data_for_using_in_JSON_format$asset_names,
                             Risk = temporaryME$asset_measures_in_JSON_format$sigmas*100)
    row.names(risk_table) <- temporaryME$data_for_using_in_JSON_format$asset_names
    risk_table <- t(risk_table)
    output$risk_table <- renderTable({risk_table})
    Flag <<- F
    toggle(id = "investor_view_mu_table", condition = Flag)
  })
    
  
  observeEvent(input$investor_view_button, {
# #    toggle(id = "investor_view_mu_table", anim = TRUE)
#     print("in")
#     Flag <<- !Flag
#     toggle(id = "investor_view_mu_table", condition = Flag)
#     print(ifelse(Flag, "use", "not use"))
    
    Flag <<- !Flag
    print(ifelse(Flag, "use", "not use"))
    
    toggle(id = "investor_view_mu_table", condition = Flag)
    
    
    # if(Flag){
    #   shinyjs::show(id = "investor_view_mu_table")
    # } else {
    #   shinyjs::hide(id = "investor_view_mu_table")
    # }
    
    
  })
  
  
  observe({
    
    if(input$portfolio_type == "min risk") {
      toggleState("min_ret_constr", TRUE)
      toggleState("max_risk_constr", FALSE)
    } else if(input$portfolio_type == "max return") {
      toggleState("min_ret_constr", FALSE)
      toggleState("max_risk_constr", TRUE)
    } else {
      toggleState("min_ret_constr", FALSE)
      toggleState("max_risk_constr", FALSE)
    }
    
    
  })
  
  
  observeEvent(input$optimization, {
    investor_view <- NULL
    print(Flag)
    if(Flag){ 
      print(str(hot_to_r(input$investor_view_mu_table)))
      investor_view <- toJSON(hot_to_r(input$investor_view_mu_table)/100)
    }
    
    JJ <<- fromJSON(portfolioOptimizer(ME, investor_view))
      
    updateTabsetPanel(session, "inTabset", selected = "panel3")
        
    quantity_table <- data.frame(Quantity = JJ$quantity, "Market share" = JJ$marketShare)
    row.names(quantity_table) <- JJ$asset_names
    output$quantity_table <- renderTable({t(quantity_table)})
    
    measures_table <- data.frame(Values = c(JJ$risk*100, JJ$riskSize, JJ$return*100, JJ$profitOrLoss, JJ$Investment))
    row.names(measures_table) <- c("Risk (in %)", "Risk size", "Expected return (in %)", "Profit or loss", "Investment")
    output$measures_table <- renderTable({t(measures_table)})
    
    q <- c(0.01, 0.05, 0.1, 0.25)
    q <- sort(c(q, 0.5, 1-q))
    
    
    nn <- trunc(length(q)/2) + 1
    
    
    pos_or_neg <- sign(JJ$PL_table)
    labels_PL <- c(paste0("Min loss in ", 100*q[which(pos_or_neg < 0)], "%"),
                   paste0("Min profit in ", 100*(1 - q[which(pos_or_neg >= 0)]), "%"))
    labels_PL[nn] <- "Expected profit/loss"
    PL_table <- t(data.frame(Value = JJ$PL_table))
    colnames(PL_table) <- labels_PL
    output$PL_table <- renderTable({PL_table})
    
    
    
    output$PV_plot <- renderPlot({plot(JJ$portfolioValues, ylab = "Portfolio returns", type = "l")})
    perc <- round(abs(JJ$marketShare)/sum(abs(JJ$marketShare))*100, 2)
    
    output$market_share_pie_chart <- renderPlot({pie(x = abs(JJ$marketShare), 
                                                     labels = paste0(JJ$asset_names, ": ", perc, "%"))})
        #updateTabsetPanel(session, "inTabset", selected = "panel1")
  })
    
    
}

shinyApp(ui, server)




