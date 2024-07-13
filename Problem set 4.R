##############################################################################
#                                                                            #
#       Backtesting of American Express Stock exchange rate                  #
#                                                                            #
############################################################################## 

################################ Part (a) ################################ 


library(yfR)
library(zoo)
library(ggplot2)
library(ggpubr)

library(fGarch)
library(ggplot2)

library(forecast)

#I have taken American Express daily stock data from 1 November 2015 to 30 December 2019 for my 
# analysis.

AMEX <- yf_get("AXP", first_date = "2015-11-01", 
            last_date = "2019-12-30")
head(AMEX)
dim(AMEX)

Close <- AMEX$price_close
Date <- as.Date(AMEX$ref_date)
Close <- zoo(Close, order.by = Date)
Ret <- diff(log(Close))


n <- length(AMEX[, 1])
AMEX <- AMEX[n:1, ]
head(AMEX)


t <- time(Close)
min_t <- min(t)
max_t <- max(t)



p1 <- ggplot(data.frame(t = t, xt = Close), aes(x = t, y = xt)) +
  geom_line() +
  xlim(min_t, max_t) +
  xlab("Year") +
  ylab("Closing price") +
  ggtitle(paste0("Daily Closing price of American Express, ", 
                 "November 2015 to December 2019"))

plot(p1)



# Log-returns of the the selected closing price series


log_close <- log(Close)
diff_close <- diff(Close)
ret <- diff(log(Close))

################################ Part (b) ################################ 


p2 <- ggplot(data.frame(t = t, xt = log_close), aes(x = t, y = xt)) +
  geom_line() +
  xlim(min_t, max_t) +
  xlab("Year") +
  ylab("Log of Closing price") +
  ggtitle("Logarithm of the Daily Closing price of American Express")


plot(p2)


 
p3 <- ggplot(data.frame(t = t[-1], xt = diff_close), aes(x = t, y = xt)) +
  geom_line() +
  xlim(min_t, max_t) +
  xlab("Year") +
  ylab("Differences of Closing price") +
  ggtitle("First difference of the Daily Closing price of American Express")

plot(p3)

p4 <- ggplot(data.frame(t = t[-1], xt = ret), aes(x = t, y = xt)) +
  geom_line() +
  xlim(min_t, max_t) +
  xlab("Year") +
  ylab("Log-return of Closing price") +
  ggtitle("Log-returns of the Daily Closing price of American Express")

plot(p4)


plot_all <- ggarrange(p1, p2, p3, p4, ncol = 1, nrow = 4)

plot_all


#the ACF of the returns and of the squared returns.


acf1 <- ggAcf(as.numeric(ret)) +
  ggtitle("Correlogram of the log-returns")
acf2 <- ggAcf(as.numeric(ret)^2) +
  ggtitle("Correlogram of the squared log-returns")
plot_acf <- ggarrange(acf1, acf2, ncol = 1, nrow = 2)
plot_acf


################################ Part (c) ################################ 


library(fGarch)
library(ufRisk)

n_test <- 250
n_ret <- length(Ret)
n_train <- n_ret - n_test

Ret_train <- head(Ret, n_train)
Loss <- -ret


GARCH11 <- garchFit(~ garch(1, 1), ret, trace = FALSE)
sigma_n <- GARCH11@sigma.t

alpha <- 0.99

Z_alpha <- qnorm(alpha)

VaR_n99 = Z_alpha * sigma_n
ES_n97.5  = dnorm(Z_alpha) / (1 - alpha) * sigma_n

violation_points_99 <- Loss > VaR_n99


aparch11 <- garchFit(~ aparch(1, 1), data = Ret_train, trace = FALSE, cond.dist = "std")
sigma_n_aparch <- predict(aparch11, n.ahead = n_test)$standardDeviation

aparch11@fit$ics[["BIC"]]    # APARCH(1,1) with the lowest BIC

aparch11

VaR_n99_aparch <- Z_alpha * sigma_n_aparch
ES_n97.5_aparch <- dnorm(Z_alpha) / (1 - alpha) * sigma_n_aparch

violation_points_99_aparch <- Loss > VaR_n99_aparch


traffic_light_test <- function(violations, alpha) {
  n <- length(violations)
  num_violations <- sum(violations)
  
  green_threshold <- qbinom(0.95, n, 1 - alpha)
  yellow_threshold <- qbinom(0.99, n, 1 - alpha)
  
  if (num_violations <= green_threshold) {
    return("Green")
  } else if (num_violations <= yellow_threshold) {
    return("Yellow")
  } else {
    return("Red")
  }
}

traffic_light_result_garch <- traffic_light_test(violation_points_99, alpha)
traffic_light_result_aparch <- traffic_light_test(violation_points_99_aparch, alpha)

cat("Traffic light test result for GARCH(1,1) model:", traffic_light_result_garch, "\n")
cat("Traffic light test result for APARCH(1,1) model:", traffic_light_result_aparch, "\n")



################################ Part  (d) ################################ 


fcast <- varcast(as.numeric(Close), model = "apARCH", distr = "std", 
                 garchOrder = c(1, 1)) 


Date_test <- tail(Date, n_test)
VaR99 <- zoo(fcast$VaR.v, order.by = Date_test)
ES975 <- zoo(fcast$ES, order.by = Date_test)
Loss_test <- tail(-Ret, 250)

violations <- Loss_test > VaR99
Date_violations <- Date_test[violations]
VaR_violations <- VaR99[violations]

df <- data.frame(Date = Date_test, VaR99 = VaR99, ES975 = ES975, 
                 Loss = Loss_test)
df_vio <- data.frame(Date = Date_violations, VaR = VaR_violations)

colors <- c("Loss_test" = "gray50", "VaR99" = "red", "ES975" = "green")
labels <- c("Loss_test" = "Losses", "VaR99" = "99%-VaR", "ES975" = "97.5%-ES")

plot_risk <- ggplot(df, aes(x = Date)) +
  geom_segment(aes(y = Loss, xend = Date, yend = 0, color = "Loss_test")) +  
  geom_line(aes(y = VaR99, color = "VaR99")) +
  geom_line(aes(y = ES975, color = "ES975")) +
  geom_point(data = df_vio, aes(x = Date, y = VaR), color = "blue", size = 3,
             pch = 13) +
  scale_color_manual(values = colors, 
                     labels = labels,
                     name = "Series") +
  xlab("Month and year") +
  ylab("Loss, 99%-VaR and 97.5%-ES") +
  ggtitle("The test losses together with 99% risk measures")
plot_risk


################################ Part  (e) ################################ 



# Fit the GARCH(1,1) model
GARCH11 <- garchFit(~ garch(1, 1), data = Ret_train, trace = FALSE)

GARCH11

# Extract parameters
mu_G <- coef(GARCH11)["mu"]
omega_G <- coef(GARCH11)["omega"]
alpha_G <- coef(GARCH11)["alpha1"]
beta_G <- coef(GARCH11)["beta1"]

# Fit the APARCH(1,1) model
aparch11 <- garchFit(~ aparch(1, 1), data = Ret_train, trace = FALSE, cond.dist = "std")
 
aparch11

# Extract parameters
mu_A <- coef(aparch11)["mu"]
omega_A <- coef(aparch11)["omega"]
alpha_A <- coef(aparch11)["alpha1"]
beta_A <- coef(aparch11)["beta1"]
delta_A <- coef(aparch11)["delta"]
gamma_A <- coef(aparch11)["gamma1"]




################################      END      ################################ 


