set.seed(123)

library(bsvarSIGNs)

data <- readxl::read_excel("VARData.xlsx")
data[, 3:7] <- 100 * log(data[, 3:7]) # log transformation

# endogenous variables
domestic <- ts(
  data[, 2:5],
  start = c(1982, 1),
  frequency = 4
)
plot.ts(domestic)

# exogenous variables
foreign <- ts(
  data[, 6:8],
  start = c(1982, 1),
  frequency = 4
)
plot(foreign)

Z <- data[, 6:8] |>
  as.matrix() |>
  bsvars::specify_data_matrices$new(p = 4) # 4 lags
foreign <- rbind(matrix(0, 4, 12), t(Z$X[-nrow(Z$X), ])) # pad with zeros


# sign restrictions of +ve monetary policy shock
# restrictions on impulse response functions
sign_irf <- matrix(NA, 4, 4)
sign_irf[1, 1] <- sign_irf[4, 1] <- 1 # +ve impact on cash rate and exchange rate
sign_irf[3, 1] <- -1 # -ve impact on consumer price index
sign_irf <- array(sign_irf, c(4, 4, 4)) # last for 4 periods

# restrictions on policy reaction function
sign_structural <- matrix(NA, 4, 4)
sign_structural[1, ] <- c(1, -1, -1, 1)

sign_irf[, , 1]

sign_structural

# specify the model
spec <- specify_bsvarSIGN$new(
  domestic,
  p = 4,
  exogenous = foreign,
  sign_irf = sign_irf,
  sign_structural = sign_structural
)

# disable dummy observation priors
spec$prior$Ysoc <- matrix(NA, nrow(spec$prior$Ysoc), 0)
spec$prior$Xsoc <- matrix(NA, nrow(spec$prior$Xsoc), 0)
spec$prior$Ysur <- matrix(NA, nrow(spec$prior$Ysur), 0)
spec$prior$Xsur <- matrix(NA, nrow(spec$prior$Xsur), 0)

# sample posterior draws
post <- estimate(spec, S = 5000)

# compute impulse response functions
irf <- compute_impulse_responses(post, horizon = 20)
plot(irf, probability = 0.68)
