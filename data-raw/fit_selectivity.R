#' Fit a logistic function
#'
#' Estimae parameters of logistic function describing
#' a selectivity curve. Ues all length data from groundfish trips
#' over all species
#'


#' define the logistic function
#'
#' @param x parameters of the model
#' @param data data used to calculate RSS
#'
#' @return Residual Sum of squares
#'
logisticF <- function(x,dataFrame) {

  lsm <- x[1]
  selb <- x[2]
  lengthData <- dataFrame$LENGTH
  prob <- dataFrame$cump

  numer <- 1
  denom <- 1 + exp(-selb*(lengthData-lsm))

  f <- numer/denom

  rss <- sum((prob-f)^2)

  return(rss)

}

#read in length data
replicatelengths <- readRDS(here::here("data/gfTripLengths.rds"))

## combine all lengths for a global selectivity
alllengths <- replicatelengths |>
  dplyr::select(Code,LENGTH) |>
  dplyr::mutate(Code = "ALL") |>
  dplyr::arrange(Code,LENGTH) |>
  dplyr::group_by(Code) |>
  dplyr::mutate(prob = (1:dplyr::n())/dplyr::n())

## Length and associated prob
dataFrame <- alllengths |>
  dplyr::ungroup() |>
  dplyr::select(LENGTH) |>
  dplyr::group_by(LENGTH) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::mutate(p = n/sum(n),
                cump=cumsum(p))

# optimization to estimate parameters by minimizing RSS
# initial param estimates
x0 <- c(30,0.1)
opts <- list("algorithm"="NLOPT_LN_COBYLA",
             "xtol_rel"=1.0e-04)
x <- nloptr::nloptr(x0=x0,
               eval_f=logisticF,
               lb=c(0,-Inf),
               ub=c(Inf,Inf),
               opts = opts,
               dataFrame=dataFrame)


x
# fit
fitted <- 1/(1 + exp(-x$solution[2]*(dataFrame$LENGTH - x$solution[1])))
## plot
plot(dataFrame$LENGTH,dataFrame$cump,type = "b")
lines(dataFrame$LENGTH,fitted)

