arsenal_mean <- function(x, na.rm=TRUE, weights = NULL, ...) {
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else {
    wtd.mean(x, weights=weights, na.rm=na.rm)
  }
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL)
}

arsenal_sd <- function(x, na.rm=TRUE, weights = NULL, ...) {
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else {
    s <- sqrt(wtd.var(x, weights=weights, na.rm=na.rm))
    if(is.Date(x)) list(as.difftime(s, units = "days")) else s
  }
  as.tbstat(y)
}

cv <- function(x, ...){
  y <- arsenal_sd(x, ...) / arsenal_mean(x, ...) * 100
  as.tbstat(y)
}

gcv <- function(x, ...){
  y <- sqrt(exp(log(gsd(x, ...))**2 - 1)) * 100
  as.tbstat(y)
}

meancv <- function(x, ...){
  m <- arsenal_mean(x, ...)
  cv <- arsenal_sd(x, ...) / arsenal_mean(x, ...) * 100
  y <- c(m, cv)
  as.tbstat(y, parens = c("(", ")"))
}


gmeancv <- function(x, ...){
  m <- gmean(x, ...)
  cv <- arsenal_sd(x, ...) / arsenal_mean(x, ...) * 100
  y <- c(m, cv)
  as.tbstat(y, parens = c("(", ")"))
}

gmeangcv <- function(x, ...){
  m <- gmean(x, ...)
  gcv <- sqrt(exp(log(gsd(x, ...))**2 - 1)) * 100
  y <- c(m, gcv)
  as.tbstat(y, parens = c("(", ")"))
}

stats.labels <- list(
  Nmiss = "N-Miss",
  Nmiss2 = "N-Miss",
  meansd = "Mean (SD)",
  medianrange = "Median (Range)",
  median = "Median",
  medianq1q3 = "Median (Q1, Q3)",
  q1q3 = "Q1, Q3",
  iqr = "IQR",
  mean = "Mean",
  sd = "SD",
  var = "Var",
  max = "Max",
  min = "Min",
  meanCI = "Mean (CI)",
  sum = "Sum",
  gmean = "Geom Mean",
  gsd = "Geom SD",
  gmeansd = "Geom Mean (Geom SD)",
  gmeanCI = "Geom Mean (CI)",
  range = "Range",
  Npct = "N (Pct)",
  Nevents = "Events",
  medSurv = "Median Survival",
  medTime = "Median Follow-Up",
  medianmad = "Median (MAD)",
  overall = "Overall",
  total = "Total",
  difference = "Difference",
  cv = 'CV (%)',
  gcv = 'Geometric CV (%)',
  meancv = 'Mean (CV (%))',
  gmeancv = 'Geometric Mean (CV (%))',
  gmeangcv = 'Geometric Mean (Geometric CV (%))'
)

# Modify summary stats to be output by the tableby function
arsenal_control <- tableby.control(
  test = FALSE,
  total = FALSE,
  numeric.stats = c("N",
                    "Nmiss",
                    "meansd",
                    # "cv",
                    # "gcv",
                    # "meancv",
                    # "gmeancv",
                    "gmeangcv",
                    "medianq1q3",
                    "range"
  ),
  stats.labels = stats.labels
)