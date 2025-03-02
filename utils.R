library(tinytable)
library(glue)

get_method <- function(mod) {
  mod_class <- class(mod)
  if (length(mod_class) == 1 & mod_class == "lm") {
    wts <- mod$call[["weights"]]
    if (is.null(wts))
      return("OLS")
    ret <- structure("WLS", weights = deparse(wts))
    return(ret)
  }
  return(NULL)
}

check_intercept <- function(mod) {
  attr(terms(mod), "intercept") == 1
}

depvar_name <- function(mod) {
  var_names <- attr(terms(mod), "variables")
  as.character(var_names[[2]])
}

sums_of_squares <- function(mod) {
  mf <- model.frame(mod)


  y <- model.response(mf)
  uhat <- residuals(mod)
  w <- model.weights(mf)
  if (!is.null(w)) {
    sqrt_w <- sqrt(w)
    y <- y * sqrt_w
    uhat <- uhat * sqrt_w
  }

  y_mean <- mean(y)
  list(SSR = as.vector(crossprod(uhat)),
       SST = as.vector(crossprod(y - y_mean)),
       SST_df = nobs(mod) - 1, SSR_df = df.residual(mod))
}

Rsq <- function(mod, adjusted = FALSE) {
  ss <- sums_of_squares(mod)
  if (!adjusted)
    return(1 - ss$SSR / ss$SST)
  else
    return(1 - (ss$SSR/ss$SSR_df) / (SST / ss$SST_df))
}

get_parameters <- function(mod, vcov_type = NULL) {
  if (!is.null(vcov_type)) {
    vcov_fn <- \(x) { sandwich::vcovHC(x, type = vcov_type)}
  } else {
    vcov_fn <- vcov
  }

  bhat <- coef(mod)
  bnames <- names(bhat)
  par <- data.frame(Parameter = bnames, Coefficient = unname(bhat))
  vbhat <- vcov_fn(mod)
  par$SE <- sqrt(diag(vbhat))
  tratio <- par$Coefficient / par$SE
  par$t <- tratio
  par$p <- 2 * (1 - pt(abs(tratio), df = df.residual(mod)))

  # Compute R-squared and adjusted R-squared
  ss <- sums_of_squares(mod)
  residual_df <- ss$SSR_df
  rsq <- 1 - ss$SSR / ss$SST
  sigma_sq <- ss$SSR / residual_df
  adj_rsq <- 1 - sigma_sq / (ss$SST / ss$SST_df)

  # Compute F test only if model has an intercept
  Ftest <- NULL
  if (check_intercept(mod)) {
    idx <- bnames != "(Intercept)"
    q <- bhat[idx]
    num_df <- ss$SST_df - residual_df
    Fstat <- as.vector(crossprod(q, solve(vbhat[idx, idx], q)) / num_df)
    Fpv <- pf(Fstat, num_df, residual_df, lower.tail = FALSE)
    Ftest <- list(Fstat = Fstat,
                  num_df = num_df,
                  den_df = residual_df,
                  pv = Fpv)
  }

  structure(par,
            vcov_type = vcov_type,
            sigma = sqrt(sigma_sq),
            residual_df = residual_df,
            n_obs = nobs(mod),
            rsq = rsq,
            adj_rsq = adj_rsq,
            Ftest = Ftest)
}


regr_table <- function(mod, vcov_type = NULL) {
  fmt_pval <- function(x) {
    ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
  }

  par <- get_parameters(mod, vcov_type)
  par_names <- names(par)
  out_names <- c("", "Estimate", "Std. Error", "t", "p-value")

  # Get the dependent variable name
  depvar <- depvar_name(mod)

  # Get the estimation method name
  # For now, we only consider OLS
  method <- get_method(mod)
  method_str <- ""
  if (!is.null(method)) {
    method_str <- glue("{method}. ")
  }

  ser <- attr(par, "sigma")
  df <- attr(par, "residual_df")
  N <- attr(par, "n_obs")
  R2 <- attr(par, "rsq")
  adj_R2 <- attr(par, "adj_rsq")
  Ftest <- attr(par, "Ftest")
  if (!is.null(Ftest)) {
    Fline <- glue("F-statistic: {format_tt(Ftest$Fstat, digits = 3)} on {Ftest$num_df} and {Ftest$den_df} d.f.,  p-value: {fmt_pval(Ftest$pv)}.")
  } else {
    Fline <- NULL
  }

  wts_line <- NULL
  if (method == "WLS") {
    wts_line <- glue("Weights: {attr(method, 'weights')}")
  }

  vcov_line <- NULL
  if (!is.null(vcov_type)) {
    wts_line <- glue("Robust covariance matrix estimator: {attr(par, 'vcov_type')}.")
  }

  lines <- c(
    glue("{method_str}Number of observations = {N}."),
    glue("Dependent variable: {depvar}."),
    vcov_line,
    wts_line,
    glue("Residual standard error: {format_tt(ser, digits = 3)} on {df} degrees of freedom."),
    glue("R-squared: {format_tt(R2, digits = 3)}, adjusted R-squared: {format_tt(adj_R2, digits = 3)}."),
    Fline
  )
  par$Parameter <- c("Constant", par$Parameter[-1])


  par |>
    setNames(out_names) |>
    tt(width = 0.9, notes = lines) |>
    style_tt(j = 2:5, align = "r") |>
    format_tt(j = out_names[2:3], digits = 3) |>
    format_tt(j = out_names[4], digits = 3) |>
    format_tt(j = out_names[5], fn = fmt_pval)
}




