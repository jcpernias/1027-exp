library(tinytable)
library(parameters)
library(glue)


vote <- read.csv2("data/vote.csv")

mod <- lm(vote_dem ~ exp_dem, data = vote)

fmt_pval <- function(x) {
  ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
}

par <- model_parameters(mod,
                        include_info = TRUE,
                        pretty_names = FALSE)
par_names <- names(par)
in_names <- c("Parameter", "Coefficient", "SE", "t", "p")
out_names <- c("", "Estimate", "Std. Error", "t", "p-value")

mod_terms <- terms(mod)
var_names <- attr(mod_terms, "variables")
depvar <- as.character(var_names[[2]])
method <- ifelse(class(mod) == "lm", "OLS", "Unknown method")

ser <- attr(par, "sigma")
df <- attr(par, "residual_df")
N <- attr(par, "n_obs")
R2_list <- attr(par, "r2")
R2 <- R2_list$R2
adj_R2 <- R2_list$R2_adjusted
Fstat <- attr(R2_list, "F")
num_df <- attr(R2_list, "df")
Fpv <- attr(R2_list, "p")

line1 <-
  glue("{method}. Dep. variable: {depvar}. Num. of obs. = {N}. ")
line2 <-
  glue("Residual standard error: {format_tt(ser, digits = 3)} on {df} degrees of freedom.")
line3 <-
  glue("R-squared: {format_tt(R2, digits = 2)}, adjusted R-squared: {format_tt(adj_R2, digits = 2)}.")
line4 <-
  glue("F-statistic: {format_tt(Fstat, digits = 2)} on {num_df} and {df} d.f.,  p-value: {fmt_pval(Fpv)}.")

par$Parameter <- c("Constant", par$Parameter[-1])


par[ , in_names] |>
  setNames(out_names) |>
  tt(width = 0.9,
     notes = c(line1, line2, line3, line4)) |>
  style_tt(j = 2:5, align = "r") |>
  format_tt(j = out_names[2:3], digits = 3) |>
  format_tt(j = out_names[4], digits = 2) |>
  format_tt(j = out_names[5], fn = fmt_pval)




