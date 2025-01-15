library(tinytable)
options(modelsummary_factory_default = "tinytable")
options(modelsummary_factory_latex = "tinytable")
options(modelsummary_factory_html = "tinytable")

library(modelsummary)

vote <- read.csv2("data/vote.csv")

mod1 <- lm(vote_dem ~ exp_dem, data = vote)
ms <- modelsummary(mod1, shape = term ~ model + statistic,
                   stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
                   estimate = "{estimate}",
                   statistic = c("Std.Error" = "{std.error}",
                                 "t" = "{statistic}",
                                 "p-value" = "{p.value}",
                                 " " = "{stars}"),
                   gof_omit = 'DF|Deviance|Log.Lik.|AIC|BIC',
                   align = "lrrrrl")
colnames(ms) <- c(" ", "Estimate", "Std. Error", "t", "p-value", "")
# raw <- c("nobs", "sigma", "r.squared", "adj.r.squared", "F")

# Eliminar nombre de modelo para informar método (OLS o WLS),
# variable dependiente y número de observaciones.
# Usar las notas para replicar la sección de bondad de ajuste de R
#
# Residual standard error: 15.68 on 171 degrees of freedom
# Multiple R-squared:  0.0708,	Adjusted R-squared:  0.06537
# F-statistic: 13.03 on 1 and 171 DF,  p-value: 0.0004024
#
# Nota con significado de las estrellas.
ms
