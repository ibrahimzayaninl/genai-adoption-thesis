# SECTION 4 — HIERARCHICAL REGRESSION: H2

install.packages("lmtest")
install.packages("lm.beta")
install.packages("car")

library(tidyverse)
library(lm.beta)   # standardised betas
library(car)       # VIF

t0 <- read_csv("t0_clean.csv") %>% mutate(wave = 0)
t1 <- read_csv("t1_clean.csv") %>% mutate(wave = 1)


combined <- bind_rows(t0, t1) %>%
  select(wave, TR_comp, IU_comp, PU_comp, PEOU_comp, SI_comp, FC_comp) %>%
  na.omit()   # listwise deletion; drops 1 case (T0 respondent missing SI items)

cat(sprintf("\nCombined N after listwise deletion: %d\n", nrow(combined)))
cat(sprintf("  T0: %d | T1: %d\n\n",
            sum(combined$wave == 0), sum(combined$wave == 1)))


# MODELS
# Model 1: Controls only (wave + PU + PEOU + SI + FC)
m1 <- lm(IU_comp ~ wave + PU_comp + PEOU_comp + SI_comp + FC_comp,
         data = combined)

# Model 2: Controls + Trust
m2 <- lm(IU_comp ~ wave + PU_comp + PEOU_comp + SI_comp + FC_comp + TR_comp,
         data = combined)



# MODEL SUMMARIES

print(summary(m1))

print(summary(m2))


# STANDARDISED BETAS


cat("\nModel 1:\n")
print(lm.beta(m1))

cat("\nModel 2:\n")
print(lm.beta(m2))


# R-SQUARED CHANGE (F-CHANGE TEST)

cat("\n========== R-SQUARED CHANGE: Model 1 -> Model 2 ==========\n")
anova_result <- anova(m1, m2)
print(anova_result)

r2_m1 <- summary(m1)$r.squared
r2_m2 <- summary(m2)$r.squared
delta_r2 <- r2_m2 - r2_m1

cat(sprintf("\n  R² Model 1: %.3f (%.1f%% variance explained)\n",
            r2_m1, r2_m1 * 100))
cat(sprintf("  R² Model 2: %.3f (%.1f%% variance explained)\n",
            r2_m2, r2_m2 * 100))
cat(sprintf("  ΔR²:        %.3f\n", delta_r2))
cat(sprintf("  F-change p: %.3f\n", anova_result$`Pr(>F)`[2]))


# ASSUMPTION CHECKS

cat("\n\n========== ASSUMPTION CHECKS: MODEL 2 ==========\n")

# 1. Multicollinearity (VIF)
cat("\n--- Variance Inflation Factors (VIF) ---\n")
cat("Rule of thumb: VIF > 5 warrants concern; VIF > 10 is problematic.\n")
vif_vals <- vif(m2)
print(round(vif_vals, 3))

# 2. Normality of residuals (Shapiro-Wilk)
cat("\n--- Shapiro-Wilk Test on Residuals ---\n")
cat("H0: residuals are normally distributed. p > .05 = assumption met.\n")
sw <- shapiro.test(residuals(m2))
cat(sprintf("  W = %.4f, p = %.3f\n", sw$statistic, sw$p.value))

# 3. Homoscedasticity (Breusch-Pagan)
cat("\n--- Breusch-Pagan Test for Homoscedasticity ---\n")
cat("H0: residuals have constant variance. p > .05 = assumption met.\n")
bp <- lmtest::bptest(m2)
cat(sprintf("  BP = %.3f, df = %d, p = %.3f\n",
            bp$statistic, bp$parameter, bp$p.value))


# CLEAN SUMMARY TABLE FOR THESIS

cat("\n\n========== overall TABLE: HIERARCHICAL REGRESSION RESULTS ==========\n")
cat(sprintf("\n%-16s %8s %6s %7s %7s    %8s %6s %7s %7s\n",
            "Predictor","B (M1)","SE","β (M1)","p (M1)",
            "B (M2)","SE","β (M2)","p (M2)"))
cat(strrep("-", 80), "\n")

m1_coef  <- summary(m1)$coefficients
m2_coef  <- summary(m2)$coefficients
m1_beta  <- lm.beta(m1)$standardized.coefficients
m2_beta  <- lm.beta(m2)$standardized.coefficients

predictors <- list(
  list(name="Wave",       row1="wave",     row2="wave"),
  list(name="PU",         row1="PU_comp",  row2="PU_comp"),
  list(name="PEOU",       row1="PEOU_comp",row2="PEOU_comp"),
  list(name="SI",         row1="SI_comp",  row2="SI_comp"),
  list(name="FC",         row1="FC_comp",  row2="FC_comp"),
  list(name="Trust",      row1=NULL,       row2="TR_comp")
)

for (p in predictors) {
  b1  <- ifelse(is.null(p$row1), NA, m1_coef[p$row1, "Estimate"])
  se1 <- ifelse(is.null(p$row1), NA, m1_coef[p$row1, "Std. Error"])
  bt1 <- ifelse(is.null(p$row1), NA, m1_beta[p$row1])
  pv1 <- ifelse(is.null(p$row1), NA, m1_coef[p$row1, "Pr(>|t|)"])

  b2  <- m2_coef[p$row2, "Estimate"]
  se2 <- m2_coef[p$row2, "Std. Error"]
  bt2 <- m2_beta[p$row2]
  pv2 <- m2_coef[p$row2, "Pr(>|t|)"]

  fmt_cell <- function(b, se, bt, pv) {
    if (is.na(b)) return(sprintf("%8s %6s %7s %7s", "—", "—", "—", "—"))
    sprintf("%8.3f %6.3f %7.3f %7.3f", b, se, bt, pv)
  }

  cat(sprintf("  %-14s %s    %s\n",
              p$name,
              fmt_cell(b1, se1, bt1, pv1),
              fmt_cell(b2, se2, bt2, pv2)))
}

cat(strrep("-", 80), "\n")
cat(sprintf("  %-14s %8.3f %6s %7s %7s    %8.3f %6s %7s %7s\n",
            "R²",
            summary(m1)$r.squared, "","","",
            summary(m2)$r.squared, "","",""))
cat(sprintf("  %-14s %8s %6s %7s %7s    %8.3f %6s %7s %7s\n",
            "ΔR²", "","","","",
            delta_r2, "","",""))
cat(sprintf("  %-14s %8.3f %6s %7s %7s    %8.3f %6s %7s %7s\n",
            "Adj. R²",
            summary(m1)$adj.r.squared, "","","",
            summary(m2)$adj.r.squared, "","",""))
cat(sprintf("  N = %d (listwise deletion)\n", nrow(combined)))
