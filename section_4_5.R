# SECTION 5 — BOOTSTRAPPED MEDIATION ANALYSIS: H4

install.packages("mediation")
library(tidyverse)
library(mediation)

set.seed(2026)   # reproducibility

t0 <- read_csv("t0_clean.csv") %>% mutate(wave = 0)
t1 <- read_csv("t1_clean.csv") %>% mutate(wave = 1)

combined <- bind_rows(t0, t1) %>%
  dplyr::select(wave, TR_comp, IU_comp, PU_comp, PEOU_comp, SI_comp, FC_comp) %>%
  na.omit()

cat(sprintf("N after listwise deletion: %d (T0: %d, T1: %d)\n\n",
            nrow(combined),
            sum(combined$wave == 0),
            sum(combined$wave == 1)))





# ---- PATH A: Wave predicts Trust (mediator model) ----

med_model <- lm(TR_comp ~ wave + PU_comp + PEOU_comp + SI_comp + FC_comp,
                data = combined)

cat("========== PATH A: Wave -> Trust (mediator model) ==========\n")
print(summary(med_model))


# ---- PATH B + C': Outcome model (trust + wave + controls) ----
# This is identical to Model 2 from Section 4.4

out_model <- lm(IU_comp ~ wave + TR_comp + PU_comp + PEOU_comp + SI_comp + FC_comp,
                data = combined)

cat("\n========== PATH B + C': Outcome model ==========\n")
print(summary(out_model))


# ---- PATH C: Total effect (wave -> IU, no mediator) ----

total_model <- lm(IU_comp ~ wave + PU_comp + PEOU_comp + SI_comp + FC_comp,
                  data = combined)

cat("\n========== PATH C: Total effect (wave -> IU, no mediator) ==========\n")
total_coef <- summary(total_model)$coefficients["wave", ]
cat(sprintf("  Wave -> IU: B = %.3f, SE = %.3f, t = %.3f, p = %.3f\n\n",
            total_coef["Estimate"],
            total_coef["Std. Error"],
            total_coef["t value"],
            total_coef["Pr(>|t|)"]))


# ---- BOOTSTRAPPED MEDIATION  ----

cat("========== BOOTSTRAPPED MEDIATION (sims = 5000) ==========\n")

med_result <- mediate(
  model.m  = med_model,
  model.y  = out_model,
  treat    = "wave",
  mediator = "TR_comp",
  boot     = TRUE,
  sims     = 5000,
  boot.ci.type = "perc"   # percentile bootstrap CIs
)

print(summary(med_result))


# CLEAN PATH SUMMARY TABLE

cat("\n\n========== PATH SUMMARY TABLE ==========\n")

a_coef <- summary(med_model)$coefficients["wave", ]
b_coef <- summary(out_model)$coefficients["TR_comp", ]
c_prime <- summary(out_model)$coefficients["wave", ]

cat(sprintf("\n  %-30s %8s %6s %8s\n", "Path", "B", "SE", "p"))
cat(strrep("-", 56), "\n")
cat(sprintf("  %-30s %8.3f %6.3f %8.3f\n",
            "a: Wave -> Trust",
            a_coef["Estimate"], a_coef["Std. Error"], a_coef["Pr(>|t|)"]))
cat(sprintf("  %-30s %8.3f %6.3f %8.3f\n",
            "b: Trust -> IU (direct)",
            b_coef["Estimate"], b_coef["Std. Error"], b_coef["Pr(>|t|)"]))
cat(sprintf("  %-30s %8.3f %6.3f %8.3f\n",
            "c: Wave -> IU (total)",
            total_coef["Estimate"], total_coef["Std. Error"], total_coef["Pr(>|t|)"]))
cat(sprintf("  %-30s %8.3f %6.3f %8.3f\n",
            "c': Wave -> IU (direct)",
            c_prime["Estimate"], c_prime["Std. Error"], c_prime["Pr(>|t|)"]))

cat(strrep("-", 56), "\n")
cat(sprintf("  %-30s %8.3f\n", "Indirect effect (a x b):",
            med_result$d0))
cat(sprintf("  %-30s [%.3f, %.3f]\n", "95% bootstrap CI:",
            med_result$d0.ci[1], med_result$d0.ci[2]))
cat(sprintf("  %-30s %8.3f\n", "p (indirect effect):",
            med_result$d0.p))

cat("\n")
if (med_result$d0.ci[1] > 0 | med_result$d0.ci[2] < 0) {
  cat("  CI does NOT include zero -> indirect effect is significant.\n")
  cat("  H4 SUPPORTED: trust mediates the training -> IU relationship.\n")
} else {
  cat("  CI includes zero -> indirect effect is not significant.\n")
  cat("  H4 NOT SUPPORTED at the 95% confidence level.\n")
}


# EXPANDED TABLE 10 INPUTS


std_table <- function(model, data, dv) {
  sm    <- summary(model)$coefficients
  sd_y  <- sd(data[[dv]])
  terms <- rownames(sm)
  out <- data.frame(
    Predictor = terms,
    B    = round(sm[, "Estimate"],   3),
    SE   = round(sm[, "Std. Error"], 3),
    beta = NA_real_,
    p    = round(sm[, "Pr(>|t|)"],   3),
    row.names = NULL
  )
  for (i in seq_along(terms)) {
    t <- terms[i]
    if (t == "(Intercept)") next
    out$beta[i] <- round(sm[t, "Estimate"] * sd(data[[t]]) / sd_y, 3)
  }
  out
}

cat("\n\n===== TABLE 10 PANEL A — MEDIATOR MODEL (Path a): Trust ~ Wave + controls =====\n")
print(std_table(med_model, combined, "TR_comp"), row.names = FALSE)

f1 <- summary(med_model)$fstatistic
cat(sprintf("  R2 = %.3f | Adj R2 = %.3f | F(%d,%d) = %.3f, p = %.4f\n",
            summary(med_model)$r.squared, summary(med_model)$adj.r.squared,
            f1[2], f1[3], f1[1], pf(f1[1], f1[2], f1[3], lower.tail = FALSE)))

cat("\n===== TABLE 10 PANEL B — OUTCOME MODEL (Paths b, c'): IU ~ Wave + Trust + controls =====\n")
cat("(Cross-check only: this equals Model 2 of Table 9)\n")
print(std_table(out_model, combined, "IU_comp"), row.names = FALSE)