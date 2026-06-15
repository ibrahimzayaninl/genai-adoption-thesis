
# SECTION 3 — T0 VS T1 COMPARISONS: H1 AND H3

install.packages("effsize")
library(tidyverse)
library(effsize)

t0 <- read_csv("t0_clean.csv")
t1 <- read_csv("t1_clean.csv")



# Welch t-test 


run_ttest <- function(construct, label, hypothesis = NULL) {
  x0 <- na.omit(t0[[construct]])
  x1 <- na.omit(t1[[construct]])

  tt  <- t.test(x1, x0, alternative = "two.sided", var.equal = FALSE)
  tt1 <- t.test(x1, x0, alternative = "greater",   var.equal = FALSE)
  cd  <- cohen.d(x1, x0)

  d_val <- cd$estimate
  d_int <- case_when(
    abs(d_val) >= 0.8 ~ "large",
    abs(d_val) >= 0.5 ~ "medium",
    abs(d_val) >= 0.2 ~ "small",
    TRUE              ~ "negligible"
  )

  cat(strrep("-", 60), "\n")
  if (!is.null(hypothesis)) cat(sprintf("  Hypothesis: %s\n", hypothesis))
  cat(sprintf("  Construct:  %s\n", label))
  cat(sprintf("  T0: M = %.2f, SD = %.2f, N = %d\n", mean(x0), sd(x0), length(x0)))
  cat(sprintf("  T1: M = %.2f, SD = %.2f, N = %d\n", mean(x1), sd(x1), length(x1)))
  cat(sprintf("  Difference: %.2f (T1 - T0)\n", mean(x1) - mean(x0)))
  cat(sprintf("\n  Welch t-test (two-tailed):\n"))
  cat(sprintf("    t(%5.2f) = %5.3f, p = %.3f\n",
              tt$parameter, tt$statistic, tt$p.value))
  cat(sprintf("  Welch t-test (one-tailed, H: T1 > T0):\n"))
  cat(sprintf("    t(%5.2f) = %5.3f, p = %.3f\n",
              tt1$parameter, tt1$statistic, tt1$p.value))
  cat(sprintf("  Cohen's d = %.3f (%s effect)\n", d_val, d_int))
  cat(sprintf("  95%% CI for d: [%.3f, %.3f]\n",
              cd$conf.int[1], cd$conf.int[2]))
  cat("\n")

  invisible(list(
    construct = construct,
    m0 = mean(x0), sd0 = sd(x0), n0 = length(x0),
    m1 = mean(x1), sd1 = sd(x1), n1 = length(x1),
    t_two  = tt$statistic,  df_two  = tt$parameter,  p_two  = tt$p.value,
    t_one  = tt1$statistic, df_one  = tt1$parameter,  p_one  = tt1$p.value,
    d = d_val, d_interp = d_int,
    d_ci_lo = cd$conf.int[1], d_ci_hi = cd$conf.int[2]
  ))
}



# HYPOTHESIS TESTS
# H1: Trust
h1 <- run_ttest(
  construct  = "TR_comp",
  label      = "Trust in Microsoft Copilot",
  hypothesis = "H1: Training intervention is positively associated with higher trust"
)

# H3: Intention to Use
h3 <- run_ttest(
  construct  = "IU_comp",
  label      = "Intention to Use Microsoft Copilot",
  hypothesis = "H3: Training intervention is positively associated with stronger intention to use"
)



# CONTROL VARIABLES: Descriptive t-tests for context


cat(strrep("=", 60), "\n")
cat("CONTROL VARIABLES (descriptive, no directional hypotheses)\n")
cat(strrep("=", 60), "\n\n")

pu   <- run_ttest("PU_comp",   "Perceived Usefulness")
peou <- run_ttest("PEOU_comp", "Perceived Ease of Use")
si   <- run_ttest("SI_comp",   "Social Influence")
fc   <- run_ttest("FC_comp",   "Facilitating Conditions")



# SUMMARY TABLE


cat(strrep("=", 60), "\n")
cat("SUMMARY TABLE\n")
cat(strrep("=", 60), "\n")

cat(sprintf("\n%-14s %7s %6s %7s %6s %8s %6s %8s %8s %10s\n",
            "Construct","T0 M","T0 SD","T1 M","T1 SD",
            "t","df","p (2t)","p (1t)","Cohen's d"))
cat(strrep("-", 90), "\n")

results <- list(
  list(label="Trust *",   r=h1),
  list(label="Int. Use *", r=h3),
  list(label="PU",        r=pu),
  list(label="PEOU",      r=peou),
  list(label="SI",        r=si),
  list(label="FC",        r=fc)
)

for (item in results) {
  r <- item$r
  cat(sprintf("  %-14s %7.2f %6.2f %7.2f %6.2f %8.3f %6.1f %8.3f %8.3f %8.3f\n",
              item$label,
              r$m0, r$sd0, r$m1, r$sd1,
              r$t_two, r$df_two, r$p_two, r$p_one, r$d))
}


