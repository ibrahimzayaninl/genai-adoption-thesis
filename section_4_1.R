# SECTION 1: SAMPLE CHARACTERISTICS AND COMPARABILITY
# AFTER clean_data.R


library(tidyverse)

t0 <- read_csv("t0_clean.csv")
t1 <- read_csv("t1_clean.csv")


# TABLE 1: SAMPLE CHARACTERISTICS

cat("\n========== TABLE 1: SAMPLE CHARACTERISTICS ==========\n")
cat(sprintf("%-30s %14s %14s\n", "Variable", "T0 (n=71)", "T1 (n=32)"))
cat(strrep("-", 60), "\n")

prow <- function(label, n0, n1, tot0 = 71, tot1 = 32) {
  cat(sprintf("  %-28s %4d (%4.1f%%)  %4d (%4.1f%%)\n",
              label, n0, 100*n0/tot0, n1, 100*n1/tot1))
}

sections <- list(
  list(name="Department", var="dept_std",
       levels=c("Sales","IT","Finance","Other")),
  list(name="Role",       var="role",
       levels=c("Employee","Manager","Specialist","Other")),
  list(name="Tenure",     var="tenure",
       levels=c("0-2 Years","3-7 Years","7+ Years")),
  list(name="Age group",  var="age_std",
       levels=c("Under 35","35-44","45-54","55+")),
  list(name="Gender",     var="gender",
       levels=c("Female","Male"))
)

for (sec in sections) {
  cat("\n", sec$name, "\n", sep="")
  for (lvl in sec$levels) {
    n0 <- sum(t0[[sec$var]] == lvl, na.rm=TRUE)
    n1 <- sum(t1[[sec$var]] == lvl, na.rm=TRUE)
    if (n0 + n1 > 0) prow(lvl, n0, n1)
  }
}

# TABLE 2: CHI-SQUARE COMPARABILITY TESTS

set.seed(2026)   # reproducibility for simulated chi-square p-values
cat("\n\n========== TABLE 2: CHI-SQUARE COMPARABILITY TESTS ==========\n")
cat(sprintf("%-20s %10s %4s %8s  %-s\n", "Variable","Chi-sq","df","p","Method"))
cat(strrep("-", 58), "\n")

run_chi <- function(var_name, col) {
  t0_vals <- na.omit(t0[[col]])
  t1_vals <- na.omit(t1[[col]])
  levels  <- sort(union(as.character(t0_vals), as.character(t1_vals)))

  tbl <- matrix(
    c(sapply(levels, function(l) sum(as.character(t0_vals) == l)),
      sapply(levels, function(l) sum(as.character(t1_vals) == l))),
    nrow = 2, byrow = TRUE
  )

  result  <- chisq.test(tbl)
  pct_low <- mean(result$expected < 5)
  method  <- "Chi-square"

  if (pct_low > 0.2) {
    result <- chisq.test(tbl, simulate.p.value = TRUE, B = 10000)
    method <- sprintf("Simulated (%d%% cells<5)", round(pct_low*100))
  }

  cat(sprintf("  %-18s %10.3f %4s %8.3f  %s\n",
              var_name,
              result$statistic,
              ifelse(is.null(result$parameter), "—", as.character(result$parameter)),
              result$p.value,
              method))
}

run_chi("Department", "dept_std")
run_chi("Role",       "role")
run_chi("Tenure",     "tenure")
run_chi("Age group",  "age_std")
run_chi("Gender",     "gender")



# TABLE 3: CONSTRUCT DESCRIPTIVES BY WAVE


cat("\n\n========== TABLE 3: CONSTRUCT DESCRIPTIVES BY WAVE ==========\n")
cat(sprintf("%-14s %7s %7s %6s  %7s %7s %6s  %7s\n",
            "Construct","T0 M","T0 SD","T0 N","T1 M","T1 SD","T1 N","Diff"))
cat(strrep("-", 68), "\n")

constructs <- list(
  c("TR_comp",   "Trust"),
  c("IU_comp",   "Int. Use"),
  c("PU_comp",   "Perc. Useful"),
  c("PEOU_comp", "Perc. EoU"),
  c("SI_comp",   "Soc. Infl."),
  c("FC_comp",   "Facil. Cond.")
)

for (item in constructs) {
  col <- item[1]; label <- item[2]
  m0  <- mean(t0[[col]], na.rm=TRUE); sd0 <- sd(t0[[col]], na.rm=TRUE)
  n0  <- sum(!is.na(t0[[col]]))
  m1  <- mean(t1[[col]], na.rm=TRUE); sd1 <- sd(t1[[col]], na.rm=TRUE)
  n1  <- sum(!is.na(t1[[col]]))
  cat(sprintf("  %-12s %7.2f %7.2f %6d  %7.2f %7.2f %6d  %+7.2f\n",
              label, m0, sd0, n0, m1, sd1, n1, m1-m0))
}


