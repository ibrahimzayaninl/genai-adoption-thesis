
# SECTION 2 — RELIABILITY ANALYSIS

install.packages("psych")
library(tidyverse)
library(psych)

t0 <- read_csv("t0_clean.csv")
t1 <- read_csv("t1_clean.csv")

scales <- list(
  "Trust (TR)"        = c("TR1","TR2","TR3","TR4"),
  "Int. to Use (IU)"  = c("IU1","IU2","IU3"),
  "Perc. Useful (PU)" = c("PU1","PU2","PU3"),
  "Perc. EoU (PEOU)"  = c("PEOU1","PEOU2","PEOU3"),
  "Soc. Infl. (SI)"   = c("SI1","SI2"),
  "Facil. Cond. (FC)" = c("FC1","FC2")
)



# TABLE 4: CRONBACH'S ALPHA BY SCALE AND WAVE

cat("\n========== TABLE 4: RELIABILITY ANALYSIS ==========\n")
cat(sprintf("%-22s %5s %9s %6s %9s %6s %10s\n",
            "Scale","Items","T0 alpha","T0 N","T1 alpha","T1 N","Assessment"))
cat(strrep("-", 72), "\n")

assess <- function(a, n_items) {
  if (n_items == 2) return("2-item scale")   # alpha is bounded for 2-item scales
  if (a >= 0.90) return("Excellent")
  if (a >= 0.80) return("Good")
  if (a >= 0.70) return("Acceptable")
  return("Below threshold")
}

for (name in names(scales)) {
  items <- scales[[name]]
  a0 <- alpha(na.omit(t0[, items]))$total$raw_alpha
  a1 <- alpha(na.omit(t1[, items]))$total$raw_alpha
  n0 <- nrow(na.omit(t0[, items]))
  n1 <- nrow(na.omit(t1[, items]))
  cat(sprintf("  %-20s %5d %9.3f %6d %9.3f %6d %10s\n",
              name, length(items), a0, n0, a1, n1, assess(a0, length(items))))
}




# FC DIAGNOSTIC: Inter-item correlation and item-total correlations

cat("\n--- FC Diagnostic ---\n")
for (wave_name in c("T0","T1")) {
  df   <- if (wave_name == "T0") t0 else t1
  data <- na.omit(df[, c("FC1","FC2")])
  r    <- cor(data$FC1, data$FC2)
  cat(sprintf("  %s inter-item correlation (FC1-FC2): r = %.3f\n", wave_name, r))
}



# SI DIAGNOSTIC: Alpha drop across waves

cat("\n--- SI Diagnostic ---\n")
for (wave_name in c("T0","T1")) {
  df   <- if (wave_name == "T0") t0 else t1
  data <- na.omit(df[, c("SI1","SI2")])
  r    <- cor(data$SI1, data$SI2)
  cat(sprintf("  %s inter-item correlation (SI1-SI2): r = %.3f\n", wave_name, r))
}



# ITEM-LEVEL DETAIL: alpha-if-item-deleted for each scale


cat("\n\n========== ITEM-LEVEL DETAIL (alpha if item deleted) ==========\n")

for (name in names(scales)) {
  items <- scales[[name]]
  if (length(items) < 3) next   # not meaningful for 2-item scales

  cat(sprintf("\n%s\n", name))
  cat(sprintf("  %-8s %12s %12s\n", "Item", "T0 a-if-del", "T1 a-if-del"))

  a0_detail <- alpha(na.omit(t0[, items]))$alpha.drop
  a1_detail <- alpha(na.omit(t1[, items]))$alpha.drop

  for (item in items) {
    a0_drop <- a0_detail[item, "raw_alpha"]
    a1_drop <- a1_detail[item, "raw_alpha"]
    cat(sprintf("  %-8s %12.3f %12.3f\n", item, a0_drop, a1_drop))
  }
}

