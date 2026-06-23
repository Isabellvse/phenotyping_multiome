##########################################
# SPINA functions
# Calculate structure parameters of
# thyroid and insulin-glucose homeostasis
# in S, including the implementation in R
# Version 5.0.0 (Rubycon)
# Last Change 20221224 by J. W. D.
##########################################

if (Sys.info()[["sysname"]] == "Darwin" | Sys.info()[["machine"]] == "Macintosh")
{
if (sys.parent() < 4) is.standalone <- TRUE else is.standalone <- FALSE;
# running as stand-alone script?
} else if (sys.parent() == 0) is.standalone <- TRUE else is.standalone <- FALSE;
# Test scenarios are only run in standalone situation

Insulin.conversion.factor <- 6;  # A Voelund 1993, L. Heinemann 2010
Glucose.conversion.factor <- 18; # derived from molar mass

SPINA.GBeta <- function(Insulin, Glucose)
# Insulin expected in pmol/l, Glucose in mmol/l
{
  pico.factor <- 1e12;
  mili.factor <- 1e3
  betaI <- 3.4e-3;
  alphaI <- 0.2;
  dBeta <- 7e-3;
  GBeta <- pico.factor * betaI * Insulin / pico.factor * (dBeta + Glucose /
    mili.factor) / (alphaI * Glucose / mili.factor);
  return(GBeta);
}

SPINA.GR <- function(Insulin, Glucose)
# Insulin in pmol/l, Glucose in mmol/l
{
  pico.factor <- 1e12;
  mili.factor <- 1e3;
  alphaG <- 0.11;
  betaG <- 7.1e-4;
  P0 <- 150e-6;
  DR <- 1.6e-9;
  GE <- 50;
  GR <- alphaG * P0 * (DR + Insulin / pico.factor) / (betaG * GE * Insulin /
    pico.factor * Glucose / mili.factor) - DR / (GE * Insulin / pico.factor)
    - 1 / GE;
  return(GR);
}

HOMA.IR <- function(Insulin, Glucose) # Insulin in pmol/l, Glucose in mmol/l
{
  IR <- Glucose * Insulin / Insulin.conversion.factor / 22.5;
  return(IR);
}

HOMA.Beta <- function(Insulin, Glucose) # Insulin in pmol/l, Glucose in mmol/l
{
  Beta <- rep(NA, times = length(Insulin));
  Beta[which(Glucose > 3.5)] <- 20 * Insulin[which(Glucose > 3.5)] / Insulin.conversion.factor / (Glucose[which(Glucose > 3.5)] - 3.5);
  return(Beta);
}

HOMA.IS <- function(Insulin, Glucose) # Insulin in pmol/l, Glucose in mmol/l
{
  IS <- 1 / HOMA.IR(Insulin, Glucose);
  return(IS);
}

QUICKI <- function(Insulin, Glucose) # Insulin in pmol/l, Glucose in mmol/l
{
  QUICKI <- 1 / (log10(Insulin / Insulin.conversion.factor) + log10(Glucose * Glucose.conversion.factor));
  return(QUICKI);
}

estimated.GT <- function(TSH, FT4)
# TSH in mU/l, FT4 in pmol/l
{
  pico.factor <- 1e12;
  alphaT <- 0.1;
  betaT <- 1.1e-6;
  DT <- 2.75;
  TBG <- 3e-7;
  TBPA <- 4.5e-6;
  k41 <- 2e10;
  k42 <- 2e8;
  TSH[which(TSH == 0)] = NA;
  FT4 <- FT4 / pico.factor;
  TT4 <- (1 + k41 * TBG + k42 * TBPA) * FT4;
  GT <- betaT * (DT + TSH) * TT4 / (alphaT * TSH) * pico.factor;
  return(GT);
}

estimated.GD <- function(FT4, FT3)
# FT4 and FT3 in pmol/l
{
  pico.factor <- 1e12;
  nano.factor <- 1e9;
  TBG <- 3e-7;
  k30 <- 2e9;
  alpha31 <- 0.026;
  beta31 <- 8e-6;
  kM1 <- 5e-7;
  FT4[which(FT4 == 0)] = NA;
  FT3 <- FT3 / pico.factor;
  FT4 <- FT4 / pico.factor;
  TT3 <- (1 + k30 * TBG) * FT3;
  GD <- beta31 * (kM1 + FT4) * TT3 / (alpha31 * FT4) * nano.factor;
  return(GD);
}

estimated.GTT <- function(TSH, T4)
# TSH in mU/l, T4 in nmol/l
{
  pico.factor <- 1e12;
  nano.factor <- 1e9;
  alphaT <- 0.1;
  betaT <- 1.1e-6;
  DT <- 2.75;
  TSH[which(TSH == 0)] = NA;
  T4 <- T4 / nano.factor;
  GT <- betaT * (DT + TSH) * T4 / (alphaT * TSH) * pico.factor;
  return(GT);
}

estimated.GDTT <- function(T4, T3)
# T4 and T3 in nmol/l
{
  pico.factor <- 1e12;
  nano.factor <- 1e9;
  TBG <- 3e-7;
  TBPA <- 4.5e-6;
  k30 <- 2e9;
  k41 <- 2e10;
  k42 <- 2e8;
  alpha31 <- 0.026;
  beta31 <- 8e-6;
  kM1 <- 5e-7;
  T4[which(T4 == 0)] = NA;
  T3 <- T3 / pico.factor;
  T4 <- T4 / pico.factor;
  FT4 <- T4 / (1 + k41 * TBG + k42 * TBPA);
  GD <- beta31 * (kM1 + FT4) * T3 / (alpha31 * FT4) * nano.factor;
  return(GD);
}

estimated.TTSI <- function(TSH, FT4, lu)
# TSH in mU/l, FT4 in arbitrary unit
# lu: upper limit of FT4 reference range, same unit as FT4
{
  TSH[which(TSH == 0)] = NA;
  ttsi <- 100 * TSH * FT4 / lu;
  return(ttsi);
}

estimated.TSHI <- function(TSH, FT4)
# TSH in mU/l, FT4 in pmol/l
{
  beta <- -0.1345;
  TSH[which(TSH == 0)] = NA;
  tshi <- log(TSH) - beta * FT4;
  return(tshi);
}

estimated.sTSHI <- function(TSH, FT4, mean = 2.7, sd = 0.676)
# TSH in mU/l, FT4 in pmol/l
{
  stshi <- (estimated.TSHI(TSH, FT4) - mean) / sd;
  return(stshi);
}

estimated.sGD <- function(FT4, FT3, mean = 30, sd = 5)
# T4 and T3 in nmol/l
{
  sgd <- (estimated.GD(FT4, FT3) - mean) / sd;
  return(sgd);
}

# Alias functions renamed according to new nomenclature (SPINA-GT and SPINA-GD):

SPINA.GT <- function(TSH, FT4) estimated.GT(TSH, FT4);
SPINA.GD <- function(FT4, FT3) estimated.GD(FT4, FT3);
SPINA.GTT <- function(TSH, T4) estimated.GTT(TSH, T4);
SPINA.GDTT <- function(T4, T3) estimated.GDTT(T4, T3);
SPINA.sGD <- function(FT4, FT3, mean = 30, sd = 5) estimated.sGD(FT4, FT3, mean, sd);

if (is.standalone)
# print if executed as stand-alone script only
{
  # Test scenarios:

  cat("\nTest Scenarios:\n");

  cat("\nScenario 9: GT = 4.7 pmol/s, GD = 25.22 nmol/s:\n");
  TSH <- 1;
  FT4 <- 16.5;
  FT3 <- 4.5;

  print(paste("GT^:", SPINA.GT(TSH, FT4)));
  print(paste("GD^:", SPINA.GD(FT4, FT3)));
  print(paste("sGD^:", SPINA.sGD(FT4, FT3)));

  cat("\nScenario 10: GT = 1.08 pmol/s, GD = 336.2 nmol/s:\n");
  TSH <- 3.24;
  FT4 <- 7.7;
  FT3 <- 28;

  print(paste("GT^:", SPINA.GT(TSH, FT4)));
  print(paste("GD^:", SPINA.GD(FT4, FT3)));
  print(paste("sGD^:", SPINA.sGD(FT4, FT3)));

  cat("\nScenario 11: GT = 3.37 pmol/s, GD = 63.7 nmol/s:\n");
  TSH <- 0.7;
  FT4 <- 9;
  FT3 <- 6.2;

  print(paste("GT^:", SPINA.GT(TSH, FT4)));
  print(paste("GD^:", SPINA.GD(FT4, FT3)));
  print(paste("sGD^:", SPINA.sGD(FT4, FT3)));

  cat("\nScenarios 9 to 11 in vector form:\n");
  TSH <- c(1, 3.24, 0.7);
  FT4 <- c(16.5, 7.7, 9);
  FT3 <- c(4.5, 28, 6.2);

  print(paste("GT^:", SPINA.GT(TSH, FT4)));
  print(paste("GD^:", SPINA.GD(FT4, FT3)));
  print(paste("sGD^:", SPINA.sGD(FT4, FT3)));

  cat("\nEvaluate pituitary function with TTSI, TSHI and sTSHI:\n");
  TSH <- c(21.97, 1.82, 0.29);
  FT4 <- c(6, 8, 8); # ng/l, upper limit of reference range: 20 ng/l
  print(paste("TTSI:", estimated.TTSI(TSH, FT4, 20)));
  print(paste("Jostel's TSHI:", estimated.TSHI(TSH, FT4 * 1.287)));
  print(paste("sTSHI:", estimated.sTSHI(TSH, FT4 * 1.287)));

  cat("\n")

  cat("\nScenario 100: GBeta = 2.8 pmol/s. GR = 2.3 mol/s:\n");
  Insulin <- 63.01;
  Glucose <- 4.34;
  print(paste("GBeta^:", SPINA.GBeta(Insulin, Glucose)));
  print(paste("GR^:", SPINA.GR(Insulin, Glucose)));

  cat("\nScenario 101: GBeta = 2.8 pmol/s. GR = 0.7 mol/s:\n");
  Insulin <- 88.77;
  Glucose <- 8.18;
  print(paste("GBeta^:", SPINA.GBeta(Insulin, Glucose)));
  print(paste("GR^:", SPINA.GR(Insulin, Glucose)));

  cat("\nScenario 102: GBeta = 0.6 pmol/s. GR = 2.3 mol/s:\n");
  Insulin <- 20.33;
  Glucose <- 9.51;
  print(paste("GBeta^:", SPINA.GBeta(Insulin, Glucose)));
  print(paste("GR^:", SPINA.GR(Insulin, Glucose)));

  cat("\nScenario 103: GBeta = 0.6 pmol/s. GR = 0.7 mol/s:\n");
  Insulin <- 24.2;
  Glucose <- 15.27;
  print(paste("GBeta^:", SPINA.GBeta(Insulin, Glucose)));
  print(paste("GR^:", SPINA.GR(Insulin, Glucose)));

  cat("\nScenario 104: GBeta = 13.0 pmol/s. GR = 2.3 mol/s:\n");
  Insulin <- 167.09;
  Glucose <- 1.96;
  print(paste("GBeta^:", SPINA.GBeta(Insulin, Glucose)));
  print(paste("GR^:", SPINA.GR(Insulin, Glucose)));
}

# Example data for thyroid homeostasis

# Source: Pilo et al. 1990

# BSA: body surface area in m^2
# IDV: initial distribution volume
# TT4: total T4 in mcg/dl
# TT3: total T3 in ng/ml
# FT4: free T4 in pg/ml
# FT3: free T3 in pg/ml
# TT4.SI: total T4 in nmol/l
# TT3.SI: total T3 in nmol/l
# FT4.SI: free T4 in pmol/l
# FT3.SI: free T3 in pmol/l
# SR: secretion rate
# CR.F, CR.S and CR.T: conversion rate (fast pool, slow pool and total)
# PAR: plasma apperance rate
# PR: production rate
# CR.6, CR.2, CR.0: conversion ratio from 6-compartment, 2-compartment and noncompartmental model

t3.mc <- data.frame(
Sex = c("m", "f", "m", "m", "m", "m", "m", "m", "f", "m", "f", "m", "f", "f"),
Age = c(54, 43, 31, 65, 44, 26, 27, 19, 53, 36, 48, 20, 44, 59),
Body.Mass = c(83, 68.5, 83, 69, 75, 73, 82, 63, 66.5, 72, 53, 65.5, 63, 60),
BSA = c(2.02, 1.7, 2.02, 1.73, 1.8, 1.9, 1.98, 1.8, 1.69, 1.81, 1.49, 1.79, 1.75, 1.57),
IDV.T4 = c(3801, 2272, 2686, 2726, 2632, 2804, 2770, 3119, 2749, 2860, 2467, 3624, 2965, 2327),
TT4 = c(8, 10.4, 7.9, 8, 8.8, 6.5, 7.7, 7.2, 6.6, 8.3, 9.5, 6.5, 7.9, 9.6),
TT4.SI = c(8, 10.4, 7.9, 8, 8.8, 6.5, 7.7, 7.2, 6.6, 8.3, 9.5, 6.5, 7.9, 9.6) * 12.87,
TT3 = c(1.23, 1.1, 1.32, 1.03, 1.33, 1.07, 1.4, 1.2, 1.36, 1.08, 1.21, 1.26, 1.21, 1.02),
TT3.SI = c(1.23, 1.1, 1.32, 1.03, 1.33, 1.07, 1.4, 1.2, 1.36, 1.08, 1.21, 1.26, 1.21, 1.02) * 1.54,
FT4 = c(10.1, 10.5, 8.8, 13.1, 11.1, 8.8, 10.2, 8.9, 6.9, 9, 10.5, 10.4, 11.8, 8.4),
FT4.SI = c(10.1, 10.5, 8.8, 13.1, 11.1, 8.8, 10.2, 8.9, 6.9, 9, 10.5, 10.4, 11.8, 8.4) * 1.287,
FT3 = c(4.3, 5.7, 4.3, 4.4, 3.5, 3.4, 4.1, 4.1, 3.6, 2.6, 3.8, 4.1, 5, 4.3),
FT3.SI = c(4.3, 5.7, 4.3, 4.4, 3.5, 3.4, 4.1, 4.1, 3.6, 2.6, 3.8, 4.1, 5, 4.3) * 1.54,
TSH = c(1.2, 1.4, 1, 1.8, 1.5, 1.5, 2, 1.9, 1.4, 1.8, 1.1, 1.3, 2, 1.5),
SR.T4 = c(44.5, 55.5, 45.3, 59.3, 59.2, 41.5, 50.9, 57.7, 54.3, 51.8, 76.5, 61.1, 65.7, 62.9),
SR.T3 = c(1.98, 4.45, 7.05, 3.14, 5.95, 5.31, 3.51, 4.15, 0.91, 1.4, 2.89, 2.47, 2.64, 0.88),
CR.F.mean = c(12.3, 6.71, 8.73, 9.32, 8.75, 9.14, 10.6, 9.35, 7.9, 12.6, 13.1, 17.3, 14.5, 9.07),
CR.S.mean = c(3.7, 1.11, 1.06, 0.18, 1.2, 0.52, 0.4, 6.74, 3.39, 0.38, 1.55, 2.02, 2.37, 3.64),
CR.T.mean = c(12.3, 6.71, 8.73, 9.32, 8.75, 9.14, 10.6, 9.35, 7.9, 12.6, 13.1, 17.3, 14.5, 9.07) + c(3.7, 1.11, 1.06, 0.18, 1.2, 0.52, 0.4, 6.74, 3.39, 0.38, 1.55, 2.02, 2.37, 3.64),
PAR.T3 = c(16.7, 11.8, 16.4, 12.4, 15.5, 14.7, 14.2, 18.8, 11.4, 14.1, 16.9, 21.1, 18.5, 12.6),
PR.T3.mean = c(18, 12.3, 16.8, 12.6, 15.9, 15, 14.5, 20.2, 12.2, 14.4, 17.5, 21.8, 19.5, 13.6),
PR.T3 = c(20, 14.4, 18.9, 15.2, 17.6, 17.3, 16.1, 21.2, 12.9, 16.3, 19.8, 24.1, 22.4, 14.6),
CR.S = c(12.3, 9.02, 11.7, 9.51, 10.6, 10.5, 9.5, 12.8, 7.74, 9.78, 12.2, 14.4, 13.9, 8.87),
CR.6 = c(42.9, 16.9, 25.8, 19.2, 20.1, 27.8, 25.8, 33.4, 24.8, 30, 22.8, 37.9, 30.7, 24.1),
CR.2 = c(41, 16.5, 25.7, 19.4, 19.9, 27.8, 25.8, 31.3, 23.9, 30.1, 22.7, 37.6, 30.2, 23.1),
CR.0 = c(39.3, 15.6, 24.2, 18.5, 19.1, 26.9, 25.2, 30.8, 22.5, 29.1, 21.5, 35, 28.5, 22.5),
QP = c(2.31, 1.47, 1.76, 1.62, 1.95, 1.58, 1.96, 2.08, 2.21, 1.71, 2, 2.55, 2.06, 1.51),
QF = c(3.22, 2.85, 2.86, 3.55, 2.55, 3.04, 2.87, 3.66, 3.31, 2.9, 3.18, 3.94, 4.35, 2.82),
QS = c(25.8, 18.3, 22.6, 19, 15.4, 14.2, 14.8, 19.4, 18, 18.7, 15.4, 30.1, 25.7, 17.8),
QT = c(31.3, 22.6, 27.3, 24, 19.9, 18.8, 19.6, 25.2, 23.5, 23.3, 20.6, 36.6, 32.2, 22.2)
);

t3.mc$GT <- SPINA.GT(t3.mc$TSH, t3.mc$FT4.SI);
t3.mc$GD <- SPINA.GD(t3.mc$FT4.SI, t3.mc$FT3.SI);

# Example data for insulin-glucose homeostasis

# Source: Dietrich et al. 2022

# Age: age in years
# Height: height in cm
# Body.Mass: body mass in kg
# BMI: Body mass index in kg/m^2
# Fasting.Glucose: fasting glucose concentration in mg/dL
# Fasting.Insulin: fasting insulin concentration in mIU/L

vellore <- data.frame(
Age = c(19, 20, 19, 18, 20, 21, 18, 18, 21, 20, 19, 20, 19, 19, 19, 19, 22, 20, 19, 19, 19, 19, 19, 20, 20, 18, 20, 22, 22, 19, 19, 18, 19, 19, 21, 20, 20, 19, 21, 21, 20, 21, 21, 19, 20, 18, 19, 19, 19, 20, 21, 19, 19, 18, 19, 18, 19, 20, 19, 21, 20, 19, 20, 21, 20, 20, 19, 19, 20, 20, 19, 21, 19, 18, 18, 19, 20, 20, 22, 19, 19, 20, 21, 19, 21, 20, 20, 21, 21, 20, 21, 19, 19, 20, 20, 20, 19, 21, 21, 20, 21, 20, 19, 21, 19, 19, 20, 21, 20, 21, 20, 21, 22, 20, 21, 20, 19),
Height = c(165, 169, 170, 165, 169, 170.5, 166, 170, 174, 165, 173, 173.5, 169, 184, 176, 171, 173, 164, 179.5, 171, 173, 169, 185, 173, 188, 167, 167, 187, 171, 173, 166.5, 183, 170, 181, 178, 168, 175, 169, 167, 166.5, 163, 168, 171, 165, 173, 177, 177, 176, 166, 173, 174, 168, 168, 176, 163, 170, 167, 170, 179.5, 167, 160, 160, 163, 177, 169.5, 166, 161, 167, 181.5, 162.5, 180.5, 160.5, 174, 172, 168, 172, 169, 171, 174, 161, 164, 161, 165, 164, 167, 155.5, 169, 174, 169, 164, 151, 167, 168.5, 175, 174, 163, 170, 169, 159, 165, 163.5, 189, 172, 157.5, 170, 166, 169, 166, 167, 163, 169.5, 162.5, 169, 169.5, 172, 158, 163),
Body.Mass = c(43, 54.3, 52, 59.45, 60.02, 65.38, 50.96, 52.6, 62.26, 52, 75.34, 50, 62.8, 61.2, 59.7, 61.52, 68.2, 65, 56.2, 62, 61, 60.94, 90.4, 66.9, 57.04, 47.3, 51.5, 59.8, 68.1, 70.9, 47.24, 59.5, 53.1, 56.54, 60.68, 61, 51.2, 44, 45.96, 70.5, 54.4, 55.06, 47.02, 52.5, 58.68, 57.3, 57.3, 54.64, 49.2, 60, 48.5, 47.5, 67.7, 60, 53.76, 55, 57, 52, 58.4, 52.46, 49.1, 48.4, 54.4, 62, 55, 48.48, 49.9, 54.7, 53, 53.5, 55.68, 41.58, 68.36, 63.3, 53.4, 58, 46, 48.7, 53.56, 70, 47.4, 53.28, 48.7, 44.6, 43.4, 43, 56.72, 57.66, 50.76, 53.52, 40.9, 48.54, 83.48, 52.3, 54.5, 45, 41.5, 54.8, 43, 52, 44.6, 66.46, 54.8, 49, 53.26, 70, 48, 45.8, 73.78, 43.5, 58.7, 46.5, 62, 77.12, 58.24, 41.5, 48),
BMI = c(15.8, 19, 18, 21.9, 21, 22.6, 18.5, 18.2, 20.5, 19.1, 25.2, 16.7, 22, 18.1, 19.3, 21, 22.8, 24.2, 17.5, 21.2, 20.4, 21.3, 26.4, 22.4, 16.1, 17, 18.5, 17.1, 23.3, 23.7, 17.1, 17.8, 18.4, 17.2, 19.1, 21.6, 16.7, 15.4, 16.5, 25.6, 20.5, 19.5, 16.1, 19.3, 19.6, 18.3, 18.3, 17.6, 17.9, 20, 16, 16.8, 24, 19.4, 20.2, 19, 20.4, 18, 18.2, 18.8, 19.2, 18.9, 20.5, 19.8, 19.1, 17.6, 19.3, 19.6, 16.1, 20.4, 17.2, 16.2, 22.6, 21.4, 18.9, 19.6, 16.1, 16.7, 17.7, 27, 17.6, 20.5, 17.9, 16.6, 15.6, 17.9, 19.9, 19, 17.8, 19.9, 17.9, 17.4, 29.5, 17.1, 18, 16.9, 14.4, 19.2, 17, 19.1, 16.8, 18.6, 18.5, 19.9, 18.4, 25.4, 16.8, 16.6, 26.4, 16.4, 20.6, 17.7, 21.7, 27, 19.7, 16.6, 18.1),
Fasting.Glucose = c(92, 85, 91, 76, 86, 7.04, 89, 91, 94, 98, 109, 84, 87, 10.04, 88, 81, 85, 80, 89, 98, 85, 104, 97, 84, 87, 87, 94, 100, 90, 89, 90, 84, 100, 93, 100, 95, 91, 100, 108, 93, 93, 82, 86, 92, 8, 89, 94, 107, 77, 83, 77, 88, 97, 82, 91, 85, 85, 84, 90, 90, 80, 90, 104, 106, 97, 87, 81, 83, 105, 103, 87, 86, 85, 81, 87, 85, 101, 86, 80, 87, 91, 94, 97, 75, 83, 84, 87, 94, 97, 89, 101, 88, 85, 93, 101, 98, 103, 102, 92, 102, 95, 78, 89, 81, 89, 86, 94, 85, 99, 97, 87, 91, 88, 123, 107, 96, 87),
Fasting.Insulin = c(1.2, 0.8, 1.2, 3.8, 5.9, 3.1, 5, 3.2, 4.7, 0.9, 8.5, 2.3, 1.5, 3.8, 3.7, 1.1, 3.3, 6.6, 2.8, 12.9, 0.5, 1, 3.5, 6.4, 4.1, 4.5, 2.6, 1.4, 2.8, 10.7, 6, 8.9, 2.2, 2.1, 1.4, 8.29, 4.3, 1.9, 29.6, 0.9, 1.9, 4.3, 0.9, 1, 5.1, 5.7, 7.5, 17.5, 3.5, 2.5, 3.3, 3.5, 18, 2.7, 9.4, 4.2, 1.8, 4.3, 6.9, 3.1, 4.5, 2.2, 10, 4.9, 4.5, 1, 2.5, 3.5, 6.67, 4.3, 3.8, 2.9, 5.3, 1.1, 2, 8.3, 6.5, 1.5, 3.6, 12.5, 22, 0.9, 3.1, 5, 0.5, 1.3, 1.1, 0.9, 2.1, 4.9, 2.3, 1, 7.2, 4.5, 5.4, 4.2, 11.18, 2.3, 11.1, 4.31, 6.8, 3.7, 31.4, 1, 11.9, 2.1, 5.8, 3.7, 22.9, 57.1, 2.6, 1, 4, 122.8, 3.2, 6.4, 5.5)
);

vellore$GR <- SPINA.GR(vellore$Fasting.Insulin * Insulin.conversion.factor, vellore$Fasting.Glucose/Glucose.conversion.factor);
vellore$GBeta <- SPINA.GBeta(vellore$Fasting.Insulin * Insulin.conversion.factor, vellore$Fasting.Glucose/Glucose.conversion.factor);
vellore$HOMA.Beta <- HOMA.Beta(vellore$Fasting.Insulin * Insulin.conversion.factor, vellore$Fasting.Glucose/Glucose.conversion.factor);
vellore$HOMA.IR <- HOMA.IR(vellore$Fasting.Insulin * Insulin.conversion.factor, vellore$Fasting.Glucose/Glucose.conversion.factor);
vellore$HOMA.IS <- HOMA.IS(vellore$Fasting.Insulin * Insulin.conversion.factor, vellore$Fasting.Glucose/Glucose.conversion.factor);
vellore$QUICKI <- QUICKI(vellore$Fasting.Insulin * Insulin.conversion.factor, vellore$Fasting.Glucose/Glucose.conversion.factor);

# References:

# 1. Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE,
#    Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for
#    Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun
#    9;7:57. doi 10.3389/fendo.2016.00057. PMID 27375554; PMCID PMC4899439.

# 2. Dietrich JW, Dasgupta R, Anoop S, Jebasingh F, Kurian ME, Inbakumari M, Boehm BO,
#    Thomas N. SPINA Carb: a simple mathematical model supporting fast in-vivo estimation
#    of insulin sensitivity and beta cell function. Sci Rep. 2022 Oct 21;12(1):17659.
#    doi 10.1038/s41598-022-22531-3. PMID 36271244; PMCID PMC9587026.

# 3. Heinemann L. Insulin assay standardization: leading to measures of insulin
#    sensitivity and secretion for practical clinical care: response to Staten et al.
#    Diabetes Care. 2010 Jun;33(6):e83; author reply e84. doi: 10.2337/dc10-0034. PMID:
#    20508228.

# 4. Pilo A, Iervasi G, Vitek F, Ferdeghini M, Cazzuola F, Bianchi R. Thyroidal and
#    peripheral production of 3,5,3'-triiodothyronine in humans by multicompartmental
#    analysis. Am J Physiol. 1990 Apr;258(4 Pt 1):E715-26. PMID 2333963.

# 5. Voelund A. Conversion of insulin units to SI units. Am J Clin Nutr. 1993
#    Nov;58(5):714-5. doi: 10.1093/ajcn/58.5.714. PMID: 8237884.
