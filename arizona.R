library(dplyr)
library(ggplot2)
library(tidyverse)

# ============================================================
# LOAD DATA
# ============================================================
games <- read.csv("MRegularSeasonDetailedResults.csv")
teams <- read.csv("MTeams.csv") %>%
  filter(LastD1Season == 2026)

ARIZONA_ID <- teams %>% filter(TeamName == "Arizona") %>% pull(TeamID)

# ============================================================
# STEP 1: BUILD GAME-LEVEL LONG FORMAT
# Includes win/loss outcome per team per game
# ============================================================
game_long <- bind_rows(
  games %>%
    transmute(
      Season,
      GameID     = row_number(),
      TeamID     = WTeamID,
      OppTeamID  = LTeamID,
      Win        = 1,
      FGM = WFGM, FGA = WFGA,
      FGM3 = WFGM3, FGA3 = WFGA3,
      FTM = WFTM, FTA = WFTA,
      ORB = WOR,  DRB = WDR,
      Ast = WAst, TO  = WTO,
      Stl = WStl, Blk = WBlk,
      PF  = WPF,
      OppFGM = LFGM, OppFGA = LFGA,
      OppFGM3 = LFGM3, OppFGA3 = LFGA3,
      OppFTM = LFTM, OppFTA = LFTA,
      OppORB = LOR,  OppDRB = LDR,
      OppTO  = LTO
    ),
  games %>%
    transmute(
      Season,
      GameID     = row_number(),
      TeamID     = LTeamID,
      OppTeamID  = WTeamID,
      Win        = 0,
      FGM = LFGM, FGA = LFGA,
      FGM3 = LFGM3, FGA3 = LFGA3,
      FTM = LFTM, FTA = LFTA,
      ORB = LOR,  DRB = LDR,
      Ast = LAst, TO  = LTO,
      Stl = LStl, Blk = LBlk,
      PF  = LPF,
      OppFGM = WFGM, OppFGA = WFGA,
      OppFGM3 = WFGM3, OppFGA3 = WFGA3,
      OppFTM = WFTM, OppFTA = WFTA,
      OppORB = WOR,  OppDRB = WDR,
      OppTO  = WTO
    )
) %>%
  filter(Season == 2026) %>%
  mutate(
    # Possessions (Kenpom formula)
    Poss        = FGA - ORB + TO + 0.475 * FTA,
    OppPoss     = OppFGA - OppORB + OppTO + 0.475 * OppFTA,
    
    # Per-possession (pace-adjusted)
    OffEff      = (FGM * 2 + FGM3 + FTM) / Poss * 100,   # points per 100 poss
    DefEff      = (OppFGM * 2 + OppFGM3 + OppFTM) / OppPoss * 100,
    
    # Shooting %
    TwoPtPct    = (FGM - FGM3) / (FGA - FGA3),
    ThreePtPct  = FGM3 / FGA3,
    ThreePtRate = FGA3 / FGA,   # how 3PT-heavy the offense is
    
    # Defensive shooting %
    OppTwoPtPct   = (OppFGM - OppFGM3) / (OppFGA - OppFGA3),
    OppThreePtPct = OppFGM3 / OppFGA3,
    
    # Misc
    RebMargin    = (ORB + DRB) - (OppORB + OppDRB),
    TOMargin     = OppTO - TO,
    
    # Low-3PT flag: below Arizona's season median 3PT%
    IsArizona    = TeamID == ARIZONA_ID
  )

# ============================================================
# STEP 2: ARIZONA GAME-LEVEL DATA
# ============================================================
az <- game_long %>% filter(IsArizona)

cat("\n--- Arizona 2026: Season Summary ---\n")
cat("Games played:", nrow(az), "\n")
cat("Win rate:", round(mean(az$Win), 3), "\n")
cat("Avg 3PT%:", round(mean(az$ThreePtPct, na.rm = TRUE), 3), "\n")
cat("Avg 3PT Rate (FGA3/FGA):", round(mean(az$ThreePtRate, na.rm = TRUE), 3), "\n")
cat("Avg 2PT%:", round(mean(az$TwoPtPct, na.rm = TRUE), 3), "\n")
cat("Avg Offensive Efficiency:", round(mean(az$OffEff, na.rm = TRUE), 1), "\n")
cat("Avg Defensive Efficiency:", round(mean(az$DefEff, na.rm = TRUE), 1), "\n")

# ============================================================
# QUESTION 1: How does Arizona win when 3PT shooting is low?
# Split games into Low vs High 3PT% and compare other stats
# ============================================================

az_median_3pt <- median(az$ThreePtPct, na.rm = TRUE)

az_split <- az %>%
  mutate(ThreeGroup = ifelse(ThreePtPct < az_median_3pt, "Low 3PT%", "High 3PT%"))

q1_summary <- az_split %>%
  group_by(ThreeGroup) %>%
  summarise(
    Games       = n(),
    WinRate     = round(mean(Win), 3),
    Avg2PtPct   = round(mean(TwoPtPct, na.rm = TRUE), 3),
    AvgFTM      = round(mean(FTM, na.rm = TRUE), 2),
    AvgFTA      = round(mean(FTA, na.rm = TRUE), 2),
    AvgRebMargin = round(mean(RebMargin, na.rm = TRUE), 2),
    AvgTOMargin  = round(mean(TOMargin, na.rm = TRUE), 2),
    AvgOffEff    = round(mean(OffEff, na.rm = TRUE), 1),
    AvgDefEff    = round(mean(DefEff, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\n--- Q1: Arizona Low vs High 3PT% Games ---\n")
print(q1_summary)

# PLOT 1: Win rate by 3PT group
ggplot(q1_summary, aes(x = ThreeGroup, y = WinRate, fill = ThreeGroup)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("Low 3PT%" = "#CC0033", "High 3PT%" = "#003366")) +
  labs(
    title = "Arizona Win Rate: Low vs High 3PT% Games",
    x = NULL, y = "Win Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# PLOT 2: What stats differ in Arizona's wins on low-3PT games?
az_low3 <- az_split %>% filter(ThreeGroup == "Low 3PT%")

az_low3_long <- az_low3 %>%
  select(Win, TwoPtPct, FTA, RebMargin, TOMargin, DefEff) %>%
  mutate(Win = factor(Win, labels = c("Loss", "Win"))) %>%
  pivot_longer(-Win, names_to = "Stat", values_to = "Value")

ggplot(az_low3_long, aes(x = Win, y = Value, fill = Win)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Loss" = "#999999", "Win" = "#CC0033")) +
  facet_wrap(~Stat, scales = "free_y") +
  labs(
    title = "Arizona: What Separates Wins from Losses in Low-3PT% Games?",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# PLOT 3: Scatter — 3PT% vs Win, with 2PT% as color
ggplot(az, aes(x = ThreePtPct, y = jitter(Win, 0.05), color = TwoPtPct)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradient(low = "#ffcccc", high = "#CC0033", name = "2PT%") +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  labs(
    title = "Arizona: 3PT% vs Game Outcome (color = 2PT%)",
    x = "3PT%", y = "Win (1) / Loss (0)"
  ) +
  theme_minimal()

# ============================================================
# QUESTION 2: Does increasing 3PT attempts improve win probability?
# Logistic regression: Win ~ 3PT rate + other controls
# ============================================================

# Game-level logistic regression for ALL teams (more power)
# Then interpret Arizona's position

logit_data <- game_long %>%
  select(Win, ThreePtRate, ThreePtPct, TwoPtPct, FTA,
         RebMargin, TOMargin, DefEff, IsArizona) %>%
  drop_na()

# Model 1: Just 3PT rate
model_q2_simple <- glm(Win ~ ThreePtRate, data = logit_data, family = binomial)
cat("\n--- Q2 Model 1: Win ~ 3PT Rate (all teams) ---\n")
print(summary(model_q2_simple))

# Model 2: Full controls
model_q2_full <- glm(
  Win ~ ThreePtRate + TwoPtPct + FTA + RebMargin + TOMargin + DefEff,
  data   = logit_data,
  family = binomial
)
cat("\n--- Q2 Model 2: Win ~ 3PT Rate + Controls (all teams) ---\n")
print(summary(model_q2_full))

# Odds ratios
cat("\n--- Odds Ratios (Model 2) ---\n")
print(round(exp(coef(model_q2_full)), 3))

# Model 3: Arizona only
model_az <- glm(
  Win ~ ThreePtRate + TwoPtPct + FTA + RebMargin + TOMargin + DefEff,
  data   = az,
  family = binomial
)
cat("\n--- Q2 Model 3: Arizona-only logistic regression ---\n")
print(summary(model_az))
cat("\n--- Odds Ratios (Arizona model) ---\n")
print(round(exp(coef(model_az)), 3))

# PLOT 4: Predicted win probability vs 3PT rate (holding others at mean)
new_data <- data.frame(
  ThreePtRate = seq(min(logit_data$ThreePtRate, na.rm = TRUE),
                    max(logit_data$ThreePtRate, na.rm = TRUE),
                    length.out = 200),
  TwoPtPct  = mean(logit_data$TwoPtPct, na.rm = TRUE),
  FTA       = mean(logit_data$FTA, na.rm = TRUE),
  RebMargin = mean(logit_data$RebMargin, na.rm = TRUE),
  TOMargin  = mean(logit_data$TOMargin, na.rm = TRUE),
  DefEff    = mean(logit_data$DefEff, na.rm = TRUE)
)
new_data$PredWin <- predict(model_q2_full, newdata = new_data, type = "response")

# Arizona's actual 3PT rate
az_3pt_rate <- mean(az$ThreePtRate, na.rm = TRUE)

ggplot(new_data, aes(x = ThreePtRate, y = PredWin)) +
  geom_line(color = "#003366", linewidth = 1.2) +
  geom_vline(xintercept = az_3pt_rate, color = "#CC0033",
             linetype = "dashed", linewidth = 1) +
  annotate("text", x = az_3pt_rate + 0.01, y = 0.45,
           label = "Arizona\navg", color = "#CC0033", hjust = 0) +
  labs(
    title = "Predicted Win Probability vs 3PT Attempt Rate",
    subtitle = "All other stats held at league average",
    x = "3PT Rate (FGA3 / FGA)", y = "Predicted Win Probability"
  ) +
  theme_minimal()

# PLOT 5: Per-possession offense — Arizona vs league
league_avg <- game_long %>%
  summarise(AvgOffEff = mean(OffEff, na.rm = TRUE),
            AvgDefEff = mean(DefEff, na.rm = TRUE))

az_avg <- az %>%
  summarise(AvgOffEff = mean(OffEff, na.rm = TRUE),
            AvgDefEff = mean(DefEff, na.rm = TRUE))

cat("\n--- Pace-Adjusted Efficiency ---\n")
cat("League Avg Off Eff:", round(league_avg$AvgOffEff, 1),
    "| Def Eff:", round(league_avg$AvgDefEff, 1), "\n")
cat("Arizona Off Eff:   ", round(az_avg$AvgOffEff, 1),
    "| Def Eff:", round(az_avg$AvgDefEff, 1), "\n")

# PLOT 6: Arizona game-level OffEff vs DefEff, colored by win/loss
ggplot(az, aes(x = OffEff, y = DefEff, color = factor(Win))) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("0" = "#999999", "1" = "#CC0033"),
                     labels = c("Loss", "Win"), name = NULL) +
  labs(
    title = "Arizona: Offensive vs Defensive Efficiency Per Game",
    subtitle = "Pace-adjusted (per 100 possessions)",
    x = "Offensive Efficiency", y = "Defensive Efficiency Allowed"
  ) +
  theme_minimal()
