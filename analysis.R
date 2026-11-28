library(haven)
library(dplyr)

evs <- foreign::read.spss("~/Downloads/ZA7503_v3-0-0.sav",
                          to.data.frame = TRUE, use.value.labels = FALSE)
# Перевіряємо хвилі для Франції (код 250)
evs %>%
  filter(S003 == 250) %>%
  count(S002EVS, S020)

library(dplyr)
library(tidyr)     # якщо помилка: install.packages("tidyr")

fr <- evs %>%
  filter(S003 == 250) %>%                                   # 1. Франція
  mutate(
    # 3. від'ємні коди (-1, -2, -4, -5) -> NA
    across(c(F050, F025, F028, F066, X002, X003), ~ ifelse(. < 0, NA, .)),
    
    # 2. рік народження: готова змінна, а якщо її немає, то рік опитування мінус вік
    birth = ifelse(!is.na(X002), X002, S020 - X003),
    
    # 5. десятирічні когорти
    cohort = cut(birth, breaks = seq(1900, 2020, 10), right = FALSE,
                 labels = paste0(seq(1900, 2010, 10), "-", seq(1909, 2019, 10))),
    
    # 4. показники 0/1
    believing    = case_when(F050 == 1 ~ 1, F050 %in% c(0, 2) ~ 0, TRUE ~ NA_real_),
    belonging    = case_when(F025 == 0 ~ 0, F025 > 0 ~ 1, TRUE ~ NA_real_),
    attend_month = case_when(F028 <= 3 ~ 1, F028 > 3 ~ 0, TRUE ~ NA_real_),
    attend_year  = case_when(F028 <= 6 ~ 1, F028 > 6 ~ 0, TRUE ~ NA_real_),
    pray_month   = case_when(F066 <= 4 ~ 1, F066 > 4 ~ 0, TRUE ~ NA_real_),
    pray_year    = case_when(F066 <= 5 ~ 1, F066 > 5 ~ 0, TRUE ~ NA_real_)
    # pray_* додамо після Блоку 1, коли побачимо кодування F066
  )

# 6. зважена частка (%) і n по клітинці "когорта × рік опитування"
make_table <- function(d, var) {
  d %>%
    filter(!is.na(cohort), !is.na(.data[[var]]), !is.na(S017)) %>%
    group_by(cohort, S020) %>%
    summarise(share = 100 * weighted.mean(.data[[var]], S017),
              n = n(), .groups = "drop") %>%
    mutate(share = ifelse(n < 50, NA, round(share, 1)))     # ховаємо малі клітинки
}

tab_believing <- make_table(fr, "believing")
tab_believing %>% select(-n) %>% pivot_wider(names_from = S020, values_from = share)


tab_belonging <- make_table(fr, "belonging")
tab_belonging %>% select(-n) %>% pivot_wider(names_from = S020, values_from = share)

tab_attend_month <- make_table(fr, "attend_month")
tab_attend_month %>% select(-n) %>% pivot_wider(names_from = S020, values_from = share)

tab_attend_year <- make_table(fr, "attend_year")
tab_attend_year %>% select(-n) %>% pivot_wider(names_from = S020, values_from = share)

for (v in c("believing","belonging","attend_month","attend_year","pray_month","pray_year")) {
  cat("\n=====", v, "=====\n")
  make_table(fr, v) %>% select(-n) %>%
    pivot_wider(names_from = S020, values_from = share) %>% print(n = Inf)
}

library(dplyr); library(tidyr); library(ggplot2)   # якщо ggplot2 немає: install.packages("ggplot2")
dir.create("results", showWarnings = FALSE)

indicators <- c("believing", "belonging", "attend_month", "attend_year",
                "pray_month", "pray_year")

# одна довга таблиця: показник × когорта × рік, зважена частка і n
all_tabs <- bind_rows(lapply(indicators, function(v) {
  fr %>%
    filter(!is.na(cohort), !is.na(.data[[v]]), !is.na(S017)) %>%
    group_by(cohort, S020) %>%
    summarise(share = 100 * weighted.mean(.data[[v]], S017),
              n = n(), .groups = "drop") %>%
    mutate(indicator = v, low_n = n < 50)       # low_n = TRUE: клітинка ненадійна
}))

write.csv(all_tabs, "results/cohort_wave_long.csv", row.names = FALSE)

# окремі "широкі" таблиці (як ти бачила в консолі) з окремими n
for (v in indicators) {
  all_tabs %>% filter(indicator == v) %>%
    mutate(share = round(share, 1)) %>%
    select(cohort, S020, share, n) %>%
    pivot_wider(names_from = S020, values_from = c(share, n)) %>%
    write.csv(paste0("results/table_", v, ".csv"), row.names = FALSE)
}

# графік: лінії когорт за роками опитування, по панелі на кожен показник
plot_df <- all_tabs %>% filter(!low_n)
p <- ggplot(plot_df, aes(S020, share, colour = cohort, group = cohort)) +
  geom_line() + geom_point(aes(size = n)) +
  facet_wrap(~ indicator, ncol = 2) +
  labs(x = "Survey year", y = "% (weighted)", colour = "Birth cohort", size = "n")
ggsave("results/cohorts_by_wave.png", p, width = 11, height = 9, dpi = 150)

# перегляд n у клітинках, щоб побачити, де вибірка мала
all_tabs %>% filter(indicator == "believing") %>%
  select(cohort, S020, n) %>%
  pivot_wider(names_from = S020, values_from = n) %>% print(n = Inf)

all_tabs <- all_tabs %>%
  mutate(low_n  = n < 50,
         se     = 100 * sqrt((share/100) * (1 - share/100) / n),
         ci_low = share - 1.96 * se,
         ci_high = share + 1.96 * se)

write.csv(all_tabs, "results/cohort_wave_long.csv", row.names = FALSE)

p <- ggplot(plot_df, aes(S020, share, colour = cohort, group = cohort)) +
  geom_line(linewidth = 0.8) + geom_point(aes(size = n)) +
  scale_colour_viridis_d(option = "turbo") +
  facet_wrap(~ indicator, ncol = 2) +
  labs(x = "Survey year", y = "% (weighted)", colour = "Birth cohort", size = "n")
ggsave("results/cohorts_by_wave.png", p, width = 11, height = 9, dpi = 150)

all_tabs %>% filter(low_n) %>% select(indicator, cohort, S020, n) %>% print(n = Inf)

pop_raw <- read.csv("data/unpopulation_dataportal_20261126193635.csv")

pop <- pop_raw %>%
  filter(IndicatorId == 47) %>%                       # тільки населення за віком
  mutate(age   = as.numeric(gsub("\\+", "", Age)),    # "100+" -> 100
         birth = Time - age) %>%                      # рік народження (±1 рік)
  filter(age >= 18, Time %in% c(2018, 2036)) %>%      # дорослі, два роки
  mutate(cohort = cut(birth, breaks = seq(1890, 2020, 10), right = FALSE,
                      labels = paste0(seq(1890, 2010, 10), "-", seq(1899, 2019, 10))))

shares <- pop %>%
  group_by(Time, cohort) %>%
  summarise(pop = sum(Value), .groups = "drop") %>%
  group_by(Time) %>%
  mutate(share = 100 * pop / sum(pop)) %>%
  ungroup()

shares %>% select(Time, cohort, share) %>%
  pivot_wider(names_from = Time, values_from = share) %>% print(n = Inf)

write.csv(shares, "results/cohort_shares_UN_WPP2024.csv", row.names = FALSE)





###############################################################################
###############################################################################
#################################PREDICTION####################################
###############################################################################
###############################################################################









cstart <- function(x) as.numeric(substr(as.character(x), 1, 4))   # початок десятиліття

forecast_ind <- function(ind, last_year, target_year, ref_new, ref_high) {
  d <- all_tabs %>%
    filter(indicator == ind, !low_n, S020 <= last_year) %>%
    mutate(cohort = as.character(cohort))
  
  # спільний дрейф: share ~ рік + когорта (кожна когорта має свій рівень)
  m   <- lm(share ~ S020 + cohort, data = d, weights = n)
  b   <- unname(coef(m)["S020"])
  ci  <- unname(confint(m)["S020", ])
  gap <- target_year - last_year
  
  known    <- sort(unique(d$cohort))
  lev_last <- sapply(known, function(cc)
    unname(predict(m, data.frame(cohort = cc, S020 = last_year))))
  
  # лінійний тренд між когортами (від 1940-х), для нижнього сценарію
  gc   <- known[cstart(known) >= 1940]
  tr   <- data.frame(y = unname(lev_last[gc]), idx = (cstart(gc) - 1900) / 10)
  trend <- lm(y ~ idx, data = tr)
  lev_grad <- function(coh)
    unname(predict(trend, data.frame(idx = (cstart(coh) - 1900) / 10)))
  
  # склад дорослого населення в цільовому році (ООН)
  tgt <- shares %>%
    filter(Time == target_year, !is.na(share)) %>%
    mutate(cohort = as.character(cohort))
  tgt$kind <- ifelse(tgt$cohort %in% known, "known",
                     ifelse(cstart(tgt$cohort) > cstart(ref_new), "new", "dropped"))
  
  scen <- function(drift, rule) {
    lv <- numeric(nrow(tgt))
    for (i in seq_len(nrow(tgt))) {
      coh <- tgt$cohort[i]
      lv[i] <- if (tgt$kind[i] == "known") {
        lev_last[[coh]] + drift * gap
      } else if (tgt$kind[i] == "new") {
        switch(rule, base = lev_last[[ref_new]], high = lev_last[[ref_high]],
               grad = lev_grad(coh)) + drift * gap
      } else NA_real_
    }
    keep <- !is.na(lv)
    sum(tgt$share[keep] * lv[keep]) / sum(tgt$share[keep])
  }
  
  grid <- expand.grid(drift = c(ci[1], b, ci[2]),
                      rule  = c("grad", "base", "high"), stringsAsFactors = FALSE)
  grid$value <- mapply(scen, grid$drift, grid$rule)
  
  tibble(indicator = ind, target_year = target_year,
         central = scen(b, "base"), low = min(grid$value), high = max(grid$value),
         replacement_only = scen(0, "base"),          # <- новий рядок
         drift_pp_per_decade = 10 * b)
}

inds <- c("believing", "belonging", "attend_month", "attend_year",
          "pray_month", "pray_year")

# ПРОГНОЗ 2036
fc <- bind_rows(lapply(inds, forecast_ind, last_year = 2018, target_year = 2036,
                       ref_new = "1990-1999", ref_high = "1980-1989"))
print(fc %>% mutate(across(where(is.numeric), ~ round(., 1))))
write.csv(fc, "results/forecast_2036.csv", row.names = FALSE)

# BACK-CAST: дані лише до 2008 року -> "передбачаємо" 2018
bc <- bind_rows(lapply(inds, forecast_ind, last_year = 2008, target_year = 2018,
                       ref_new = "1980-1989", ref_high = "1970-1979"))
nat <- function(v, yr) {
  x <- fr[fr$S020 == yr, ]
  100 * weighted.mean(x[[v]], x$S017, na.rm = TRUE)
}
bc$actual_2018    <- sapply(bc$indicator, nat, yr = 2018)
bc$no_change_2008 <- sapply(bc$indicator, nat, yr = 2008)
bc$error_model    <- bc$central - bc$actual_2018
bc$error_naive    <- bc$no_change_2008 - bc$actual_2018
bc$inside_range   <- bc$actual_2018 >= bc$low & bc$actual_2018 <= bc$high
print(bc %>% mutate(across(where(is.numeric), ~ round(., 1))))
write.csv(bc, "results/backcast_2018.csv", row.names = FALSE)

all_tabs <- all_tabs %>% mutate(cohort = factor(gsub("–", "-", as.character(cohort))))
shares   <- shares   %>% mutate(cohort = factor(gsub("–", "-", as.character(cohort))))

bc %>%
  mutate(error_drift = central - actual_2018,
         error_repl  = replacement_only - actual_2018,
         error_naive = no_change_2008 - actual_2018,
         better = ifelse(abs(error_repl) < abs(error_drift),
                         "без дрейфу", "з дрейфом")) %>%
  select(indicator, error_drift, error_repl, error_naive, better) %>%
  mutate(across(where(is.numeric), ~ round(., 1)))
all_tabs <- all_tabs %>% mutate(cohort = factor(gsub("–", "-", as.character(cohort))))
shares   <- shares   %>% mutate(cohort = factor(gsub("–", "-", as.character(cohort))))










# =========================================================
# ФІНАЛЬНА МОДЕЛЬ (варіант B)
# центр: когортна заміна (ООН) + дрейф лише для belonging
# нижня межа: найпесимістичніший сценарій моделі
# верхня межа: найоптимістичніший сценарій моделі або наївний прогноз
# =========================================================

nat <- function(v, yr) {                     # загальнонаціональна зважена частка
  x <- fr[fr$S020 == yr, ]
  100 * weighted.mean(x[[v]], x$S017, na.rm = TRUE)
}

forecast_B <- function(ind, last_year, target_year, ref_new, ref_high, use_drift) {
  d <- all_tabs %>%
    filter(indicator == ind, !low_n, S020 <= last_year) %>%
    mutate(cohort = as.character(cohort))
  
  m   <- lm(share ~ S020 + cohort, data = d, weights = n)
  b   <- unname(coef(m)["S020"])             # дрейф (пункти за рік)
  ci  <- unname(confint(m)["S020", ])        # межі оцінки дрейфу
  d0  <- if (use_drift) b else 0             # дрейф у центральному прогнозі
  gap <- target_year - last_year
  
  known    <- sort(unique(d$cohort))
  lev_last <- sapply(known, function(cc)
    unname(predict(m, data.frame(cohort = cc, S020 = last_year))))
  
  gc    <- known[cstart(known) >= 1940]
  tr    <- data.frame(y = unname(lev_last[gc]), idx = (cstart(gc) - 1900) / 10)
  trend <- lm(y ~ idx, data = tr)
  lev_grad <- function(coh)
    unname(predict(trend, data.frame(idx = (cstart(coh) - 1900) / 10)))
  
  tgt <- shares %>%
    filter(Time == target_year, !is.na(share)) %>%
    mutate(cohort = as.character(cohort))
  tgt$kind <- ifelse(tgt$cohort %in% known, "known",
                     ifelse(cstart(tgt$cohort) > cstart(ref_new), "new", "dropped"))
  
  scen <- function(drift, rule) {
    lv <- numeric(nrow(tgt))
    for (i in seq_len(nrow(tgt))) {
      coh <- tgt$cohort[i]
      lv[i] <- if (tgt$kind[i] == "known") {
        lev_last[[coh]] + drift * gap
      } else if (tgt$kind[i] == "new") {
        switch(rule, base = lev_last[[ref_new]], high = lev_last[[ref_high]],
               grad = lev_grad(coh)) + drift * gap
      } else NA_real_
    }
    keep <- !is.na(lv)
    sum(tgt$share[keep] * lv[keep]) / sum(tgt$share[keep])
  }
  
  grid <- expand.grid(drift = c(ci[1], d0, ci[2]),
                      rule  = c("grad", "base", "high"), stringsAsFactors = FALSE)
  grid$value <- mapply(scen, grid$drift, grid$rule)
  
  naive <- nat(ind, last_year)
  
  # похибка вибірки (95%), з урахуванням ваг через ефективний розмір вибірки
  x     <- fr[fr$S020 == last_year & !is.na(fr[[ind]]) & !is.na(fr$S017), ]
  n_eff <- sum(x$S017)^2 / sum(x$S017^2)
  p     <- naive / 100
  moe   <- 100 * 1.96 * sqrt(p * (1 - p) / n_eff)
  
  tibble(indicator   = ind,
         value_last  = naive,
         target_year = target_year,
         central     = scen(d0, "base"),
         low         = min(grid$value) - moe,
         high        = max(c(grid$value, naive)) + moe,
         moe         = moe,
         n_eff       = n_eff,
         drift_used_pp_per_decade = 10 * d0)
}

inds      <- c("believing", "belonging", "attend_month", "attend_year",
               "pray_month", "pray_year")
drift_ind <- c("belonging")                        # дрейф лише тут

# ---- ПРОГНОЗ 2036 ----
fcB <- bind_rows(lapply(inds, function(v)
  forecast_B(v, last_year = 2018, target_year = 2036,
             ref_new = "1990-1999", ref_high = "1980-1989",
             use_drift = v %in% drift_ind)))
print(fcB %>% mutate(across(where(is.numeric), ~ round(., 1))))
write.csv(fcB, "results/forecast_2036_final.csv", row.names = FALSE)

# ---- BACK-CAST тієї ж моделі: дані до 2008 -> 2018 ----
bcB <- bind_rows(lapply(inds, function(v)
  forecast_B(v, last_year = 2008, target_year = 2018,
             ref_new = "1980-1989", ref_high = "1970-1979",
             use_drift = v %in% drift_ind)))
bcB$actual_2018  <- sapply(bcB$indicator, nat, yr = 2018)
bcB$error        <- bcB$central - bcB$actual_2018
bcB$inside_range <- bcB$actual_2018 >= bcB$low & bcB$actual_2018 <= bcB$high
print(bcB %>% mutate(across(where(is.numeric), ~ round(., 1))))
write.csv(bcB, "results/backcast_2018_final.csv", row.names = FALSE)

# ---- ГРАФІК: спостереження 1981–2018 + прогноз 2036 з діапазоном ----
obs <- expand.grid(indicator = inds, S020 = sort(unique(fr$S020)),
                   stringsAsFactors = FALSE)
obs$share <- mapply(nat, obs$indicator, obs$S020)
obs <- obs %>% filter(!is.na(share))

p2 <- ggplot() +
  geom_line(data = obs, aes(S020, share)) +
  geom_point(data = obs, aes(S020, share)) +
  geom_segment(data = fcB, aes(x = 2018, xend = target_year,
                               y = value_last, yend = central),
               linetype = "dashed", colour = "red") +
  geom_errorbar(data = fcB, aes(x = target_year, ymin = low, ymax = high),
                width = 1.5, colour = "red") +
  geom_point(data = fcB, aes(target_year, central), colour = "red", size = 3) +
  facet_wrap(~ indicator, ncol = 2) +
  labs(x = "Year", y = "% of adults (weighted)",
       title = "France: religion 1981–2018 (EVS/WVS) and forecast for 2036",
       subtitle = "Red: central forecast and range; cohort replacement (UN WPP 2024), drift for belonging only")
ggsave("results/forecast_2036.png", p2, width = 10, height = 9, dpi = 150)



