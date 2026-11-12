# Numbers for the opening of the results: every recorded return in both field
# seasons (not only the trials the models use), written as LaTeX macros to
# spatialVis_writing/tables/returns_numbers.tex. All names start with ret.
# Needs joint from join_2024-25_datasets.R: one row per return, SR = 2024,
# CS = 2025. light_pred is log10 lx from predictlight::add_light_level();
# time_rel_setrise is minutes from sunrise (am returns) or sunset (pm returns).
source("../shared/latex_tables.R")  # num(), write_numbers()

returns <- joint
stopifnot(!anyNA(returns$light_pred), all(returns$session %in% c("am", "pm")))

# light summaries for a set of returns, named with a prefix, e.g. retAmLightMean
light_numbers <- function(x, prefix) {
  rng <- range(x$light_pred)
  c(setNames(nrow(x), paste0(prefix, "N")),
    setNames(c(num(mean(x$light_pred), 2), num(sd(x$light_pred), 2),
               num(rng[1], 2), num(rng[2], 2), num(diff(rng), 1)),
             paste0(prefix, "Light", c("Mean", "SD", "Min", "Max", "Range"))))
}

# dates of a season, e.g. "22 February to 28 June 2024"
season_dates <- function(d) {
  day <- function(x) as.integer(format(x, "%d"))  # no leading zero
  paste(day(min(d)), format(min(d), "%B"), "to", day(max(d)), format(max(d), "%B %Y"))
}

sr_ret <- filter(returns, experiment == "sr")
cs_ret <- filter(returns, experiment == "cs")
am_ret <- filter(returns, session == "am")
pm_ret <- filter(returns, session == "pm")

# fold change in illuminance over the range of all returns, to 2 significant
# figures with a thousands separator, e.g. 160,000
fold <- 10^diff(range(returns$light_pred))

c(
  # all returns, per season and per nest
  light_numbers(returns, "ret"),
  retNSR       = nrow(sr_ret),
  retNCS       = nrow(cs_ret),
  retNNests    = n_distinct(returns$nest),
  retNNestsSR  = n_distinct(sr_ret$nest),
  retNNestsCS  = n_distinct(cs_ret$nest),
  retDatesSR   = season_dates(sr_ret$date),
  retDatesCS   = season_dates(cs_ret$date),
  retLightFold = formatC(signif(fold, 2), format = "d", big.mark = ","),
  # morning and evening returns
  light_numbers(am_ret, "retAm"),
  light_numbers(pm_ret, "retPm"),
  # minutes from sunrise (am) or sunset (pm); negative is before
  retAmMinFrom = num(min(am_ret$time_rel_setrise), 0),
  retAmMinTo   = num(max(am_ret$time_rel_setrise), 0),
  retPmMinFrom = num(min(pm_ret$time_rel_setrise), 0),
  retPmMinTo   = num(max(pm_ret$time_rel_setrise), 0),
  # returns outside the light model's time range, so their light is extrapolated
  retNExtrapolated = sum(returns$light_pred_extrapolated)
) %>%
  write_numbers("returns_numbers.tex")
