# =============================================================================
# Aravis: gallows load (TrawlTension / traction_center) versus the skipper's
# catch weights from an Excel sheet, with a trip_uid <-> trip_id link table
# -----------------------------------------------------------------------------
# Input Excel (one row per haul), columns:
#   vessel | trip_id | haul | date | time | vangst_kg     (time = shoot time)
#
# A. Read the Excel; give each haul the Poseidat trip_uid its shoot time falls in.
# B. Read the raw ~1 Hz TrawlTension data (traction_center = gallows load).
# C. Detect lifts: short excursions of traction_center above the empty level.
# D. Couple lifts to hauls: the lift(s) of haul N lie between the shoot of
#    haul N and the shoot of haul N+1 (the catch comes aboard before the next shoot).
# E. Calibrate manual kg against the sensor load (three baseline candidates),
#    flag outliers, write Excel + PDF.
#
# dplyr style throughout; no apply family, no purrr.
# Poseidat_functions.R must be sourced (for .onedrive() and .poseidat_load_data()).
# =============================================================================

library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(readxl)
library(ggplot2)
library(writexl)

source(file.path(here::here(),"R","poseidat", "Poseidat_functions.R"))

setup_poseidat_sync()

# ---- 0. Settings ------------------------------------------------------------

vessel_name   <- "Aravis"
pad_excel     <- file.path("C:/TEMP", "SCH99 vangst per trek 20260301-20261119.xlsx")   # ADJUST
trawl_device  <- "c511bf10-1445-4962-8f51-c9deff4f7b67"   # Trekkrachtmeter / TRAWL_TENSION
raw_dir       <- file.path(.onedrive(), "raw")
uit_dir       <- file.path(.onedrive(), "analyse", "hekgalgmeter")
tz_lokaal     <- "Europe/Amsterdam"
tz_excel      <- "Europe/Amsterdam"   # clock in which the skipper writes the times
periode       <- c(as_date("2026-10-15"), Sys.Date())

par <- list(
  spike_k        = 5,     # running median width (samples) against single spikes
  max_gap_s      = 30,    # a gap longer than this starts a new segment
  leeg_drempel   = 150,   # gallows (nearly) empty below this
  min_lift_s     = 20,    # shorter excursions are swings/spikes
  max_lift_min   = 20,    # longer excursions are not lifts
  min_piek       = 250,   # a lift reaches at least this load
  samenvoeg_s    = 60,    # excursions closer than this are one lift
  plateau_frac   = 0.6,   # plateau = median of samples >= frac * 95th percentile
  voor_venster_s = c(-120, -5),
  na_venster_s   = c(30, 180),
  na_schiet_min  = 15,    # ignore lifts in the first minutes after shooting (gear going out)
  laatste_haul_h = 6      # window length for the last haul of a trip (no next shoot)
)

dir.create(uit_dir, recursive = TRUE, showWarnings = FALSE)


# ---- A. Skipper's Excel and trip link ---------------------------------------

# Excel may deliver the time as a POSIXct (1899-12-31 HH:MM), a day fraction,
# or text "HH:MM"; reduce all three to "HH:MM".
als_hhmm <- function(x) {
  if (inherits(x, "POSIXt")) return(format(x, "%H:%M", tz = "UTC"))
  num <- suppressWarnings(as.numeric(x))
  if (all(is.na(num) == is.na(x))) {
    return(format(as.POSIXct(num * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"))
  }
  substr(trimws(as.character(x)), 1, 5)
}

hand_raw <- read_excel(pad_excel)

# date may arrive as a date-time or as text "19/11/2026"
als_datum <- function(x) if (inherits(x, c("POSIXt", "Date"))) as_date(x) else dmy(x)

hauls <- hand_raw |>
  mutate(hhmm = als_hhmm(time), datum = als_datum(date)) |>
  transmute(
    vessel    = as.character(vessel),
    trip_id   = as.character(trip_id),
    haul_id   = as.integer(haul),
    shoot_time = as.POSIXct(paste(datum, hhmm), format = "%Y-%m-%d %H:%M",
                           tz = tz_excel) |> with_tz(tz_lokaal),
    vangst_kg = as.numeric(gsub(",", "", as.character(vangst_kg)))
  ) |>
  filter(!is.na(shoot_time),
         as_date(shoot_time) >= periode[1], as_date(shoot_time) <= periode[2]) |>
  arrange(trip_id, shoot_time)

cat("Hauls in Excel:", nrow(hauls), " trips:", n_distinct(hauls$trip_id),
    " vessel(s):", paste(unique(hauls$vessel), collapse = ", "), "\n")
if (nrow(hauls) == 0) stop("No hauls with a valid date/time in ", pad_excel)

# Poseidat trips from the thinned file (trip_uid is already written there)
pos_trips <- .poseidat_load_data("thinned", vessel_name = vessel_name) |>
  filter(!is.na(trip_uid)) |>
  group_by(vessel_name, trip_uid) |>
  summarise(departure_time = min(datetime), arrival_time = max(datetime), .groups = "drop") |>
  mutate(departure_time = with_tz(departure_time, tz_lokaal),
         arrival_time   = with_tz(arrival_time,   tz_lokaal))

# Link per haul: each shoot time gets the Poseidat trip it falls in. Works for
# trips of any length; if Poseidat splits an elog trip (e.g. after a short
# harbour visit), that trip_id simply links to more than one trip_uid.
hauls <- hauls |>
  left_join(pos_trips |> select(trip_uid, departure_time, arrival_time),
            by = join_by(between(shoot_time, departure_time, arrival_time))) |>
  select(-departure_time, -arrival_time)

# One row per trip_id x trip_uid pair
trip_link <- hauls |>
  filter(!is.na(trip_uid)) |>
  group_by(vessel, trip_id, trip_uid) |>
  summarise(n_hauls = n(), eerste_shoot = min(shoot_time), laatste_shoot = max(shoot_time),
            .groups = "drop") |>
  left_join(pos_trips |> select(trip_uid, departure_time, arrival_time), by = "trip_uid") |>
  arrange(eerste_shoot)

# One row per Excel trip: how many of its hauls got a trip_uid
trip_dekking <- hauls |>
  group_by(trip_id) |>
  summarise(n_hauls        = n(),
            n_met_trip_uid = sum(!is.na(trip_uid)),
            trip_uids      = paste(sort(unique(na.omit(trip_uid))), collapse = ", "),
            eerste_shoot   = min(shoot_time),
            laatste_shoot  = max(shoot_time),
            .groups = "drop") |>
  mutate(status = case_when(
    n_met_trip_uid == n_hauls ~ "ok",
    n_met_trip_uid == 0       ~ "geen Poseidat trip",
    TRUE                      ~ "deels"
  )) |>
  arrange(eerste_shoot)

cat("\nExcel trips and their Poseidat trip_uid(s):\n")
print(trip_dekking, n = Inf, width = Inf)

excel_periode <- range(hauls$shoot_time)
cat("Poseidat trips within the Excel period without any haul:",
    paste(pos_trips |>
            filter(arrival_time >= excel_periode[1], departure_time <= excel_periode[2],
                   !trip_uid %in% trip_link$trip_uid) |>
            pull(trip_uid), collapse = ", "), "\n")

write_parquet(trip_link, file.path(uit_dir, paste0(vessel_name, "_trip_link.parquet")))


# ---- B. Raw TrawlTension data ----------------------------------------------

trawl_files <- list.files(raw_dir, pattern = paste0(trawl_device, ".*\\.parquet$"),
                          recursive = TRUE, full.names = TRUE)

hek <- open_dataset(trawl_files) |>
  select(datetime, traction_center) |>
  filter(!is.na(traction_center)) |>
  collect() |>
  distinct(datetime, .keep_all = TRUE) |>
  mutate(datetime = with_tz(datetime, tz_lokaal)) |>
  filter(as_date(datetime) >= periode[1], as_date(datetime) <= periode[2]) |>
  arrange(datetime) |>
  mutate(
    dt_s    = as.numeric(difftime(datetime, lag(datetime), units = "secs")),
    segment = cumsum(is.na(dt_s) | dt_s > par$max_gap_s)
  ) |>
  group_by(segment) |>
  mutate(load = if (n() >= par$spike_k) {
    as.numeric(runmed(traction_center, par$spike_k, endrule = "keep"))
  } else {
    traction_center
  }) |>
  ungroup() |>
  select(datetime, segment, raw = traction_center, load)

cat("\nTrawlTension records:", nrow(hek), " from", format(min(hek$datetime)),
    "to", format(max(hek$datetime)), "\n")


# ---- C. Detect lifts --------------------------------------------------------

excursies <- hek |>
  mutate(
    boven = load > par$leeg_drempel,
    run   = cumsum(boven != lag(boven, default = FALSE) |
                   segment != lag(segment, default = first(segment)))
  ) |>
  filter(boven) |>
  group_by(segment, run) |>
  summarise(start = min(datetime), eind = max(datetime), piek = max(load), .groups = "drop") |>
  arrange(start)

lifts <- excursies |>
  mutate(gap_s   = as.numeric(difftime(start, lag(eind), units = "secs")),
         lift_id = cumsum(is.na(gap_s) | gap_s > par$samenvoeg_s)) |>
  group_by(lift_id) |>
  summarise(start = min(start), eind = max(eind), piek = max(piek), .groups = "drop") |>
  mutate(duur_s = as.numeric(difftime(eind, start, units = "secs"))) |>
  filter(duur_s >= par$min_lift_s, duur_s <= par$max_lift_min * 60, piek >= par$min_piek) |>
  mutate(lift_id = row_number())

lift_plateau <- hek |>
  select(datetime, load) |>
  inner_join(lifts |> select(lift_id, start, eind), by = join_by(between(datetime, start, eind))) |>
  group_by(lift_id) |>
  mutate(hoog = load >= par$plateau_frac * quantile(load, 0.95)) |>
  summarise(
    plateau        = median(load[hoog]),
    plateau_duur_s = sum(hoog),
    .groups = "drop"
  )

venster_mediaan <- function(hek, lifts, anker, van_s, tot_s, naam) {
  vensters <- lifts |>
    transmute(lift_id, v_van = .data[[anker]] + van_s, v_tot = .data[[anker]] + tot_s)
  hek |>
    select(datetime, load) |>
    inner_join(vensters, by = join_by(between(datetime, v_van, v_tot))) |>
    group_by(lift_id) |>
    summarise("{naam}" := median(load), .groups = "drop")
}

lifts_tab <- lifts |>
  left_join(lift_plateau, by = "lift_id") |>
  left_join(venster_mediaan(hek, lifts, "start", par$voor_venster_s[1], par$voor_venster_s[2], "basis_voor"),
            by = "lift_id") |>
  left_join(venster_mediaan(hek, lifts, "eind", par$na_venster_s[1], par$na_venster_s[2], "basis_na"),
            by = "lift_id") |>
  mutate(netto_voor = plateau - basis_voor,
         netto_na   = plateau - basis_na) |>
  # attach the Poseidat trip_uid the lift falls in
  left_join(pos_trips |> select(trip_uid, departure_time, arrival_time),
            by = join_by(between(start, departure_time, arrival_time))) |>
  select(-departure_time, -arrival_time)

cat("Detected lifts:", nrow(lifts_tab), "\n")


# ---- D. Couple lifts to hauls (shoot N .. shoot N+1) -----------------------

haul_vensters <- hauls |>
  group_by(trip_id) |>
  mutate(volgende_shoot = lead(shoot_time)) |>
  ungroup() |>
  mutate(w_van = shoot_time + minutes(par$na_schiet_min),
         w_tot = coalesce(volgende_shoot, shoot_time + hours(par$laatste_haul_h)))

haul_lifts <- haul_vensters |>
  inner_join(lifts_tab |> select(-trip_uid),
             by = join_by(between(y$start, x$w_van, x$w_tot)))

gekoppeld <- haul_vensters |>
  left_join(
    haul_lifts |>
      group_by(trip_id, haul_id) |>
      summarise(
        n_lifts        = n(),
        eerste_lift    = min(start),
        laatste_lift   = max(eind),
        piek           = max(piek),
        plateau_ruw    = sum(plateau,    na.rm = TRUE),
        netto_voor     = sum(netto_voor, na.rm = TRUE),
        netto_na       = sum(netto_na,   na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("trip_id", "haul_id")
  ) |>
  mutate(
    n_lifts           = coalesce(n_lifts, 0L),
    shoot_tot_lift_min = as.numeric(difftime(eerste_lift, shoot_time, units = "mins")),
    status = case_when(
      n_lifts == 0 & (is.na(vangst_kg) | vangst_kg == 0) ~ "geen vangst, geen lift",
      n_lifts == 0                                       ~ "vangst maar geen lift",
      !is.na(vangst_kg) & vangst_kg == 0                 ~ "lift maar vangst 0",
      n_lifts > 2                                        ~ "veel lifts: controleren",
      TRUE                                               ~ "ok"
    )
  )

ongekoppeld_sensor <- lifts_tab |> anti_join(haul_lifts |> select(lift_id), by = "lift_id")

cat("\nCoupling status per haul:\n")
print(count(gekoppeld, status))
cat("Lifts outside any haul window:", nrow(ongekoppeld_sensor), "\n")
cat(sprintf("Shoot -> first lift: median %.0f min (IQR %.0f to %.0f)\n",
            median(gekoppeld$shoot_tot_lift_min, na.rm = TRUE),
            quantile(gekoppeld$shoot_tot_lift_min, 0.25, na.rm = TRUE),
            quantile(gekoppeld$shoot_tot_lift_min, 0.75, na.rm = TRUE)))
cat("  Expect roughly the tow duration. A value ~60 or ~120 min off points to a time-zone difference (tz_excel).\n")


# ---- E. Calibration --------------------------------------------------------

fit_stats <- function(x, y) {
  m  <- lm(y ~ x)
  m0 <- lm(y ~ 0 + x)
  tibble(
    n              = length(y),
    intercept      = coef(m)[[1]],
    helling        = coef(m)[[2]],
    r2             = summary(m)$r.squared,
    rmse           = sqrt(mean(resid(m)^2)),
    rmse_loo       = sqrt(mean((resid(m) / (1 - hatvalues(m)))^2)),
    helling_door_0 = coef(m0)[[1]],
    rmse_door_0    = sqrt(mean(resid(m0)^2))
  )
}

kal_lang <- gekoppeld |>
  filter(n_lifts > 0, !is.na(vangst_kg), vangst_kg > 0) |>
  select(trip_id, haul_id, shoot_time, vangst_kg, n_lifts, plateau_ruw, netto_voor, netto_na) |>
  pivot_longer(c(plateau_ruw, netto_voor, netto_na), names_to = "basis", values_to = "sensor") |>
  filter(!is.na(sensor))

kalibratie <- kal_lang |>
  group_by(basis) |>
  filter(n() >= 4) |>
  summarise(fit_stats(sensor, vangst_kg), .groups = "drop") |>
  arrange(rmse_loo)

print(kalibratie)
beste <- kalibratie |> slice(1)
cat(sprintf("Best baseline: %s  ->  kg = %.1f + %.3f * sensor  (R2 = %.2f, LOO RMSE = %.0f kg)\n",
            beste$basis, beste$intercept, beste$helling, beste$r2, beste$rmse_loo))

per_haul <- kal_lang |>
  filter(basis == beste$basis) |>
  mutate(voorspeld_kg = beste$intercept + beste$helling * sensor,
         residu_kg    = vangst_kg - voorspeld_kg,
         residu_pct   = 100 * residu_kg / vangst_kg,
         afwijkend    = abs(residu_kg) > 2 * beste$rmse) |>
  left_join(hauls |> select(trip_id, haul_id, trip_uid), by = c("trip_id", "haul_id")) |>
  arrange(shoot_time)

cat("Hauls flagged (|residual| > 2 x RMSE):", sum(per_haul$afwijkend), "\n")


# ---- F. Output -------------------------------------------------------------

stempel <- paste0(vessel_name, "_hekgalg_kalibratie_", Sys.Date())

write_xlsx(
  list(
    per_haul           = per_haul,
    kalibratie         = kalibratie,
    alle_hauls         = gekoppeld,
    trip_link          = trip_link,
    trip_dekking       = trip_dekking,
    lifts              = lifts_tab,
    ongekoppeld_sensor = ongekoppeld_sensor,
    instellingen       = tibble::enframe(par, name = "parameter") |>
                           unnest_longer(value) |>
                           group_by(parameter) |>
                           summarise(waarde = paste(value, collapse = " .. "), .groups = "drop")
  ),
  file.path(uit_dir, paste0(stempel, ".xlsx"))
)

pdf(file.path(uit_dir, paste0(stempel, ".pdf")), width = 11.7, height = 8.3)

print(
  ggplot(kal_lang, aes(sensor, vangst_kg)) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "grey40", linewidth = 0.5) +
    geom_point(aes(colour = factor(n_lifts)), size = 2) +
    facet_wrap(~ basis, scales = "free_x") +
    labs(title = "Manual catch weight versus gallows load (traction_center)",
         x = "Sensor load", y = "Manual catch (kg)", colour = "Lifts per haul") +
    theme_minimal()
)

print(
  ggplot(per_haul, aes(shoot_time, residu_kg)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_hline(yintercept = c(-2, 2) * beste$rmse, linetype = 2, colour = "grey70") +
    geom_point(aes(colour = afwijkend), size = 2) +
    scale_colour_manual(values = c(`FALSE` = "steelblue", `TRUE` = "firebrick")) +
    labs(title = paste("Residuals over time, baseline:", beste$basis),
         x = NULL, y = "Manual minus predicted (kg)", colour = "Flagged") +
    theme_minimal()
)

# Traces per haul window (12 per page)
sporen <- hek |>
  select(datetime, load) |>
  inner_join(haul_vensters |> select(trip_id, haul_id, vangst_kg, w_van, w_tot),
             by = join_by(between(datetime, w_van, w_tot))) |>
  mutate(label = sprintf("%s-%s (%s kg)", trip_id, haul_id, vangst_kg))

lift_lijnen <- haul_lifts |>
  mutate(label = sprintf("%s-%s (%s kg)", trip_id, haul_id, vangst_kg))

labels <- unique(sporen$label)
n_pag  <- ceiling(length(labels) / 12)

for (p in seq_len(n_pag)) {
  deze <- labels[((p - 1) * 12 + 1):min(p * 12, length(labels))]
  print(
    ggplot(filter(sporen, label %in% deze), aes(datetime)) +
      geom_line(aes(y = load), linewidth = 0.3, colour = "steelblue") +
      geom_segment(data = filter(lift_lijnen, label %in% deze),
                   aes(x = start, xend = eind, y = plateau, yend = plateau),
                   colour = "firebrick", linewidth = 0.8, inherit.aes = FALSE) +
      geom_segment(data = filter(lift_lijnen, label %in% deze),
                   aes(x = start - 120, xend = start, y = basis_voor, yend = basis_voor),
                   colour = "darkgreen", linewidth = 0.8, inherit.aes = FALSE) +
      geom_segment(data = filter(lift_lijnen, label %in% deze),
                   aes(x = eind, xend = eind + 180, y = basis_na, yend = basis_na),
                   colour = "orange", linewidth = 0.8, inherit.aes = FALSE) +
      facet_wrap(~ label, scales = "free", ncol = 4) +
      scale_x_datetime(date_labels = "%H:%M", timezone = tz_lokaal) +
      labs(title = sprintf("Haul windows (shoot N to shoot N+1), page %d of %d", p, n_pag),
           subtitle = "blue = traction_center, red = plateau, green/orange = baselines",
           x = NULL, y = NULL) +
      theme_minimal(base_size = 8)
  )
}

dev.off()
cat("Output written to", uit_dir, "\n")
