# Poseidat Code Ecosystem — Structure & Process Guide

*DIGIvloot / EFICE · May 2026*

---

## 1. Current file inventory

| File | Role | Status |
|---|---|---|
| `Poseidat_config.R` | Vessel/device registry | ✓ Clean |
| `Poseidat_functions.R` | All API, sync, data access, and plot functions | ✓ Core library |
| `Poseidat_sync_daily.R` | Scheduled daily runner | ✓ Clean |
| `run_poseidat_sync.bat` | Windows Task Scheduler launcher | ✓ Clean |
| `Poseidat_sandbox.R` | Interactive exploration | ✓ Clean |
| `Poseidat_data_check.R` | Raw vs. thinned parquet comparison | ⚠ Partially redundant |
| `Poseidat_quality.R` | Sensor uptime, gaps, outliers | ✓ Valuable, standalone |
| `Poseidat_fuel_diagnostics.R` | Fuel sensor unit/calibration diagnostics | ✓ Valuable, standalone |
| `Poseidat_resolution_comparison.R` | High-res vs. thinned data comparison | ⚠ Partially redundant |

---

## 2. Recommended file structure

The key insight from auditing the codebase is that there are **two distinct concerns** that get mixed across the current files:

- **Infrastructure** — config, sync, raw data storage, thinning. Runs automatically, needs to be stable.
- **Analysis** — QC, diagnostics, exploratory work. Run interactively, evolves over time.

A second issue is **redundancy**: `Poseidat_resolution_comparison.R` contains its own `build_thinned()` and internal helpers (`.load_vessel_parquet`, `.vessel_dirs`, `.device_label`) that duplicate what is already in `Poseidat_functions.R`. `Poseidat_data_check.R` largely replicates `poseidat_files()` and `poseidat_value_types()` that already exist as functions. `Poseidat_fuel_diagnostics.R` also carries its own copy of the same internal helpers.

### Proposed layout

```
R/poseidat/
│
├── infrastructure/
│   ├── Poseidat_config.R           (unchanged — run once to register vessels)
│   ├── Poseidat_functions.R        (unchanged — the single core library)
│   ├── Poseidat_sync_daily.R       (unchanged — scheduled runner)
│   └── run_poseidat_sync.bat       (unchanged — Windows launcher)
│
├── analysis/
│   ├── Poseidat_sandbox.R          (unchanged — interactive exploration)
│   ├── Poseidat_quality.R          (refactored — remove private helpers, use functions.R)
│   ├── Poseidat_fuel_diagnostics.R (refactored — remove private helpers, use functions.R)
│   └── Poseidat_resolution_comparison.R  (refactored — see below)
│
└── Poseidat_ecosystem.md           (this document)
```

### What to remove / consolidate

**`Poseidat_data_check.R` → retire**

Every check it performs is already available via functions in `Poseidat_functions.R`:

| Data check section | Replacement |
|---|---|
| Raw file inventory | `poseidat_files()` |
| Date range per vessel | `poseidat_value_types()` |
| Thinned file inventory | `poseidat_files()` |
| Raw vs. thinned gap | `poseidat_daily()` + `poseidat_value_types()` |

Move any checks that are genuinely missing into `Poseidat_functions.R` and retire the file.

**`Poseidat_resolution_comparison.R` → slim down**

The `build_thinned()` function in this file predates `poseidat_thin()` in `Poseidat_functions.R` and does essentially the same job. The internal helpers (`.load_vessel_parquet`, `.vessel_dirs`, `.device_label`, `.poseidat_root`) are all duplicated from `Poseidat_functions.R`.

Keep only the unique analytical value: `compare_resolution()`, `build_obs_comparison()`, `build_fuel_comparison()`, and their plot/table companions. Remove `build_thinned()` and all private helpers; replace with a `source()` call to `Poseidat_functions.R`.

**Private helpers in `Poseidat_quality.R` and `Poseidat_fuel_diagnostics.R`**

Both files carry `.poseidat_root()`, `.vessel_dirs()`, `.load_vessel_parquet()`, and `.device_label()` — exact or near-exact copies of what lives in `Poseidat_functions.R`. After each file gains a `source()` call to `Poseidat_functions.R`, these can be deleted. The one exception is `Poseidat_fuel_diagnostics.R`'s `.fuel_device_ids()` and `.speed_device_ids()` helpers — these are genuinely specific to that file and should stay.

---

## 3. The data flow

```
API (EFICE / PEFA / DBMatic)
        │
        │  sync_all_vessels()          [daily, automatic]
        ▼
raw/<source>/<vessel>/YYYY/MM/*.parquet
        │
        │  poseidat_thin()             [runs after sync, saves 1-min summaries]
        ▼
processed/thinned/<vessel>/*.parquet
        │
        ├──► Interactive exploration   Poseidat_sandbox.R
        ├──► Overview plots            poseidat_plot_*() in Poseidat_functions.R
        ├──► Quality control           Poseidat_quality.R
        ├──► Fuel diagnostics          Poseidat_fuel_diagnostics.R
        └──► Resolution comparison     Poseidat_resolution_comparison.R
```

All reading for plots and analysis goes through the **thinned** parquet by default (`data_source = "thinned"`), with `data_source = "raw"` available when full resolution is needed. This keeps interactive work fast while preserving the option to drill down.

---

## 4. Process description

### 4.1 Initial setup (run once)

1. Set `.Renviron` entries: `EFICE_TOKEN`, `EFICE_API_URL`, `PEFA_TOKEN`, `PEFA_API_URL`, `ONEDRIVE_POSEIDAT`.
2. Run `Poseidat_config.R` to register vessels and device labels, and save `poseidat_vessels.rds` to OneDrive.
3. Use `Poseidat_sandbox.R` STEP 1 to discover device UUIDs for each vessel (`get_devices()` / `print_devices()`), then update `Poseidat_config.R` with labels and re-run.
4. Schedule `run_poseidat_sync.bat` in Windows Task Scheduler (e.g. daily at 06:00).

### 4.2 Daily automated sync

`Poseidat_sync_daily.R` runs via `run_poseidat_sync.bat`:

1. `setup_poseidat_sync()` — validates paths and credentials.
2. `sync_all_vessels()` — for each enabled vessel × device, downloads records since `max(datetime)` of the existing parquet (or 30-day fallback on first run). Merges into additive monthly parquet files under `raw/`.
3. `poseidat_thin()` — re-aggregates all raw parquet to 1-minute summaries, saves to `processed/thinned/`, keeps only the 3 most recent thinned files per vessel.
4. Prints a summary to console and log: records downloaded, last observation per vessel, total on disk.

The sync is incremental — only new records are downloaded. Re-running is safe; duplicates are removed on `entry_id`.

### 4.3 Monitoring new sensors

Run `audit_new_sensors(vessel_config, source = "efice")` periodically (or add it to the daily runner). It queries the live device list for each vessel and compares against `vessel_config`. Any UUID not yet labelled appears as `NEW ⚠` in the output table. Once identified, add the UUID and label to `Poseidat_config.R` and re-run it to update the RDS.

### 4.4 Interactive exploration (Poseidat_sandbox.R)

Working through the steps manually:

- **STEP 1** — Explore the API: list vessels, list devices, check monthly availability per device.
- **STEP 2** — Download a single device in memory and do a quick sanity-check plot.
- **STEP 3** — Sync a single device to parquet (useful for testing before running the full sync).
- **STEP 4** — Full sync across all vessels.
- **STEP 5** — Verify stored data: file inventory, value_types, daily counts, API vs. parquet comparison.
- **STEP 6** — Rebuild thinned parquet and inspect maps.
- **STEP 7** — Plots and overviews: operational dashboard, timeseries, maps, frequency analysis.

### 4.5 Quality control (Poseidat_quality.R)

Source `Poseidat_functions.R` first. Then:

1. `build_quality_summary()` — reads raw parquet for all vessels, computes per-device per-day uptime, gap count, max gap, and outlier count.
2. `plot_quality_heatmap()` — calendar tile plot of uptime % or gap count.
3. `build_gap_catalogue()` — detailed list of every gap with start/end/duration/type.
4. `plot_gap_timeline()` — horizontal timeline of gaps, coloured by severity.
5. `plot_uptime_distribution()` — box-plots of daily uptime per device.
6. `table_quality_summary()` — gt table of aggregated quality metrics.

Run this after syncing new data to confirm all sensors are reporting as expected.

### 4.6 Fuel diagnostics (Poseidat_fuel_diagnostics.R)

Source `Poseidat_functions.R` first. Then:

1. `build_fuel_thinned()` — loads 1-minute thinned fuel data across all vessels.
2. `build_speed_thinned()` — loads 1-minute thinned SOG derived from POSITION fixes.
3. `diagnose_fuel_units()` — density and box-plots to detect unit mismatches (L/h vs L/min).
4. `diagnose_fuel_idle()` — zero/idle behaviour: what fraction of time does each sensor report zero?
5. `diagnose_fuel_operational()` — fuel rate vs. SOG cross-plot: is consumption correlated with speed?
6. `diagnose_fuel_daily()` — daily totals and active-hour-normalised rate.
7. `table_fuel_diagnostics()` — gt summary table of all metrics across vessels/sensors.

Common findings: ratio ~60× suggests L/min vs. L/h; ratio ~2× suggests one vessel reports per-engine while another reports total; ratio ~0.82× suggests kg/h vs. L/h.

### 4.7 Resolution comparison (Poseidat_resolution_comparison.R)

Source `Poseidat_functions.R` first. Use to validate whether the thinned (1-minute) data is adequate for a given analysis, or whether raw (~10-second) resolution is needed:

1. `compare_resolution()` — computes information-loss metrics per device.
2. `plot_resolution_traces()` — side-by-side high-res vs. thinned time series.
3. `plot_resolution_density()` — value distribution comparison.
4. `plot_peak_detection()` — how many peaks/events are missed in thinned data?
5. `build_obs_comparison()` / `build_fuel_comparison()` — daily observation counts and fuel estimates at both resolutions.

---

## 5. Storage layout

```
<ONEDRIVE_POSEIDAT>/
├── config/
│   └── poseidat_vessels.rds        vessel config + device labels
│   └── sync_state.rds              last sync time per device
├── logs/
│   └── poseidat_sync_YYYYMM.log    one log file per calendar month
├── raw/
│   └── <source>/                   efice | pefa | dbmatic
│       └── <vessel_name>/
│           └── YYYY/MM/
│               └── <vessel>_<device>_YYYYMM.parquet
└── processed/
    └── thinned/
        └── <vessel_name>/
            └── thinned_<vessel>_<date>.parquet   (3 most recent kept)
```

Each raw parquet file covers one vessel × device × calendar month. New records are merged in on every sync; duplicates are removed on `entry_id`. There is never more than one file per vessel/device/month.

---

## 6. Key design decisions

**Single core library.** All API access, sync logic, data loading, thinning, and plotting lives in `Poseidat_functions.R`. Analysis scripts source this file and use its functions rather than reimplementing data access.

**Additive monthly parquet.** Replacing date-stamped files with one-file-per-month avoids fragmentation and makes reading efficient. Arrow can open a directory tree as a dataset without listing individual files.

**`data_source` parameter.** Every plot function accepts `data_source = "thinned"` (default) or `"raw"`. Thinned is fast and sufficient for most work; raw is available for sub-minute detail.

**Adaptive x-axis.** All time series plots auto-select break interval based on the plotted span: ≤3 days → 1-day with time labels; ≤21 days → 2-day; longer → weekly.

**Device discovery is always live.** The sync always queries `get_devices()` from the API rather than relying solely on `vessel_config`. Labels in config are for display only — a new sensor is synced automatically even before it has a label, and `audit_new_sensors()` surfaces it.
