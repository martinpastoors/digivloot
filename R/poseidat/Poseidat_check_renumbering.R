# ============================================================================
# Poseidat_check_renumbering.R
# Detect devices that were renumbered / replaced on the server side.
#
# Idea: a configured device that has gone silent ("dead") and a device that is
# NOT in the config but does deliver the same value_type ("candidate") are a
# renumbering pair. Nothing is changed in the config; apply_device_renumbering()
# is a separate, explicit step.
#
# Sources of evidence
#   - local thinned data : last record per configured device_id
#   - EFICE              : audit_sensor_config() (devices seen in recent entries)
#   - PEFA               : get_devices() (equipment_inventory: name, type,
#                          complete_up_to) compared with the config
#   - local thinned data : ids present in the data but not in the config
# Requires Poseidat_functions.R to be sourced. dplyr style, no apply family.
# ============================================================================

check_device_renumbering <- function(vessel_config,
                                     stale_days   = 7,
                                     sample_days  = 60,
                                     sample_limit = 2000L,
                                     sources      = c("efice", "pefa")) {

  now <- Sys.time()
  cfg <- vessel_config |> filter(enabled)

  # ---- 1. configured devices: id + label ------------------------------------
  cfg_list <- vector("list", nrow(cfg))
  for (i in seq_len(nrow(cfg))) {
    lab <- cfg$device_labels[[i]]
    cfg_list[[i]] <- tibble(
      vessel_name = cfg$vessel_name[i],
      source      = cfg$source[i],
      journal_id  = cfg$journal_id[i],
      device_id   = names(lab) %||% character(),
      label       = as.character(unlist(lab, use.names = FALSE)) %||% character()
    )
  }
  cfg_dev <- bind_rows(cfg_list)

  # ---- 2. local data: first/last record per device ---------------------------
  files <- list.files(file.path(.onedrive(), "processed", "thinned"),
                      pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  local_list <- vector("list", length(files))
  for (fi in seq_along(files)) {
    local_list[[fi]] <- arrow::open_dataset(files[fi]) |>
      select(vessel_name, device_id, value_type, datetime) |>
      group_by(vessel_name, device_id, value_type) |>
      summarise(first_obs = min(datetime), last_obs = max(datetime)) |>
      collect() |>
      ungroup()
  }
  local <- bind_rows(local_list) |>
    group_by(vessel_name, device_id, value_type) |>
    summarise(first_obs = min(first_obs), last_obs = max(last_obs), .groups = "drop")

  # configured devices that are dead (silent for > stale_days)
  dead <- cfg_dev |>
    inner_join(local, by = c("vessel_name", "device_id")) |>
    filter(last_obs < now - as.difftime(stale_days, units = "days")) |>
    select(vessel_name, journal_id, old_device_id = device_id, label,
           value_type, old_last_obs = last_obs)

  # ---- 3. candidates: live devices that are not in the config ----------------
  cand_list <- list()

  # 3a. ids in the thinned data but not in the config (and still active)
  cand_list[["thinned"]] <- local |>
    anti_join(cfg_dev, by = c("vessel_name", "device_id")) |>
    filter(last_obs >= now - as.difftime(stale_days, units = "days")) |>
    transmute(vessel_name, new_device_id = device_id, value_type,
              new_first_obs = first_obs, device_name = NA_character_,
              evidence = "thinned data")

  # 3b. EFICE: devices seen in recent entries, not in the config
  if ("efice" %in% sources && any(cfg$source == "efice")) {
    ef <- audit_sensor_config(vessel_config, source = "efice",
                              sample_days = sample_days, sample_limit = sample_limit)
    cand_list[["efice"]] <- ef |>
      filter(status == "missing_in_config") |>
      transmute(vessel_name, new_device_id = device_id, value_type,
                new_first_obs = as.POSIXct(NA), device_name = NA_character_,
                evidence = "EFICE entries")
  }

  # 3c. PEFA: devices in the equipment inventory but not in the config
  pefa_cfg <- cfg |> filter(source == "pefa")
  if ("pefa" %in% sources && nrow(pefa_cfg) > 0) {
    for (i in seq_len(nrow(pefa_cfg))) {
      live <- tryCatch(
        get_devices("pefa", pefa_cfg$journal_id[i], pefa_cfg$vessel_name[i]),
        error = function(e) { message("  get_devices failed: ", conditionMessage(e)); tibble() })
      if (nrow(live) == 0) next
      known <- names(pefa_cfg$device_labels[[i]])
      cand_list[[paste0("pefa_", i)]] <- live |>
        filter(!device_id %in% known) |>
        transmute(vessel_name = pefa_cfg$vessel_name[i],
                  new_device_id = device_id,
                  value_type    = device_type,
                  new_first_obs = as.POSIXct(NA),
                  device_name   = paste(equipment_name, device_name, sep = " / "),
                  evidence      = paste0("PEFA inventory, complete_up_to ",
                                         format(complete_up_to, "%Y-%m-%d")))
    }
  }

  candidates <- bind_rows(cand_list) |> distinct()

  # ---- 4. pair dead devices with candidates of the same value_type ----------
  pairs <- dead |>
    inner_join(candidates, by = c("vessel_name", "value_type"),
               relationship = "many-to-many") |>
    arrange(vessel_name, label, old_last_obs)

  cat(sprintf("\nDead configured devices (> %d days silent): %d\n", stale_days, nrow(dead)))
  if (nrow(dead) > 0) print(dead, n = Inf)
  cat(sprintf("\nUnconfigured devices that look active: %d\n", nrow(candidates)))
  cat(sprintf("Possible renumbering pairs: %d\n", nrow(pairs)))

  invisible(list(dead = dead, candidates = candidates, pairs = pairs))
}


# Explicit step: add the new device_id to the vessel's device_labels under the
# SAME label as the old one (the old id stays, so history keeps its label).
# pairs: tibble with vessel_name, new_device_id, label
apply_device_renumbering <- function(vessel_config, pairs, save = FALSE) {
  for (k in seq_len(nrow(pairs))) {
    i <- which(vessel_config$vessel_name == pairs$vessel_name[k] & vessel_config$enabled)
    if (length(i) != 1) stop("Vessel not unique in config: ", pairs$vessel_name[k])
    lab <- vessel_config$device_labels[[i]]
    lab[[pairs$new_device_id[k]]] <- pairs$label[k]
    vessel_config$device_labels[[i]] <- lab
    cat(sprintf("%s: %s -> %s\n", pairs$vessel_name[k], pairs$new_device_id[k], pairs$label[k]))
  }
  if (save) {
    saveRDS(vessel_config, file.path(.onedrive(), "config", "poseidat_vessels.rds"))
    cat("Config saved.\n")
  }
  invisible(vessel_config)
}


# ============================================================================
# Targeted probe for EFICE: what delivers data right AFTER a device went silent?
# The EFICE entries endpoint ignores sort direction (oldest first), so a broad
# "recent sample" misses changes. Instead: small windows just after old_last_obs.
# dead = chk$dead from check_device_renumbering()
# ============================================================================
probe_after_silence <- function(vessel_config, dead,
                                offsets_h = c(0, 6, 24, 72, 168),
                                width_h   = 2,
                                limit     = 1000L) {

  adp <- .get_adapter("efice")

  efice_ids <- vessel_config |> filter(enabled, source == "efice") |> pull(journal_id)

  targets <- dead |>
    filter(journal_id %in% efice_ids) |>
    distinct(vessel_name, journal_id, old_last_obs)

  fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out <- list()

  for (i in seq_len(nrow(targets))) {
    for (off in offsets_h) {
      t0 <- targets$old_last_obs[i] + off * 3600
      t1 <- t0 + width_h * 3600
      cat(sprintf("%s: window %s (+%dh)\n", targets$vessel_name[i], fmt(t0), off))

      resp <- tryCatch(
        httr2::request(paste0(adp$base_url, "/journals/entries")) |>
          httr2::req_auth_bearer_token(adp$token) |>
          httr2::req_url_query(journalId = targets$journal_id[i],
                               startDate = fmt(t0), endDate = fmt(t1),
                               limit = limit) |>
          httr2::req_error(is_error = \(r) FALSE) |>
          httr2::req_timeout(120) |>
          httr2::req_retry(max_tries = 2) |>
          httr2::req_perform(),
        error = function(e) { message("  failed: ", conditionMessage(e)); NULL })

      if (is.null(resp) || httr2::resp_status(resp) >= 400) next
      items <- httr2::resp_body_json(resp)$items %||% list()
      if (length(items) == 0) next

      dev <- character(length(items)); vt <- character(length(items))
      for (k in seq_along(items)) {
        dev[k] <- as.character(items[[k]]$device_id  %||% NA_character_)
        vt[k]  <- as.character(items[[k]]$value$type %||% NA_character_)
      }
      out[[length(out) + 1]] <- tibble(
        vessel_name = targets$vessel_name[i], journal_id = targets$journal_id[i],
        offset_h = off, device_id = dev, value_type = vt)
    }
  }

  seen <- bind_rows(out)
  if (nrow(seen) == 0) { cat("Nothing returned.\n"); return(invisible(tibble())) }

  known <- vessel_config |>
    filter(enabled) |> select(journal_id, device_labels)
  known_tbl <- list()
  for (i in seq_len(nrow(known))) {
    lab <- known$device_labels[[i]]
    known_tbl[[i]] <- tibble(journal_id = known$journal_id[i],
                             device_id = names(lab) %||% character(),
                             label = as.character(unlist(lab, use.names = FALSE)) %||% character())
  }

  res <- seen |>
    count(vessel_name, journal_id, device_id, value_type, name = "n_entries") |>
    left_join(bind_rows(known_tbl), by = c("journal_id", "device_id")) |>
    mutate(in_config = !is.na(label)) |>
    arrange(vessel_name, in_config, value_type)

  print(res, n = Inf)
  invisible(res)
}
