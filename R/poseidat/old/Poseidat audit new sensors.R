# ============================================================================
# audit_new_sensors()
#
# Compares live devices returned by the EFICE API against the device_labels
# currently stored in vessel_config. Reports any unlabelled or previously
# unseen devices, along with a sample of value_types they produce.
#
# Returns a tibble; if empty, nothing new has been activated.
#
# Dependencies: dplyr, httr2, glue
# ============================================================================

audit_new_sensors <- function(
    vessel_config,
    source          = "efice",
    sample_days     = 30,        # look back this many days for value_type probing
    sleep_sec       = 0.3        # pause between API calls to be polite
) {
  
  adp        <- .get_adapter(source)
  cutoff_dt  <- format(Sys.time() - lubridate::days(sample_days),
                       "%Y-%m-%dT%H:%M:%SZ")
  now_dt     <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  
  active_vessels <- vessel_config |>
    filter(source == .env$source, enabled)
  
  print(active_vessels)
  
  results <- vector("list", nrow(active_vessels))
  
  for (i in seq_len(nrow(active_vessels))) {
    
    row        <- active_vessels[i, ]
    jid        <- row$journal_id
    vname      <- row$vessel_name
    known_ids  <- names(row$device_labels[[1]])  # UUIDs already in config
    
    # --- 1. Fetch live device list from API ---
    live_devices <- get_devices(source, jid)          # your existing helper
    
    # --- 2. Flag devices not yet labelled in config ---
    new_devices <- live_devices |>
      filter(!device_id %in% known_ids) |>
      mutate(vessel_name = vname)
    
    if (nrow(new_devices) == 0) {
      results[[i]] <- tibble(
        vessel_name  = vname,
        device_id    = character(),
        value_types  = character(),
        first_seen   = as.POSIXct(NA),
        n_records    = integer(),
        labelled     = logical()
      )
      next
    }
    
    # --- 3. For each new device: probe value_types and record count ---
    probed <- vector("list", nrow(new_devices))
    
    for (j in seq_len(nrow(new_devices))) {
      
      did <- new_devices$device_id[j]
      
      resp <- httr2::request(
        paste0(adp$base_url, "/journals/entries")
      ) |>
        httr2::req_auth_bearer_token(adp$token) |>
        httr2::req_url_query(
          journalId = jid,
          deviceId  = did,
          startDate = cutoff_dt,
          endDate   = now_dt,
          limit     = 100,
          skip      = 0
        ) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform()
      
      data <- httr2::resp_body_json(resp)
      
      vtypes <- data$items |>
        (\(items) if (length(items) == 0) NA_character_
         else paste(unique(sapply(items, \(x) x$value$type)),
                    collapse = ", "))()
      
      # Earliest timestamp in the sample (proxy for activation date)
      first_seen <- data$items |>
        (\(items) if (length(items) == 0) as.POSIXct(NA)
         else min(as.POSIXct(
           sapply(items, \(x) x$recorded_at),
           format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
         )))()
      
      probed[[j]] <- tibble(
        vessel_name = vname,
        device_id   = did,
        value_types = vtypes,
        first_seen  = first_seen,
        n_records   = data$total %||% 0L,
        labelled    = FALSE
      )
      
      Sys.sleep(sleep_sec)
    }
    
    results[[i]] <- bind_rows(probed)
  }
  
  bind_rows(results)
}