# =============================================================================
# corridor_prioritization_data_prep.R
# =============================================================================

# Load packages
library(sf)
library(dplyr)
library(readr)
library(readxl)
library(tigris)
library(stringr)
library(tidyr)
library(scales)
library(purrr)


# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
cfg <- list(
  crs_wgs84 = 4326,
  crs_ca = 3310,
  m2mi = 0.000621371,
  buff_miles = 0.5,
  
  root = "/Users/S152973/Downloads/Corridor_Prioritization_Tool",
  
  shs = "Data/State_Highway_Network_Lines.geojson",
  county_abbr = "Data/CountyAbbr.xlsx",
  district_bdy = "Data/Caltrans_Districts.geojson",
  eqi = "Data/EQI.rds",
  hpi = "Data/hpi.csv",
  hpi_state_fips = "06",
  hpi_year = 2010,
  tims_folder = "Data/TIMS_CSVs/",
  apr_gdb = "Data/APR.gdb",
  apr_roads = "APR_Roads_corrected",
  hex_gdb = "Data/SpecializedTruckCorridor_20250808.gdb",
  hex_layer = "HexGrid_May17",
  mpr = "Data/MobilityPerformanceReports.csv",
  
  out_final = "Outputs/corridor_final_dcr.csv",
  out_folder = "Outputs/District_Data_Tables"
)

setwd(cfg$root)

# Standard join keys used throughout the pipeline.
KEYS <- c("District", "CountyA", "Route")

# Columns kept from each TIMS crash file (names vary across files).
TIMS_COLS <- c("POINT_X", "POINT_Y", "STATE_HWY_IND",
               "COLLISION_SEVERITY", "STATE_ROUTE", "COUNTY")

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

# Rescale positive values to [1, 5]; zeros / NA collapse to 1.
# Used for APR (climate) sub-scores.
rescale_1_to_5 <- function(x) {
  x <- replace_na(x, 0)
  out <- rep(1, length(x))
  pos <- x > 0
  if (any(pos)) out[pos] <- rescale(x[pos], to = c(1, 5))
  out
}

# Percent-rank within District, appended as `<col>_norm`.
# `reverse = TRUE` flips the rank (1 - rank) so that "worse" sorts high.
norm_by_district <- function(df, col, reverse = FALSE) {
  new <- paste0(col, "_norm")
  df %>%
    group_by(District) %>%
    mutate("{new}" := {
      r <- percent_rank(.data[[col]])
      if (reverse) 1 - r else r
    }) %>%
    ungroup()
}

# Read one TIMS crash CSV, standardizing column names and keeping only
# the required columns (missing ones are created as NA so binds align).
read_tims_file <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- toupper(gsub("\\s+", "_", trimws(names(df))))
  
  if ("SEVERITY" %in% names(df) && !"COLLISION_SEVERITY" %in% names(df))
    df$COLLISION_SEVERITY <- df$SEVERITY
  if ("ROUTE" %in% names(df) && !"STATE_ROUTE" %in% names(df))
    df$STATE_ROUTE <- df$ROUTE
  
  for (mc in setdiff(TIMS_COLS, names(df))) df[[mc]] <- NA
  df <- df[, TIMS_COLS]
  
  filter(df, !is.na(POINT_X), !is.na(POINT_Y))
}

# Summarize an APR point layer (culverts / bridges) into a per-corridor
# reverse-priority score. Returns columns District, CountyA, Route, <prefix>Score.
apr_point_score <- function(gdb, layer, route_field, prefix) {
  st_read(gdb, layer = layer, quiet = TRUE) %>%
    st_zm(drop = TRUE, what = "ZM") %>%
    st_transform(cfg$crs_ca) %>%
    st_join(district_poly["District"], join = st_within) %>%
    st_join(select(DCR_geom, CountyA, Route), join = st_intersects) %>%
    mutate(
      Route       = coalesce(Route, trimws(as.character(.data[[route_field]]))),
      RevPriority = replace_na(6 - as.numeric(FinalPriority), 0)
    ) %>%
    st_drop_geometry() %>%
    filter(!is.na(District),
           !is.na(CountyA), CountyA != "",
           !is.na(Route),   Route   != "") %>%
    group_by(District, CountyA, Route) %>%
    summarise("{prefix}Score" := sum(RevPriority, na.rm = TRUE),
              .groups = "drop")
}

# -----------------------------------------------------------------------------
# 1. State Highway System geometry & corridor (DCR) baseline
# -----------------------------------------------------------------------------
abbr <- read_xlsx(cfg$county_abbr)

SHS_raw <- st_read(cfg$shs, quiet = TRUE) %>%
  st_transform(cfg$crs_ca) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  mutate(
    AlignCode_cleaned = gsub(" Independent", "", AlignCode),
    miles_seg = as.numeric(st_length(.)) * cfg$m2mi,
    Route = as.character(Route),
    District = as.integer(District)
  ) %>%
  st_buffer(cfg$buff_miles * 1609.34)

district_poly <- st_read(cfg$district_bdy, quiet = TRUE) %>%
  { if ("DISTRICT" %in% names(.)) rename(., District = DISTRICT) else . } %>%
  mutate(District = as.integer(District)) %>%
  st_transform(cfg$crs_ca) %>%
  select(District, geometry)

# Collapse alignment segments -> single corridor geometry per County/Route.
DCR_geom <- SHS_raw %>%
  group_by(Route, County, AlignCode_cleaned, District) %>%
  summarise(miles = sum(miles_seg, na.rm = TRUE),
            geometry = st_union(geometry),
            .groups = "drop") %>%
  group_by(County, Route, District) %>%
  summarise(county_route_miles = mean(miles, na.rm = TRUE),
            geometry = st_union(geometry),
            .groups = "drop") %>%
  left_join(abbr, by = c("County" = "COUNTY_FULL")) %>%
  mutate(
    CountyA = coalesce(CountyA, County),
    route_miles_dcr = as.numeric(county_route_miles)
  ) %>%
  select(District, County, CountyA, Route, route_miles_dcr, geometry)

# Non-spatial corridor mileage table — the spine every indicator joins onto.
DCR_miles_tbl <- DCR_geom %>%
  st_drop_geometry() %>%
  group_by(District, CountyA, Route) %>%
  summarise(route_miles_dcr = sum(route_miles_dcr, na.rm = TRUE),
            .groups = "drop")

# -----------------------------------------------------------------------------
# 2. Safety indicator — fatal and severe crashes on the SHS (TIMS)
# -----------------------------------------------------------------------------
tims_files <- list.files(cfg$tims_folder, pattern = "(?i)\\.csv$",
                         full.names = TRUE)

TIMS_pts <- tims_files %>%
  map(read_tims_file) %>%
  compact() %>%
  bind_rows() %>%
  mutate(
    POINT_X = as.numeric(POINT_X),
    POINT_Y = as.numeric(POINT_Y),
    COLLISION_SEVERITY = as.integer(COLLISION_SEVERITY)
  ) %>%
  filter(COLLISION_SEVERITY %in% c(1, 2), STATE_HWY_IND == "Y") %>%
  st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = cfg$crs_wgs84) %>%
  st_transform(cfg$crs_ca)

CrashSHS_density <- TIMS_pts %>%
  st_join(district_poly["District"], join = st_within, left = FALSE) %>%
  left_join(abbr, by = c("COUNTY" = "COUNTY_FULL")) %>%
  mutate(
    CountyA = coalesce(CountyA, COUNTY),
    Route = as.character(STATE_ROUTE),
    District = as.integer(District)
  ) %>%
  st_drop_geometry() %>%
  count(District, CountyA, Route, name = "crash_count") %>%
  right_join(DCR_miles_tbl, by = KEYS) %>%
  mutate(
    crash_count = replace_na(crash_count, 0L),
    crash_dens_per_mile = if_else(route_miles_dcr > 0,
                                  crash_count / route_miles_dcr, NA_real_)
  ) %>%
  norm_by_district("crash_dens_per_mile") %>%
  select(all_of(KEYS), crash_dens_per_mile_norm) %>%
  rename(crash_density_norm = crash_dens_per_mile_norm)

# -----------------------------------------------------------------------------
# 3. Equity indicators (EQI): income, access, traffic proximity
# -----------------------------------------------------------------------------
# Each EQI polygon is clipped to the corridor buffer; population is allocated
# proportionally by the share of the polygon's area that falls in the corridor.
EQI_inter <- read_rds(cfg$eqi) %>%
  st_set_crs(cfg$crs_wgs84) %>%
  st_transform(cfg$crs_ca) %>%
  mutate(area1 = st_area(.)) %>%
  st_intersection(DCR_geom, .) %>%
  mutate(
    cov = as.numeric(st_area(.) / area1),
    new_pop = POP20 * cov
  ) %>%
  st_drop_geometry()

AccessToDestination <- EQI_inter %>%
  group_by(District, CountyA, Route) %>%
  summarise(
    ped_ratio = weighted.mean(PED_RATIO, new_pop, na.rm = TRUE),
    bike_ratio = weighted.mean(BIKE_RATIO, new_pop, na.rm = TRUE),
    transit_ratio = weighted.mean(TRANSIT_RATIO_POIs, new_pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(District) %>%
  mutate(
    ped_ratio_norm = 1 - percent_rank(ped_ratio),
    bike_ratio_norm = 1 - percent_rank(bike_ratio),
    transit_ratio_norm = 1 - percent_rank(transit_ratio)
  ) %>%
  ungroup() %>%
  select(all_of(KEYS),
         ped_ratio, bike_ratio, transit_ratio,
         ped_ratio_norm, bike_ratio_norm, transit_ratio_norm)

LowIncome <- EQI_inter %>%
  group_by(District, CountyA, Route, DEMOGRAPHIC_OVERLAY_SCREEN) %>%
  summarise(tot_pop_est = sum(new_pop, na.rm = TRUE), .groups = "drop") %>%
  group_by(District, CountyA, Route) %>%
  summarise(
    tot_pop = sum(tot_pop_est, na.rm = TRUE),
    li_pop = sum(tot_pop_est[DEMOGRAPHIC_OVERLAY_SCREEN == "Yes"],
                  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_li = if_else(tot_pop > 0, li_pop / tot_pop, 0)) %>%
  mutate(pct_li = replace_na(pct_li, 0)) %>%
  group_by(District) %>%
  mutate(pct_li = percent_rank(pct_li)) %>%
  ungroup() %>%
  select(all_of(KEYS), pct_li)

TrafficProxV <- EQI_inter %>%
  mutate(
    weighted_aadt_score = replace_na(weighted_aadt_score, 0),
    new_pop = replace_na(new_pop, 0)
  ) %>%
  group_by(District, CountyA, Route) %>%
  summarise(
    traffic_prox_score = weighted.mean(weighted_aadt_score, new_pop,
                                       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  norm_by_district("traffic_prox_score") %>%
  select(all_of(KEYS), traffic_prox_score_norm) %>%
  rename(traffic_prox_norm = traffic_prox_score_norm)

# -----------------------------------------------------------------------------
# 4. Healthy Places Index (HPI), area-weighted by tract population
# -----------------------------------------------------------------------------
hpi_raw <- read.csv(cfg$hpi) %>%
  mutate(GEO_ID = as.numeric(GEO_ID), pop = as.numeric(pop))

tracts_sf <- tracts(state = cfg$hpi_state_fips, year = cfg$hpi_year) %>%
  mutate(GEO_ID = as.numeric(GEOID10)) %>%
  st_transform(cfg$crs_ca) %>%
  mutate(area1 = st_area(.)) %>%
  left_join(hpi_raw, by = "GEO_ID")

HPI_inter <- st_intersection(DCR_geom, tracts_sf) %>%
  mutate(pop_est = pop * as.numeric(st_area(.) / area1)) %>%
  st_drop_geometry() %>%
  group_by(District, CountyA, Route) %>%
  summarise(avg_hpi = weighted.mean(hpi_pctile, pop_est, na.rm = TRUE),
            .groups = "drop") %>%
  norm_by_district("avg_hpi", reverse = TRUE) %>%
  select(all_of(KEYS), avg_hpi_norm) %>%
  rename(hpi_reverse_norm = avg_hpi_norm)

# -----------------------------------------------------------------------------
# 5. Climate risk — Adaptation Priority Reports (APR)
# -----------------------------------------------------------------------------
# Roads: length-weighted reverse priority within each corridor.
APR_RoadsDcr <- st_read(cfg$apr_gdb, layer = cfg$apr_roads, quiet = TRUE) %>%
  st_transform(cfg$crs_ca) %>%
  mutate(
    CountyStd = as.character(CountyName),
    Route = as.character(Route),
    RevPriority = replace_na(6 - as.numeric(FinalPriority), 0)
  ) %>%
  select(CountyStd, Route, RevPriority) %>%
  st_intersection(select(district_poly, District)) %>%
  mutate(LenM = as.numeric(st_length(st_geometry(.)))) %>%
  st_drop_geometry() %>%
  filter(!is.na(District),
         !is.na(CountyStd), CountyStd != "",
         !is.na(Route), Route != "") %>%
  group_by(District, CountyStd, Route) %>%
  summarise(
    TotalLenM = sum(LenM, na.rm = TRUE),
    RawPrioritySum = sum(LenM * RevPriority, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(WeightedPriority = if_else(TotalLenM > 0,
                                    RawPrioritySum / TotalLenM, NA_real_)) %>%
  mutate(CountyStd = toupper(CountyStd)) %>%
  left_join(abbr %>% transmute(CountyStd = toupper(COUNTY_FULL), CountyA),
            by = "CountyStd") %>%
  select(District, CountyA, Route, WeightedPriority)

# Point assets: small / large culverts and bridges.
APR_CrDr <- DCR_miles_tbl %>%
  left_join(APR_RoadsDcr, by = KEYS) %>%
  left_join(apr_point_score(cfg$apr_gdb, "Small_Culverts_APR",
                            "SysRoute", "SmallCulverts_"), by = KEYS) %>%
  left_join(apr_point_score(cfg$apr_gdb, "Large_Culverts_APR",
                            "RTE", "LargeCulverts_"), by = KEYS) %>%
  left_join(apr_point_score(cfg$apr_gdb, "Bridges_APR",
                            "RTE", "Bridges_"), by = KEYS) %>%
  mutate(
    WeightedPriority = replace_na(as.numeric(WeightedPriority), 0),
    SmallCulverts_Score = replace_na(SmallCulverts_Score, 0),
    LargeCulverts_Score = replace_na(LargeCulverts_Score, 0),
    Bridges_Score = replace_na(Bridges_Score, 0),
    
    APR_EqualWeightRaw = (WeightedPriority +
                            rescale_1_to_5(SmallCulverts_Score) +
                            rescale_1_to_5(LargeCulverts_Score) +
                            rescale_1_to_5(Bridges_Score)) / 4,
    APR_EqualWeight = rescale_1_to_5(APR_EqualWeightRaw)
  ) %>%
  group_by(District, CountyA, Route) %>%
  summarise(APR_EqualWeight = max(APR_EqualWeight, na.rm = TRUE),
            .groups = "drop") %>%
  norm_by_district("APR_EqualWeight") %>%
  select(all_of(KEYS), APR_EqualWeight_norm)

# -----------------------------------------------------------------------------
# 6. Mobility — delay from Mobility Performance Reports
# -----------------------------------------------------------------------------
MPR <- read.csv(cfg$mpr) %>%
  rename(CountyA = County) %>%
  mutate(Route = str_extract(Unique_ID, "\\d+")) %>%
  group_by(District, CountyA, Route) %>%
  summarise(Delay = sum(Delay, na.rm = TRUE), .groups = "drop") %>%
  select(all_of(KEYS), Delay)

# -----------------------------------------------------------------------------
# 7. Freight — specialized truck corridor hex grid
# -----------------------------------------------------------------------------
hex <- st_read(cfg$hex_gdb, layer = cfg$hex_layer, quiet = TRUE) %>%
  st_transform(cfg$crs_ca) %>%
  select(Conn_WS, Network_WS)

Freight_dcr <- DCR_geom %>%
  select(District, CountyA, Route, route_miles_dcr) %>%
  st_intersection(hex) %>%
  mutate(
    SegMiles = as.numeric(route_miles_dcr),
    ConnNum = Conn_WS * SegMiles,
    NetNum = Network_WS * SegMiles
  ) %>%
  st_drop_geometry() %>%
  group_by(District, CountyA, Route) %>%
  summarise(ConnNum = sum(ConnNum, na.rm = TRUE),
            NetNum  = sum(NetNum,  na.rm = TRUE),
            .groups = "drop") %>%
  left_join(DCR_miles_tbl, by = KEYS) %>%
  mutate(
    connws = if_else(route_miles_dcr > 0, ConnNum / route_miles_dcr,
                        NA_real_),
    networkws = if_else(route_miles_dcr > 0, NetNum / route_miles_dcr,
                        NA_real_)
  ) %>%
  group_by(District) %>%
  mutate(
    connws_norm = percent_rank(connws),
    networkws_norm = percent_rank(networkws)
  ) %>%
  ungroup() %>%
  select(all_of(KEYS), connws_norm, networkws_norm)

# -----------------------------------------------------------------------------
# 8. Assemble final corridor table
# -----------------------------------------------------------------------------
postmile <- SHS_raw %>%
  st_drop_geometry() %>%
  group_by(District, County, Route) %>%
  summarise(min_bPM = min(bPM, na.rm = TRUE),
            max_ePM = max(ePM, na.rm = TRUE),
            .groups = "drop") %>%
  rename(CountyA = County)

Final_tbl <- DCR_miles_tbl %>%
  left_join(AccessToDestination, by = KEYS) %>%
  left_join(LowIncome, by = KEYS) %>%
  left_join(TrafficProxV, by = KEYS) %>%
  left_join(HPI_inter, by = KEYS) %>%
  left_join(CrashSHS_density, by = KEYS) %>%
  left_join(APR_CrDr, by = KEYS) %>%
  left_join(Freight_dcr, by = KEYS) %>%
  left_join(MPR, by = KEYS) %>%
  mutate(Delay = replace_na(Delay, 0)) %>%
  group_by(District) %>%
  mutate(delay_norm = percent_rank(Delay)) %>%
  ungroup() %>%
  select(-Delay) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  mutate(
    Unique_ID = paste0(CountyA, "_", Route),
    .after = Route
  ) %>%
  mutate(across(where(is.character), ~ replace_na(.x, ""))) %>%
  left_join(postmile, by = KEYS)

# -----------------------------------------------------------------------------
# 9. Write outputs (one CSV per District + combined statewide)
# -----------------------------------------------------------------------------
if (!dir.exists(cfg$out_folder)) dir.create(cfg$out_folder, recursive = TRUE)

Final_tbl %>%
  group_by(District) %>%
  group_walk(~ write_csv(
    .x,
    file.path(cfg$out_folder,
              paste0("corridor_district_", .y$District, ".csv"))
  ))

write_csv(Final_tbl, cfg$out_final)
