# Corridor Prioritization Tool Data Prep

`corridor_prioritization_data_prep.R` is an R script that builds a per-District corridor scoring table for the
California State Highway System (SHS). Each row is a corridor — a unique
**District × County × Route** combination — annotated with normalized
indicators across safety, equity, climate risk/GHG, access, traffic burden, and freight. The
script writes one CSV per District plus a combined statewide CSV. These .CSV files are used as inputs for an Excel-based tool.

## Requirements

R with the following packages: `sf`, `dplyr`, `readr`, `readxl`, `tigris`,
`stringr`, `tidyr`, `scales`, `purrr`. The `tigris` step downloads census tract
geometry, so an internet connection is needed on first run.

## Setup

All paths and parameters are defined in the `cfg` list at the top of the script.
Before running, set `cfg$root` to the project folder and confirm the input
files exist under `Data/`:

- `State_Highway_Network_Lines.geojson` — SHS centerlines
- `CountyAbbr.xlsx` — county full-name X abbreviation crosswalk
- `Caltrans_Districts.geojson` — District boundary polygons
- `EQI.rds` — Caltrans Transportation Equity Index (EQI) data
- `hpi.csv` — Healthy Places Index (HPI) data
- `TIMS_CSVs/` — folder of TIMS county-level crash data .csv files
- `APR.gdb` — Adaptation Priority Reports geodatabase (roads, culverts, bridges)
- `SpecializedTruckCorridor_*.gdb` — freight network data
- `MobilityPerformanceReports.csv` — Mobility performance reports data

To get access to any of the raw data inputs listed above, please contact Henry.McKay@dot.ca.gov.

Outputs are written to `Outputs/`.

## How it works

The script runs as a sequential pipeline, organized into indicator-specific sections:

1. **SHS baseline.** Highway lines are reprojected to California Albers
   (EPSG:3310), buffered by half a mile, and dissolved into one geometry per
   District/County/Route. This produces `DCR_geom` (the corridor footprint) and
   `DCR_miles_tbl` — the non-spatial mileage table that every indicator joins
   onto.

2. **Safety.** TIMS crash CSVs are read, filtered
   to severe crashes (severity 1–2) on the SHS, converted to points, joined to
   Districts, and counted summed by corridor. The count is divided by corridor mileage
   to get crash density.

3. **Low-Income, Access to Destinations, and Traffic Exposure (EQI data).** EQI Census block polygons are intersected with the corridor buffer.
   Population is allocated proportionally by the share of each polygon's area
   that falls inside the corridor, then used to compute population-weighted
   access, low-income share, and traffic-proximity figures.

4. **Healthy Places Index (HPI).** Census tracts are joined to HPI scores, intersected with
   corridors, and combined into a population-weighted average HPI percentile per corridor.

5. **Climate risk factors (APR).** Road segments contribute a length-weighted reverse
   priority; small culverts, large culverts, and bridges each contribute a
   point-based score. The four components are rescaled and equally weighted into
   a single climate score.

6. **GHG.** Delay from the Mobility Performance Reports is used to percentile-rank the corrdiors with the greatest amount of delay, per the the top 20 bottlenecks of 2024 table in each District report.

7. **Freight.** Freight hex grid data from a Caltrans study is intersected with corridors to
   derive connectivity and network weight scores per corridor mile.

8. **Assembly.** All indicators are left-joined onto `DCR_miles_tbl`, numeric
   columns are rounded, a `Unique_ID` is added, and postmile range is attached.

9. **Output.** The final table is split by District into
   `Outputs/District_Data_Tables/corridor_district_<N>.csv` and also written in
   full to `Outputs/corridor_final_dcr.csv`.

## Normalization

Most indicators are normalized within each District using percent-rank, so
values are comparable across corridors in the same District. This is handled by
the `norm_by_district()` helper; its `reverse = TRUE` flag flips the rank so
that a "worse" condition (e.g. lower HPI) sorts high. Climate sub-scores use
`rescale_1_to_5()`, which maps positive values to a 1–5 range and collapses
zeros/NA to 1.

## Notes

- `setwd()` uses an absolute path from `cfg$root`; update it for your machine,
  or adapt to a project-relative approach for portability.
- The script assumes input schemas match the expected column names. TIMS files
  are the exception — column names are normalized and missing required columns
  are filled with `NA` so files with varying headers still align.
