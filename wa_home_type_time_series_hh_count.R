library(data.table)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Load & Convert to data.table
# ------------------------------------------------------------------------------
acs <- fread("C:/Users/gmann/Downloads/usa_00011.csv/usa_00011.csv")

# ------------------------------------------------------------------------------
# 2. Filter & Clean Data
# ------------------------------------------------------------------------------
# Recode UNITSSTR into categories using fcase()
acs[, units_category := fcase(
  UNITSSTR == 3, "1-Unit Detached",
  UNITSSTR == 4, "1-Unit Attached",
  UNITSSTR == 5, "2 Units (Duplex)",
  UNITSSTR %in% c(6, 7), "3–9 Units",
  UNITSSTR %in% c(8, 9, 10), "10+ Units",
  UNITSSTR == 2, "Mobile Home / Trailer / Other",
  default = "Other / Missing"
)]

# Exclude missing/other records
acs <- acs[units_category != "Other / Missing"]

# Convert to factor with explicit ordering for the legend
levels_units <- c(
  "1-Unit Detached",
  "1-Unit Attached",
  "2 Units (Duplex)",
  "3–9 Units",
  "10+ Units",
  "Mobile Home / Trailer / Other"
)
acs[, units_category := factor(units_category, levels = levels_units)]

# Filter for Households (GQ == 1 or 2), WA (STATEFIP == 53), and Clark County (COUNTYFIP == 11 / "011")
acs_clark <- acs[GQ %in% c(1, 2) & 
                   STATEFIP %in% c(53, "53") & 
                   COUNTYFIP %in% c(11, "011", "11")]

acs_king <- acs[GQ %in% c(1, 2) & 
                   STATEFIP %in% c(53, "53") & 
                   COUNTYFIP %in% c(33, "033", "33")]

# ------------------------------------------------------------------------------
# 3. Aggregate Household Counts (Weighted by HHWT)
# ------------------------------------------------------------------------------
acs_summary <- acs_clark[, .(total_households = sum(HHWT, na.rm = TRUE)), 
                         by = .(YEAR, units_category)]

# ------------------------------------------------------------------------------
# 4. Generate ggplot Time-Series Line Graphic
# ------------------------------------------------------------------------------
ggplot(acs_summary, aes(x = YEAR, y = total_households, color = units_category, group = units_category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set1", name = "Structure Type") +
  # Explicitly label every individual year on the x-axis
  scale_x_continuous(
    breaks = seq(2010, 2024, by = 1)
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Household Growth by Building Structure Type",
    subtitle = "Clark County, WA (ACS 1-Year Estimates, 2010–2024)",
    x = "Year",
    y = "Estimated Total Households",
    caption = "Source: IPUMS USA ACS 1-Year Microdata (STATEFIP = 53, COUNTYFIP = 011)\nNote: 2020 ACS 1-year data omitted by US Census Bureau due to pandemic collection issues."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 6)),
    plot.subtitle = element_text(color = "grey30", size = 11, margin = margin(b = 12)),
    axis.text.x = element_text(angle = 45, hjust = 1), # Angle text for clean, un-cramped year labels
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey88")
  )

