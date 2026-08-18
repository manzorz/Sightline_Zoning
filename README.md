# Clark County Zoning & Housing Capacity Model

A reproducible spatial data science pipeline built in R to evaluate **net realizable housing headroom** across Clark County, Washington. This model moves beyond theoretical paper density to evaluate real-world buildable limits at the individual tax parcel scale.

---

## Model Architecture & Core Logic

The pipeline processes property data through a multi-tiered **Spatial, Volumetric, and Jurisdictional Reduction Model**:

[ Total Parcel Surface Area ]
              │
              ▼  (Subtract Environmental and Sovereign Constraints)
[ Net Developable Footprint ]
              │
              ▼  (Apply Dimensional Yard Setbacks and Buffers)
[ Buildable Lot Footprint ]
              │
              ▼  (Multiply by Floor-Area Ratios & Story Heights)
[ Three-Dimensional Building Envelope ]
              │
              ▼  (Enforce Unit Sizes and Density Limits via pmin)
[ Net Realizable Housing Capacity Headroom ]


---

## Data Portfolio & Layer Classifications

The model ingests 14 distinct regional data layers and processes them based on their legal planning severity:

### 1. Absolute Spatial Prohibitions (Footprint drops to 0)
* **`TaxlotsPublic.shp`**: Base property tax parcel bounds.
* **`Zoning.shp`**: Local municipal regulatory frameworks.
* **`Cemetery.shp`**: Legally protected interred grounds.
* **`WetInv.shp`**: High-resolution Washington Ecology vector wetlands.
* **`Habitat.shp`**: Core fish and wildlife conservation shapes.
* **`HydPoly.shp`**: Regional open water bodies and rivers.
* **`Mines.shp`**: Active surface mining operations.
* **`Slopes.shp`** *(Filtered)*: Extreme topography **exceeding 40% slope**.

### 2. Sovereign Cutouts (Jurisdictional Exclusions)
* **`TribalLands.shp`**: Native American sovereign lands. This layer removes parcels from municipal housing capacity metrics since local city/county codes do not apply.

### 3. Mitigable Engineering Constraints (Preserves Land, Injects Financial Premiums)
* **`Slopes.shp`** *(Filtered)*: Mild rolling gradients.
  * **15–25% Slope**: Injects a placeholder premium of **+$15/sqft** for basic terracing.
  * **25–40% Slope**: Injects a placeholder premium of **+$35/sqft** for structural piers/stilts.
* **`ErosionHazard.shp`**: Fragile soils requiring shoring (**+$12/sqft**).
* **`Liquefaction.shp`**: Earthquake-fluid soils requiring deep foundational pilings (**+$25/sqft**).
* **`Aquifer.shp`**: Critical recharges requiring vault filtration networks (**+$8/sqft**).
* **`WildlandUrbanInterfaceProposed.shp`**: Wildfire hazard zones requiring structural hardening (**+$15/sqft**).

---

## Repository Outputs & Visualizations

The script processes vector data frames simultaneously and exports high-resolution metrics directly to your output drive:

### Map 1: Growth Potential Baseline
Maps macro zoning shapes based on whether they contain any remaining property-level capacity.
![Zoning Limitations Categorical](graphics/Zoning_Limitations_Categorical.png)

### Map 2: Hazard-Adjusted Headroom Gradient
Tracks net realizable new home capacity lot-by-lot. Areas hitting regulatory ceilings or physical restrictions fade to grey, while active development nodes scale from yellow to intense red.
![Zoning Headroom Gradient](graphics/Zoning_Headroom_Gradient.png)

### Map 3: Housing Footprint Matrix
Splits the county landscape into a binary view of legal development allowances. It highlights where housing can exist versus where it is barred by policy.
![Zoning Residential Footprint Matrix](graphics/Zoning_Residential_Footprint_Matrix.png)

### Map 4: Commercial & Mixed-Use Expansion Audit
Visualizes the expanded capacity unlocked across downtown, village, and town center codes. It captures hidden vertical multi-family capacity that standard models miss.
![Zoning MixedUse Housing Additions](graphics/Zoning_MixedUse_Housing_Additions.png)

---

## Tabular Metrics & Performance

The pipeline generates two production `.csv` tables detailing spatial constraints and housing targets across all urban growth boundaries (UGB):

1. **`City_UGB_Unified_Housing_Capacity_Report.csv`**
   * Tracks total viable new housing units per city.
   * Quantifies exact unit capacity lost to environmental limits.
2. **`City_UGB_Housing_Loss_Constraint_Attibility.csv`**
   * Disaggregates capacity losses by constraint type.
   * Tracks losses from wetlands, slopes, and cemeteries.

---

## Dependencies & Installation

To run this model workspace, open your R console and install the required processing packages:

```R
install.packages(c("sf", "dplyr", "ggplot2", "viridis", "terra", "exactextractr"))
```

### Execution Instructions:
1. Open your background workspace session.
2. Clear active environments using `rm(list = ls())`.
3. Update file directories in **Section 1**.
4. Execute the script to run calculations and export assets.