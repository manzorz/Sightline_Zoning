# Clark County Zoning & Capacity Model

A reproducible spatial data science pipeline built in R to evaluate **Zoning-Constrained Housing Potential** and track **Net New Homes Allowed by Current Law** across Clark County, Washington. This model moves beyond theoretical paper density to evaluate real-world buildable limits at the individual tax parcel scale by combining 12 environmental vector layers with neighborhood-level IPUMS NHGIS demographic data.

---

## Data Sourcing & Citations

### 1. Spatial Boundary & Parcel Layers
All base geography shapes, tax lots, and planning bounds are sourced from the **Clark County Open Data Portal**:
* **Source Hub:** [Clark County Digital GIS Data Download](https://hub-clarkcountywa.opendata.arcgis.com/pages/digital-gis-data-download)

### 2. Demographic & Economic Baseline Data
Tabular tract-level socioeconomic metrics and historical shapefile reference frameworks are obtained via IPUMS NHGIS.

* **Academic Research & Publication Citation:**
  > Jonathan Schroeder, David Van Riper, Steven Manson, Grace Cooper, Zachary Krause, Tracy Kugler, Tsu Zhu, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 21.0 [dataset]. Minneapolis, MN: IPUMS. 2026. http://doi.org/10.18128/D050.V21.0

* **Policy Briefs, Online Media, & General Press Citation:**
  > IPUMS NHGIS, University of Minnesota, www.nhgis.org

---

## Model Architecture & Core Logic

The pipeline processes property data through a multi-tiered **Spatial, Volumetric, and Jurisdictional Reduction Model**:



<pre>
[ Total Parcel Surface Area ]
              │
              ▼
  (Subtract Environmental and Sovereign Constraints)
              │
              ▼
[ Net Developable Footprint ]
              │
              ▼
  (Apply Dimensional Yard Setbacks and Buffers)
              │
              ▼
[ Buildable Lot Footprint ]
              │
              ▼
  (Multiply by Floor-Area Ratios & Story Heights
   Categorized by Zone)
              │
              ▼
[ Three-Dimensional Building Envelope ]
              │
              ▼
  (Enforce Unit Sizes and Density Caps via pmin)
              │
              ▼
[ Net New Homes Allowed by Current Law ]
</pre>

---


### Reference Baselines & Remaining Potential Capacity Matrix
Below is the lot-level analysis output tracking remaining unit capacity across the county landscape:

![Zoning Capacity Gradient Map](image_TzczPt.png)

---

## Model Parameter Classifications & Sizing Assumptions

The underlying calculation engine rejects a "one-size-fits-all" framework. It adjusts physical structure capacities and height limits dynamically across **four clear zone tiers** based on the properties found in your zoning `.dbf` attribute tables:

| Zoning District Tier | Assumed Average Unit Size | Maximum Allowed Height | Maximum Story Cap | Built Product Type |
| :--- | :--- | :--- | :--- | :--- |
| **High Density / Urban Core** | **900 Sq Ft** | 60 to 75 Feet | **7 Stories** | Stacked flats / Podium mixed-use apartments |
| **Medium Density / Apartments** | **1,200 Sq Ft** | 45 Feet | **4 Stories** | Garden walk-ups, multiplexes, & townhomes |
| **Low Density / Single-Family** | **2,100 Sq Ft** | 35 Feet | **3 Stories** | Detached suburban residential layouts |
| **Rural / Resource Lands** | **2,400 Sq Ft** | 35 Feet | **2 Stories** | Sprawling rural estates & farmhouses |

### Additional Underlying Model Assumptions
To maintain a defensible rough assessment of zoning impacts without succumbing to diminishing computational returns, the script asserts the following baseline constraints:
* **Average Story Height:** Assumed at a fixed **11 feet** (shorthand for a standard 9-foot clear interior ceiling coupled with 2 feet of structural floor trusses, plumbing lines, and utility duct space).
* **Yard Setback Deductions:** Net buildable polygon acreage is calculated using a dynamic scaling factor based on front yard rules (`Front_Setback_Ft`). Lots with a $\ge$ 20 ft setback lose 30% of their gross footprint area (`0.70` reduction multiplier); 15 ft setbacks lose 25% area (`0.75`); and tight $\le$ 10 ft setbacks preserve 85% of buildable lot space (`0.85`).
* **Urban Core Density Exemptions:** In compliance with Clark County Unified Development Code (UDC) Title 40 incentive allowances, regulatory density caps (`UnitsPerAc`) are completely **waived (set to Infinity)** inside urban centers, village clusters, and mixed-use codes. The housing yield there is bounded strictly by the three-dimensional physical envelope of the building box.
* **Cemetery Protections:** Any property parcel intersecting a known cemetery polygon layout undergoes an absolute zero-out override, dropping both its net construction capacity and potential new homes metrics to zero.

---

## Data Portfolio & Layer Classifications

The model ingests 14 distinct regional data layers and processes them based on their legal planning severity and jurisdictional structures:

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
* **`TribalLands.shp`**: Native American sovereign lands. This layer removes parcels from local municipal housing capacity metrics since local city/county codes do not legally apply.

### 3. Mitigable Engineering Constraints (Preserves Land, Injects Financial Premiums)
* **`Slopes.shp`** *(Filtered)*: Mild rolling gradients.
  * **15–25% Slope**: Injects a placeholder premium of **+$15/sqft** for terraced foundations.
  * **25–40% Slope**: Injects a placeholder premium of **+$35/sqft** for deep structural pier anchors.
* **`ErosionHazard.shp`**: Fragile soils requiring advanced grading shoring (**+$12/sqft**).
* **`Liquefaction.shp`**: Earthquake-fluid soils requiring deep foundational structural pilings (**+$25/sqft**).
* **`Aquifer.shp`**: Critical recharges requiring underground vault stormwater filtration networks (**+$8/sqft**).
* **`WildlandUrbanInterfaceProposed.shp`**: Wildfire hazard zones requiring Class A fire-resistant building envelope hardening (**+$15/sqft**).

---

## Integrated IPUMS NHGIS Demographic Indicators

To transition from legal capacity to economic demand forecasting, the pipeline integrates 5-Year ACS Estimates at the Census Tract level using an instantaneous vector text `left_join` on matched `GISJOIN` keys:

* **Purchasing Power Tracking:** Integrates median household incomes separated by owner and renter tenure status to evaluate neighborhood economic absorption limits.
* **Family Formation Trajectory:** Monitors the ratio of married couples and single parents raising young children under 18 to align zoning targets with actual housing product type needs.
* **Infill大 Absorption Score:** Measures the baseline neighborhood concentration of residents currently occupying multi-family configurations (2+ unit structures) to identify high-density alignment hotspots.
* **Generational Home Equity Proxy:** Tracks median owner-occupied property values against the structural age of the housing stock to proxy localized generational down-payment equity reserves.

---

## Repository Outputs & Visualizations

The script processes vector data frames simultaneously and exports high-resolution metrics directly to your output drive:

* **Map 1 (`Zoning_Limitations_Categorical.png`)**: Maps macro zoning shapes based on whether they contain any remaining property-level capacity left to expand.
* **Map 2 (`Zoning_Headroom_Gradient.png`)**: Tracks net realizable new home capacity lot-by-lot, scaling from light yellow up to intense red.
* **Map 3 (`Zoning_Residential_Footprint_Matrix.png`)**: Splits the county landscape into a high-contrast binary view of legal development allowances (Violet vs. Periwinkle).
* **Map 4 (`Zoning_MixedUse_Vancouver_Zoom.png`)**: Zoomed perspective isolating urban core parcel adjustments unlocked by capturing vertical multi-family allowances in commercial hubs.
* **Map 5 (`Zoning_MixedUse_Housing_Additions.png`)**: County-wide view showing downtown, village, and town center village codes that legally permit vertical multi-family housing.

---

## Tabular Metrics & Performance

The pipeline generates two production `.csv` tables detailing spatial constraints and housing targets across all urban growth boundaries (UGB):

1. **`City_UGB_Unified_Housing_Capacity_Report.csv`**
   * Tracks total viable new housing units per city.
   * Quantifies exact unit capacity lost to environmental limits.
2. **`City_UGB_Housing_Loss_Constraint_Attribution.csv`**
   * Disaggregates capacity losses by constraint type.
   * Tracks losses from vector wetlands, steep slopes, and cemeteries.

---

## Dependencies & Installation

To run this model workspace, open your R console and install the required processing packages:

```R
install.packages(c("sf", "dplyr", "ggplot2", "viridis", "terra", "exactextractr"))
```

### Execution Instructions:
1. Open your background workspace session.
2. Clear active environments using `rm(list = ls())`.
3. Update file directories in **`main_controller.R`**.
4. Execute the script to run calculations, display live graphics in your RStudio 
