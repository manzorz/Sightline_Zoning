import re
import requests
import geopandas as gpd
import pandas as pd

# The ID extracted directly from your Experience Builder URL
APP_ID = "92de7af280a74da9bd2daadde61c49cf"
CONFIG_URL = f"https://experience.arcgis.com/builder/api/apps/{APP_ID}/config"

print(f"Fetching Experience Builder configuration for App ID: {APP_ID}...")
res = requests.get(CONFIG_URL)

if res.status_code != 200:
    # Alternate public item data endpoint
    CONFIG_URL = f"https://www.arcgis.com/sharing/rest/content/items/{APP_ID}/data?f=json"
    res = requests.get(CONFIG_URL)

app_config = res.text

# Extract all FeatureServer or MapServer layer URLs embedded in the app config
pattern = r'https?://[^\s"\'\\]+?/(?:FeatureServer|MapServer)(?:/\d+)?'
raw_urls = re.findall(pattern, app_config)

# Clean and deduplicate endpoints
endpoints = sorted(list(set(raw_urls)))

print(f"Found {len(endpoints)} map layer service endpoints embedded in the app.\n")

all_gdfs = []

for idx, url in enumerate(endpoints):
    # Ensure URL targets a specific sub-layer (append /0 if base server)
    if not re.search(r'/\d+$', url):
        query_url = f"{url}/0/query?where=1%3D1&outFields=*&f=geojson"
    else:
        query_url = f"{url}/query?where=1%3D1&outFields=*&f=geojson"

    print(f"[{idx+1}/{len(endpoints)}] Pulling data from: {url}")
    
    try:
        gdf = gpd.read_file(query_url)
        if not gdf.empty:
            gdf["source_endpoint"] = url
            all_gdfs.append(gdf)
            print(f"   -> Success: {len(gdf)} features retrieved.")
        else:
            print("   -> Empty layer skipped.")
    except Exception as e:
        print(f"   -> Failed to query endpoint: {e}")

# Stitch all vector layers together
if all_gdfs:
    print("\nStitching layers into unified master file...")
    master_gdf = pd.concat(all_gdfs, ignore_index=True)
    output_filename = "waza_zoning_extracted.gpkg"
    master_gdf.to_file(output_filename, driver="GPKG")
    print(f"🎉 Complete! Saved {len(master_gdf)} total features to '{output_filename}'.")
else:
    print("\nNo feature data could be downloaded directly. The layers may require token authentication.")
