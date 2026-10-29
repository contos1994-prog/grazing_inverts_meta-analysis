"""Assign a RESOLVE Ecoregions 2017 biome (Dinerstein et al. 2017, BioScience 67:534)
to every study site in MetaAnalysis_data_v2.xlsx.

Each unique Study_ID x (lat, lon) is queried as a point against the public ArcGIS
feature service below. Points that fall just outside a polygon (coastal sites,
rounding) are matched to the nearest ecoregion within 25 km. Studies without
coordinates in the data get approximate coordinates from their site description
(MANUAL_COORDS); these are flagged in coord_source.

Writes data/study_biomes.csv. Run: python3 scripts/01_extract_biomes.py
(needs openpyxl and network access; the R analyses only read the CSV).
"""
import csv, json, time, urllib.parse, urllib.request
import openpyxl

SERVICE = ("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/"
           "Resolve_Ecoregions/FeatureServer/0/query")
OUT = "data/study_biomes.csv"

# approximate site coordinates for studies with none in the data (lat, lon, note)
MANUAL_COORDS = {
    "#1796": (52.35, 0.30, "approximate: tall-herb fen, England, exact site not given (East Anglian fens assumed)"),
    "#2260": (57.0, -4.0, "approximate: centroid of 8 Scottish native pinewoods"),
    "#1825": (60.0, 16.0, "approximate: 16 stands across three latitudes in Sweden; mid-latitude point used, ambiguous"),
    "#1349": (41.88, -89.34, "approximate: Nachusa Grasslands, Illinois"),
    "#1589": (31.59, -110.51, "approximate: Appleton-Whittell Research Ranch, Arizona"),
    "#3610": (31.59, -110.51, "approximate: Appleton-Whittell Research Ranch, Arizona"),
    "#1048": (39.13, -84.25, "approximate: Cincinnati Nature Center, Ohio"),
    "#2091": (39.20, -76.88, "approximate: Middle Patuxent Environmental Area, Maryland"),
    "#1278": (29.58, 101.88, "approximate: Gongga Mountain alpine station, Sichuan"),
    "#2189": (43.85, -102.25, "approximate: Badlands National Park North Unit, South Dakota"),
    "#1609": (40.0, -3.7, "approximate: Spain, site not given (central Spain assumed), ambiguous"),
    "#1503": (53.07, -4.05, "approximate: Llyn Llydaw, Snowdonia"),
    "#2386": (37.1, -3.0, "approximate: SE Spain montane (Sierra Nevada assumed)"),
}


_cache = {}


def query(lat, lon, distance=None):
    # the service allows 60 queries per minute: pace requests, retry on HTTP 429
    key = (lat, lon, distance)
    if key in _cache:
        return _cache[key]
    p = {"geometry": f"{lon},{lat}", "geometryType": "esriGeometryPoint", "inSR": 4326,
         "spatialRel": "esriSpatialRelIntersects", "returnGeometry": "false",
         "outFields": "BIOME_NAME,BIOME_NUM,ECO_NAME,REALM", "f": "json"}
    if distance:
        p.update(distance=distance, units="esriSRUnit_Meter")
    url = SERVICE + "?" + urllib.parse.urlencode(p)
    for attempt in range(6):
        try:
            time.sleep(1.1)
            with urllib.request.urlopen(url, timeout=60) as r:
                res = json.load(r)
        except Exception:
            time.sleep(2 ** (attempt + 1))
            continue
        if "error" in res:
            time.sleep(61)
            continue
        _cache[key] = [f["attributes"] for f in res.get("features", [])]
        return _cache[key]
    raise RuntimeError(f"query failed for {lat},{lon}")


def classify(lat, lon):
    hits = [a for a in query(lat, lon) if a["BIOME_NAME"] not in (None, "N/A")]
    if hits:
        return hits[0], "point in polygon"
    for d in (5000, 25000):
        hits = [a for a in query(lat, lon, d) if a["BIOME_NAME"] not in (None, "N/A")]
        if hits:
            names = sorted({a["BIOME_NAME"] for a in hits})
            note = f"nearest within {d // 1000} km" + ("" if len(names) == 1 else f"; candidates: {' | '.join(names)}")
            return hits[0], note
    return {"BIOME_NAME": None, "BIOME_NUM": None, "ECO_NAME": None, "REALM": None}, "no match within 25 km"


wb = openpyxl.load_workbook("MetaAnalysis_data_v2.xlsx", read_only=True, data_only=True)
rows = list(wb["Invertebrates"].iter_rows(values_only=True))
H = rows[0]
ix = {h: i for i, h in enumerate(H) if h}
sites = {}
for r in rows[1:]:
    if not r[0]:
        continue
    sid = r[0].strip()
    lat, lon = r[ix["Latitude_decimal"]], r[ix["Longitude_decimal"]]
    if isinstance(lat, (int, float)) and isinstance(lon, (int, float)):
        sites.setdefault((sid, float(lat), float(lon)), ("data", r[ix["Biomes"]]))
    elif sid in MANUAL_COORDS:
        la, lo, note = MANUAL_COORDS[sid]
        sites.setdefault((sid, None, None), ("manual", r[ix["Biomes"]], la, lo, note))
    else:
        sites.setdefault((sid, None, None), ("none", r[ix["Biomes"]]))

out = []
for (sid, lat, lon), info in sorted(sites.items(), key=lambda x: x[0][0]):
    src, biome_orig = info[0], info[1]
    qlat, qlon, note = lat, lon, ""
    if src == "manual":
        qlat, qlon, note = info[2], info[3], info[4]
    if qlat is None:
        a, how = {"BIOME_NAME": None, "BIOME_NUM": None, "ECO_NAME": None, "REALM": None}, "no coordinates"
    else:
        a, how = classify(qlat, qlon)
    out.append({"Study_ID": sid, "Latitude_decimal": lat, "Longitude_decimal": lon,
                "query_lat": qlat, "query_lon": qlon, "coord_source": src, "coord_note": note,
                "match": how, "BIOME_NAME": a["BIOME_NAME"], "BIOME_NUM": a["BIOME_NUM"],
                "ECO_NAME": a["ECO_NAME"], "REALM": a["REALM"], "Biomes_as_entered": biome_orig})
    print(sid, qlat, qlon, "->", a["BIOME_NAME"], "|", a["ECO_NAME"], "|", how)

with open(OUT, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(out[0]))
    w.writeheader()
    w.writerows(out)
print(f"wrote {OUT} ({len(out)} study-sites)")
