"""Read R-written Zarr V2 and V3 stores and report results as JSON.

Called by cross-validate.R with two arguments:
  1. working directory (contains r_v2.zarr and r_v3.zarr)
  2. comma-separated list of array names to read

Prints a JSON array of result objects to stdout.
"""

import json
import os
import sys

import numpy as np
import zarr

work_dir = sys.argv[1]
array_names = sys.argv[2].split(",")
os.chdir(work_dir)

results = []

for fmt in [2, 3]:
    path = f"r_v{fmt}.zarr"
    if fmt == 2:
        root = zarr.open_group(path, mode="r", zarr_format=2)
    else:
        root = zarr.open_group(path, mode="r", zarr_format=3)

    for name in array_names:
        try:
            arr = root[name]
            data = arr[:].flatten(order="C").tolist()
            results.append({"name": name, "format": fmt,
                            "data": data, "shape": list(arr.shape), "ok": True})
        except Exception as e:
            results.append({"name": name, "format": fmt,
                            "data": [], "shape": [], "ok": False, "error": str(e)})

    # meta_group attrs
    try:
        sub = root["meta_group"]
        results.append({"name": "meta_group/attrs", "format": fmt,
                        "data": dict(sub.attrs), "shape": [], "ok": True})
    except Exception as e:
        results.append({"name": "meta_group/attrs", "format": fmt,
                        "data": {}, "shape": [], "ok": False, "error": str(e)})

    # meta_group/temperature data
    try:
        sub = root["meta_group"]
        t = sub["temperature"]
        results.append({"name": "meta_group/temperature", "format": fmt,
                        "data": t[:].flatten(order="C").tolist(),
                        "shape": list(t.shape), "ok": True})
    except Exception as e:
        results.append({"name": "meta_group/temperature", "format": fmt,
                        "data": [], "shape": [], "ok": False, "error": str(e)})

    # meta_group/temperature attrs
    try:
        sub = root["meta_group"]
        t = sub["temperature"]
        results.append({"name": "meta_group/temperature/attrs", "format": fmt,
                        "data": dict(t.attrs), "shape": [], "ok": True})
    except Exception as e:
        results.append({"name": "meta_group/temperature/attrs", "format": fmt,
                        "data": {}, "shape": [], "ok": False, "error": str(e)})

print(json.dumps(results))
