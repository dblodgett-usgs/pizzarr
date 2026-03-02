"""
Convert bcsd.zarr (V2) to bcsd_v3.zarr (V3).

Unzips the V2 bcsd.zarr.zip, reads each array and group,
and writes an equivalent Zarr V3 store using zarr-python >= 3.x.

Usage:
  pip install "zarr>=3" numpy numcodecs
  python convert-bcsd-v3.py

Outputs bcsd_v3.zarr.zip in the same directory as this script.
"""

import os
import shutil
import tempfile
import zipfile

import numpy as np
import zarr

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
V2_ZIP = os.path.join(SCRIPT_DIR, "bcsd.zarr.zip")
V3_ZIP = os.path.join(SCRIPT_DIR, "bcsd_v3.zarr.zip")

# Clean up any previous output
if os.path.exists(V3_ZIP):
    os.remove(V3_ZIP)

# Work entirely in a temp directory to avoid Windows permission issues
tmpdir = tempfile.mkdtemp()
v2_extract = os.path.join(tmpdir, "v2")
v3_dir = os.path.join(tmpdir, "bcsd_v3.zarr")

# Unzip V2 store
with zipfile.ZipFile(V2_ZIP, "r") as zf:
    zf.extractall(v2_extract)

# The zip contains files under bcsd.zarr/
v2_path = os.path.join(v2_extract, "bcsd.zarr")
print(f"V2 store extracted to: {v2_path}")

v2_root = zarr.open_group(v2_path, mode="r", zarr_format=2)

# Create V3 output store in temp directory
v3_root = zarr.open_group(v3_dir, mode="w", zarr_format=3)

# Copy root-level attributes (use update_attributes for single write)
root_attrs = dict(v2_root.attrs)
v3_root = v3_root.update_attributes(root_attrs)
print(f"Root attributes: {len(root_attrs)} keys copied")

# Copy each array
for name in v2_root.array_keys():
    v2_arr = v2_root[name]
    data = v2_arr[:]

    print(f"  Array '{name}': shape={v2_arr.shape}, dtype={v2_arr.dtype}, "
          f"chunks={v2_arr.chunks}")

    # Create V3 array then write data separately
    v3_arr = v3_root.create_array(
        name,
        shape=v2_arr.shape,
        chunks=v2_arr.chunks,
        dtype=v2_arr.dtype,
        fill_value=v2_arr.fill_value,
    )
    v3_arr[:] = data

    # Copy array attributes
    arr_attrs = dict(v2_arr.attrs)
    if arr_attrs:
        v3_arr.update_attributes(arr_attrs)
        print(f"    Attributes: {list(arr_attrs.keys())}")

print(f"\nV3 store written to: {v3_dir}")

# Verify by reading back
v3_check = zarr.open_group(v3_dir, mode="r")
for name in v3_check.array_keys():
    arr = v3_check[name]
    print(f"  Verify '{name}': shape={arr.shape}, dtype={arr.dtype}")

# Zip it up directly to the final location
print(f"\nCreating {V3_ZIP}...")
with zipfile.ZipFile(V3_ZIP, "w", zipfile.ZIP_DEFLATED) as zf:
    for root, dirs, files in os.walk(v3_dir):
        for f in files:
            full_path = os.path.join(root, f)
            arc_name = os.path.relpath(full_path, os.path.dirname(v3_dir))
            zf.write(full_path, arc_name)

print("Done!")

# Clean up temp directory
shutil.rmtree(tmpdir)
print("Cleaned up temp directories")
