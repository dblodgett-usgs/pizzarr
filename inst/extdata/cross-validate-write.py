"""Write Zarr V2 and V3 stores for cross-validation with pizzarr.

Called by cross-validate.R with one argument: the working directory.
Creates python_v2.zarr and python_v3.zarr in that directory.
"""

import os
import sys

import numpy as np
import zarr
import numcodecs
from zarr.codecs import GzipCodec, ZstdCodec

work_dir = sys.argv[1]
os.chdir(work_dir)

# ---- V2 ----
root2 = zarr.open_group("python_v2.zarr", mode="w", zarr_format=2)

# ---- V3 ----
root3 = zarr.open_group("python_v3.zarr", mode="w", zarr_format=3)

# ---- Test arrays ----
# Each array is written identically to both V2 and V3 stores.
# The data, dtypes, shapes, and chunks must match the R side in cross-validate.R.

# int32_1d_gzip
root2.create_array("int32_1d_gzip", shape=(6,), chunks=(3,), dtype="int32",
                   fill_value=0, compressor=numcodecs.GZip())
root2["int32_1d_gzip"][:] = [10, 20, 30, 40, 50, 60]

root3.create_array("int32_1d_gzip", shape=(6,), chunks=(3,), dtype="int32",
                   fill_value=0, compressors=(GzipCodec(),), filters=None)
root3["int32_1d_gzip"][:] = [10, 20, 30, 40, 50, 60]

# float64_1d_zstd
root2.create_array("float64_1d_zstd", shape=(5,), chunks=(5,), dtype="float64",
                   fill_value=0, compressor=numcodecs.Zstd())
root2["float64_1d_zstd"][:] = [1.1, 2.2, 3.3, 4.4, 5.5]

root3.create_array("float64_1d_zstd", shape=(5,), chunks=(5,), dtype="float64",
                   fill_value=0, compressors=(ZstdCodec(),), filters=None)
root3["float64_1d_zstd"][:] = [1.1, 2.2, 3.3, 4.4, 5.5]

# bool_1d_none
root2.create_array("bool_1d_none", shape=(4,), chunks=(4,), dtype="bool",
                   fill_value=False, compressor=None)
root2["bool_1d_none"][:] = [True, False, True, False]

root3.create_array("bool_1d_none", shape=(4,), chunks=(4,), dtype="bool",
                   fill_value=False, compressors=None, filters=None)
root3["bool_1d_none"][:] = [True, False, True, False]

# uint8_1d_none
root2.create_array("uint8_1d_none", shape=(4,), chunks=(2,), dtype="uint8",
                   fill_value=0, compressor=None)
root2["uint8_1d_none"][:] = [0, 127, 128, 255]

root3.create_array("uint8_1d_none", shape=(4,), chunks=(2,), dtype="uint8",
                   fill_value=0, compressors=None, filters=None)
root3["uint8_1d_none"][:] = [0, 127, 128, 255]

# float32_1d_gzip
root2.create_array("float32_1d_gzip", shape=(4,), chunks=(4,), dtype="float32",
                   fill_value=0, compressor=numcodecs.GZip())
root2["float32_1d_gzip"][:] = np.array([-1.5, 0.0, 1.5, 3.14], dtype="float32")

root3.create_array("float32_1d_gzip", shape=(4,), chunks=(4,), dtype="float32",
                   fill_value=0, compressors=(GzipCodec(),), filters=None)
root3["float32_1d_gzip"][:] = np.array([-1.5, 0.0, 1.5, 3.14], dtype="float32")

# int16_2d_gzip
root2.create_array("int16_2d_gzip", shape=(4, 3), chunks=(2, 3), dtype="int16",
                   fill_value=0, compressor=numcodecs.GZip())
root2["int16_2d_gzip"][:] = np.arange(12, dtype="int16").reshape(4, 3)

root3.create_array("int16_2d_gzip", shape=(4, 3), chunks=(2, 3), dtype="int16",
                   fill_value=0, compressors=(GzipCodec(),), filters=None)
root3["int16_2d_gzip"][:] = np.arange(12, dtype="int16").reshape(4, 3)

# float64_3d_zstd
root2.create_array("float64_3d_zstd", shape=(2, 3, 4), chunks=(2, 3, 4), dtype="float64",
                   fill_value=0, compressor=numcodecs.Zstd())
root2["float64_3d_zstd"][:] = np.arange(24, dtype="float64").reshape(2, 3, 4)

root3.create_array("float64_3d_zstd", shape=(2, 3, 4), chunks=(2, 3, 4), dtype="float64",
                   fill_value=0, compressors=(ZstdCodec(),), filters=None)
root3["float64_3d_zstd"][:] = np.arange(24, dtype="float64").reshape(2, 3, 4)

# fill_custom
root2.create_array("fill_custom", shape=(6,), chunks=(3,), dtype="float64",
                   fill_value=-9999.0, compressor=None)
root2["fill_custom"][:] = [1.0, 2.0, 3.0, -9999.0, -9999.0, -9999.0]

root3.create_array("fill_custom", shape=(6,), chunks=(3,), dtype="float64",
                   fill_value=-9999.0, compressors=None, filters=None)
root3["fill_custom"][:] = [1.0, 2.0, 3.0, -9999.0, -9999.0, -9999.0]

# ragged_2d_gzip
root2.create_array("ragged_2d_gzip", shape=(5, 7), chunks=(3, 4), dtype="int32",
                   fill_value=0, compressor=numcodecs.GZip())
root2["ragged_2d_gzip"][:] = np.arange(35, dtype="int32").reshape(5, 7)

root3.create_array("ragged_2d_gzip", shape=(5, 7), chunks=(3, 4), dtype="int32",
                   fill_value=0, compressors=(GzipCodec(),), filters=None)
root3["ragged_2d_gzip"][:] = np.arange(35, dtype="int32").reshape(5, 7)

# ---- meta_group V2 ----
sub2 = root2.create_group("meta_group")
sub2.attrs["description"] = "test group"
sub2.attrs["version"] = 1
t2 = sub2.create_array("temperature", shape=(3, 4), chunks=(3, 4), dtype="float32",
                       fill_value=0, compressor=numcodecs.GZip())
t2[:] = np.array([280.1, 281.2, 282.3, 283.4, 284.5, 285.6,
                  286.7, 287.8, 288.9, 290.0, 291.1, 292.2],
                 dtype="float32").reshape(3, 4)
t2 = t2.update_attributes({"units": "K"})

# ---- meta_group V3 ----
sub3 = root3.create_group("meta_group",
                          attributes={"description": "test group", "version": 1})
t3 = sub3.create_array("temperature", shape=(3, 4), chunks=(3, 4), dtype="float32",
                       fill_value=0, compressors=(GzipCodec(),), filters=None,
                       attributes={"units": "K"})
t3[:] = np.array([280.1, 281.2, 282.3, 283.4, 284.5, 285.6,
                  286.7, 287.8, 288.9, 290.0, 291.1, 292.2],
                 dtype="float32").reshape(3, 4)

print("Python write complete")
