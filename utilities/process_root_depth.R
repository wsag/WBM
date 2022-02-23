### Processing root depth data from https://data.csiro.au/collection/csiro:19813v1
### written by Danielle Grogan, 2021-12-14

## 2 Processing steps: 
# 1. Remove unreasonable high values (> 4m)
# 2. Patch no-data values with data from the FAO/UNESCO digital soil map of the world v3.6; https://www.worldcat.org/title/digital-soil-map-of-the-world-and-derived-soil-properties/oclc/52200846

#### Libraries
library(raster)
library(rgdal)
library(rgeos)

### Step 1. Remove high values
root.depth = raster("/net/nfs/merrimack/raid2/data/RootDepth/Effective_Rooting_Depth.tif")    # unit: m

max.value = 4
root.depth.cut = root.depth
root.depth.cut[root.depth.cut > max.value] = max.value   # set values > 4m equal to 4m


### Step 2. Patch NA values with alternative data
root.depth.fao = raster("/net/nfs/zero/home/WBM_TrANS/data/Global_Terrain_RootDepthWBM_LTXXXX_30min.nc")  # units: mm
root.depth.fao.m = root.depth.fao / 1000  # convert units from mm to m
root.depth.fao.m = resample(root.depth.fao.m, root.depth.cut)  # set to same spatial coordinate system
root.depth.fao.m[root.depth.fao.m < 0] = 0  # remove negative values that result from resampling

# identify grid cells with NA values
root.depth.na = root.depth.cut
root.depth.na[!is.na(root.depth.na)] = 0  # set non-NA to 0
root.depth.na[is.na(root.depth.na)]  = 1  # set NA values to 1

# make a layer of FAO values only where the root depth data is NA
root.depth.fao.m.patch = root.depth.fao.m * root.depth.na

# apply the FAO data patch
root.depth.cut[is.na(root.depth.cut)] = 0
root.depth.cut.patch = overlay(root.depth.cut, root.depth.fao.m.patch, fun = sum)

# save file
writeRaster(root.depth.cut.patch, 
            filename = "/net/nfs/merrimack/raid2/data/RootDepth/Effective_Rooting_Depth_mod.tif",
            format   = "GTiff")
