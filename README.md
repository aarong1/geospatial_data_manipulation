# Geospatial Data Manipulation 🗺️

- This is not a _package_.
- However the steps to building manipulating and formatting geospatial data structures, including **topo-** and **geo-json** and **.shp shapefiles**, can be arduous. 🗃️
- This was motivation enough to document progress on common or innovative design patterns on the mamipulation of such data. 
- **Documenting the steps and functions to build, tidy, merge and work with geo spatial data structures.  Less of a package more or a stream of consciousness.**

## See the example below, constructing a polygon set from sample constituent members 🌍

### Working left to right top to bottom 👉 👇
1. We take a shapefile (the statisical areas it describes are not important) and take the union to construst our bounding outline.
2. We sample points belonging to PostCode stubs, Bt1 etc. - UK postal codes, akin to american zip codes.
3. We construct a voroni tesselation of all these points and the take the union grouped by the union of all those relating to each Pc stub.
4. Thereby by sampling only single digit points per Pc stub, we recreate shapefiles of reasonable quality to represent in chloropeth mappings.
5. This technique is also at an extreme advantage aesthetically over the inital voroni tesselation simply using PC stub **centroids**


![overview_geo](https://user-images.githubusercontent.com/48027133/137812273-e0f06b72-a4a5-448e-8732-1f86caa03a8a.png)

AG
