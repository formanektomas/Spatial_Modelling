# Spatial Modelling (4EK662)  
# Prostorové modely (4EK462)  


--- 

**Taught by:**  
[Tomas Formanek](https://insis.vse.cz/auth/lide/clovek.pl?id=46723)     
Department of Econometrics   
Faculty of Informatics and Statistics  
University of Economics, Prague  


--- 

### Requirements and classification  

--- 

### Block 1: Geodata: visualization and data wrangling

+ [https://ruettenauer.github.io/SICSS-Spatial/index.html](https://ruettenauer.github.io/SICSS-Spatial/index.html)

**Spatial geometries (sf objects)**  
+ Spatial objects, projections (coordinate reference systems), visualization (maps, choropleths)
+ Subsetting spatial data, distances, intersections, buffers, and other geometry-related operations
    + [https://ruettenauer.github.io/Geodata_Spatial_Regression/02_spatial-data.html](https://ruettenauer.github.io/Geodata_Spatial_Regression/02_spatial-data.html)  

**Raster data**  
+ Remote sensing: satellite imaging and other raster data (formats, layers)  
+ Raster data wrangling with `{terra}` package  
+ Raster-vector interactions  
    + [https://r.geocompx.org/raster-vector](https://r.geocompx.org/raster-vector)  

**Graph represeentations of spatial data**  
+ Creating map-based graphs (connectivity, distance-based)    
+ Transportation-related applications  
+ Urbanistic applications (space syntax analysis)   
+ Centrality indices: node/edge importance  


--- 

### Block 2: Geostatistcs  

+ Descriptive statistics: stochastic spatial processes, covarigram, (semi)variogram
+ Neighbors: definitions  
+ Detecting spatial dependence  
+ Spatial clustering, local indicators of spatial association (LISA)  
+ Spatial interpolation, krigging


--- 

### Block 3: Spatial regression models  

**Models for cross-sectional data**  
+ Spatial regression models: taxonomy, marginal effects, diagnostics (stability with respect to spatial structure/adjacency)  
+ Local regression on spatial data: geographically weighted regression models (GWR)  
+ Spatial filter: Moran's eigenvector maps in spatial econometrics   
    + [https://ruettenauer.github.io/SICSS-Spatial/SICSS-spatial_part2.html#Spatial_Regression_Models](https://ruettenauer.github.io/SICSS-Spatial/SICSS-spatial_part2.html#Spatial_Regression_Models)
+ Spatial regression models for limited dependent variables 

**Models for spatio-temporal (panel) data**  
+ Static panel data models
+ Dynamic panel data models
    + [https://ruettenauer.github.io/Geodata_Spatial_Regression/09_spatiotemporal.html](https://ruettenauer.github.io/Geodata_Spatial_Regression/09_spatiotemporal.html)


--- 

### Block 4: Case studies and applications  

+ [Transportation](https://r.geocompx.org/transport)
+ [Geomarketing](https://r.geocompx.org/location)
+ [Ecology](https://r.geocompx.org/eco)   


[Homepage](https://formanektomas.github.io/Spatial_Modelling/)
