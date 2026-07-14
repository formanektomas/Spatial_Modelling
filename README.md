# Spatial Modelling (4EK662)  
# Prostorové modely (4EK462)  


--- 

**Taught by:**  
[Tomas Formanek](https://insis.vse.cz/auth/lide/clovek.pl?id=46723){:target="_blank"}     
Department of Econometrics   
Faculty of Informatics and Statistics  
University of Economics, Prague  


--- 

## Basic information

+ [Course classification](https://github.com/formanektomas/Spatial_Modelling/CourseClassification.html)  

+ [Literature and supporting materials for the course](https://formanektomas.github.io/Spatial_Modelling/LiteratureResources.html)  


--- 

### Block 1: Geodata: visualization and data wrangling

+ [https://ruettenauer.github.io/SICSS-Spatial/index.html](https://ruettenauer.github.io/SICSS-Spatial/index.html){:target="_blank"}

**Spatial geometries (sf objects)**  
+ Spatial objects, projections (coordinate reference systems), visualization (maps, choropleths)
+ Subsetting spatial data, distances, intersections, buffers, and other geometry-related operations
    + [https://ruettenauer.github.io/Geodata_Spatial_Regression/02_spatial-data.html](https://ruettenauer.github.io/Geodata_Spatial_Regression/02_spatial-data.html){:target="_blank"}  

**Raster data**  
+ Remote sensing: satellite imaging and other raster data (formats, layers)  
+ Raster data wrangling with `{terra}` package  
+ Raster-vector interactions  
    + [https://r.geocompx.org/raster-vector](https://r.geocompx.org/raster-vector){:target="_blank"}  

**Graph represeentations of spatial data**  
+ Creating map-based graphs (connectivity, distance-based)   
+ Centrality indices: node/edge importance  
+ [https://r-spatial.org/r/2019/09/26/spatial-networks.html](https://r-spatial.org/r/2019/09/26/spatial-networks.html){:target="_blank"}  

**Case studies and applications**   
+ Transportation-related applications  
+ Urbanistic applications (space syntax analysis)   
+ [Ecology](https://r.geocompx.org/eco){:target="_blank"}   

--- 

### Block 2: Geostatistcs  

+ Descriptive statistics: stochastic spatial processes, covarigram, (semi)variogram
+ Neighbors: definitions  
+ Detecting spatial dependence  
+ Spatial clustering, local indicators of spatial association (LISA)  
+ Spatial interpolation, krigging

**Case studies and applications**  
+ [Lake Rangsdorf](https://www.geo.fu-berlin.de/en/v/soga-py/Advanced-statistics/Spatial-Interpolation/Data-sets-used/Lake-Rangsdorf/index.html)

--- 

### Block 3: Spatial regression models  

**Models for cross-sectional data**  
+ Spatial regression models: taxonomy, marginal effects, diagnostics (stability with respect to spatial structure/adjacency)  
+ Local regression on spatial data: geographically weighted regression models (GWR)  
+ Spatial filter: Moran's eigenvector maps in spatial econometrics   
    + [https://ruettenauer.github.io/SICSS-Spatial/SICSS-spatial_part2.html#Spatial_Regression_Models](https://ruettenauer.github.io/SICSS-Spatial/SICSS-spatial_part2.html#Spatial_Regression_Models){:target="_blank"}
+ Spatial regression models for limited dependent variables 

**Models for spatio-temporal (panel) data**  
+ Static panel data models
+ Dynamic panel data models
    + [https://ruettenauer.github.io/Geodata_Spatial_Regression/09_spatiotemporal.html](https://ruettenauer.github.io/Geodata_Spatial_Regression/09_spatiotemporal.html){:target="_blank"}

**Case studies and applications**  
+ [https://rpubs.com/quarcs-lab/tutorial-gwr1](https://rpubs.com/quarcs-lab/tutorial-gwr1){:target="_blank"}  

--- 

### Block 4: Case studies and applications  

+ [Transportation](https://r.geocompx.org/transport){:target="_blank"}
+ [Geomarketing](https://r.geocompx.org/location){:target="_blank"}
+ [Ecology](https://r.geocompx.org/eco){:target="_blank"}   


[Homepage](https://formanektomas.github.io/Spatial_Modelling/){:target="_blank"}
