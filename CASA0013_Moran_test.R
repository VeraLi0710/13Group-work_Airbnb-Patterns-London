#install and library the packages
install.packages("spdep")
library("spdep")
library("sp")
library("sf")
library("tmap")
library("dplyr")
#read the csv and shapefile of London
ratio <- read.csv("data/borough_listings_ratio.csv")
borough <- st_read("data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")

#merge the two datasets
borough.ratio <- merge(borough, ratio, 
                       by.x="GSS_CODE", by.y="Borough_code")

# Calculate neighbours
neighbours <- poly2nb(borough.ratio)
neighbours


# Convert the neighbour data to a listw object
listw <- nb2listw(neighbours, style="W")
listw

# global spatial autocorrelation
moran.test(borough.ratio$Ratio_of_more_than_90, listw)

# creates a moran plot
moran <- moran.plot(borough.ratio$Ratio_of_more_than_90, 
                    listw = nb2listw(neighbours, style = "W"),asp=T)

# creates a local moran output
local <- localmoran(x = borough.ratio$Ratio_of_more_than_90, 
                    listw = nb2listw(neighbours, style = "W"))

# binds results to our polygon shapefile
moran.map <- borough.ratio

moran.map <- borough.ratio %>%
  bind_cols(local)

# maps the results
tm_shape(moran.map) + 
  tm_fill(col = "Ii", style = "quantile", title = "local moran statistic") 

#map the p-value
names(moran.map)
tm_shape(moran.map) + 
  tm_fill(col = "Pr(z != E(Ii))", style = "quantile", title = "P-value") 

### to create LISA cluster map ### 
quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.ratio <- borough.ratio$Ratio_of_more_than_90 - mean(borough.ratio$Ratio_of_more_than_90)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])

# significance threshold
signif <- 0.1 

# builds a data quadrant by extracting the row numbers from the m.qualification and m.local objects that both correspond to the criteria and then using them to update the rows with the same number in the quadrant object.
quadrant[m.ratio <0 & m.local>0] <- 1      
quadrant[m.ratio <0 & m.local<0] <- 2
quadrant[m.ratio >0 & m.local<0] <- 3
quadrant[m.ratio >0 & m.local>0] <- 4 
quadrant[local[,5]>signif] <- 0  

# Plot
borough.ratio$quadrant <- quadrant
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(borough.ratio["quadrant"], border = "lightgray", col = colors[findInterval(borough.ratio$quadrant, brks, all.inside = FALSE)])
legend("bottomleft",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")

#check if the data makes sense, but it seems fine
hist(borough.ratio$Ratio_of_more_than_90, main = "Distribution of Ratio_of_more_than_90", xlab = "Value")
summary(borough.ratio$Ratio_of_more_than_90)
summary(local[,1])
table(local[,5] < 0.1)
outliers <- which((m.ratio > 0 & m.local < 0) | (m.ratio < 0 & m.local > 0))
length(outliers)
plot(m.ratio, m.local, main = "Moran's I vs. Variable of Interest",
     xlab = "Centered Variable of Interest (m.ratio)",
     ylab = "Centered Local Moran's I (m.local)")
abline(h = 0, v = 0, col = "red")

# Step 1: Extract centroids
centroids <- st_centroid(borough.ratio)

# Step 2: Extract coordinates
coords <- st_coordinates(centroids)

# Step 3: Create neighbors within 20 km
nb <- dnearneigh(coords, d1 = 0, d2 = 20000)

# Create spatial weights
listw_new <- nb2listw(nb, style = "W")

# Perform Moran's I
local_new <- localmoran(x = borough.ratio$Ratio_of_more_than_90, listw = listw_new)

# Bind Moran's I results to sf object
local_df <- as.data.frame(local)  # Convert Moran's I results to data.frame
names(local_df) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "P.Ii")
borough.ratio <- cbind(borough.ratio, local_df)

#Create map
tm_shape(borough.ratio) +
  tm_fill(
    col = "Ii",
    style = "quantile",
    title = "Local Moran Statistic"
  )
