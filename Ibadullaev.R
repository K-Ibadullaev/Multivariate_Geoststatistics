
# Import Packages
library("gstat")
library("RColorBrewer")
library("magrittr")
library("ggplot2")
library("manipulate")
library("sp")
library("scico")
library("dplyr")
library("gmGeostats")
library("knitr")
library("rstudioapi") 


setwd(dirname(getActiveDocumentContext()$path)) # Set active directory
getwd()


ds = read.csv("Ibadullaev.csv",stringsAsFactors = T) # load data


colnames(ds) # check cols
ds = ds[,-1] # clip 1st col
colnames(ds) # check cols

## Univariate analysis

### Descriptive summaries

# After we have explored names variables we can pick a pair of them with regard
# to geological coherence and correlation 
ds[,c(3,4,5,7,8)] %>% pairs()
ds[,c(3,4,5,7,8)] %>% cor%>% kable()

# looks not pleasant, so we cast a lognormal transformation as plot 
# should be more evident
ds[,c(3,4,5,7,8)] %>% log %>% pairs()
ds[,c(3,4,5,7,8)] %>% log%>% cor%>% kable()


# We are after cobalt ores which are tethered Ni 
#ds %>%select(Co,Fe) %>% summary %>% kable() # The most coherent elements
ds %>%select(Co,Ni) %>% summary %>% kable()# The most coherent elements

### Standard deviation
#ds$Fe %>% sd() # Fe
ds$Co %>% sd() # Co
ds$Ni %>% sd() # Ni


### Variation
# ds$Fe %>% var() # Fe
ds$Co  %>% var() # Co
ds$Ni %>% var() # Ni


### The coefficients of variation
# sd(ds$Fe)  / mean(ds$Fe)  # Fe
sd(ds$Co) / mean(ds$Co)  # Co
sd(ds$Ni)/ mean(ds$Ni) # Ni


### Correlation 
#ds %>% select(Co,Fe) %>% cor %>% kable()
ds %>% select(Co,Ni) %>% cor %>% kable()

# We see a moderate difference between the two variables in case of Ni and Co, in particular 
# in the variability given by the standard deviation (or the interquartile range).

# Additionally, Co and Ni are seem to be positively correlated   
# It is therefore interesting to check the equivalent statistics 
# for the log-transformed variables

### lognorm stats

# ds %>%select(Co,Fe) %>%log %>%  summary %>% kable()            # Fe & Co
# ds %>% select(Co,Fe) %>% log %>% sapply(FUN =  sd) %>% kable() # Fe & Co
# ds %>% select(Co,Fe) %>% log %>% cor %>% kable()               # Fe & Co

ds %>% select(Co,Ni) %>%log %>%  summary %>% kable()           # Ni & Co
ds %>% select(Co,Ni) %>% log %>% sapply(FUN = sd) %>% kable()  # Ni & Co
ds %>% select(Co,Ni) %>% log %>% cor %>% kable()               # Ni & Co

# from that analysis of lognormal statistics one can observe that 
# the both variables are in comparable scale and correlation between them has grown.

# Now we take a closer look on each of both variables
# We start with Co.
par(mfcol=c(3,2))
# Co
h_Co = hist(ds$Co, main="Co", col=3)
boxplot(ds$Co, horizontal = TRUE, ylim=range(h_Co$breaks), col=3)
qqnorm(ds$Co, col=3)
qqline(ds$Co,col=2)
# logCo
h_logCo = hist(log(ds$Co), main="log(Co)", col=3)
boxplot(log(ds$Co), horizontal = TRUE, ylim=range(h_logCo$breaks), col=3)
qqnorm(log(ds$Co), col=3)
qqline(log(ds$Co),col=2)

# Unfortunately, the data of Co isn't normally distributed or even lognormally
# The lognorm curve is skewed.

# We continue the exploration with Ni.
par(mfcol=c(3,2))
# Ni
h_Ni = hist(ds$Ni, main="Ni", col=3)
boxplot(ds$Ni, horizontal = TRUE, ylim=range(h_Ni$breaks), col=3)
qqnorm(ds$Ni, col=3)
qqline(ds$Ni,col=2)
# logNi
h_logNi = hist(log(ds$Ni), main="log(Ni)", col=3)
boxplot(log(ds$Ni), horizontal = TRUE, ylim=range(h_logNi$breaks), col=3)
qqnorm(log(ds$Ni), col=3)
qqline(log(ds$Ni),col=2)

# Also the data of Ni isn't normally distributed or even lognormally
# The lognorm curve is also skewed.

# While the form of the distribution of both components is much uglier than 
# lognormal one, we proceed with the lognormal scale.

# Bivariate analysis
# Co vs Ni
# A scatterplot of these two variables of interest
par(mfrow=c(1,1))
plot(Co~Ni, data=ds, main="Co vs Ni",log="xy")

#shows a relatively high, positive linear dependence between the two variables,
#consistent with a correlation of 0.7965048(0.6139465) .



## Spatial descriptive analysis
### Data maps
   # Bubble and color maps are shown for each variable of interest. The following
par(mfrow=c(1,1))


# Co
#scico::scico_palette_show() 
Co_plot = ggplot(ds, aes(X, Y)) +  
  geom_point(aes(size=log(Co), col=log(Co))) + 
  theme_bw() + 
  coord_fixed(ratio=1)+
  ggtitle("Map of Log(Co)")+
  scico::scale_color_scico(palette = "roma")
Co_plot
# One can observe that the highest values of Log(Co) are located in North and 
# the North-West  regions

# Ni
Ni_plot = ggplot(ds, aes(X, Y)) +  
  geom_point(aes(size=log(Ni), col=log(Ni))) + 
  theme_bw() + 
  coord_fixed(ratio=1)+
  scico::scale_color_scico(palette = "roma")+
  ggtitle("Map of Log(Ni)")
Ni_plot
# One can observe that the highest values of Log(Ni) are located in North and
# the North-West regions.

# The Log(Co) have larger variability in comparison to Log(Ni). One can see that
# lowest values are distributed in direction from SW to NE  and prevail in the center.
# Also the locations of the largest values overlap each other.
# Thus, there's some relationship between variables.

### Swath plots
# Here one explores the spatial dependency for each of variables with regard
# to spatial coordinates X and Y separately.

# Co
# ... as a function of location (one coordinate) : swath plot
# par(mfrow=c(1,1))
# aux_Co = log(select(ds, Co))  
# gmGeostats::swath(aux_Co, loc=ds$X, pch=19)
# gmGeostats::swath(aux_Co, loc=ds$Y, pch=19)
gswx_Co = ggplot(ds) + 
  geom_point(aes(x=X, y=log(Co))) +
  theme_bw() + 
  ggtitle(" Log(Co) vs X")+
  geom_smooth(aes(x=X, y=log(Co)), method=loess, col=2) 
gswy_Co = ggplot(ds) + 
  geom_point(aes(x=Y, y=log(Co))) +
  theme_bw() +
  ggtitle(" Log(Co) vs Y")+ 
  geom_smooth(aes(x=Y, y=log(Co)), method=loess, col=2) 
gridExtra::grid.arrange(gswx_Co, gswy_Co, nrow = 2)  

# Reasonable to assume the stationarity for log(Co) as the curve is smooth
# doesn't change significantly.

gswx_Ni = ggplot(ds) + 
  geom_point(aes(x=X, y=log(Ni))) +
  theme_bw() + 
  ggtitle(" Log(Ni) vs X")+
  geom_smooth(aes(x=X, y=log(Ni)), method=loess, col=2) 
gswy_Ni = ggplot(ds) + 
  geom_point(aes(x=Y, y=log(Ni))) +
  theme_bw() +
  ggtitle(" Log(Ni) vs Y")+ 
  geom_smooth(aes(x=Y, y=log(Ni)), method=loess, col=2) 
gridExtra::grid.arrange(gswx_Ni, gswy_Ni, nrow = 2)  

# Reasonable to assume the stationarity for log(Ni) as the curve is smooth
# doesn't change significantly.

### Structural analysis
  ## Marginal variograms
    # Isotropic variography
     #Co
## selection of the maximum distance for the calculation of the variogram
# crit 1.- median value or mode of the histogram of distances
ds[,c("X", "Y")] %>% dist %>% as.matrix %>% hist(main=" Histogram of distances")
abline(v=median(ds[,c("X", "Y")] %>% dist),col=2)
ds[,c("X", "Y")] %>% dist %>% as.matrix %>% median

# crit 2.- radius of the largest inscribed circle in the map
plot(Y~X, data=ds, asp=1)
# using radius of circles on a map we set cutoff = 225 and width("lag")=11.25
gs_Co = gstat(formula=log(Co)~1, locations = ~X+Y, data=ds)
# ... and compute+plot its variogram
vgo_Co = variogram(gs_Co, cutoff=225, width=11.25) #longest distance  we want-cutoff ;width- minimum interval between points 
plot(vgo_Co)
vgt_Co = vgm(model="Gau", range=10, psill=0.8) 
plot(vgo_Co, model=vgt_Co)

# Function for an isotropic variogram
myvariomodelplot = function(nug=0, c=0.1, a=4,vgo,model){
  vgt = vgm(model=model, nugget=nug, psill=c, range=a)
  plot(vgo, model = vgt)
}


# step 2.- call it within manipulate for Co
manipulate(
  myvariomodelplot(n,c,a,vgo = vgo_Co,model ),    # function call                   
  n = slider(min = 0, max = 1, initial = 0.852), # control specifying how n (nugget) should vary
  c = slider(min = 0, max =1.2, initial = 0.4704),  # same for c (partial sill)
  a = slider(min = 1, max = 200, initial = 110), # same for a (range)
  model = picker("Exponential"="Exp","Gaussian"="Gau","Spherical"= "Sph", initial = "Gaussian")

  )

# redefine vgt_Co 
# or use the vgm below, though cross-validation results are similar
# vgt_Co = vgm(model="Sph", nugget=0.744,range=129, psill=0.4752)
 vgt_Co = vgm(model="Gau", nugget=0.852,range=110, psill=0.4704)
plot(vgo_Co, model = vgt_Co,main="Isotropic variogram of Co")




# with local neigbourhood
gs_Co = gstat(id="Co", formula=log(Co)~1, locations = ~X+Y, 
             data=ds, model=vgt_Co ,
             # neighbourhood parameters: see ?gstat help
             nmax = 20,    # maximum number of datapoints => increase if variogram has oscillations
             omax = 6,    # maximum nr of datapoints per octant/quadrants 
             nmin = 10,    # minimum nr of datapoints (otherwise return NA)
             maxdist = 20, # maximum distance to seek for datapoints
             force = TRUE  # ignore maxdist if nmin is not satisfied?
)
xv_Co = gstat.cv(gs_Co, nfold=nrow(ds))

## some diagnostic plots

# observations vs predictions
plot(observed~Co.pred, data=xv_Co, asp=1,main="observations vs predictions for Co")
abline(a=0, b=1, col=2, lwd=2)
#data points aren't concentrated along y=x
# histogram of the standardized residuals
hist(xv_Co$zscore, probability = T,main = "Histogram of the standardized residuals")  
curve(dnorm, from=-3, to=3, col=2, lwd=2, add=T)
# deviation to slightly larger values than 0

# normal qq plot of the standardized residuals
qqnorm(xv_Co$zscore,main = "Normal qq plot of the standardized residuals")
qqline(xv_Co$zscore, col=2, lwd=2)
#map of residuals
resid_map_Co=ggplot(data=xv_Co,aes(X, Y)) +
  geom_point(aes(size=residual,col=residual))+
  theme_bw() + 
  coord_fixed(ratio=1)+
  ggtitle("Map of residuals Co")+
  scico::scale_color_scico(palette = "bilbao")
resid_map_Co
#normality of residuals





# Ni
gs_Ni = gstat(formula=log(Ni)~1, locations = ~X+Y, data=ds)
# ... and compute+plot its variogram
vgo_Ni = variogram(gs_Ni, cutoff=225, width=11.25) #longest distance  we want-cutoff ;width- minimum interval between points 
plot(vgo_Ni)
vgt_Ni = vgm(model="Gau", range=50, psill=0.6) 
plot(vgo_Ni, model=vgt_Ni)

# step 2.- call it within manipulate for Ni
manipulate(
  myvariomodelplot(n,c,a,vgo = vgo_Ni,model ),    # function call                   
  n = slider(min = 0, max = 1, initial = 0.408), # control specifying how n (nugget) should vary
  c = slider(min = 0, max =1.2, initial = 0.3504),  # same for c (partial sill)
  a = slider(min = 1, max = 200, initial = 84), # same for a (range)
  model = picker("Exponential"="Exp","Gaussian"="Gau","Spherical"= "Sph", initial = "Gaussian")
  
)

# redefine vgt_Ni

vgt_Ni = vgm(model="Gau", nugget=0.408,range=84, psill=0.3504)
plot(vgo_Ni, model = vgt_Ni,main="Isotropic variogram of Ni")

# or use the vgm below, though cross-validation results are similar
# vgt_Ni = vgm(model="Sph", nugget=0.34,range=141, psill=0.3888)


# with local neigbourhood
gs_Ni = gstat(id="Ni", formula=log(Ni)~1, locations = ~X+Y, 
              data=ds, model=vgt_Ni ,
              # neighbourhood parameters: see ?gstat help
              nmax = 20,    # maximum number of datapoints => increase if variogram has oscillations
              omax = 6,    # maximum nr of datapoints per octant/quadrants 
              nmin = 10,    # minimum nr of datapoints (otherwise return NA)
              maxdist = 20, # maximum distance to seek for datapoints
              force = TRUE  # ignore maxdist if nmin is not satisfied?
)# (not all parameters are necessary! comment those you do not need; adapt the rest to your needs)
## do leave-one-out cross-validation
xv_Ni = gstat.cv(gs_Ni, nfold=nrow(ds))


## some diagnostic plots

# observations vs predictions
plot(observed~Ni.pred, data=xv_Ni, asp=1,main="observations vs predictions for Ni")
abline(a=0, b=1, col=2, lwd=2)
#data points aren't concentrated along y=x
# histogram of the standardized residuals
hist(xv_Ni$zscore, probability = T,main = "Histogram of the standardized residuals")  
curve(dnorm, from=-3, to=3, col=2, lwd=2, add=T)
# deviation to slightly larger values than 0

# normal qq plot of the standardized residuals
qqnorm(xv_Ni$zscore,main = "Normal qq plot of the standardized residuals")
qqline(xv_Ni$zscore, col=2, lwd=2)
#map of residuals
resid_map_Ni=ggplot(data=xv_Ni,aes(X, Y)) +
  geom_point(aes(size=residual,col=residual))+
  theme_bw() + 
  coord_fixed(ratio=1)+
  ggtitle("Map of residuals Ni")+
  scico::scale_color_scico(palette = "bilbao")
resid_map_Ni

# Show all vgms
library("kableExtra")
vgm_params=kbl(rbind(vgt_Co,vgt_Ni)) %>% 
  kable_paper("striped") %>%
  pack_rows("Co", 1, 2) %>%
  pack_rows("Ni", 3, 4)

vgm_params
 


# to define range of coordinates 
rangesXY = ds[,c("X", "Y")] %>% sapply(range)

# vectors of x and y coordinates of the grid nodes,
#   covering the range of the data, and with a step 
#   allowing for interpolation between data
par(mfrow=c(1,1))
plot(Y~X, data=ds, asp=1)
x = seq(from=rangesXY[1,"X"]-20, to=rangesXY[2,"X"]+20, by=10)
y = seq(from=rangesXY[1,"Y"]-20, to=rangesXY[2,"Y"]+20, by=10)
# 4 points 
# grid definition must be the same in gs object Easting=x, Northing=y
xy_grid = expand.grid(X=x, Y=y)
points(xy_grid, pch=".", col=2) # check   
#Local
kriged_Co = predict(gs_Co, newdata=xy_grid, debug.level=-1)
kriged_Ni = predict(gs_Ni, newdata=xy_grid, debug.level=-1)

# global
gs_Nig = gstat(id="Ni", formula=log(Ni)~1, locations = ~X+Y, 
              data=ds, model=vgt_Ni)
gs_Cog = gstat(id="Co", formula=log(Co)~1, locations = ~X+Y, 
              data=ds, model=vgt_Co)
# cv for Nig
xv_Nig = gstat.cv(gs_Nig, nfold=nrow(ds))


## some diagnostic plots

# observations vs predictions
plot(observed~Ni.pred, data=xv_Nig, asp=1,main="observations vs predictions for Ni")
abline(a=0, b=1, col=2, lwd=2)
#data points aren't concentrated along y=x
# histogram of the standardized residuals
hist(xv_Nig$zscore, probability = T,main = "Histogram of the standardized residuals")  
curve(dnorm, from=-3, to=3, col=2, lwd=2, add=T)
# deviation to slightly larger values than 0

# normal qq plot of the standardized residuals
qqnorm(xv_Nig$zscore,main = "Normal qq plot of the standardized residuals")
qqline(xv_Nig$zscore, col=2, lwd=2)
#map of residuals
resid_map_Nig=ggplot(data=xv_Nig,aes(X, Y)) +
  geom_point(aes(size=residual,col=residual))+
  theme_bw() + 
  coord_fixed(ratio=1)+
  ggtitle("Map of residuals Ni")+
  scico::scale_color_scico(palette = "bilbao")
resid_map_Nig

#cv for Co global
xv_Cog = gstat.cv(gs_Cog, nfold=nrow(ds))


## some diagnostic plots

# observations vs predictions
plot(observed~Co.pred, data=xv_Cog, asp=1,main="observations vs predictions for Ni")
abline(a=0, b=1, col=2, lwd=2)
#data points aren't concentrated along y=x
# histogram of the standardized residuals
hist(xv_Cog$zscore, probability = T,main = "Histogram of the standardized residuals")  
curve(dnorm, from=-3, to=3, col=2, lwd=2, add=T)
# deviation to slightly larger values than 0

# normal qq plot of the standardized residuals
qqnorm(xv_Cog$zscore,main = "Normal qq plot of the standardized residuals")
qqline(xv_Cog$zscore, col=2, lwd=2)
#map of residuals
resid_map_Cog=ggplot(data=xv_Cog,aes(X, Y)) +
  geom_point(aes(size=residual,col=residual))+
  theme_bw() + 
  coord_fixed(ratio=1)+
  ggtitle("Map of residuals Ni")+
  scico::scale_color_scico(palette = "bilbao")
resid_map_Cog










kriged_Cog = predict(gs_Cog, newdata=xy_grid, debug.level=-1)
kriged_Nig = predict(gs_Nig, newdata=xy_grid, debug.level=-1)

# plot
kplot_Co=ggplot(kriged_Co, aes(X, Y))+
  geom_raster(aes(fill=Co.pred))+
  coord_fixed()+
  theme_bw()+
  ggtitle("Results of local kriging for Log(Co)")+
  scico::scale_fill_scico(palette = "roma",direction = -1)
kplot_Ni=ggplot(kriged_Ni, aes(X, Y))+
  geom_raster(aes(fill=Ni.pred))+
  coord_fixed()+
  theme_bw()+
  ggtitle("Results of local krigingfor Log(Ni)")+
  scico::scale_fill_scico(palette = "roma",direction =-1)

kplot_Cog=ggplot(kriged_Cog, aes(X, Y))+
  geom_raster(aes(fill=Co.pred))+
  coord_fixed()+
  theme_bw()+
  ggtitle("Results of global kriging results for Log(Co)")+
  scico::scale_fill_scico(palette = "roma",direction = -1)
kplot_Nig=ggplot(kriged_Nig, aes(X, Y))+
  geom_raster(aes(fill=Ni.pred))+
  coord_fixed()+
  theme_bw()+
  ggtitle("Results of global kriging  for Log(Ni)")+
  scico::scale_fill_scico(palette = "roma",direction =-1)

kriged_Cog$difference = kriged_Co$Co.pred - kriged_Cog$Co.pred

kplot_Codiff=ggplot(kriged_Cog, aes(X, Y))+
  geom_raster(aes(fill=difference))+
  coord_fixed()+
  theme_bw()+
  ggtitle("Difference between local and global\n  kriging  for Log(Co)")+
  scico::scale_fill_scico(palette = "roma",direction = -1)

kriged_Nig$difference = kriged_Ni$Ni.pred - kriged_Nig$Ni.pred
kplot_Nidiff=ggplot(kriged_Nig, aes(X, Y))+
  geom_raster(aes(fill=difference))+
  coord_fixed()+
  theme_bw()+
  ggtitle("Difference between local  and global\n kriging  for Log(Ni)")+
  scico::scale_fill_scico(palette = "roma",direction =-1)
gridExtra::grid.arrange(kplot_Co,kplot_Ni,kplot_Cog,kplot_Nig,kplot_Codiff,
                        kplot_Nidiff,nrow=3,ncol=2)


# Now we check anisotropy
#for anisotropy
## anisotropic variograms ----------------
#Co
vga_Co = variogram(gs_Co, alpha=c(0,30,60,90,120,150)) #every 30 or 45

par(mfrow=c(1,1))
col_dirs = RColorBrewer::brewer.pal(6,"Set1") # get a list of 6 set colors
plot(gamma~dist, data=vga_Co, pch=21, cex=1.2,  # plot the points ...
     bg=col_dirs[as.factor(dir.hor)],  # ... with color according to the direction
     xlim=range(0,vga_Co$dist), ylim=range(0, vga_Co$gamma)) # ... and axes including the (0,0)
vga_Co[,c("dist", "gamma", "dir.hor")] %>% 
  split(as.factor(vga_Co$dir.hor)) %>% 
  sapply(lines, col="grey")

myvariomodelplot = function(model, nug=0, c=0.1, a=4, anisAngle=0, anisRatio=1,vga){
  # variogram model
  vgt = vgm(model= model, nugget=nug, psill=c, range=a, anis=c(anisAngle, anisRatio))
  
  # color scale
  col_dirs = RColorBrewer::brewer.pal(6,"Set1") 
  
  # plot
  plot(gamma~dist, data=vga, pch=21, cex=1.2,  
       bg=col_dirs[as.factor(dir.hor)],  
       xlim=range(0,vga$dist), ylim=range(0, vga$gamma))
  vga[,c("dist", "gamma", "dir.hor")] %>% 
    split(as.factor(vga$dir.hor)) %>% 
    sapply(lines, col="grey") 
  
  # add empirical variogram lines
  myfun = function(x){
    lines(x[,c(1,2)], col=col_dirs[x$dir.hor/30+1], lty=2)
  }
  vga[,c("dist", "gamma", "dir.hor")] %>% split(as.factor(vga$dir.hor)) %>% sapply(myfun)
  
  # add model variogram lines
  for(i in 1:6){
    angle = pi/2 - ((i-1)*30)*pi/180
    direction = c(sin(angle), cos(angle) ,0)
    variodens = variogramLine(vgt, maxdist= 1.1*max(vga$dist), dir=direction)
    lines(variodens, col=col_dirs[i], lwd=2)
  }
}
# check
myvariomodelplot(vga=vga_Co,model="Exp",nug = 0, c =0.15, a = 5, anisAngle = 0, anisRatio = 0.5 )


# step 2.- call it within manipulate
manipulate(
  myvariomodelplot(model,n,c,a, alpha, r,vga=vga_Co),    # function call                   
  n = slider(min = 0, max = 1, initial = 0.876), # control specifying how n (nugget) should vary
  c = slider(min = 0, max = 1, initial = 0.428),  # same for c (partial sill)
  a = slider(min = 1, max = 250, initial = 133),                  # same for range
  alpha = slider(min = 0, max = 150, initial = 18),# same for direction of maximum continuity (0, 180)
  r = slider(min = 0, max =1,  initial = 0.5),# same for ratio of smallest range/largest range (<1)
  model = picker("exponential"="Exp","Gaussian"="Gau","spherical"= "Sph", initial = "Gaussian")
)

# analyzing anisotrpic variogram one can say that the angla of anistropy is 18°
# with ratio r=0.5

# redefine vgt_Co
anisvgt_Co = vgm(model= "Gau", nugget=0.876, psill=0.428, range=133, anis=c(18, 0.5))
plot(vga_Co, model = anisvgt_Co,main="Anisotropic variogram of Co")


# We stick to the local neigbourhood
# with local neigbourhood
anisgs_Co = gstat(id="Co", formula=log(Co)~1, locations = ~X+Y, 
              data=ds, model=anisvgt_Co ,
              # neighbourhood parameters: see ?gstat help
              nmax = 20,    # maximum number of datapoints => increase if variogram has oscillations
              omax = 6,    # maximum nr of datapoints per octant/quadrants 
              nmin = 10,    # minimum nr of datapoints (otherwise return NA)
              maxdist = 20, # maximum distance to seek for datapoints
              force = TRUE  # ignore maxdist if nmin is not satisfied?
)# (not all parameters are necessary! comment those you do not need; adapt the rest to your needs)
## do leave-one-out cross-validation
anisxv_Co = gstat.cv(anisgs_Co, nfold=nrow(ds))


## some diagnostic plots for 

# observations vs predictions
plot(observed~Co.pred, data=anisxv_Co, asp=1,main="observations vs predictions for Co")
abline(a=0, b=1, col=2, lwd=2)
#data points aren't concentrated along y=x
# histogram of the standardized residuals
hist(anisxv_Co$zscore, probability = T,main = "Histogram of the standardized residuals")  
curve(dnorm, from=-3, to=3, col=2, lwd=2, add=T)
# deviation to slightly larger values than 0

# normal qq plot of the standardized residuals
qqnorm(anisxv_Co$zscore,main = "Normal qq plot of the standardized residuals")
qqline(anisxv_Co$zscore, col=2, lwd=2)
#Here the data is more concentrated along y=x than in the isotropic case.
#map of residuals
resid_map_aCo=ggplot(data=anisxv_Co,aes(X, Y)) +
  geom_point(aes(size=residual,col=residual))+
  theme_bw() + 
  coord_fixed(ratio=1)+
  ggtitle("Map of residuals Co")+
  scico::scale_color_scico(palette = "bilbao")
resid_map_aCo



# anisotropy of Ni
vga_Ni = variogram(gs_Ni, alpha=c(0,30,60,90,120,150)) #every 30 or 45

par(mfrow=c(1,1))
col_dirs = RColorBrewer::brewer.pal(6,"Set1") # get a list of 6 set colors
plot(gamma~dist, data=vga_Ni, pch=21, cex=1.2,  # plot the points ...
     bg=col_dirs[as.factor(dir.hor)],  # ... with color according to the direction
     xlim=range(0,vga_Ni$dist), ylim=range(0, vga_Ni$gamma)) # ... and axes including the (0,0)
vga_Ni[,c("dist", "gamma", "dir.hor")] %>% 
  split(as.factor(vga_Ni$dir.hor)) %>% 
  sapply(lines, col="grey")



# step 2.- call it within manipulate
manipulate(
  myvariomodelplot(model,n,c,a, alpha, r,vga=vga_Ni),    # function call                   
  n = slider(min = 0, max = 1, initial = 0.384), # control specifying how n (nugget) should vary
  c = slider(min = 0, max = 1, initial = 0.356),  # same for c (partial sill)
  a = slider(min = 1, max = 250, initial = 123),                  # same for range
  alpha = slider(min = 0, max = 150, initial = 18),# same for direction of maximum continuity (0, 180)
  r = slider(min = 0, max =1,  initial = 0.404),# same for ratio of smallest range/largest range (<1)
  model = picker("exponential"="Exp","Gaussian"="Gau","spherical"= "Sph", initial = "Gaussian")
)

# analyzing anisotrpic variogram one can say that the angla of anistropy is 18°
# with ratio r=0.404

# redefine vgt_Ni
anisvgt_Ni = vgm(model= "Gau", nugget=0.384, psill=0.356, range=123, anis=c(18, 0.404))
plot(vga_Ni, model = anisvgt_Ni,main="Anisotropic variogram of Ni")


# We stick to the local neigbourhood
# with local neigbourhood
anisgs_Ni = gstat(id="Ni", formula=log(Ni)~1, locations = ~X+Y, 
                  data=ds, model=anisvgt_Ni ,
                  # neighbourhood parameters: see ?gstat help
                  nmax = 20,    # maximum number of datapoints => increase if variogram has oscillations
                  omax = 6,    # maximum nr of datapoints per octant/quadrants 
                  nmin = 10,    # minimum nr of datapoints (otherwise return NA)
                  maxdist = 20, # maximum distance to seek for datapoints
                  force = TRUE  # ignore maxdist if nmin is not satisfied?
)# (not all parameters are necessary! comment those you do not need; adapt the rest to your needs)
## do leave-one-out cross-validation
anisxv_Ni = gstat.cv(anisgs_Ni, nfold=nrow(ds))


## some diagnostic plots for 

# observations vs predictions
plot(observed~Ni.pred, data=anisxv_Ni, asp=1,main="observations vs predictions for Ni")
abline(a=0, b=1, col=2, lwd=2)
#data points aren't concentrated along y=x
# histogram of the standardized residuals
hist(anisxv_Ni$zscore, probability = T,main = "Histogram of the standardized residuals")  
curve(dnorm, from=-3, to=3, col=2, lwd=2, add=T)
# deviation to slightly larger values than 0

# normal qq plot of the standardized residuals
qqnorm(anisxv_Ni$zscore,main = "Normal qq plot of the standardized residuals")
qqline(anisxv_Ni$zscore, col=2, lwd=2)
#Here the data is more concentrated along y=x than in the isotropic case.
#map of residuals
resid_map_aNi=ggplot(data=anisxv_Ni,aes(X, Y)) +
  geom_point(aes(size=residual,col=residual))+
  theme_bw() + 
  coord_fixed(ratio=1)+
  ggtitle("Map of residuals Ni")+
  scico::scale_color_scico(palette = "bilbao")
resid_map_aNi


# Show all anisotropic vgms
library("kableExtra")
anis_vgm_params=kbl(rbind(anisvgt_Co,anisvgt_Ni)) %>% 
  kable_paper("striped") %>%
  pack_rows("Co", 1, 2) %>%
  pack_rows("Ni", 3, 4)

anis_vgm_params

# Analyzing the results of anisotropical variograms on can conclude 
# that both Co and Ni have similar anistropy ratios and the same angle, which could be
# the indicator of their codependency.

anis_kriged_Co = predict(anisgs_Co, newdata=xy_grid, debug.level=-1)
anis_kriged_Ni = predict(anisgs_Ni, newdata=xy_grid, debug.level=-1)

# plot
anis_kplot_Co=ggplot(anis_kriged_Co, aes(X, Y))+
  geom_raster(aes(fill=Co.pred))+
  coord_fixed()+
  theme_bw()+
  ggtitle("Results of anisotropical local kriging for Log(Co)")+
  scico::scale_fill_scico(palette = "roma",direction = -1)
anis_kplot_Ni=ggplot(anis_kriged_Ni, aes(X, Y))+
  geom_raster(aes(fill=Ni.pred))+
  coord_fixed()+
  theme_bw()+
  ggtitle("Results of anisotropical local kriging for Log(Ni)")+
  scico::scale_fill_scico(palette = "roma",direction =-1)
gridExtra::grid.arrange(anis_kplot_Co,anis_kplot_Ni)

# One can observe that anisotropy has almost no impact on results of 
# kriging. So we stick to isotropy.


## simulation ---------------------------------
# sequential Gaussian simulation: always with local neighbourhood!
# We obtain kriging mean and variance for each random point
sim_Co = predict(gs_Co, newdata=xy_grid, debug.level=-1,nsim=100) # Co
sim_Ni = predict(gs_Ni, newdata=xy_grid, debug.level=-1,nsim=100) # Ni


#ggplot version
simulationplot = function(i,sim_ds,varname){
  varname=as.character(varname)
  aux = ggplot(sim_ds, aes(X, Y))+
    geom_raster(aes(fill=sim_ds[,2+i]))+
    coord_fixed()+
    theme()+
    scico::scale_fill_scico(palette = "roma")+ggtitle(paste(sep="",i," Realisation of " , varname))
  aux
}
# for Co
manipulate(simulationplot(i,sim_Co,varname="Co"), i=slider(1,100))
# for Ni
manipulate(simulationplot(i,sim_Ni,varname="Ni"), i=slider(1,100))
## Categorical values


## Now we explore impact of the given lithology 


# Additionally to the chemical concentrations, the data set includes as well
# information about the lithology of the underlying bedrock, in the variable Lcode
par(mfrow=c(1,1))
ltypes = levels(factor(ds$Lcode))
summary(factor(ds$Lcode))
# Lcode data
par(mfrow=c(1,2))
ds$Lcode %>% plot(main="Lcode",col=1:length(levels(ds$Lcode)))
plot(Y~X, data=ds, bg=Lcode, col=NA, asp=1, pch=22, main="Ground truth")
legend("topleft", fill=1:length(levels(ds$Lcode)), legend=levels(factor(ds$Lcode)))
# There exist 4 types of lithology. The FZ type has the maximum occurrence 
# and is almost 8 times more frequent than the UM type with the lowest frequency 
# value of 52. SA also quite near to UM, but ~3.9 times more rare than SM and 7 
# times less frequent than FZ.Now we inspect the behavior and influence on chemical variables 
# log(Co) and log(Ni) using boxplots
par(mfrow=c(1,2))
boxplot(log(Co)~Lcode, data=ds, col=2:6, main ="log(Co) vs Lcode")
boxplot(log(Ni)~Lcode, data=ds, col=2:6,  main ="log(Ni) vs Lcode")

# One can observe the relatively pairwise similar behavior of FZ & UM and 
# SA & SM for Log(Co) and SA & SM do so in case of log(Ni). It appears in both 
# Co and Ni that samples taken on lithology FZ and UM tend to 
# show lower concentrations of these variables. Furthermore, FZ has a large variability
# and many outliers UM has the lowest occurrence in the data set.
# So one can consider SA and SM associated with CO and Ni.
# We carry on with the assumption of an isotropy and consider SA and Sm.









## MULTIVARIATE


# Here we inspect the interaction between all chemical variables in the data set .

# get names of chemical variables
chemvars = colnames(ds[,c(4,3,5,6,7,8)])
chemvars 
# create a multivariate variogram of chemical variables
gs_chem_all = make.gmMultivariateGaussianSpatialModel(
  data = log(ds[,chemvars ]),
  coords = ds[,c("X", "Y")],
  formula = ~1,
  nmax=40, #define to boost the calculation of cokriging
  omax = 20,
  #define to boost the calculation of cokriging
  maxdist=80#define to boost the calculation of cokriging
)

# change class
gs_chem_all = gs_chem_all %>% as.gstat()


## compute empirical variogram ---------------
vge_all = variogram(gs_chem_all, cutoff=225)
plot(vge_all)
## variogram model ---------------------------
vgt_all = vgm(psill=0.3, range=60, nugget=0.1, model="Gau")

gs_chem_all = gstat::fit.lmc(v=vge_all, g = gs_chem_all, model = vgt_all, correct.diagonal = 1.001)   
# "compositions" and "gstat" both bring a function with the same name; use package::function !!!
# attention: the multivariate case is different from the univariate one!
#    the result of the fitting is directly the expanded gstat object!
plot(vge_all, model=gs_chem_all$model)
par(mfrow=c(1,1))
#plot the grid and data points
plot(Y~X, data=ds, asp=1)
points(xy_grid, col=2, pch=".")

#CV
xv_chem_all = gstat.cv(gs_chem_all, nfold=nrow(ds))

## some diagnostic plots

# observations vs predictions
plot(observed~Co.pred, data=xv_chem_all, asp=1,main="observations vs predictions for Co")
abline(a=0, b=1, col=2, lwd=2)
#data points aren't concentrated along y=x
# histogram of the standardized residuals
hist(xv_chem_all$zscore, probability = T,main = "Histogram of the standardized residuals")  
curve(dnorm, from=-3, to=3, col=2, lwd=2, add=T)
# deviation to slightly larger values than 0

# normal qq plot of the standardized residuals
qqnorm(xv_chem_all$zscore,main = "Normal qq plot of the standardized residuals")
qqline(xv_chem_all$zscore, col=2, lwd=2)
#map of residuals
resid_map_Co=ggplot(data=xv_chem_all,aes(X, Y)) +
  geom_point(aes(size=residual,col=residual))+
  theme_bw() + 
  coord_fixed(ratio=1)+
  ggtitle("Map of residuals Co")+
  scico::scale_color_scico(palette = "bilbao")
resid_map_Co
#normality of residuals




#kriging of all vars
cokriged = predict(gs_chem_all, newdata=xy_grid, debug.level=-1)
#maps
cokriged %>% head()
logAlmap=ggplot()+
  geom_raster(data=cokriged ,aes( x=X, y=Y,fill=Al.pred))+
  coord_fixed()+
  theme()+
  scico::scale_fill_scico(palette = "bilbao")
logComap=ggplot()+
  geom_raster(data=cokriged ,aes( x=X, y=Y,fill=Co.pred))+
  coord_fixed()+
  theme()+
  scico::scale_fill_scico(palette = "vik")
logFemap=ggplot()+
  geom_raster(data=cokriged ,aes( x=X, y=Y,fill=Fe.pred))+
  coord_fixed()+
  theme()+
  scico::scale_fill_scico(palette = "lajolla")
logMgdmap=ggplot()+
  geom_raster(data=cokriged ,aes(x=X, y=Y,fill=Mg.pred))+
  coord_fixed()+
  theme()+
  scico::scale_fill_scico(palette = "imola")
logNidmap=ggplot()+
  geom_raster(data=cokriged ,aes(x=X, y=Y,fill=Ni.pred))+
  coord_fixed()+
  theme()+
  scico::scale_fill_scico(palette = "batlowW")
logFillermap=ggplot()+
  geom_raster(data=cokriged ,aes(x=X, y=Y,fill=Filler.pred))+
  coord_fixed()+
  theme()+
  scico::scale_fill_scico(palette = "batlowW")
gridExtra::grid.arrange(logAlmap, logComap,logFemap,logFillermap,logMgdmap,logNidmap,nrow=3,ncol=2) #set the number of plots

#simulation 
cosim_l = predict(gs_chem_all, newdata = xy_grid, debug.level = -1, nsim=100)
#ggplot version
allsimulationplot = function(i,sim_ds){
  a=ggplot()+
    geom_raster(data=sim_ds,aes( x=X, y=Y,fill=sim_ds[,2+i]))+
    coord_fixed()+
    theme_classic()+
    scico::scale_fill_scico(palette = "bilbao")+
    ggtitle(paste(sep="",i," Realisation of log Co " ))
  b=ggplot()+
    geom_raster(data=sim_ds ,aes( x=X, y=Y,fill=sim_ds[,2+i+100]))+
    coord_fixed()+
    theme_classic()+
    scico::scale_fill_scico(palette = "vik")+
    ggtitle(paste(sep="",i," Realisation of log  Al" ))
  c=ggplot()+
    geom_raster(data=sim_ds,aes( x=X, y=Y,fill=sim_ds[,2+i+200]))+
    coord_fixed()+
    theme_classic()+
    scico::scale_fill_scico(palette = "lajolla")+
    ggtitle(paste(sep="",i," Realisation of log Fe " ))
    d=ggplot()+
    geom_raster(data=sim_ds,aes(x=X, y=Y,fill=sim_ds[,2+i+400]))+
    coord_fixed()+
    theme_classic()+
    scico::scale_fill_scico(palette = "imola")+
  ggtitle(paste(sep="",i," Realisation of log Mg " ))
  e=ggplot()+
    geom_raster(data=sim_ds ,aes(x=X, y=Y,fill=sim_ds[,2+i+500]))+
    coord_fixed()+#(paste(".sim",i,sep=""))
    theme_classic()+
    scico::scale_fill_scico(palette = "batlowW")+
    ggtitle(paste(sep="",i," Realisation of log Ni " ))
  f=ggplot()+
    geom_raster(data=sim_ds ,aes(x=X, y=Y,fill=sim_ds[,2+i+300]))+
    coord_fixed()+#(paste(".sim",i,sep=""))
    theme_classic()+
    scico::scale_fill_scico(palette = "batlowW")+
    ggtitle(paste(sep="",i," Realisation of log Filler " ))
  gridExtra::grid.arrange(a, b,c,d,e,f,nrow=3,ncol=2) #set the number of plots
  
  
}
manipulate(allsimulationplot(i,cosim_l), i=slider(1,100))
## compositional analysis ----------------------------
library("compositions") # for compositional data analysis
par(mfrow=c(1,1))

#
# declare a subset of the columns to be compositional
vcomp0 = ds %>%  select(Fe, Ni, Co) %>% acomp%>%
 perturbe(c(Fe=1, Ni=1e2, Co=1e2)) 
#plot 
plot(vcomp0)
isoPortionLines(by=0.2, col="grey") 

# different units! Co, Ni in %, Fe. So we set them in  more comparable scale
summary(vcomp0)

library("gmGeostats")
# define the composition and the spatial coordinates
vcomp = ds %>%  select(Al,Fe,Filler,Mg, Ni, Co,) %>%  acomp %>% 
  perturbe(c( Al=1, Fe=1, Ni=1e2,Filler=1,Mg=1, Co=1e2))
X = ds %>% select(X, Y)
#One can observe a dominance of Fe and Ni over Co.
# swath
swath(vcomp, loc=X$X)  # quite flat --> constant mean
swath(vcomp, loc=X$Y)   # possible Y quadratic trend?



# define the gmSpatialModel object
gmv = make.gmCompositionalGaussianSpatialModel(
  data = vcomp, # use vicaria-comp
  coords = X,   # spatial coordinates
  V = "alr",
  # eventually, use alr logratios if needed
  #formula = ~1, # consider mean constant but unknown, ordinary cokriging
   formula = ~1+Y, # consider drift in Y direction
  nmax=60,
  omax = 12,
  maxdist =30#define to boost the calculation of cokriging
    
)




# compute logratio variograms
lrvg = gmGeostats::logratioVariogram(gmv,maxdist=600,nbins=15)
plot(lrvg)



# anisotropy
lrvga = gmGeostats::logratioVariogram(gmv, maxdist=600, nbins=10,
                                      azimuth=(0:5)*30, azimuth.tol=60)
par(mfrow=c(1,1))
plot(lrvga)
plot.new()#helps to identify directions, though not possible to incorporate these plots
legend("topleft", pch = 1:6,
       legend = paste((0:5)*30,"°"),col=RColorBrewer::brewer.pal(6,"Set1"),title = "Anisotropy angle")
# define a compositional linear model
lrmd = CompLinModCoReg(~nugget() +sph(111), comp=vcomp)#gaussian model produces singularity

# fit
lrmdf = gmGeostats::fit_lmc(v = lrvg, model=lrmd)#,  correct.diagonal=1.01
plot(lrvg, lrvg=vgram2lrvgram(lrmdf))



plot(lrvga)
# it is possible to plot as well an image of the anisotropy
image(lrvga)  # this works better if we make a whole circle:
gmGeostats::logratioVariogram(gmv, maxdist=600, nbins=10,
                  azimuth=(0:11)*30, azimuth.tol=60) %>% 
image
# Here one can observe orientations of variograms for each of chemical variables.
# Interesting thing is that orientations of low values(blue)  coincide for
# Co and Ni in SW-NE which is one more sign of their correlation and 
# orientation of Fe(high values) with CO and Ni in W-E.














##Categorical values--------------------
plot(Y~X, data=ds, bg=Lcode, col=NA, asp=1, pch=22, main="Lcode")
legend("topleft", fill=1:4, legend=levels(factor(ds$Lcode)))
#Now it's interesting to inspect interaction between lithological variables
isSM = ds$Lcode=="SM"

# check that what have we done is meaningful:
summary(isSM)
table(isSM, ds$Lcode)


# create a gstat object
gs_SM = gstat(id="SM", formula=isSM~1, locations = ~X+Y, 
             data=cbind(ds, isSM))






## indicator variography -----------------------
# empirical variogram
vg_SM = variogram(gs_SM, cutoff=225)
plot(vg_SM)

# model template
vgt_SM = vgm(psill=0.07, model="Exp", range=60, nugget=0.13)  # try with a periodic variogram
plot(vg_SM, model=vgt_SM)


# fit
vgm_SM = fit.variogram(vg_SM, model = vgt_SM)
plot(vg_SM, model=vgm_SM)


## grid and kriging -----------------------------
# # recreate gstat object with variogram model
# gs_SM = gstat(id="SM", formula=isSM~1, locations = ~X+Y, 
#              data=cbind(ds, isSM),
#              model=vgt_SM,
#              nmax=40
#               )

gs_lcode = gstat(id="SM", formula=(Lcode=="SM")~1, locations = ~X+Y, 
               data=ds,
               model=vgt_SM, nmax=60) %>% 
  gstat(id="FZ", formula=(Lcode=="FZ")~1, locations = ~X+Y, 
        data=ds,
        model=vgt_SM,nmax=60) %>% 
  gstat(id="UM", formula=(Lcode=="UM")~1, locations = ~X+Y, 
        data=ds,
        model=vgt_SM,nmax=60) %>% 
  gstat(id="SA", formula=(Lcode=="SA")~1, locations = ~X+Y, 
        data=ds,
        model=vgt_SM,nmax=60)  

vg_lcode = variogram(gs_lcode, cutoff=225) 
gs_lcode =gstat::fit.lmc(v=vg_lcode, model = vgm_SM,
                       g = gs_lcode, correct.diagonal = 1.001) # correct.diagonal needed to force positive-definiteness
#plot variograms
plot(vg_lcode, model=gs_lcode$model)
# cockriging 
cokr_all = predict(gs_lcode, newdata=xy_grid, debug.level = -1)#,indicators=T
# plot the results
a =ggplot(cokr_all, aes(X, Y))+
  geom_raster(aes(fill=SM.pred))+
  coord_fixed()+
  theme_classic()+
  scico::scale_fill_scico(palette = "roma")+ggtitle("SM")

b =ggplot(cokr_all, aes(X, Y))+
  geom_raster(aes(fill=FZ.pred))+
  coord_fixed()+
  theme_classic()+
  scico::scale_fill_scico(palette = "roma")+ggtitle("FZ")

c =ggplot(cokr_all, aes(X, Y))+
  geom_raster(aes(fill=UM.pred))+
  coord_fixed()+
  theme_classic()+
  scico::scale_fill_scico(palette = "roma")+ggtitle("UM")

d =ggplot(cokr_all, aes(X, Y))+
  geom_raster(aes(fill=SA.pred))+
  coord_fixed()+
  theme_classic()+
  scico::scale_fill_scico(palette = "roma")+ggtitle("SA")

gridExtra::grid.arrange(a,b,c,d,nrow=2, ncol=2)


#simulation for Lcodes
#simlcodes = predict(gs_lcode, newdata=xy_grid, debug.level = -1,nsim=100)
#WE use indicators
simlcodes = predict(gs_lcode, newdata=xy_grid, debug.level = -1,indicators=T,nsim=100)
library("manipulate")
all_lcode_simulationplot = function(i,sim_ds){
  a=ggplot()+
    geom_raster(data=sim_ds,aes( x=X, y=Y,fill=sim_ds[,2+i]))+
    coord_fixed()+
    theme_classic()+
    scico::scale_fill_scico(palette = "bilbao")+
    ggtitle(paste(sep="",i," Realisation of SM " ))
  b=ggplot()+
    geom_raster(data=sim_ds ,aes( x=X, y=Y,fill=sim_ds[,2+i+100]))+
    coord_fixed()+
    theme_classic()+
    scico::scale_fill_scico(palette = "vik")+
    ggtitle(paste(sep="",i," Realisation of FZ " ))
  c=ggplot()+
    geom_raster(data=sim_ds,aes( x=X, y=Y,fill=sim_ds[,2+i+200]))+
    coord_fixed()+
    theme_classic()+
    scico::scale_fill_scico(palette = "lajolla")+
    ggtitle(paste(sep="",i," Realisation of  UM" ))
  d=ggplot()+
    geom_raster(data=sim_ds,aes(x=X, y=Y,fill=sim_ds[,2+i+300]))+
    coord_fixed()+
    theme_classic()+
    scico::scale_fill_scico(palette = "bilbao")+
    ggtitle(paste(sep="",i," Realisation of SA " ))
  
  gridExtra::grid.arrange(a, b,c,d,nrow=2,ncol=2) #set the number of plots
  
  
}
manipulate(all_lcode_simulationplot(i,simlcodes), i=slider(1,100))
# We observe that the most probable area of SM overlap or even coincide with regions
# of the high values of Co and Ni.


## Lcode + Co

(mat = matrix(c(1,2,1,3), ncol=2))
layout(mat, height=c(2,3))  # alternative to par(mfrow=...)

(lvl = abbreviate(levels(ds$Lcode))) # useful later
# rock type vs logCo
plot(log(Co)~Lcode, data=ds, border=1:5)

## Apparently, Co is lowest in FZ, lower in UM, 
#  average in SM  and highest in
#  SA. There is a dependence between CO and lithology!
#  Given these results +  low number of data in Portlandian:

#convert categorical values to integers
ds$lithcode = 
  ds$Lcode %>% # take the original Lcode
  as.integer %>%   # convert the categories to integers (==position in the lvl vector)
       factor(labels=c("FZ", "SA", "SM", "UM")) 
# check the operation was well done
summary(ds$lithcode)
table(ds$lithcode, ds$Lcode)


contrasts(ds$lithcode) <- contr.treatment(levels(ds$lithcode), base=2)
# check the effect: (if necessary, read ?contr.treatment)
ds$lithcode


contr.treatment(levels(ds$lithcode), base=2) %>% 
  {lm(log(Co)~lithcode, data=ds, contrasts=.)} %>% summary
#  conclusions:
#    a. dependence exists (p-value of F-statistic <<< 0.01)
#    b. difference SA-FZ strongly significant (p-value t-statistic <<< 0.001)
#    c. difference SA-SM barely significant (0.01 < p-value t-statistic < 0.05 )
#    d. difference SP-UM strongly significant (p-value t-statistic <<< 0.001)


### option 2: consider rock-Co simultaneously ---------------


## multivariate model for Co and rock together
gs_Co4rock = gstat(id="Co", formula=log(Co)~1, locations = ~X+Y, 
                   data=ds, nmax=50) %>% 
  gstat(id="FZ", formula=(lithcode=="FZ")~1, locations = ~X+Y, 
        data=ds, nmax=50) %>% 
  gstat(id="SA", formula=(lithcode=="SA")~1, locations = ~X+Y, 
        data=ds, nmax=50) %>% 
  gstat(id="UM", formula=(lithcode=="UM")~1, locations = ~X+Y, 
        data=ds, nmax=50)  


par(mfrow=c(1,1))
# empirical variogram
vg_Co4rock = variogram(gs_Co4rock, cutoff=225)
plot(vg_Co4rock)


# variogram model
# let's start with the model for Ni, without nugget
# adding the wave model of Tuesday
vgt = vgm(model="Sph", range=100,nugget = 0.2 ,psill=0.05)
# you can also try switching in and out some of the components and slightly move ranges

gs_Co4rock = gstat::fit.lmc(v=vg_Co4rock, model=vgt, g=gs_Co4rock, correct.diagonal = 1.0001)

plot(vg_Co4rock, model=gs_Co4rock$model)





# vectors of x and y coordinates of the grid nodes,
#   covering the range of the data, and with a step 
#   allowing for interpolation between data
par(mfrow=c(1,1))
plot(Y~X, data=ds, asp=1)
xx = seq(from=rangesXY[1,"X"], to=rangesXY[2,"X"],by=10, length.out = length(ds$X))
yy = seq(from=rangesXY[1,"Y"], to=rangesXY[2,"Y"],by=10, length.out = length(ds$X))
# 4 points 
# grid definition must be the same in gs object Easting=x, Northing=y
xxyy_grid = expand.grid(X=xx, Y=yy)
points(xxyy_grid,col=2,pch=".")
xv_Co4rock = gstat.cv(gs_Co4rock)
# cokriging
cok_Co4rock = predict(gs_Co4rock, newdata=xxyy_grid, debug.level = -1)

myplot = function(x, variable, breaks=10, colorscale=RColorBrewer::brewer.pal(11, "Spectral")){
  # allow for giving specific breaks or the desired number of breaks
  if(length(breaks)==1){
    breaks = pretty(x[,variable], n = breaks)
  }
  # ensure that the color scale has always one color less than breaks
  if(length(breaks)-length(colorscale)!=1){
    colorscale = colorRampPalette(colorscale)(length(breaks)-1)
  }
  # plot
  cols = colorscale[cut(as.numeric(x[,variable]), breaks = breaks)]
  plot(Y~X, data=x, bg=cols, col=NA, asp=1, pch=22)
  invisible(list(breaks=breaks, color=colorscale)) # return invisibly the elements of the legend
}

par(mfrow=c(3,3))

#1
myplot(cok_Co4rock, variable = "Co.pred");
title("Co.pred")
#2
myplot(cok_Co4rock, variable = "FZ.pred",breaks =c(-2,0,0.25,0.5,1,2));
title("FZ.pred")
myplot(cok_Co4rock, variable = "SA.pred",breaks =c(-2,0,0.25,0.5,1,2));
title("SA.pred")
myplot(cok_Co4rock, variable = "UM.pred",breaks =c(-2,0,0.25,0.5,1,2));
title("UM.pred")
#red is negative or zero, orange 0 and 0.25, yellow transition zone, blue , 1 and 2 dark blue-the highest prob
#3
cok_Co4rock$SM.pred = 1 -cok_Co4rock$FZ.pred -cok_Co4rock$SA.pred-cok_Co4rock$UM.pred
myplot(cok_Co4rock, variable = "SM.pred",breaks =c(-2,0,0.25,0.5,1,2));
title("SM.pred")

#4

mostProb=cok_Co4rock[,c(5,7,9,17)] %>% apply(1, which.max)
cok_Co4rock$Lcode.pred =mostProb
myplot(cok_Co4rock, variable = "Lcode.pred",breaks =c(0,1.5,2.5,3.5,4.5),col=1:4);
title("The most probable")

myplot(ds, variable = "Lcode",breaks =c(0,1.5,2.5,3.5,4.5),col=1:4);
title("The truth")





# for Ni

## multivariate model for Ni and rock together
gs_Ni4rock = gstat(id="Ni", formula=log(Ni)~1, locations = ~X+Y, 
                   data=ds, nmax=50) %>% 
  gstat(id="FZ", formula=(lithcode=="FZ")~1, locations = ~X+Y, 
        data=ds, nmax=50) %>% 
  gstat(id="SA", formula=(lithcode=="SA")~1, locations = ~X+Y, 
        data=ds, nmax=50) %>% 
  gstat(id="UM", formula=(lithcode=="UM")~1, locations = ~X+Y, 
        data=ds, nmax=50)  


par(mfrow=c(1,1))
# empirical variogram
vg_Ni4rock = variogram(gs_Ni4rock, cutoff=225)
plot(vg_Ni4rock)


# variogram model
# let's start with the model for Ni, without nugget
# adding the wave model of Tuesday
vgt = vgm(model="Sph", range=100,nugget = 0.2 ,psill=0.05)
# you can also try switching in and out some of the components and slightly move ranges

gs_Ni4rock = gstat::fit.lmc(v=vg_Ni4rock, model=vgt, g=gs_Ni4rock, correct.diagonal = 1.0001)

plot(vg_Ni4rock, model=gs_Ni4rock$model)





# vectors of x and y coordinates of the grid nodes,
#   covering the range of the data, and with a step 
#   allowing for interpolation between data
par(mfrow=c(1,1))
plot(Y~X, data=ds, asp=1)
xx = seq(from=rangesXY[1,"X"], to=rangesXY[2,"X"], length.out = length(ds$X))
yy = seq(from=rangesXY[1,"Y"], to=rangesXY[2,"Y"], length.out = length(ds$X))
# 4 points 
# grid definition must be the same in gs object Easting=x, Northing=y
xxyy_grid = expand.grid(X=xx, Y=yy)
points(xxyy_grid,col=2,pch=".")
xv_Ni4rock = gstat.cv(gs_Ni4rock)
# cokriging
cok_Ni4rock = predict(gs_Ni4rock, newdata=xxyy_grid, debug.level = -1)

myplot = function(x, variable, breaks=10, colorscale=RColorBrewer::brewer.pal(11, "Spectral")){
  # allow for giving specific breaks or the desired number of breaks
  if(length(breaks)==1){
    breaks = pretty(x[,variable], n = breaks)
  }
  # ensure that the color scale has always one color less than breaks
  if(length(breaks)-length(colorscale)!=1){
    colorscale = colorRampPalette(colorscale)(length(breaks)-1)
  }
  # plot
  cols = colorscale[cut(as.numeric(x[,variable]), breaks = breaks)]
  plot(Y~X, data=x, bg=cols, col=NA, asp=1, pch=22)
  invisible(list(breaks=breaks, color=colorscale)) # return invisibly the elements of the legend
}

par(mfrow=c(3,3))

#1
myplot(cok_Ni4rock, variable = "Ni.pred");
title("Ni.pred")
#2
myplot(cok_Ni4rock, variable = "FZ.pred",breaks =c(-2,0,0.25,0.5,1,2));
title("FZ.pred")
myplot(cok_Ni4rock, variable = "SA.pred",breaks =c(-2,0,0.25,0.5,1,2));
title("SA.pred")
myplot(cok_Ni4rock, variable = "UM.pred",breaks =c(-2,0,0.25,0.5,1,2));
title("UM.pred")
#red is negative or zero, orange 0 and 0.25, yellow transition zone, blue , 1 and 2 dark blue-the highest prob
#3
cok_Ni4rock$SM.pred = 1 -cok_Ni4rock$FZ.pred -cok_Ni4rock$SA.pred-cok_Ni4rock$UM.pred
myplot(cok_Ni4rock, variable = "SM.pred",breaks =c(-2,0,0.25,0.5,1,2));
title("SM.pred")

#4

mostProb=cok_Ni4rock[,c(5,7,9,17)] %>% apply(1, which.max)
cok_Ni4rock$Lcode.pred =mostProb
myplot(cok_Ni4rock, variable = "Lcode.pred",breaks =c(0,1.5,2.5,3.5,4.5),col=1:4);
title("The most probable")

myplot(ds, variable = "Lcode",breaks =c(0,1.5,2.5,3.5,4.5),col=1:4);
title("The truth")
legend("bottomright",legend = levels(ds$Lcode), col = 1:4)








cok_Co4rocksim = predict(gs_Co4rock, newdata=xxyy_grid, debug.level = -1, nsim=100)

cok_Ni4rocksim = predict(gs_Ni4rock, newdata=xxyy_grid, debug.level = -1, nsim=100)





















