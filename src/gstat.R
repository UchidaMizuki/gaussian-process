source("src/setup.R")

# gstat -------------------------------------------------------------------

land_price_posted <- read_sf("dat/land_price_posted.gpkg")

.grid <- land_price_posted %>% 
  # st_bbox() %>% 
  # st_as_sfc() %>% 
  st_make_grid(cellsize = 10 ^ 4,
               what = "centers")
  # st_as_sf() %>% 
  # mutate(X = st_coordinates(.)[, "X"],
  #        Y = st_coordinates(.)[, "Y"]) %>% 
  # st_drop_geometry()
# gridded(.grid) <- ~ X + Y

land_price_posted %>% 
  ggplot() +
  geom_sf(data = .grid,
          size = 0.5,
          color = "red") +
  geom_sf(size = 0.5)

.variogram <- variogram(log(price_per_m2) ~ 1,
                        data = land_price_posted,
                        cutoff = 10 ^ 5)
plot(.variogram)

.fit.variogram <- fit.variogram(.variogram, 
                                model = vgm("Mat"))
plot(.variogram, .fit.variogram)

.krige <- krige(log(price_per_m2) ~ 1,
                land_price_posted,
                land_price_posted,
                .fit.variogram,
                maxdist = 10 ^ 5)

.krige %>% 
  filter(is.finite(var1.var)) %>% 
  mutate(var1.var = gtools::quantcut(var1.var, 10)) %>% 
  ggplot(aes(color = var1.var)) +
  geom_sf() +
  scale_color_viridis_d()

# library(sp)
# data(meuse)
# coordinates(meuse) = ~x+y
# data(meuse.grid)
# gridded(meuse.grid) = ~x+y
# m <- vgm(.59, "Sph", 874, .04)
# # ordinary kriging:
# x <- krige(log(zinc)~1, meuse, meuse.grid, model = m)
# spplot(x["var1.pred"], main = "ordinary kriging predictions")
# spplot(x["var1.var"],  main = "ordinary kriging variance")
# # simple kriging:
# x <- krige(log(zinc)~1, meuse, meuse.grid, model = m, beta = 5.9)
# # residual variogram:
# m <- vgm(.4, "Sph", 954, .06)
# # universal block kriging:
# x <- krige(log(zinc)~x+y, meuse, meuse.grid, model = m, block = c(40,40))
# spplot(x["var1.pred"], main = "universal kriging predictions")
# 
# # krige0, using user-defined covariance function and multiple responses in y:
# # exponential variogram with range 500, defined as covariance function:
# v = function(x, y = x) { exp(-spDists(coordinates(x),coordinates(y))/500) }
# # krige two variables in a single pass (using 1 covariance model):
# y = cbind(meuse$zinc,meuse$copper,meuse$lead,meuse$cadmium)
# x <- krige0(zinc~1, meuse, meuse.grid, v, y = y)
# meuse.grid$zinc = x[,1]
# spplot(meuse.grid["zinc"], main = "zinc")
# meuse.grid$copper = x[,2]
# spplot(meuse.grid["copper"], main = "copper")
# 
# # the following has NOTHING to do with kriging, but --
# # return the median of the nearest 11 observations:
# x = krige(zinc~1, meuse, meuse.grid, set = list(method = "med"), nmax = 11)
# # get 25%- and 75%-percentiles of nearest 11 obs, as prediction and variance:
# x = krige(zinc~1, meuse, meuse.grid, nmax = 11, 
#           set = list(method = "med", quantile = 0.25))
# # get diversity (# of different values) and mode from 11 nearest observations:
# x = krige(zinc~1, meuse, meuse.grid, nmax = 11, set = list(method = "div"))
