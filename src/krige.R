library(tidyverse)
library(sf)
library(units)
library(rstan)
library(car)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

theme_set(theme_light())

JGD2000 <- 4612
JGD2000_UTM_zone_53N <- 3099

# sample
land_price <- list.files("raw/land_price",
                         pattern = "\\.csv$",
                         full.names = T) %>% 
  read_csv() %>% 
  drop_na()

land_price_sf <- land_price %>% 
  st_as_sf(coords = c("X", "Y"),
           crs = JGD2000) %>% 
  st_transform(JGD2000_UTM_zone_53N)

# 香川県
land_price_sf %>% 
  filter(pref == "香川県") %>% 
  ggplot(aes(size = price,
             color = price)) +
  geom_sf() +
  scale_fill_viridis_c()

# grid
land_price_sf %>% 
  filter(pref == "香川県") %>%
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_make_grid(cellsize = 5 * 10 ^ 3,
               what = "centers") %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = land_price_sf %>% 
            filter(pref == "香川県"),
          color = "red")

.land_price <- land_price %>% 
  filter(pref == "香川県") %>% 
  st_as_sf(coords = c("X", "Y"),
           crs = JGD2000) %>% 
  st_transform(JGD2000_UTM_zone_53N) %>% 
  mutate(X = st_coordinates(.)[, "X"],
         Y = st_coordinates(.)[, "Y"],
         price_per_m2 = price / area_m2) %>% 
  st_drop_geometry() %>% 
  sample_n(5 * 10 ^ 2)

.grid <- land_price_sf %>% 
  filter(pref == "香川県") %>%
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_make_grid(cellsize = 5 * 10 ^ 3,
               what = "centers") %>% 
  st_as_sf() %>% 
  mutate(X = st_coordinates(.)[, "X"],
         Y = st_coordinates(.)[, "Y"]) %>% 
  st_drop_geometry()

# .land_price %>% 
#   st_as_sf(coords = c("X", "Y"),
#            crs = JGD2000_UTM_zone_53N) %>% 
#   ggplot() +
#   geom_sf()

# .lambda <- .land_price %>% 
#   .$price_per_m2 %>% 
#   powerTransform() %>% 
#   .$lambda

.data <- list(N1 = nrow(.land_price),
              x1 = .land_price[c("X", "Y")],
              y1 = .land_price %>% 
                .$price_per_m2 %>% 
                bcPower(.lambda),
              N2 = nrow(.grid),
              x2 = .grid[c("X", "Y")])

.stan_model <- stan_model("src/gp_predict.stan")

.sampling <- sampling(.stan_model,
                      data = .data,
                      seed = 123)

# .vb <- vb(.stan_model,
#           data = .data,
#           seed = 123,
#           tol_rel_obj = 10 ^ -4)

.sampling %>% 
  summary() %>% 
  .$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  filter(str_detect(rowname, "^y2"))
  # .$mean
  