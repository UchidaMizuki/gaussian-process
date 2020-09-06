source("src/setup.R")

# land price --------------------------------------------------------------

# 公示地価
land_price_posted <- list.files("raw/land_price/L01-20_GML",
                                pattern = "\\.shp$",
                                full.names = T) %>% 
  read_sf() %>% 
  st_transform(JGD2000_UTM_zone_53N) %>% 
  select(L01_006 ,L01_024) %>% 
  rename(price_per_m2 = L01_006,
         area_m2 = L01_024) %>% 
  mutate(across(c(price_per_m2, area_m2), 
                parse_number),
         price = price_per_m2 * area_m2)

land_price_posted %>% 
  write_sf("dat/land_price_posted.gpkg")

land_price_posted %>% 
  mutate(X = st_coordinates(.)[, "X"],
         Y = st_coordinates(.)[, "Y"]) %>% 
  st_drop_geometry() %>% 
  write_csv("dat/land_price_posted.csv")

# sample <- .land_price %>% 
#   st_drop_geometry() %>% 
#   head(1) %>% 
#   rowid_to_column() %>% 
#   pivot_longer(-rowid)
