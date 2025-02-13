library(basemaps)
library(biscale)
library(cowplot) #combine map and legend bivariate choropleth
library(ggmap)
library(ggspatial) #expand spatial capability of ggplot
library(here) # set workind directory
library(MARSS) # runs multivariate autoregressive state space models
library(marssTMB) # runs MARSS models with TMB (faster)
library(panelr) # set data wide
library(raster)
library(readxl) # read excel sheets
library(sf) #vector data
library(spData) #datasets
library(tidyverse) #data manipulation

#set loc
here::i_am("code/STKchl.R")

# set defaults for basemaps
basemaps::set_defaults(map_service = "osm", map_type = "streets_de")

# read in and clean district data
comdists <- read_sf(here(
  "data", "dat_STKchl", "SEAK_ComDists", "Southeast_Salmon_Districts_GCS_WGS1984.shp")
)
class(comdists)
comdists[15, 9] = "115"
  # district number coded wrong in raw data
comdists$District <- as.numeric(comdists$DISTRICT_C)
  # rename district variable for merge
comdists <- comdists[-c(1:2, 9), ]
  # throw out entrance observation

# read in stream spatial data
streams_map <- read_sf(here(
  "data", "dat_STKchl", "SEAK_Streams", "SEAK_streams.shp")
)
class(streams_map)

# read in and clean stream data
streams <- read_excel(here(
  "data", "dat_STKchl", "awc_stream.xlsx"), col_names = TRUE)
streams <- streams[-c(1, 4:7)]
  # drop extraneous variables from streams
names(streams)[names(streams) == "SHAPE_Length"] <- "LENGTH"
  # rename vars in streams.df 
streams$STREAM_NO <- substr(streams$AWC_CODE, 1, 12)
  # create new var w/ streamID substring
streams <- streams[-c(1:2)]
  # drop extraneous variables from streams.df
streams$STREAM_NO <- substr(streams$STREAM_NO, 1, 11)
streams$STREAM_NOstart <- substr(streams$STREAM_NO, 1, 7)
streams$STREAM_NOend <- substr(streams$STREAM_NO, 9, 11)
streams$STREAM_NO <- paste(streams$STREAM_NOstart, streams$STREAM_NOend, sep="")
streams <- streams[-c(3:4)]
  # cut extraneous characters (8 and 12) out of strings for matching
streams <- streams %>%
  group_by(STREAM_NO) %>%
  summarize(LENGTH = sum(LENGTH))
  # collapse around stream_NO
streams$District <- substr(streams$STREAM_NO, 1, 3)
  # get district variable for streams
streams <- streams[streams$District < 116,]
  # drop if district > 115
streams$District <- as.numeric(streams$District)
  # Stream # and district as numeric

# crop little map of SEAK
alaska <- alaska %>% st_transform(., crs = 3338)
st_crs(alaska)
st_bbox(alaska)
st_bbox(streams_map)
seak <- st_crop(alaska, xmin = 910000, ymin = 736000, xmax = 1490000, ymax = 1220000)

# read in and clean SEAK pink counts
pinks <- read_excel(here("data", "dat_STKchl", "adfg_pink.xlsx"), col_names = TRUE)
pinks$ct <- pinks$PEAK_COUNT
pinks <- pinks[-c(4:10)]
  # drop extraneous variables from pinks.df
class(pinks)
pinksE <- pinks %>% filter(YEAR %% 2 == 0)
  # data transform - split into odd/even runs

# merge stream and pink data
merge <- merge(pinksE, streams, by=c("STREAM_NO", "District"))

# group length and ct by district and year
grouped <- merge %>%
  group_by(District, YEAR) %>%
  summarize(ct = sum(ct), length = sum(LENGTH))

grouped$LENGTHkm <- (grouped$length/1000)
grouped$ESCbyKM <- (grouped$ct/grouped$LENGTHkm)
  # create escapement per km
  grouped <- grouped[-c(3:5)]
  # drop extraneous variables from grouped

# keep grouped df as esc
esc <- grouped
esc <- grouped %>%
  group_by(District) %>%
  summarize(avgESCbyKM = mean(ESCbyKM))
  
# natural log of count variable
grouped$ct <- log(grouped$ESCbyKM)

#standardize ln(ct) at district level
grouped <- grouped %>% 
  group_by(District) %>% 
  mutate(standard_ct=scale(ct))
grouped <- grouped[-c(3, 4)]

# set data wide (rows = districts, columns = year)
wide <- panel_data(grouped, id = District, wave = YEAR)
wide <- widen_panel(wide, separator = "_")

# prepare MARSS model
dat <- data.matrix(wide[2:ncol(wide)])
  # convert counts to matrix
years <- names(wide)
years <- years[-1]
years <- substring(years, first=13, last=16)
n <- nrow(wide)
  # grab obs & years
b.model <- "identity"
u.model <- matrix(c(paste0("u", seq(n))))
q.model <- "diagonal and unequal"
z.model <- "identity"
a.model <- "zero"
r.model <- "diagonal and equal"
x0.model <- "unequal"
v0.model <- "zero"
  # specify arguments for MARSS model
model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0)
  # build model list

# specify MARSS model
ptm <- proc.time()
ss <- MARSS(dat, model = model.list)
proc.time()[3] - ptm

# grab variance terms
var.est <- ss$par[[6]]
head(var.est)
esc$vari <- var.est[,1]
head(esc)

# spatial join
comdists_pinks <- full_join(comdists, esc, by = "District")
class(comdists_pinks)
head(comdists_pinks)

# grab basemap
seak_map <- basemap_terra(comdists_pinks)
seak_map
class(seak_map)

# # change crs to match basemap
comdists_pinks <- comdists_pinks %>% st_transform(., crs = 3857)

# seak basemap
# st_bbox(seak)
# ggmap::register_stadiamaps("0bc59edc-8bea-46fb-bab7-63e88f62981c", write = FALSE)
# seak_map <- get_stadiamap(bbox = c(left = -138, bottom = 54.5, right = -130, top = 59.5), source = "stadia", zoom = 7, maptype = "stamen_terrain")
# ggmap(seak_map)

# define bivariate data
bi_dat <- bi_class(
  comdists_pinks,
  x = avgESCbyKM,
  y = vari,
  style = "quantile",
  dim = 4
)

# call palette
bipal = "Brown2"

# generate map
bi_map <- ggplot() +
    layer_spatial(data = seak_map) +
    geom_sf(
    data = bi_dat,
    mapping = aes(fill = bi_class), color = "black",
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  bi_scale_fill(pal = bipal, dim = 4) +
  labs(title = "Avergae pink salmon escapement v. year-to-year yariability", 
       subtitle = "SE Alaska Commercial Fishing Districts, 1960 - 2022") +
  bi_theme()
bi_map

bi_leg <- bi_legend(
  pal = bipal,
  dim = 4,
  xlab = "Avg. escapement/km",
  ylab = "Year-to-year variance",
  size = 8
)
bi_leg

bi_pleth <- ggdraw() +
  draw_plot(bi_map, 0, 0, 1, 1) +
  draw_plot(bi_leg, 0.66, 0.59, 0.29, 0.29)
bi_pleth 

ggsave(here("output", "STKcho.png"), plot=bi_pleth, device="png", dpi=300, width = 15, height = 25)
