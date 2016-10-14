#
# Plot density of Swiss train stations as a function of political entity
# 
# 2016-May, Nima Riahi
#

rm(list = ls())

library(rgdal)
library(rgeos)  # To compute areas
library(ggplot2)
library(plyr)

source("utils.R")


# # Check out list of CRS
# EPSG <- make_EPSG() # create data frame of available EPSG codes 
# EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code
# tmp <- EPSG[grepl("UTM", EPSG$note), ] # search for WGS 84 code


# # Look up proj.4 strings for different projections
# EPSG <- make_EPSG()
# EPSG[grep("1903", EPSG$note, ignore.case=TRUE), 1:2]

# Train station data location
data.dir <- 'haltestellen-offentl-verk/'

# Shape file location of administrative regions in CH
shp.dir <- 'admin-regions/gis-data/'


# Directory to put maps in
out.dir <- './maps/'
dir.create(out.dir,showWarnings = FALSE)


# Read shapefile (G3G09 for Gemeinden, G3B09 for Bezirke), this should be stored
# in the CH1903 projection already
groupname <- 'KURZ' # For cantons
chmap <- readOGR(dsn=shp.dir, layer="G3B09")

# Add a new column termed "id" composed of the rownames of data
chmap@data$id <- rownames(chmap@data)

# Add area of each polygons to SP data frame
chmap@data$Area <- gArea(chmap,byid=TRUE)/1e6

# Load Kanton boundaries, to be used for plotting later
chmapK <- readOGR(dsn=shp.dir, layer="G3K09")
chmapK@data$id <- rownames(chmapK@data)


# Try to reproject to Swissgrid (which does not seem to be the CH1903 system)
# chmap_ch1903 <- spTransform(chmap, CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"))


# Read public transit stations CSV file (presumably in swiss grid coordinate
# system)
fname <- 'PointExploitation.csv'  # Filename of public transit stations
dat <- read.csv(paste(data.dir,fname,sep=''),sep=',')

# Create a SpatialPointsDataFrame object from station locations and project to
# same coordiante system as the polygons
xy <- dat[,c('y_Coord_Est','x_Coord_Nord')]
proj4str <- proj4string(chmap)
stations.spdf <- SpatialPointsDataFrame(coords = xy, data = dat, proj4string = CRS(proj4str))


# Count number of stations in each polygon. This solution is very condensed and
# uses an aggregation by polygons in 'chmap'
chmap$Nstations <- aggregate(x=stations.spdf["Nom"],by=chmap,FUN=length)$Nom

# Compute station density
chmap$StatDens <- chmap@data$Nstations / chmap@data$Area


# # Bar plot of station density, sorted in descending order by density
# p <- ggplot(pubStats,aes(x=reorder(KURZ,-StatDens),y=StatDens)) +
#   geom_bar(stat='identity') + xlab('Kanton') + ylab('Haltestellendichte [/km^2]')
# 
# ggsave(paste(out.dir,"Haltestellendichte",'.pdf',sep=''),plot=p,units = 'cm',width = 18,height = 8) 


# Prepare the map for plotting by ggplot2
chmap.df <- fortify(chmap)
chmap.df <- merge(chmap.df,chmap@data,by='id')

# Remove NA entries
chmap.df <- chmap.df[!is.na(chmap.df),]

# Do the same for the Kanton boundaries
chmapK.df <- fortify(chmapK)
chmapK.df <- merge(chmapK.df,chmapK@data,by='id')


# Create a quantized colorscale
out <- quantileQuantize(chmap.df$StatDens)

# Save quantized values as factors
chmap.df$densQ <- factor(out$qvals)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Plot cantons, stations, and canton fill with Nstations
p2 <- ggplot()
p2 <- p2 + geom_polygon(data=chmap.df,aes(x=long/1000,y=lat/1000,group=group,fill=densQ),colour='white',linetype=1,size=0.25)
p2 <- p2 + geom_polygon(data=chmapK.df,aes(x=long/1000,y=lat/1000,group=group),colour='white',linetype=1,size=1,alpha=0.2)
# geom_point(data=dat,aes(x=y_Coord_Est,y=x_Coord_Nord),size=0.005)
p2 <- p2 + coord_equal() +
  # scale_x_continuous(breaks=seq(500000,800000,50000)) +
  # scale_y_continuous(breaks=seq(100000,300000,50000)) +
  # scale_fill_discrete(name='[/km^2]',breaks=levels(chmap.df$densQ),labels=out$LabelStr)
  scale_fill_manual(name='[/km^2]',values=cbPalette,breaks=levels(chmap.df$densQ),labels=out$LabelStr)
  # scale_fill_distiller(guide='legend',palette = "Spectral")
  # scale_fill_distiller(guide='legend',name='Stationsdichte')
p2 <- p2 + ggtitle('Haltestellendichte CH') +
  xlab('x [km]') + ylab('y [km]')
p2 <- p2 + theme(
  plot.title=element_text(size=16,face='bold.italic'),
  axis.title.x=element_text(size=14,face='bold'),
  axis.title.y=element_text(size=14,face='bold')
)

ggsave(paste(out.dir,"station-density-ch-map.pdf",sep=''), width=12.5, height=8.25, dpi=72) 






