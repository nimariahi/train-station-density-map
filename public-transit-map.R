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
# library(maptools)
# library(utils)

#' 
#' Quantize values based on percentile thresholds
#' 
#' This can be used to quantize a list of continuous values, e.g. to reduce the 
#' number of levels for mapping.
#' 
#' @param vals A list/vector of numeric values
#' @param ths Percentile thresholds defining quantization levels.
#'   
#' @return Named list, where \code{$qvals} are the quantized values, 
#'   \code{limits} are the quantization limits, and \code{LabelStr} is a string
#'   describing the quantizatio ranges (useful to label a colorbar).
#'   
#'   
quantileQuantize <- function(vals,ths=c(0,0.25,.5,.75,1)){
  
  # Create field with percentile entries
  qs <- quantile(vals,ths,na.rm=T)
  
  densQ <- vals
  
  for (i in 1:length(qs)-1){
    sel <- vals>=qs[i] & vals<qs[i+1]
    densQ[sel] <- i
  }
  densQ[vals>=qs[length(qs)]] = length(qs)-1
  
  qs <- as.numeric(qs)
  Q <- (cbind(qs[1:length(qs)-1],qs[2:length(qs)]))
  
  # Given a 2-el numeric vector return a dash-separated string with the two numbers
  a.lab <- function(a,fmt='%f') {
    fmt_ <- sprintf('%s-%s',fmt,fmt)
    sprintf(fmt_,a[1],a[2])
  }
  
  label.str <- apply(Q,1,a.lab,'%04.2f')

  return(list('qvals'=densQ,'limits'=cbind(ths[1:length(ths)-1],ths[2:length(ths)]),'LabelStr'=label.str))
  
}


# # Check out list of CRS
# EPSG <- make_EPSG() # create data frame of available EPSG codes 
# EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code
# tmp <- EPSG[grepl("UTM", EPSG$note), ] # search for WGS 84 code


# # Look up proj.4 strings for different projections
# EPSG <- make_EPSG()
# EPSG[grep("1903", EPSG$note, ignore.case=TRUE), 1:2]


data.dir <- 'HST_MGMD_2015-12-13/'

out.dir <- './maps/'
dir.create(out.dir,showWarnings = FALSE)

# Filename of public transit stations
fname <- 'PointExploitation.csv'

# Shape file location of administrative regions in CH
shp.dir <- 'g3g09_shp_090626/'


# Read shapefile (G3G09 for Gemeinden, G3B09 for Bezirke), this should be stored in CH1903 grid already
groupname <- 'KURZ' # For cantons
chmap <- readOGR(dsn=shp.dir, layer="G3B09")

# Add a new column termed "id" composed of the rownames of data
chmap@data$id <- rownames(chmap@data)

## Add area of each polygon to SP data frame
chmap@data$Area <- gArea(chmap,byid=TRUE)/1e6

# Load Kanton boundaries, to be used for plotting later
chmapK <- readOGR(dsn=shp.dir, layer="G3K09")
chmapK@data$id <- rownames(chmapK@data)


# Try to reproject to Swissgrid (which does not seem to be the CH1903 system)
# chmap_ch1903 <- spTransform(chmap, CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"))


# Read public transit stations CSV file (presumably in swiss grid coordinate system)
dat <- read.csv(paste(data.dir,fname,sep=''),sep=',')

# Create a SpatialPointsDataFrame object from station locations (projected coordinate object)
xy <- dat[,c('y_Coord_Est','x_Coord_Nord')]
proj4str <- proj4string(chmap)
stations.spdf <- SpatialPointsDataFrame(coords = xy, data = dat, proj4string = CRS(proj4str))


# Count number of stations in each polygon
# This solution is very condensed and uses an aggregation by polygons in 'chmap'
chmap$Nstations <- aggregate(x=stations.spdf["Nom"],by=chmap,FUN=length)$Nom

# Compute station density
chmap$StatDens <- chmap@data$Nstations / chmap@data$Area


# # Bar plot of station density, sorted in descending order by density
# p <- ggplot(pubStats,aes(x=reorder(KURZ,-StatDens),y=StatDens)) +
#   geom_bar(stat='identity') + xlab('Kanton') + ylab('Haltestellendichte [/km^2]')
# 
# ggsave(paste(out.dir,"Haltestellendichte",'.pdf',sep=''),plot=p,units = 'cm',width = 18,height = 8) 


# Prepare the map for usage by ggplot2
chmap_df <- fortify(chmap)
chmap_df <- merge(chmap_df,chmap@data,by='id')

chmap_df <- chmap_df[!is.na(chmap_df),]

# Do the same for the Kanton boundaries
chmapK.df <- fortify(chmapK)
chmapK.df <- merge(chmapK.df,chmapK@data,by='id')


# Create a quantized colorscale
out <- quantileQuantize(chmap_df$StatDens)

# Save quantized values as factors
chmap_df$densQ <- factor(out$qvals)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Plot cantons, stations, and canton fill with Nstations
p2 <- ggplot()
p2 <- p2 + geom_polygon(data=chmap_df,aes(x=long/1000,y=lat/1000,group=group,fill=densQ),colour='white',linetype=1,size=0.25)
p2 <- p2 + geom_polygon(data=chmapK.df,aes(x=long/1000,y=lat/1000,group=group),colour='white',linetype=1,size=1,alpha=0.2)
# geom_point(data=dat,aes(x=y_Coord_Est,y=x_Coord_Nord),size=0.005)
p2 <- p2 + coord_equal() +
  # scale_x_continuous(breaks=seq(500000,800000,50000)) +
  # scale_y_continuous(breaks=seq(100000,300000,50000)) +
  # scale_fill_discrete(name='[/km^2]',breaks=levels(chmap_df$densQ),labels=out$LabelStr)
  scale_fill_manual(name='[/km^2]',values=cbPalette,breaks=levels(chmap_df$densQ),labels=out$LabelStr)
  # scale_fill_distiller(guide='legend',palette = "Spectral")
  # scale_fill_distiller(guide='legend',name='Stationsdichte')
p2 <- p2 + ggtitle('Haltestellendichte CH') +
  xlab('x [km]') + ylab('y [km]')
p2 <- p2 + theme(
  plot.title=element_text(size=16,face='bold.italic'),
  axis.title.x=element_text(size=14,face='bold'),
  axis.title.y=element_text(size=14,face='bold')
)

ggsave(paste(out.dir,"ch-map-ggplot.pdf",sep=''), width=12.5, height=8.25, dpi=72) 









