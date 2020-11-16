# HEADER --------------------------------------------------------------------------

# Project: The influence of temperature on the seasonality of historical plague
# Author: Fabienne Krauer, University of Oslo
# Contact: fabienne.krauer@ibv.uio.no
# Created: 12.12.2019
# Last updated: 27.10.2020



# HOUSEKEEPING ------------------------------------------------------------

rm(list=ls())
set.seed(42)

library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(reshape2)
library(lubridate)
library(raster)

Sys.setlocale("LC_TIME", "English")


# 1. PLAGUE DATA ------------------------------------------------------------

rawdata <- read_csv("input/rawdata.csv")

# convert dates to gregorian wherever needed and possible
rawdata$date <- rawdata$dateorig
rawdata$date <- ifelse(rawdata$interval %in% c("daily", "weekly", "biweekly"), rawdata$date, NA)
rawdata$date <- as.Date(rawdata$date, format="%Y-%m-%d")
rawdata$date <- ifelse(rawdata$calendar=="julian"  & rawdata$startyear<1700, rawdata$date + 10,
                       ifelse(rawdata$calendar=="julian" & rawdata$startyear>=1700 & rawdata$startyear<1800, rawdata$date + 11,
                              ifelse(rawdata$calendar=="julian" & rawdata$startyear>=1800, rawdata$date + 12, 
                                     rawdata$date)))
rawdata$date <- as.Date(rawdata$date, format="%Y-%m-%d", origin="1970-01-01")
foo <- ifelse(is.na(rawdata$date), rawdata$dateorig, as.character(rawdata$date))
foo <- plyr::ldply(strsplit(foo, "-"), rbind)
colnames(foo) <- c("year", "month", "day")
foo[] <- lapply(foo, function(x) as.integer(as.character(x)))
rawdata <- cbind(rawdata, foo)
rm(foo)

rawdata$century <- ifelse(rawdata$startyear<1400, "14th", 
                           ifelse(rawdata$startyear>=1400 & rawdata$startyear<1500, "15th", 
                                  ifelse(rawdata$startyear>=1500 & rawdata$startyear<1600, "16th",
                                         ifelse(rawdata$startyear>=1600 & rawdata$startyear<1700, "17th",
                                                ifelse(rawdata$startyear>=1700 & rawdata$startyear<1800, "18th", "19th")))))

rawdata <- rawdata %>% dplyr::group_by(id) %>% dplyr::mutate(multipeak=ifelse(min(peakid)!=max(peakid),1,0))
saveRDS(rawdata, "output/rawdata.rds")


# 1.2 Aggregate datasets for analysis and plotting

# monthly aggregated data
monthly <- rawdata %>% dplyr::group_by(peakid, month, year) %>% 
                          dplyr::summarise(n=sum(n, na.rm=T), id=id[1], type=type[1], place=place[1], country=country[1], 
                                           startyear=startyear[1], lat=lat[1], lon=lon[1], population=population[1], multipeak=multipeak[1], obs=obs[1], 
                                           peakobs=peakobs[1], values=values[1], complete=complete[1], century=century[1])
monthly <- monthly %>% dplyr::group_by(id)  %>% dplyr::arrange(obs) %>% dplyr::mutate(obs=1:n())
monthly <- monthly %>% dplyr::group_by(peakid) %>% dplyr::arrange(peakobs) %>% dplyr::mutate(peakobs=1:n(), peak=ifelse(row_number()==min(which(n==max(n, na.rm=T))),1,0))

# Calculate duration, cumulative and peak cases and final sizes
monthly <- monthly %>% dplyr::group_by(id) %>% # calculate duration
  dplyr::mutate(duration=ifelse(type=="plague mortality" & complete=="yes", max(obs, na.rm=T), NA),
                ncumul=ifelse(type=="plague mortality" & complete=="yes" & values=="counts", sum(n, na.rm=T), NA),
                npeak=ifelse(values=="counts", max(n, na.rm=T), NA))
monthly$fs <- (monthly$ncumul/0.66)/monthly$population
monthly$fs <- ifelse(monthly$fs>1 & !is.na(monthly$fs),1,monthly$fs)
saveRDS(monthly, "output/data_all_monthly.rds")


# weekly aggregatd dataset 
weekly <- rawdata %>% dplyr::filter(interval %in% c("daily", "weekly"))

ids <- sort(unique(weekly$id))

out <- vector("list", length(ids))
for (i in ids) {
  subset <- weekly[weekly$id==i,c("id", "date")]
  dates <- data.frame("id"=i, "date"=seq(from=min(subset$date), to=max(subset$date)+7, by=1))
  dates$woy <- isoweek(dates$date)
  out[[which(ids==i)]] <- dates
}

out <- plyr::ldply(out, "rbind")
weekly <- merge(weekly, out, by=c("id", "date"), all=T)

weekly <- weekly %>% dplyr::group_by(id) %>% dplyr::arrange(date) %>% dplyr::mutate(obs=1:n())

# Make new identifier for each isoweek by ID
weekly <- weekly[order(weekly$id, weekly$date),]
out <- vector("list", length(ids))
for (i in ids) {
  
  subset <- weekly[weekly$id==i,c("id", "date", "woy", "obs")]
  count <- 1
  subset$woyobs <- NA
  subset$woyobs[1] <- count
  for (j in 2:nrow(subset)) {
    
      if (subset$woy[j]!=subset$woy[j-1]) {
        count <- count+1
        subset$woyobs[j] <- count
      } else {
        subset$woyobs[j] <- count
      }
  }
  out[[which(ids==i)]] <- subset
}

out <- plyr::ldply(out, "rbind")

weekly <- merge(weekly, out, by=intersect(names(weekly), names(out)), all=T)

weekly <- weekly %>% dplyr::group_by(id) %>% dplyr::arrange(type) %>% 
  dplyr::mutate(country=country[1], place=place[1], 
                startyear=startyear[1], endyear=endyear[1], type=type[1],
                interval=interval[1], source=source[1],
                calendar=calendar[1], sourcetype=sourcetype[1], values=values[1],
                lat=lat[1], lon=lon[1], population=population[1], complete=complete[1],
                century=century[1], multipeak=multipeak[1])

# Collapse by isoweek
weekly <- weekly %>% dplyr::group_by(id, woyobs) %>% dplyr::arrange(date) %>% 
  dplyr::summarise(obs=min(obs, na.rm=T), peakid=min(peakid, na.rm=T), 
                   peakobs=min(peakobs, na.rm=T), woy=min(woy), date=max(date),
                   country=country[1], place=place[1], n=sum(n, na.rm=T), 
                  peakno=min(peakno, na.rm=T), type=type[1], interval=interval[1],
                  lat=lat[1], lon=lon[1], population=population[1], 
                  source=source[1],
                  startyear=startyear[1], endyear=endyear[1], century=century[1], 
                  multipeak=multipeak[1],             
                  calendar=calendar[1], sourcetype=sourcetype[1], values=values[1],
                  complete=complete[1])

weekly <- weekly[weekly$peakid!=Inf,]

# rescale the datasets with proportions:
props <- unique(weekly$peakid[weekly$values=="proportion"])
for (i in props) {
  weekly$n[weekly$peakid==i] <- (weekly$n[weekly$peakid==i]-min(weekly$n[weekly$peakid==i], na.rm=T)) / 
    (max(weekly$n[weekly$peakid==i], na.rm=T)-min(weekly$n[weekly$peakid==i], na.rm=T))
}

weekly <- weekly %>% dplyr::group_by(id)  %>% dplyr::arrange(date) %>% dplyr::mutate(obs=1:n())

# Calculate peak cases and normalised cases by peakid
weekly <- weekly %>% dplyr::group_by(peakid) %>% dplyr::arrange(date) %>% 
  dplyr::mutate(peakobs=1:n(),
                npeak=ifelse(values=="counts", max(n, na.rm=T), NA),
                nnorm=ifelse(type=="plague mortality", (n-min(n, na.rm=T))/(max(n, na.rm=T)-min(n, na.rm=T)), NA),
                peak=ifelse(row_number()==min(which(n==max(n, na.rm=T))),1,0)) # Find peak time point

# Calculate duration, cumulative cases and final size
weekly <- weekly %>% dplyr::group_by(id) %>% dplyr::arrange(date) %>%
  dplyr::mutate(duration=ifelse(type=="plague mortality" & complete=="yes", date[n()]-date[1], NA),
                ncumul=ifelse(type=="plague mortality" & complete=="yes" & values=="counts", sum(n, na.rm=T), NA))
weekly <- weekly[order(weekly$id, weekly$obs),]
saveRDS(weekly, "output/data_all_weekly.rds")


# 2 CLIMATE DATA -----------------------------------------------------------------

places <- unique(rawdata[,c("place", "lat", "lon")])

# get temperature data
tempraster <- brick("input/CRU_TS4/cru_ts4.03.1901.2018.tmp.dat.nc", values=T)
names <- tempraster@data@names
names <- gsub("X", "", names)

temp <- data.frame(extract(tempraster, places[,c(3,2)])) # Extract data for given locations
colnames(temp) <- names
temp <- cbind(places, temp)
temp <- melt(data=temp, id.vars=c("place", "lat", "lon"), variable.name = "date", value.name="temp")
temp$date <- as.Date(temp$date, format="%Y.%m.%d")
temp <- temp[temp$date<=as.Date("1939-12-31"),]
temp$day <- as.numeric(format(temp$date, format="%d"))
temp$month <- as.numeric(format(temp$date, format="%m"))
temp$doy <- julian(as.Date(paste0("2001-", temp$month, "-", temp$day)), orig=as.Date("2001-01-01")) + 1

# Calculate average annual temperature by location
tempmean <- temp %>% dplyr::group_by(place) %>% dplyr::summarise(tmean=mean(temp))

# Predict daily average temperature by location with fourier series model
temppred <- vector("list", length(places$place))
for (i in 1:length(places$place)) {
  
  subset <- temp[temp$place==places$place[i],]

  model <- lm(temp ~ sin(2*pi/365*doy) + cos(2*pi/365*doy) + sin(4*pi/365*doy) + cos(4*pi/365*doy),
              data=subset)

  temppred[[i]] <- predict(model, newdata=data.frame("doy"=c(1:365)))
  
}
names(temppred) <- places$place
temppred <- plyr::ldply(temppred, rbind)
temppred <- melt(temppred, id.vars=c(".id"))
colnames(temppred) <- c("place", "doy", "temp")
temppred$doy <- as.numeric(as.character(temppred$doy))



# get precipitation data
prec <- brick("input/CRU_TS4/cru_ts4.03.1901.2018.pre.dat.nc", values=T)
names <- prec@data@names
names <- gsub("X", "", names)

prec <- data.frame(extract(prec, places[,c(3,2)])) # Extract data for given locations
colnames(prec) <- names
prec <- cbind(places, prec)
prec <- melt(data=prec, id.vars=c("place", "lat", "lon"), variable.name = "date", value.name="prec")
prec$date <- as.Date(prec$date, format="%Y.%m.%d")
prec <- prec[prec$date<=as.Date("1939-12-31"),]
prec$day <- as.numeric(format(prec$date, format="%d"))
prec$month <- as.numeric(format(prec$date, format="%m"))
prec$doy <- julian(as.Date(paste0("2001-", prec$month, "-", prec$day)), orig=as.Date("2001-01-01")) + 1
prec$year <- as.numeric(format(prec$date, format="%Y"))

# Calculate average annual precipitation by location
precmean <- prec %>% dplyr::group_by(place) %>% dplyr::summarise(precmean=mean(prec))


# Predict daily average precipitation by location with fourier series model
precpred <- vector("list", length(places$place))
for (i in 1:length(places$place)) {
  
  subset <- prec[prec$place==places$place[i],]
  model <- lm(prec ~ sin(2*pi/365*doy) + cos(2*pi/365*doy) + sin(4*pi/365*doy) + cos(4*pi/365*doy),
              data=subset)
  
  precpred[[i]] <- predict(model, newdata=data.frame("doy"=c(1:365)))
  
}
names(precpred) <- places$place
precpred <- plyr::ldply(precpred, rbind)
precpred <- melt(precpred, id.vars=c(".id"))
colnames(precpred) <- c("place", "doy", "prec")
precpred$doy <- as.numeric(as.character(precpred$doy))


climate <- list("tempmean"=tempmean, "temppred"=temppred, "tempdata"=temp, 
                "precmean"=precmean, "precpred"=precpred, "precdata"=prec)
saveRDS(climate, "output/climate.rds")
