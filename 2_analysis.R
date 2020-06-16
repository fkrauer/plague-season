# HEADER --------------------------------------------------------------------------

# Project: The influence of temperature on the seasonality of historical plague
# Author: Fabienne Krauer, University of Oslo
# Contact: fabienne.krauer@ibv.uio.no
# Created: 12.12.2019
# Last updated: 15.06.2020



# HOUSEKEEPING ------------------------------------------------------------

rm(list=ls())
set.seed(42)

library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(geepack)
library(mgcv)
library(grid)
library(gridExtra)
library(ggridges)
library(ggrepel)
library(MASS)

Sys.setlocale("LC_TIME", "English")

fignosize <- 18

# Read data
rawdata <- readRDS("output/rawdata.rds")
monthly <- readRDS("output/data_all_monthly.rds")
weekly <- readRDS("output/data_all_weekly.rds")
climate <- readRDS("output/climate.rds")



## 1 Description of data --------------------------------------------------------

metadata <- rawdata %>% dplyr::filter(obs==1)

# Table 2
table(metadata$type) # Number of datasets
rawdata %>% dplyr::group_by(type) %>% dplyr::summarise(npeaks=length(unique(peakid))) # Number of peaks
table(rawdata$type[rawdata$peakobs==1])
prop.table(table(rawdata$type[rawdata$peakobs==1]))

# time interval
table(rawdata$interval[rawdata$obs==1], rawdata$type[rawdata$obs==1])
round(prop.table(table(rawdata$interval[rawdata$obs==1 & rawdata$type=="plague mortality"]))*100,1)
round(prop.table(table(rawdata$interval[rawdata$obs==1 & rawdata$type=="all-cause mortality"]))*100,1)
round(prop.table(table(rawdata$interval[rawdata$obs==1]))*100,1)


# Locations and country
metadata %>% dplyr::group_by(type) %>% dplyr::summarise(nlocations=length(unique(place)))
moo <- metadata %>% dplyr::group_by(type, country) %>% dplyr::summarise(n=n())
table(metadata$country, metadata$type)
data.frame(round(prop.table(table(metadata$country[metadata$type=="plague mortality"]))*100,1))
data.frame(round(prop.table(table(metadata$country[metadata$type=="all-cause mortality"]))*100,1))

# century
table(metadata$century, metadata$type)
round(prop.table(table(metadata$century[metadata$type=="plague mortality"]))*100,1)
round(prop.table(table(metadata$century[metadata$type=="all-cause mortality"]))*100,1)

table(metadata$century)
round(prop.table(table(metadata$century))*100,1)

# Duration, peak sizes and cumulative sizes
summary(monthly$duration[monthly$obs==1 & monthly$type=="plague mortality"])
summary(weekly$duration[weekly$obs==1 & weekly$type=="plague mortality"])/7
summary(monthly$ncumul[monthly$obs==1 & monthly$type=="plague mortality"])
summary(monthly$npeak[monthly$obs==1 & monthly$type=="plague mortality"])
summary(weekly$npeak[weekly$obs==1 & weekly$type=="plague mortality"])


# Fig. S1 ==================================================================
mapdata <- rawdata[rawdata$peakobs==1,c("peakid", "place", "country", "lat", "lon", "type")]
mapdata <- mapdata %>% dplyr::group_by(lat, lon, type) %>% dplyr::summarise(n=n(), place=place[1], country=country[1])
map <- map_data("world")

figS1 <- ggplot() + 
        geom_polygon(data=map, aes(x=long, y=lat, group=group), fill='white', color="gray82") + 
        #coord_fixed(1.3) +
        coord_cartesian(xlim=c(min(mapdata$lon)-3, max(mapdata$lon)+3),
                  ylim=c(min(mapdata$lat)-2, max(mapdata$lat)+3)) +
        guides(color=guide_legend(title="Data type"), 
         size=guide_legend(title="N of datasets")) + 
        geom_point(data=mapdata, aes(x=lon, y=lat, size=n, color=type), alpha=0.5) + 
        geom_text_repel(aes(label=unique(mapdata$place), 
                            x=unique(mapdata$lon), y=unique(mapdata$lat), size=3)) +
        xlab("Longitude") + ylab("Latitude")  +
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
      
figS1

tiff(file = paste0("output/figS1.tif"),
       width = 2500,
       height = 2000,
       res = 300)
fig1
dev.off()


# Fig. S2  ==================================================================
# all epi curves
rawdata <- rawdata %>% dplyr::group_by(peakid) %>% 
  dplyr::mutate(peaklabel=ifelse(year[1]==year[n()],
                                 paste(place, year[1], sep=" "),
                                 paste0(place, " ", year[1], "-", year[n()])))
rawdata$dateplot <- rawdata$date
rawdata$dateplot <- ifelse(is.na(rawdata$date), 
                           as.Date(paste0(rawdata$year, "-", rawdata$month, "-01"), format="%Y-%m-%d"),
                           rawdata$dateplot)
rawdata$dateplot <- as.Date(rawdata$dateplot, format="%Y-%m-%d", origin=as.Date("1970-01-01"))
  
peaklabels <- sort(unique(rawdata$peaklabel))

figS2 <- vector("list", length(peaklabels)) 
for (i in peaklabels) {
  subset <- rawdata[rawdata$peaklabel==i,]
  color <- ifelse(subset$type[1]=="plague mortality", "coral2", "cyan3")
    plot <- ggplot(subset) + geom_bar(aes(x=dateplot, y=n), fill=color, stat="identity") + 
      theme_minimal() + ggtitle(i) + ylab("mortality") + xlab(NULL) +  scale_x_date()
    figS2[[which(peaklabels==i)]] <- ggplotGrob(plot)
}


tiff(file = paste0("output/figS2.tif"),
     width = 6000,
     height = 20000,
     res = 300)
grid.arrange(arrangeGrob(grobs=figS2, x=unit(0, "npc"), y=unit(1, "npc"), just=c("left", "top"), 
                         gp=gpar(col="black",fontsize=size), ncol=4))
dev.off()

pdf(file = "output/figS2.pdf",
     width = 20,
     height = 100)
grid.arrange(arrangeGrob(grobs=figS2, x=unit(0, "npc"), y=unit(1, "npc"), just=c("left", "top"), 
                         gp=gpar(col="black",fontsize=size), ncol=4))
dev.off()


ggsave("output/figS2.pdf", 
       grid.arrange(top=textGrob("\n Fig. S2. Epidemiological curves from plague mortality data (red bars) and all-cause mortality data (blue bars). \n", x=0, hjust=0, 
                                                     gp=gpar(fontsize=20, font=1)), 
                                        arrangeGrob(grobs=figS2, ncol=4)), 
       width=20, height = 100, limitsize = F)



## 2. Analysis  --------------------------------------------------------------

# 2.1 Assessment of seasonality  --------------------------------------------------------------
data0 <- monthly[monthly$type=="plague mortality" & monthly$complete=="yes" & monthly$values=="counts",
                 c("id", "peakid", "place", "country", "month", "population", "lat", "lon", "n", "values")]

data0 <- data0 %>% dplyr::group_by(place) %>% dplyr::mutate(noutbreaks=length(unique(peakid)))
data0 <- data0[data0$noutbreaks>3,]
data0$month <- factor(data0$month, labels=month.abb[1:12])

places <- sort(unique(data0$place))

# Fig. 1  ==================================================================

fig1 <- vector("list", length(unique(data0$place)))
ylims <- c(350, 2000, 1000, 2000, 250)
names(ylims) <- places

for (i in places) {
  subset <- data0[data0$place==i,]
  plot <- ggplot(subset) + theme_minimal() + ggtitle(subset$place[[1]]) +
                                  geom_boxplot(aes(x=month, y=n), stat="boxplot") +
                                  #scale_x_discrete(labels=month.abb[1:12]) +
                                  coord_cartesian(ylim=c(0, ylims[[i]])) +
                                  xlab(NULL) + ylab("monthly plague deaths")
  
  fig1[[which(places==i)]] <- ggplotGrob(plot)
}
tiff(file = paste0("output/fig1.tif"),
     width = 3000,
     height = 3000,
     res = 300)
grid.arrange(arrangeGrob(grobs=fig1, ncol=2))
dev.off()



# Regression model for season
data0 <- data0 %>% dplyr::mutate(season=ifelse(month %in% c("Dec", "Jan", "Feb"), 1, 
                                                      ifelse(month %in% c("Mar", "Apr", "May"), 2,
                                                             ifelse(month %in% c("Jun", "Jul", "Aug"), 3, 4))))
data0$season <- factor(data0$season, labels=c("winter", "spring", "summer", "autumn"))
 
models0 <- vector("list", length(places))
lrtests <- c()
for (i in places) {
  subset <- data0[data0$place==i,]
  
  model <- glm.nb(formula=n ~ season + offset(log(population)), data=subset)
  modelnull <- glm.nb(formula=n ~ offset(log(population)), data=subset) # Compare to null model
  lrtest <- anova(model, modelnull)$`Pr(Chi)`[2]
  
  est <- exp(cbind(estimate = coef(model), confint(model)))[-1,]
  est <- cbind("category"=rownames(est), est)

  models0[[which(places==i)]] <- est
  lrtests <- c(lrtests, lrtest)
}

names(models0) <- places
models0 <- plyr::ldply(models0, rbind)
colnames(models0) <- c("place", "category", "estimate", "low95CI", "up95CI")
models0$result <- paste0(round(as.numeric(as.character(models0$estimate)),2), " [", 
                         round(as.numeric(as.character(models0$low95CI)),2), " - ",
                         round(as.numeric(as.character(models0$up95CI)),2), "]")
models0$category <- gsub("season", "", models0$category)

models0 <- reshape(direction="wide", data=models0[,c("place", "category", "result")], 
                   idvar="place", timevar="category", v.names="result")

models0$lrtest <- round(lrtests,4)
models0



## 2.2 Seasonal peak timing --------------------------------------------------------------

# Function to calculate marginal R2 for the GEE model 
# from: https://onlinelibrary.wiley.com/doi/epdf/10.1002/%28SICI%291097-0258%2820000530%2919%3A10%3C1265%3A%3AAID-SIM486%3E3.0.CO%3B2-U
r2marg <- function(model) {
  return(1-(sum(model$weights * (model$y - model$fitted.values)^2, na.rm = T)/
              sum(model$weights*(model$y - mean(model$y, na.rm = T))^2, na.rm = T)))
}


# Function to predict the confidence interval for fitted values in a GEE model
# see here https://online.stat.psu.edu/stat462/node/126/
predictGEE <- function(model) {
  
  yhat <- model$fitted.values
  y <- model$model[,1]
  x <- model$model[,2]
  n <- length(y)
  mse <- sum((y - yhat)^2) / (n - 2)
  t <- qt(0.975, n - 2)
  
  sefit <- sqrt(mse * (1/n + ((x - mean(x))^2 / (sum((x - mean(x))^2)))))
  
  low95CI <- yhat - t*sefit
  up95CI <- yhat + t*sefit
  
  out <- data.frame(cbind(y, x, yhat, low95CI, up95CI))
  colnames(out) <- c(colnames(model$model)[1], colnames(model$model)[2], "fit", "low95CI", "up95CI")
  
  return(out)
  
}


# Prep data
data1 <- weekly[weekly$peak==1 & weekly$type=="plague mortality",
                c("id", "peakid", "place", "country", "woy", "population", "lat", "lon", "n", "values")]
data1 <- merge(data1, climate$tempmean, by="place", all.x=T)
data1 <- merge(data1, climate$precmean, by="place", all.x=T)
data1 <- data1[order(data1$place),]
data1$place <- as.factor(data1$place)

# Fit models

length(unique(data1$place))
foo <- data1 %>% dplyr::group_by(place) %>% dplyr::summarise(noutbreaks=n())
prop.table(table(foo$noutbreaks))
summary(data1$woy)


## annual mean temperature
model1 <- geeglm(woy ~ tmean, id=place, data=data1, family="gaussian", corstr = "exchangeable")
model1pred <- predictGEE(model1)
summary(model1)
r2marg(model1)

## annual mean precipitation
model2 <- geeglm(woy ~ precmean, id=place, data=data1, family="gaussian", corstr = "exchangeable")
model2pred <- predictGEE(model2)
summary(model2)
r2marg(model2)

## latitude
model3 <- geeglm(woy ~ lat, id=place, data=data1, family="gaussian", corstr = "exchangeable")
model3pred <- predictGEE(model3)
summary(model3)
r2marg(model3)


model1_1 <- geeglm(woy ~ tmean + precmean, id=place, data=data1, family="gaussian", corstr = "exchangeable")
r2marg(model1_1)


# Fig. 2  ==================================================================
fig2a <- ggplot(model1pred) + 
  theme_light() +
  geom_point(aes(x=tmean, y=woy)) + 
  geom_line(aes(x=tmean, y=fit), color="gray20") +
  geom_ribbon(aes(x=tmean, ymin=low95CI, ymax=up95CI), fill="gray20", alpha=0.3) +
  theme(panel.grid.minor.x = element_blank(),
        axis.ticks.x=element_blank()) + 
  ylab("calendar week of epidemic peak") + xlab("annual mean temperature (°C)") 

fig2a

fig2b <- ggplot(model2pred) +
  theme_light() +
  geom_point(aes(x=precmean, y=woy)) + 
  geom_line(aes(x=precmean, y=fit), color="gray20") +
  geom_ribbon(aes(x=precmean, ymin=low95CI, ymax=up95CI), fill="gray20", alpha=0.3) +
  guides(size = guide_legend(title.position = "top", title="Peak size", title.hjust=1)) +
  theme(panel.grid.minor.x = element_blank(),
        axis.ticks.x=element_blank())  + labs(size="Peak size") +
  ylab("calendar week of epidemic peak") + xlab("annual average precipitation (mm)") 
fig2b

fig2c <- ggplot(model3pred) +
  theme_light() +
  geom_point(aes(x=lat, y=woy)) + 
  geom_line(aes(x=lat, y=fit), color="gray20") +
  geom_ribbon(aes(x=lat, ymin=low95CI, ymax=up95CI), fill="gray20", alpha=0.3) +
  theme(panel.grid.minor.x = element_blank(),
        axis.ticks.x=element_blank())  + labs(size="Peak size") +
  ylab("calendar week of epidemic peak") + xlab("latitude (° North)") 
fig2c

tiff(file = paste0("output/fig2.tif"),
     width = 3000,
     height = 1200,
     res = 300)
grid.arrange(arrangeGrob(fig2a, top=textGrob("A", x=unit(0, "npc"), y=unit(0, "npc"), 
                                             just=c("left", "top"), gp=gpar(col="black", fontsize=fignosize))), 
             arrangeGrob(fig2b, top=textGrob("B", x=unit(0, "npc"), y=unit(0, "npc"), 
                                             just=c("left", "top"), gp=gpar(col="black",fontsize=fignosize))),
             arrangeGrob(fig2c, top=textGrob("C", x=unit(0, "npc"), y=unit(0, "npc"), 
                                             just=c("left", "top"), gp=gpar(col="black",fontsize=fignosize))),
             
             ncol=3)
dev.off()



# Fig. S3  ==================================================================

# Ridge plot: weekly by latitude
figS3 <- ggplot(weekly[weekly$type=="plague mortality",], aes(x = woy, y = lat, group=as.factor(peakid), fill=as.factor(place))) + 
              geom_density_ridges(aes(height=nnorm), stat="identity", alpha=0.5, color=NA) +
              scale_x_continuous(expand = c(0,0)) + 
              theme_ridges(center_axis_labels = T, font_size = 10) + 
              xlab("Week of the year") +
              ylab("Latitude (° N)") + labs(fill="Place") + guides(fill=guide_legend(ncol=1)) +
              coord_cartesian(clip = "off")
        
figS3

tiff(file = paste0("output/figS3.tif"),
     width = 2500,
     height = 2500,
     res = 300)
figS3
dev.off()


## 2.3 Seasonal peak timing sensitivity analysis --------------------------------------------------------------

# including plague monthly data
data2 <- monthly[monthly$peak==1 & monthly$type=="plague mortality",
                c("id", "peakid", "place", "country", "month", "population", "lat", "lon", "n", "values")]
data2 <- merge(data2, climate$tempmean, by="place", all.x=T)
data2 <- data2[order(data2$place),]
data2$place <- as.factor(data2$place)

model4 <- geeglm(month ~ tmean, id=place, data=data2, family="gaussian", corstr = "exchangeable")
model4pred <- predictGEE(model4)
summary(model4)
r2marg(model4)


# including weekly plague and all-cause data
data3 <- weekly[weekly$peak==1, c("id", "peakid", "place", "country", "woy", "population", "lat", "lon", "n", "values")]
data3 <- merge(data3, climate$tempmean, by="place", all.x=T)
data3 <- data3[order(data3$place),]
data3$place <- as.factor(data3$place)

model5 <- geeglm(woy ~ tmean, id=place, data=data3, family="gaussian", corstr = "exchangeable")
model5pred <- predictGEE(model5)
summary(model5)
r2marg(model5)


# including monthly plague and all-cause data
data4 <- monthly[monthly$peak==1, c("id", "peakid", "place", "country", "month", "population", "lat", "lon", "n", "values")]
data4 <- merge(data4, climate$tempmean, by="place", all.x=T)
data4 <- data4[order(data4$place),]
data4$place <- as.factor(data4$place)

model6 <- geeglm(month ~ tmean, id=place, data=data4, family="gaussian", corstr = "exchangeable")
model6pred <- predictGEE(model6)
summary(model6)
r2marg(model6)


# Including only outbreaks with 100 or more at peak
model7 <- geeglm(woy ~ tmean, id=as.factor(place), data=data1[data1$n>=100,], family="gaussian", corstr = "exchangeable")
model7pred <- predictGEE(model7)
summary(model7)
r2marg(model7)



# Fig. S4  ==================================================================
figS4a <- ggplot(model4pred) +
  theme_light() + 
  geom_point(aes(x=tmean, y=month)) + 
  geom_line(aes(x=tmean, y=fit), color="gray20") +
  geom_ribbon(aes(x=tmean, ymin=low95CI, ymax=up95CI), fill="gray20", alpha=0.3) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank())  + labs(size="Peak size") +
  ylab("Peak month of plague deaths") + xlab("annual mean temperature (°C)") +
  scale_y_continuous(breaks=c(1:12), labels=month.abb[1:12])
figS4a

figS4b <- ggplot(model5pred) +
  theme_light() +
  geom_point(aes(x=tmean, y=woy)) + 
  geom_line(aes(x=tmean, y=fit), color="gray20") +
  geom_ribbon(aes(x=tmean, ymin=low95CI, ymax=up95CI), fill="gray20", alpha=0.3) +
  theme(panel.grid.minor.x = element_blank(),
        axis.ticks.x=element_blank())  + labs(size="Peak size") +
  ylab("Peak week of plague and all-cause deaths") + xlab("annual mean temperature (°C)") 
figS4b


figS4c <- ggplot(model6pred) +
  theme_light() + 
  geom_point(aes(x=tmean, y=month)) + 
  geom_line(aes(x=tmean, y=fit), color="gray20") +
  geom_ribbon(aes(x=tmean, ymin=low95CI, ymax=up95CI), fill="gray20", alpha=0.3) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank())  + labs(size="Peak size") +
  ylab("Peak month of plague and all-cause deaths") + xlab("annual mean temperature (°C)") +
  scale_y_continuous(breaks=c(1:12), labels=month.abb[1:12])
figS4c

figS4d <- ggplot(model7pred) +
  theme_light() +
  geom_point(aes(x=tmean, y=woy)) + 
  geom_line(aes(x=tmean, y=fit), color="gray20") +
  geom_ribbon(aes(x=tmean, ymin=low95CI, ymax=up95CI), fill="gray20", alpha=0.3) +
  theme(panel.grid.minor.x = element_blank(),
        axis.ticks.x=element_blank())  + labs(size="Peak size") +
  ylab("Peak week of plague deaths (large outbreaks only)") + xlab("annual mean temperature (°C)") 
figS4d


tiff(file = paste0("output/figS4.tif"),
     width = 3000,
     height = 3000,
     res = 300)
grid.arrange(arrangeGrob(figS4a, top=textGrob("A", x=unit(0, "npc"), y=unit(0, "npc"), 
                                             just=c("left", "top"), gp=gpar(col="black", fontsize=fignosize))), 
             arrangeGrob(figS4b,top=textGrob("B", x=unit(0, "npc"), y=unit(0, "npc"), 
                                            just=c("left", "top"), gp=gpar(col="black",fontsize=fignosize))),
             arrangeGrob(figS4c,top=textGrob("C", x=unit(0, "npc"), y=unit(0, "npc"), 
                                            just=c("left", "top"), gp=gpar(col="black",fontsize=fignosize))),
             arrangeGrob(figS4d,top=textGrob("D", x=unit(0, "npc"), y=unit(0, "npc"), 
                                            just=c("left", "top"), gp=gpar(col="black",fontsize=fignosize))),
             
             
             
             ncol=2)
dev.off()


## 2.4 Epidemic growth rate --------------------------------------------------------------------------

# Prepare weekly data
data5 <- weekly[weekly$values=="counts" & weekly$type=="plague mortality" & weekly$complete=="yes",
                c("id", "peakid", "obs", "woy", "date", "place", "country", "lat", "lon", "population", "n", "npeak", "peak")] 
data5$n <- ifelse(data5$n==0,NA, data5$n)

# estimate cases from deaths
moo <- data5[,c("id", "n", "date")]
moo$date <- moo$date-7
moo$ninf <- round(moo$n/0.66)

data5 <- merge(data5, moo[,c("id", "date", "ninf")], by=c("id", "date"), all=T)
rm(moo)

data5$woy <- isoweek(data5$date)

for (i in 1:(nrow(data5)-1)) {
  data5$peakid[i] <- ifelse(is.na(data5$obs[i]) & is.na(data5$peakid[i]), 
                            data5$peakid[i+1], data5$peakid[i])
}

data5 <- data5 %>% dplyr::group_by(peakid) %>% dplyr::arrange(date) %>% 
  dplyr::mutate(obs=1:n(), time=julian(date, origin=date[1])+1,
                place=unique(na.omit(place)), country=unique(na.omit(country)), npeak=unique(na.omit(npeak)),
                lat=min(lat, na.rm=T), lon=min(lon, na.rm=T),
                population=min(population, na.rm=T))

data5$year <- format(data5$date, format="%Y")
data5 <- data5 %>% dplyr::group_by(peakid, year) %>% 
  dplyr::mutate(doy=julian(date, origin=as.Date(paste0(year[1], "-01-01")))+1)


# Define data to be used for growth rate calculation: 
# first to last time when the incident cases surpass 20 and at least 10 non-missing observations
data5 <- data5 %>% dplyr::group_by(peakid) %>% arrange(date) %>% 
  dplyr::mutate(included=ifelse(obs>=min(which(ninf>=21)) &
                                  obs<=max(which(ninf>=21)) &
                                  !is.na(ninf),1,0),
                nincluded=sum(included))
data5 <- data5[data5$nincluded>=10,]
  
length(unique(data5$peakid))
length(unique(data5$place))

# Calculate growth rates

# Assess growth rate with a log-linear fit with a sliding window of 4 weeks size = 5 observations
peakids <- sort(unique(data5$peakid[data5$included==1]))
window <- 4
rmodels <- vector("list", length=length(peakids))
growth <- c()
for (i in 1:length(peakids)) {
  
  subset <- data5[data5$peakid==peakids[i] & data5$included==1,]
  subset <- subset[1:(nrow(subset)-1),]
  times <- sort(subset$time)
  nslides <- length(times)-window

  rmodels[[i]] <- vector("list", length=nslides)
  for (j in 1:nslides) {
    
    subsubset <- subset[subset$time %in% times[j:(j+window)],c("time", "ninf")]
    try(rmodels[[i]][[j]] <- lm(formula=log(ninf)~time, data=subsubset))
    try(rmodels[[i]][[j]]$CI <- confint(rmodels[[i]][[j]]))
    rmodels[[i]][[j]]$data <- subsubset
    
    if (!is.null(rmodels[[i]][[j]]) && !(all(is.na(rmodels[[i]][[j]]$CI)))) {
      
      moo <- c("peakid"=peakids[i],
                "r"=summary(rmodels[[i]][[j]])$coefficients[2,1], 
               "se"=summary(rmodels[[i]][[j]])$coefficients[2,2],
               "low95CI"=rmodels[[i]][[j]]$CI[2,1],
               "up95CI"=rmodels[[i]][[j]]$CI[2,2],
               "time"=rmodels[[i]][[j]]$data$time,
               "ninf"=rmodels[[i]][[j]]$data$ninf)
      growth <- rbind(growth, moo)
      
    }
    
  }
  names(rmodels[[i]]) <- times[1:nslides]
}
names(rmodels) <- peakids

rm(subset)
rm(subsubset)

growth <- data.frame(growth)

data5 <- merge(data5, growth, by.x=c("peakid", "time"), by.y=c("peakid", "time3"), all=T)

# Mark the first maximum of the growth rate and the subsequent point when r=0
data5 <- data5 %>% dplyr::group_by(peakid) %>% 
dplyr::mutate(rmax=ifelse(obs==min(which(r==max(r, na.rm=T) & !is.na(r))),1,0),
              rzero=ifelse(obs==min(which(r<=0 & !is.na(r) & cumsum(rmax)==1)),1,0))

# Merge with predicted daily temperature
temp <- climate$temppred
temp <- temp[temp$place %in% unique(data5$place),]
data5 <- merge(data5, temp, by=c("place", "doy"), all=T)
data5 <- data5[!is.na(data5$id),]

prec <- climate$precpred
prec <- prec[prec$place %in% unique(data5$place),]
data5 <- merge(data5, prec, by=c("place", "doy"), all=T)
data5 <- data5[!is.na(data5$id),]


saveRDS(data5, "output/data5.rds")



# Fig. S5  ==================================================================

# Plot time-varying growth rate for each epidemic
peakids <- sort(unique(data5$peakid))
figS5 <- vector("list", length(peakids))
for (i in 1:length(peakids)) {
  
  subset <- data5[data5$peakid==peakids[i],]
  scale <- max(subset$up95CI, na.rm=T)/max(subset$ninf, na.rm=T)
  from <- min(subset$year)
  to <- max(subset$year)
  year <- if (from==to) from else paste(from, to, sep=" - ")

  plot <- ggplot(subset) + 
    geom_bar(aes(x=date, y=ninf), stat="identity", fill="grey60", color="grey60") +
    geom_bar(aes(x=date, y=n), stat="identity", fill="grey30", color="grey30") +
    geom_line(aes(x=date, y=r/scale), color="red") +
    geom_ribbon(aes(x=date, ymin=low95CI/scale, ymax=up95CI/scale), fill="red", alpha=0.3) +
    scale_y_continuous(sec.axis = sec_axis(~.*scale), expand=c(0,0)) + 
    ylab("Incidence") + xlab(NULL) +
    theme(axis.line.y.right = element_line(color="red"),
          axis.ticks.y.right = element_line(color="red"),
          axis.text.y.right = element_text(color="red")) +
    ggtitle(paste(unique(subset$place), year, sep=" "))
  
  figS5[[i]] <- ggplotGrob(plot)
}


tiff(file = paste0("output/figS5.tif"),
     width = 4400,
     height = 9000,
     res = 300)
grid.arrange(arrangeGrob(grobs=figS5, x=unit(0, "npc"), y=unit(1, "npc"), just=c("left", "top"), 
                         gp=gpar(col="black",fontsize=size), ncol=4))


dev.off()

ggsave("output/figS5.pdf", 
       grid.arrange(top=textGrob("\n Fig. S5. Epidemiological curves and the corresponding time-varying growth rates. The dark grey bars represent the incident plague mortality, the light grey \n bars represent the incident plague cases calculated from the mortality data. The red line and pink ribbon are the mean estimate and 95% CI for the time-varying \n epidemic growth rate calculate based on the incident case data (right y-axis). Values below 0 indicate no growth (i.e. a decline of the epidemic). The values on \n the right y-axis represent the daily growth rate. \n", 
                                 x=0, hjust=0, 
                                 gp=gpar(fontsize=20, font=1)), 
                    arrangeGrob(grobs=figS5, ncol=4)), 
       width=20, height = 50, limitsize = F)




# Results 
length(unique(data5$peakid))
length(unique(data5$place))

min(data5$r, na.rm = T)
max(data5$r, na.rm = T)
median(data5$r[data5$r>0], na.rm = T)
median(data5$r[data5$r<=0], na.rm = T)

min(data5$temp)
max(data5$temp)
summary(data5$temp[data5$rmax==1])
summary(data5$temp[data5$r>0])

# Fit a GAM model

# Using all available data
model8 <- gam(r ~ s(temp, bs="tp"), data=data5[!is.na(data5$r),], family="gaussian", link="identity", methods="REML")
summary(model8)
coef(model8)
plot(model8)
gam.check(model8)
AIC(model8)
model8pred <- cbind(data5[!is.na(data5$r), c("id", "peakid", "place", "temp", "r")], predict(model8, se.fit=T))
model8pred$low95CI <- model8pred$fit - 1.96*model8pred$se.fit
model8pred$up95CI <- model8pred$fit + 1.96*model8pred$se.fit
unique(model8pred$temp[model8pred$fit==max(model8pred$fit)])

# the min/max temperature at which predicted growth is positive
min(model8pred$temp[model8pred$fit>0],na.rm=T)
min(model8pred$temp[model8pred$up95CI>0 & model8pred$temp>5],na.rm=T)
min(model8pred$temp[model8pred$low95CI>0],na.rm=T)

max(model8pred$temp[model8pred$fit>0],na.rm=T)
max(model8pred$temp[model8pred$low95CI>0],na.rm=T)
max(model8pred$temp[model8pred$up95CI>0],na.rm=T)


# check if a model with a random effect at outbreak level is better
model8re <- gam(r ~ s(temp, bs="tp") + s(peakid, bs="re"), data=data5[!is.na(data5$r),], methods="REML")
summary(model8re)
AIC(model8, model8re) # no difference to model without random effect


# compare to a linear model
model8lin <- gam(r ~ temp, data=data5[!is.na(data5$r),], methods="REML")
summary(model8lin)
anova(model8, model8lin, test="Chisq") # GAM model is definitely the better fit
AIC(model8, model8lin)

# There is a lot of noise in the data, partly because some of the epi curves themselves are noisy.
data5subset <- data5[data5$npeak>=500 & !is.na(data5$r),]
min(data5subset$temp[data5subset$r>0])
max(data5subset$temp[data5subset$r>0])

# Using only large outbreaks
model9 <- gam(r ~ s(temp, bs="tp"), data=data5subset, family="gaussian", link="identity", methods="REML")
summary(model9)
coef(model9)
plot(model9)
gam.check(model9)
model9pred <- cbind(data5subset[, c("id", "peakid", "place", "temp", "r")], predict(model9, se.fit=T))
model9pred$low95CI <- model9pred$fit - 1.96*model9pred$se.fit
model9pred$up95CI <- model9pred$fit + 1.96*model9pred$se.fit
unique(model9pred$temp[model9pred$fit==max(model9pred$fit)])

# the min/max temperature at which predicted growth is positive
min(model9pred$temp[model9pred$fit>0],na.rm=T)
tempmin95CI <- min(model9pred$temp[model9pred$up95CI>0],na.rm=T)
tempmin95CI
min(model9pred$temp[model9pred$low95CI>0],na.rm=T)

max(model9pred$temp[model9pred$fit>0],na.rm=T)
max(model9pred$temp[model9pred$low95CI>0],na.rm=T)
tempmax95CI <- max(model9pred$temp[model9pred$up95CI>0],na.rm=T)
tempmax95CI


# Fig. 3  ==================================================================

# all data

# Histogram
fig3a <- ggplot() +  theme_light() + #theme(panel.grid = element_blank()) +
  geom_histogram(data=data5[data5$r>0,], aes(x=temp, y=..count.., fill="> 0"), bins=60) +
  geom_histogram(data=data5[data5$r<=0,], aes(x=temp, y=-..count.. , fill="<=0"), bins=60) +
  ylab("frequency") + xlab("temperature (°C)") + scale_fill_hue("Growth rates")
fig3a

# Fit
fig3b <- ggplot(model8pred, aes(x=temp, y=r)) +
  theme_light() +
  geom_point(aes(color=place)) + 
  #stat_smooth(method="gam", formula=y ~ s(x, bs=smoother), fill="grey50", color="grey40") +
  geom_line(aes(x=temp, y=fit), color="red") +
  geom_ribbon(aes(x=temp, ymin=low95CI, ymax=up95CI), fill="red", alpha=0.3) +
  ylab("daily epidemic growth rate") + xlab("temperature (°C)") +
  geom_hline(aes(yintercept=0))
fig3b


# Using only large outbreaks

# Histogram
fig3c <- ggplot() +  theme_light() + #theme(panel.grid = element_blank()) +
  geom_histogram(data=data5subset[data5subset$r>0,], aes(x=temp, y=..count.., fill="> 0"), bins=60) +
  geom_histogram(data=data5subset[data5subset$r<=0,], aes(x=temp, y=-..count.. , fill="<=0"), bins=60) +
  ylab("frequency") + xlab("temperature (°C)") + scale_fill_hue("Growth rates")
fig3c

# Fit
fig3d <- ggplot(model9pred, aes(x=temp, y=r)) +
  theme_light() +
  geom_point(aes(color=place)) + 
  #stat_smooth(method="gam", formula=y ~ s(x, bs=smoother), fill="grey50", color="grey40") +
  geom_line(aes(x=temp, y=fit), color="red") +
  geom_ribbon(aes(x=temp, ymin=low95CI, ymax=up95CI), fill="red", alpha=0.3) +
  ylab("daily epidemic growth rate") + xlab("temperature (°C)") +
  geom_hline(aes(yintercept=0))
fig3d



# combine plot
tiff(file = paste0("output/fig3.tif"),
     width = 4000,
     height = 3000,
     res = 300)
grid.arrange(arrangeGrob(fig3a, top=textGrob("A", x=unit(0, "npc"), y=unit(0, "npc"), 
                                             just=c("left", "top"), gp=gpar(col="black",fontsize=fignosize))), 
             arrangeGrob(fig3b, top=textGrob("B", x=unit(0, "npc"), y=unit(0, "npc"), 
                                             just=c("left", "top"), gp=gpar(col="black", fontsize=fignosize))),
             arrangeGrob(fig3c, top=textGrob("C", x=unit(0, "npc"), y=unit(0, "npc"), 
                                             just=c("left", "top"), gp=gpar(col="black",fontsize=fignosize))),
             arrangeGrob(fig3d, top=textGrob("D", x=unit(0, "npc"), y=unit(0, "npc"), 
                                             just=c("left", "top"), gp=gpar(col="black",fontsize=fignosize))),
             
             ncol=2)
dev.off()

# Repeat the analysis with tmeperature Without London data
model10 <- gam(r ~ s(temp, bs="tp"), data=data5[!is.na(data5$r) & data5$place!="London",], 
               family="gaussian", link="identity", methods="REML")
summary(model10)
model10pred <- cbind(data5[!is.na(data5$r) & data5$place!="London",], predict(model10, se.fit=T))
model10pred$low95CI <- model10pred$fit - 1.96*model10pred$se.fit
model10pred$up95CI <- model10pred$fit + 1.96*model10pred$se.fit
unique(model10pred$temp[model10pred$fit==max(model10pred$fit)])



# Repeate the analysis with precipitation: all data
model8prec <- gam(r ~ s(prec, bs="tp"), 
                  data=data5[!is.na(data5$r),],
                  family="gaussian", link="identity", methods="REML")
summary(model8prec)
model8precpred <- cbind(data5[!is.na(data5$r), c("id", "peakid", "place", "prec", "r")], 
                        predict(model8prec, se.fit=T))
model8precpred$low95CI <- model8precpred$fit - 1.96*model8precpred$se.fit
model8precpred$up95CI <- model8precpred$fit + 1.96*model8precpred$se.fit


# Repeate the analysis with precipitation: without London
model10prec <- gam(r ~ s(prec, bs="tp"), 
                  data=data5[!is.na(data5$r) & data5$place!="London",], 
                  family="gaussian", link="identity", methods="REML")
summary(model10prec)
model10precpred <- cbind(data5[!is.na(data5$r) & data5$place!="London",c("id", "peakid", "place", "prec", "r")], 
                        predict(model10prec, se.fit=T))
model10precpred$low95CI <- model10precpred$fit - 1.96*model10precpred$se.fit
model10precpred$up95CI <- model10precpred$fit + 1.96*model10precpred$se.fit



# Fig. S6  ==================================================================

figS6a <- ggplot(model10pred, aes(x=temp, y=r)) +
  theme_light() +
  geom_point(aes(color=place)) + 
  #stat_smooth(method="gam", formula=y ~ s(x, bs=smoother), fill="grey50", color="grey40") +
  geom_line(aes(x=temp, y=fit), color="red") +
  geom_ribbon(aes(x=temp, ymin=low95CI, ymax=up95CI), fill="red", alpha=0.3) +
  ylab("daily epidemic growth rate") + xlab("temperature (°C)") +
  geom_hline(aes(yintercept=0))
figS6a


figS6b <- ggplot(model8precpred, aes(x=prec, y=r)) +
  theme_light() +
  geom_point(aes(color=place)) + 
  #stat_smooth(method="gam", formula=y ~ s(x, bs=smoother), fill="grey50", color="grey40") +
  geom_line(aes(x=prec, y=fit), color="red") +
  geom_ribbon(aes(x=prec, ymin=low95CI, ymax=up95CI), fill="red", alpha=0.3) +
  ylab("daily epidemic growth rate") + xlab("Precipitation (mm)") +
  geom_hline(aes(yintercept=0))
figS6b

figS6c <- ggplot(model10precpred, aes(x=prec, y=r)) +
  theme_light() +
  geom_point(aes(color=place)) + 
  #stat_smooth(method="gam", formula=y ~ s(x, bs=smoother), fill="grey50", color="grey40") +
  geom_line(aes(x=prec, y=fit), color="red") +
  geom_ribbon(aes(x=prec, ymin=low95CI, ymax=up95CI), fill="red", alpha=0.3) +
  ylab("daily epidemic growth rate") + xlab("Precipitation (mm)") +
  geom_hline(aes(yintercept=0))
figS6c

# combine plot
tiff(file = paste0("output/figS6.tif"),
     width = 3000,
     height = 2700,
     res = 300)
grid.arrange(arrangeGrob(figS6a, top=textGrob("A", x=unit(0, "npc"), y=unit(0, "npc"), 
                                             just=c("left", "top"), gp=gpar(col="black",fontsize=fignosize))), 
             arrangeGrob(figS6b, top=textGrob("B", x=unit(0, "npc"), y=unit(0, "npc"), 
                                             just=c("left", "top"), gp=gpar(col="black", fontsize=fignosize))),
             arrangeGrob(figS6c, top=textGrob("C", x=unit(0, "npc"), y=unit(0, "npc"), 
                                              just=c("left", "top"), gp=gpar(col="black", fontsize=fignosize))),
             layout_matrix=rbind(c(1, NA), c(2,3)))
dev.off()




## 2.5 Temperature data agreement ----------------------------------------------------------------------------------

# agreement of actual temperature with predicted temperature
cairo <- read.delim("input/temp_cairo.txt", stringsAsFactors = F)
cairo$temphist <- rowMeans(cairo[,c(2:4)], na.rm=T)
cairo <- merge(cairo, climate$temppred[climate$temppred$place=="Cairo",], by="doy", all=T)
colnames(cairo)[7] <- "tempCRUpred"
cairo <- cairo[!is.na(cairo$temphist),]
cairo <- merge(cairo, data5[data5$place=="Cairo" & !is.na(data5$r),c("doy", "r")], by="doy", all=T)

# Calculate daily average temperature by location with fourier series fit
cairo$temphistpred <- predict(lm(temphist ~ sin(2*pi/365*doy) + cos(2*pi/365*doy) + sin(4*pi/365*doy) + cos(4*pi/365*doy),
                                  data=cairo),
                                newdata=data.frame("doy"=c(1:nrow(cairo))))

# calculate difference between predicted from CRU data and predicted from actual data
cairo$diff <- cairo$temphistpred-cairo$tempCRUpred
hist(cairo$diff)
summary(cairo$diff)


# Plot

# Fig. S7  ==================================================================

CRUcairodata <- climate$tempdata[climate$tempdata$place=="Cairo", c("doy", "temp")]


breaksx <- c(1, 32, 60, 91, 121, 152)
figS7 <- ggplot(cairo) + geom_point(aes(x=doy, y=temphist), color="red") + 
  theme_minimal() + coord_cartesian(xlim=c(0,(nrow(cairo)+5))) +
  geom_point(data=CRUcairodata, aes(x=doy, y=temp), color="blue") +
          geom_line(aes(x=doy, y=temphistpred),color="red") +
          geom_line(aes(x=doy, y=tempCRUpred),color="blue") + 
          xlab(NULL) + ylab("temperature (°C) \n") +
          scale_x_continuous(breaks=breaksx,
                             labels=month.abb[1:length(breaksx)],
                             expand=c(0,0)) +
          theme(axis.text.x = element_text(hjust=-0.5), 
                axis.ticks.x=element_blank(),
                panel.grid.minor.x = element_blank()) 
        
figS7

tiff(file = paste0("output/figS7.tif"),
     width = 2000,
     height = 2000,
     res = 300)
figS7
dev.off()


