# install.packages("BiocManager")
.cran_packages <- c("dplyr", "data.table", "ggplot2", "ggmap", "raster", "rgeos",
                    "maptools", "rgdal")
.bioc_packages <- c()
.inst <- .cran_packages %in% installed.packages()
if(any(!.inst)) {
  install.packages(.cran_packages[!.inst])
}
.inst <- .bioc_packages %in% installed.packages()
if(any(!.inst)) {
  BiocManager::install(.bioc_packages[!.inst])
}

sapply(c(.cran_packages, .bioc_packages), require, character.only = TRUE)

wd_path <- "C:/Users/kevin/Dropbox/SNU_medical/의사추계연구/Macro_trend/Total_modeling/markdown/mid_final"
# wd_path <- "D:/Dropbox/SNU_medical/의사추계연구/Macro_trend/Total_modeling/markdown/mid_final"
setwd(wd_path)
# setwd("C:/Users/kevin/Dropbox/SNU_medical/의사추계연구/Macro_trend/Total_modeling/markdown/report_0615")
korea = shapefile("CTPRVN_202005/CTPRVN.shp")
korea

korea<-spTransform(korea, CRS("+proj=longlat"))
korea_map<-fortify(korea)

## Data --------

# f_n <- list.files(paste0(wd_path, "/result_local_renewal"), full.names = T)
f_n <- list.files(paste0(wd_path, "/result_local_renewal2"), full.names = T)

for(i in 1:length(f_n)){
  subdat <- fread(f_n[i], header = T)
  subdat <- subdat %>% dplyr::select(year, area, sup_demand_med_tech, sup_demand_med_tech_1000)
  
  map_2018_dat <- subdat %>% filter(year == 2018) %>% dplyr::select(-year)
  map_2047_dat <- subdat %>% filter(year == 2047) %>% dplyr::select(-year)
  
  # dat <- fread("ppt_map.csv", header = T)
  # dat <- fread("ppt_map_2018.csv", header = T)
  # dat_t <- dat %>% t()
  # colnames(dat_t)[1:2] <- c("Doc_num", "Pop_num")
  
  map_2018_dat <- map_2018_dat %>% mutate(id = case_when(area == "Seoul" ~ 0,
                                                         area == "Busan" ~ 1,
                                                         area == "Daegu" ~ 2,
                                                         area == "Incheon" ~ 3,
                                                         area == "Gwangju" ~ 4,
                                                         area == "Daejeon" ~ 5,
                                                         area == "Ulsan" ~ 6,
                                                         area == "Sejong" ~ 7,
                                                         area == "Gyunggi" ~ 8,
                                                         area == "Gwangwon" ~ 9,
                                                         area == "Chungbuk" ~ 10,
                                                         area == "Chungnam" ~ 11,
                                                         area == "Jeonbuk" ~ 12,
                                                         area == "Jeonnam" ~ 13,
                                                         area == "Kyungbuk" ~ 14,
                                                         area == "Kyungnam" ~ 15,
                                                         area == "Jaeju" ~ 16) %>% as.character())
  
  
  
  
  merge_map <- korea_map %>% left_join(map_2018_dat, by = "id")
  
  annote_map <-  merge_map %>% group_by(area) %>% summarise(long = mean(long),
                                                            lat = mean(lat),
                                                            id = mean(id %>% as.numeric),
                                                            sup_demand_med_tech = mean(sup_demand_med_tech) %>% round() %>% as.character,
                                                            sup_demand_med_tech_1000 = mean(sup_demand_med_tech_1000) %>% round(3) %>% as.character)
  
  
  
  spsp <- function(x){
    y <- strsplit(x, "/") %>% unlist
    z <- y[length(y)]
    gsub(".csv", "", z)
  }
  
  pdf_n <- spsp(f_n[i])
  
  setwd(paste0(wd_path, "/plotting/map_local2"))
  # setwd(paste0(wd_path, "/plotting/map_local"))
  
  pdf(paste0(pdf_n, "_2018_absolute.pdf"))
  p <- ggplot() + geom_map(data=merge_map, map = korea_map, aes(x=long, y=lat, fill=sup_demand_med_tech, map_id = id), color = "black") +
    scale_fill_gradient2(na.value = "transparent",limits = c(-21000,20000),
                         breaks=c(-20000,0,19000),labels=c("Undersupply",0,"Oversupply"))+
    theme(legend.position = "right",
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg)
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank()
    )+
    labs(fill = "")+xlab("")+ylab("")
  print(p)
  dev.off()
  
  
  # pdf("map2.pdf")
  pdf(paste0(pdf_n, "_2018_per1000.pdf"))
  p <- ggplot() + geom_map(data=merge_map, map = korea_map, aes(x=long, y=lat, fill=sup_demand_med_tech_1000, map_id = id), color = "black") +
    scale_fill_gradient2(na.value = "transparent",limits = c(-2.5,2.5),
                         breaks=c(-2,0,2),labels=c("Undersupply",0,"Oversupply"))+
    theme(legend.position = "right",
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg)
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()
    )+
    labs(fill = "")+xlab("")+ylab("")
  print(p)
  dev.off()
  
  ## year 2047 -----------
  
  
  map_2047_dat <- map_2047_dat %>% mutate(id = case_when(area == "Seoul" ~ 0,
                                                         area == "Busan" ~ 1,
                                                         area == "Daegu" ~ 2,
                                                         area == "Incheon" ~ 3,
                                                         area == "Gwangju" ~ 4,
                                                         area == "Daejeon" ~ 5,
                                                         area == "Ulsan" ~ 6,
                                                         area == "Sejong" ~ 7,
                                                         area == "Gyunggi" ~ 8,
                                                         area == "Gwangwon" ~ 9,
                                                         area == "Chungbuk" ~ 10,
                                                         area == "Chungnam" ~ 11,
                                                         area == "Jeonbuk" ~ 12,
                                                         area == "Jeonnam" ~ 13,
                                                         area == "Kyungbuk" ~ 14,
                                                         area == "Kyungnam" ~ 15,
                                                         area == "Jaeju" ~ 16) %>% as.character())
  
  
  
  
  merge_map <- korea_map %>% left_join(map_2047_dat, by = "id")
  
  annote_map <-  merge_map %>% group_by(area) %>% summarise(long = mean(long),
                                                            lat = mean(lat),
                                                            id = mean(id %>% as.numeric),
                                                            sup_demand_med_tech = mean(sup_demand_med_tech) %>% round() %>% as.character,
                                                            sup_demand_med_tech_1000 = mean(sup_demand_med_tech_1000) %>% round(3) %>% as.character)
  
  
  
  spsp <- function(x){
    y <- strsplit(x, "/") %>% unlist
    z <- y[length(y)]
    gsub(".csv", "", z)
  }
  
  # pdf_n <- spsp(f_n[i])
  
  # setwd(paste0(wd_path, "/plotting/map_local2"))
  # setwd(paste0(wd_path, "/plotting/map_local"))
  pdf(paste0(pdf_n, "_2047_absolute.pdf"))
  p <- ggplot() + geom_map(data=merge_map, map = korea_map, aes(x=long, y=lat, fill=sup_demand_med_tech, map_id = id), color = "black") +
    scale_fill_gradient2(na.value = "transparent",limits = c(-21000,20000),
                         breaks=c(-20000,0,19000),labels=c("Undersupply",0,"Oversupply"))+
    theme(legend.position = "right",
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg)
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank()
    )+
    labs(fill = "")+xlab("")+ylab("")
  print(p)
  dev.off()
  
  
  # pdf("map2.pdf")
  pdf(paste0(pdf_n, "_2047_per1000.pdf"))
  p <- ggplot() + geom_map(data=merge_map, map = korea_map, aes(x=long, y=lat, fill=sup_demand_med_tech_1000, map_id = id), color = "black") +
    scale_fill_gradient2(na.value = "transparent",limits = c(-2.5,2.5),
                         breaks=c(-2,0,2),labels=c("Undersupply",0,"Oversupply"))+
    theme(legend.position = "right",
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg)
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()
    )+
    labs(fill = "")+xlab("")+ylab("")
  print(p)
  dev.off()
  
}
