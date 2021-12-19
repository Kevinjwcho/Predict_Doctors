library(data.table)
library(dplyr)

drop_path <- "C:/Users/kevin/"
wd_path <-"Dropbox/SNU_medical/의사추계연구/Macro_trend/Total_modeling/markdown/mid_final"
setwd(paste0(drop_path, wd_path))

## Local age initial data load -----------------

local_dat <- fread("local_doc.csv", header = T)


local_dat <- local_dat %>%
  mutate(area = case_when(Local == "서울" ~ "Seoul",
                          Local == "부산" ~ "Busan",
                          Local == "대구" ~ "Daegu",
                          Local == "인천" ~ "Incheon",
                          Local == "광주" ~ "Gwangju",
                          Local == "대전" ~ "Daejeon",
                          Local == "울산" ~ "Ulsan",
                          Local == "세종" ~ "Sejong",
                          Local == "경기" ~ "Gyunggi",
                          Local == "강원" ~ "Gwangwon",
                          Local == "충북" ~ "Chungbuk",
                          Local == "충남" ~ "Chungnam",
                          Local == "전북" ~ "Jeonbuk",
                          Local == "전남" ~ "Jeonnam",
                          Local == "경북" ~ "Kyungbuk",
                          Local == "경남" ~ "Kyungnam",
                          Local == "제주" ~ "Jaeju",
                          TRUE ~ "Total"))


local_dat <- local_dat %>% mutate(total = age20+age30+age40+age50+age60,
                                  local_per = total/96180) %>% dplyr::select(-Local)

### pop estimation sido loading --------------------

### pop_est_sido ---------------------

pop_estimate  <- fread("pop_est_sido.csv", header = T)

str(pop_estimate)
colnames(pop_estimate) <- c("scean", "sido", "sex", "age", "year", "pop_est")

# table(pop_estimate$scean)
pop_est1 <- pop_estimate %>% filter(sex != "계",
                                    age != "계",
                                    age != "80세이상") %>% dplyr::select(-scean)

# pop_est2 <- pop_est1 %>% reshape2::melt(id = 1:2)
# colnames(pop_est2)[3:4] <- c("year", "pop_est")
str(pop_est1)

pop_est_85 <- pop_est1 %>% filter(age %in% c("85 - 89세", "90 - 94세", "95 - 99세", "100세 이상")) %>% group_by(sido, sex, year) %>% summarise(sum(pop_est))

pop_est_85$age <- c("age85")

pop_est_85_2 <- pop_est_85[,c(1,2,5,3, 4)] %>% as.data.frame
colnames(pop_est_85_2)[5] <- "pop_est"

pop_est3 <- rbind(pop_est1 %>% filter(!(age %in% c("85 - 89세", "90 - 94세", "95 - 99세", "100세 이상"))),
                  pop_est_85_2)

pop_est4<- pop_est3 %>% mutate(sex = ifelse(sex == "남자", "m", "f"),
                               age = case_when(
                                 age == "0 - 4세" ~ "age0004",
                                 age == "5 - 9세" ~ "age0509",
                                 age == "10 - 14세" ~ "age1014",
                                 age == "15 - 19세" ~ "age1519",
                                 age == "20 - 24세" ~ "age2024",
                                 age == "25 - 29세" ~ "age2529",
                                 age == "30 - 34세" ~ "age3034",
                                 age == "35 - 39세" ~ "age3539",
                                 age == "40 - 44세" ~ "age4044",
                                 age == "45 - 49세" ~ "age4549",
                                 age == "50 - 54세" ~ "age5054",
                                 age == "55 - 59세" ~ "age5559",
                                 age == "60 - 64세" ~ "age6064",
                                 age == "65 - 69세" ~ "age6569",
                                 age == "70 - 74세" ~ "age7074",
                                 age == "75 - 79세" ~ "age7579",
                                 age == "80 - 84세" ~ "age8084",
                                 age == "age85" ~ "age85"
                               ),
                               year = tstrsplit(year, " ", keep = 1) %>% unlist %>% as.numeric)


pop_est_total <- pop_est4 %>% group_by(sido, year) %>% summarise(pop_est = sum(pop_est))
pop_est_total1 <- pop_est_total %>%
  mutate(sido = case_when(sido == "서울특별시" ~ "Seoul",
                          sido == "부산광역시" ~ "Busan",
                          sido == "대구광역시" ~ "Daegu",
                          sido == "인천광역시" ~ "Incheon",
                          sido == "광주광역시" ~ "Gwangju",
                          sido == "대전광역시" ~ "Daejeon",
                          sido == "울산광역시" ~ "Ulsan",
                          sido == "세종특별자치시" ~ "Sejong",
                          sido == "경기도" ~ "Gyunggi",
                          sido == "강원도" ~ "Gwangwon",
                          sido == "충청북도" ~ "Chungbuk",
                          sido == "충청남도" ~ "Chungnam",
                          sido == "전라북도" ~ "Jeonbuk",
                          sido == "전라남도" ~ "Jeonnam",
                          sido == "경상북도" ~ "Kyungbuk",
                          sido == "경상남도" ~ "Kyungnam",
                          sido == "제주특별자치도" ~ "Jaeju",
                          sido == "전국" ~ "Total"))

pop_est_total <- pop_est_total1 %>% filter(side != "Total")


### Loading demand data ----------


library(readxl)    
read_excel_allsheets <- function(filename, tibble = TRUE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_xlsx(filename, sheet = X, col_types = "numeric"))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

path <- c("C:/Users/kevin/Dropbox/SNU_medical/의사추계연구/Macro_trend/origin_data/demand/성별연령별_입원외래방문횟수_2003_2018.xlsx")
# path <- c("D:/Dropbox/SNU_medical/의사추계연구/Macro_trend/origin_data/demand/성별연령별_입원외래방문횟수_2003_2018.xlsx")
demand_data <- read_excel_allsheets(path)


for(i in 1:16){
  demand_data[[i]] <- demand_data[[i]] %>% dplyr::select(-`...1`) %>% mutate_all(as.numeric)%>%
    mutate(year = names(demand_data)[i] %>% as.numeric,
           var = c("total", "total_m", "total_f",
                   "age00", "age00_m", "age00_f",
                   "age0104", "age0104_m", "age0104_f",
                   "age0509", "age0509_m", "age0509_f",
                   "age1014", "age1014_m", "age1014_f",
                   "age1519", "age1519_m", "age1519_f",
                   "age2024", "age2024_m", "age2024_f",
                   "age2529", "age2529_m", "age2529_f",
                   "age3034", "age3034_m", "age3034_f",
                   "age3539", "age3539_m", "age3539_f",
                   "age4044", "age4044_m", "age4044_f",
                   "age4549", "age4549_m", "age4549_f",
                   "age5054", "age5054_m", "age5054_f",
                   "age5559", "age5559_m", "age5559_f",
                   "age6064", "age6064_m", "age6064_f",
                   "age6569", "age6569_m", "age6569_f",
                   "age7074", "age7074_m", "age7074_f",
                   "age7579", "age7579_m", "age7579_f",
                   "age8084", "age8084_m", "age8084_f",
                   "age85", "age85_m", "age85_f")
    ) 
}
demand_data <- do.call("rbind", demand_data)
demand_data <- demand_data %>% as.data.frame
rownames(demand_data) <- NULL
str(demand_data)
colnames(demand_data) <- c("out_p", "host_p", "insurance", "out_per", "host_per", "year", "sex_age")



demand_18yr <- demand_data %>% filter(year == 2018)
str(demand_18yr)

demand_18yr_1 <- demand_18yr %>% dplyr::select(-insurance) %>% mutate(pop_num = out_p/out_per) %>% dplyr::select(-out_per, -host_per, -year)
demand_18yr_1<- demand_18yr_1 %>% mutate(sex = tstrsplit(sex_age, "_", keep = 2) %>% unlist,
                                         age = tstrsplit(sex_age, "_", keep = 1) %>% unlist)

demand_18yr_2 <- demand_18yr_1 %>% filter(!is.na(sex), age != "total") %>% dplyr::select(-sex_age)

sub_m<- c(demand_18yr_2[1, 1:3] + demand_18yr_2[3, 1:3], "m", "age0004") %>% unlist
sub_f<- c(demand_18yr_2[2, 1:3] + demand_18yr_2[4, 1:3], "f", "age0004") %>% unlist
# with(demand_18yr_2, sum(out_p[sex == "m" & age %in% c("age00", "age0104")]))

demand_18yr_3 <- rbind(sub_m, sub_f, demand_18yr_2[-c(1:4), ])

str(demand_18yr_3)

demand_18yr_3[,1:3] <- demand_18yr_3[,1:3] %>% mutate_all((as.numeric))

demand_18yr_3 <- demand_18yr_3 %>% mutate(out_per = out_p/ pop_num,
                                          host_per = host_p/pop_num)



demand_18yr_fin<- demand_18yr_3 %>% dplyr::select(sex, age, out_per, host_per)

demand_est <- pop_est4 %>% filter(year != 2017) %>%  left_join(demand_18yr_fin) %>%
  mutate(out_p_est = round(pop_est*out_per),
         host_p_est = round(host_per*pop_est)) %>% arrange(year, sex)

demand_est_yr <- demand_est %>% group_by(sido, year) %>% summarise(out_p_est = sum(out_p_est),
                                                                   host_p_est = sum(host_p_est))

demand_est_yr1 <- demand_est_yr %>% mutate(total_demand = out_p_est + host_p_est * 3)

# demand_est_yr2 <- demand_est_yr1
# demand_est_yr2$out_p_est <- format(demand_est_yr2$out_p_est, digits = 10, big.mark = ",")
# demand_est_yr2$host_p_est <- format(demand_est_yr2$host_p_est, digits = 10, big.mark = ",")



#################
local_n <- c("Seoul", "Busan", "Daegu", "Incheon", "Gwangju", "Daejeon",
             "Ulsan", "Sejong", "Gyunggi", "Gwangwon", "Chungbuk", "Chungnam",
             "Jeonbuk", "Jeonnam", "Kyungbuk", "Kyungnam", "Jaeju")

demand_est_fin <- demand_est_yr1 %>% as.data.frame %>%
  mutate(sido = case_when(sido == "서울특별시" ~ "Seoul",
                          sido == "부산광역시" ~ "Busan",
                          sido == "대구광역시" ~ "Daegu",
                          sido == "인천광역시" ~ "Incheon",
                          sido == "광주광역시" ~ "Gwangju",
                          sido == "대전광역시" ~ "Daejeon",
                          sido == "울산광역시" ~ "Ulsan",
                          sido == "세종특별자치시" ~ "Sejong",
                          sido == "경기도" ~ "Gyunggi",
                          sido == "강원도" ~ "Gwangwon",
                          sido == "충청북도" ~ "Chungbuk",
                          sido == "충청남도" ~ "Chungnam",
                          sido == "전라북도" ~ "Jeonbuk",
                          sido == "전라남도" ~ "Jeonnam",
                          sido == "경상북도" ~ "Kyungbuk",
                          sido == "경상남도" ~ "Kyungnam",
                          sido == "제주특별자치도" ~ "Jaeju"))

demand_est_fin1 <- demand_est_fin %>% reshape::cast(year ~ sido, value = "total_demand") %>% dplyr::select(local_n)
demand_est_fin2 <- demand_est_fin1 %>% mutate(year = 2018:2047)
demand_est_fin3 <- demand_est_fin2 %>% mutate(Total = apply(demand_est_fin2 %>% dplyr::select(-year), 1, sum))


### forforfor -----------
med_admin_p_1 = c(1371, 1371, 1371, 1538, 1538, 2557, 2557, 2889, 2602, 2889, 2889) # initial admitted students.
med_admin_p_2 = c(1643, 1689, 1689, 1689, 1689, 1242, 1227, 203, 174, 174, 169) # initial admitted college students.
med_admin_p_3 = c(97, 93, 95, 98, 98, 277, 277, 585, 583, 380, 307)

admin_set_2 = 80
admin_set_3 = 0
dead_rate = c(36.6, 66.5, 147, 322.4, 684.9, 2092.6)
pass_rate = .95
year= 2047

for(admin_inc in c(0, 250, 500, 750, 1000, 1250, 1500)){
  for(old_capa in c(0.5, 0.75)){
    admin_set_1 = 2977 + admin_inc
    
    local_all_dat <- list()
    for(m in 1:nrow(local_dat)){
      age_init <- local_dat[m, c(1:5)] %>% as.numeric
      local_n <- local_dat$area[m]
      
      k = year - 2017 + 1
      yr = c(2017:year)
      
      local_per = numeric(k)
      local_per[1] = local_dat$local_per[m]
      
      age20_ago = numeric(k)
      age30_ago = numeric(k)
      age40_ago = numeric(k)
      age50_ago = numeric(k)
      age60_ago = numeric(k)
      age65_ago = numeric(k)
      age70_ago = numeric(k)
      
      age20_ago[1] = age_init[1]
      age30_ago[1] = age_init[2]
      age40_ago[1] = age_init[3]
      age50_ago[1] = age_init[4]
      age60_ago[1] = age_init[5]*1/3
      age65_ago[1] = age_init[5]*1/3
      age70_ago[1] = age_init[5]*1/3
      
      
      age20 = numeric(k)
      age30 = numeric(k)
      age40 = numeric(k)
      age50 = numeric(k)
      age60 = numeric(k)
      age65 = numeric(k)
      age70 = numeric(k)
      
      # age20_med = numeric(k)
      # age30_med = numeric(k)
      # age40_med = numeric(k)
      # age50_med = numeric(k)
      # age60_med = numeric(k)
      # age65_med = numeric(k)
      # age70_med = numeric(k)
      # 
      med_total = numeric(k)
      med_young = numeric(k)
      med_old = numeric(k)
      
      pass_20 = numeric(k)
      pass_30 = numeric(k)
      
      med_admin_1 = c(med_admin_p_1, rep(admin_set_1, k-length(med_admin_p_1)))
      med_admin_2 = c(med_admin_p_2, rep(admin_set_2, k-length(med_admin_p_2)))
      med_admin_3 = c(med_admin_p_3, rep(admin_set_3, k-length(med_admin_p_3)))
      
      applicant_20 = numeric(k)
      applicant_30 = numeric(k)
      
      # fail_20 = numeric(k)
      # fail_30 = numeric(k)
      
      ## variables for supply - demand
      
      
      doc_capa_med = numeric(k)
      doc_demand_med_tech = numeric(k)
      sup_demand_med_tech = numeric(k)
      sup_demand_med_tech_1000 = numeric(k)
      
      
      result <- cbind(yr, pass_20, pass_30, age20, age30, age40, age50, age60, age65, age70,
                      med_total, med_young, med_old,
                      age20_ago, age30_ago, age40_ago, age50_ago, age60_ago, age65_ago, age70_ago,
                      med_admin_1, med_admin_2, med_admin_3, applicant_20, applicant_30,local_per,
                      doc_capa_med, doc_demand_med_tech, sup_demand_med_tech, sup_demand_med_tech_1000) %>% as.data.frame
      result <- result %>% mutate(area = local_n)
      
      ## join demand data 
      colnames(result)[1] <- "year"
      demand_sub <- demand_est_fin3 %>% dplyr::select(year, local_n)
      demand_2017 <- data.frame(year = 2017, local_n = 0)
      colnames(demand_2017)[2] <- local_n
      demand_sub <- rbind(demand_2017, demand_sub)
      colnames(demand_sub)[2] <- "total_demand"
      
      result <- result %>% left_join(demand_sub, by = "year")
      
      ## join pop estimation data
      
      pop_est_local <- pop_est_total1 %>% filter(sido == local_n)
      pop_est_local_2017 = data.frame(sido = local_n, year = 2017, pop_est = 0)
      pop_est_local <- rbind(pop_est_local_2017, pop_est_local) %>% dplyr::select(-sido)
      result <- result %>% left_join(pop_est_local, by = "year")
      
      
      local_all_dat[[m]] <- result
      
      
    }
    
    
    for(i in 1:k){
      neg_candi <- matrix(nrow = 18, ncol = 2)
      for(m in 1:length(local_all_dat)){
        local_n <- local_dat$area[m]
        subdat <- local_all_dat[[m]]
        
        if(subdat$applicant_20[i] == 0) {
          subdat$applicant_20[i] <- subdat$med_admin_1[i]
          subdat$applicant_30[i] <- subdat$med_admin_2[i] + subdat$med_admin_3[i]
        }
        if(local_n == "Total"){
          subdat$local_per[i] = 1
          local_per = 1
        }else if (i == 2){
          local_per <- subdat$local_per[1]
        }else{
          local_per <- subdat$local_per[i]
        }
        local_per_noinc <- subdat$local_per[1]
        
        if(i %in% c(1:11)){
          subdat$pass_20[i] = subdat$applicant_20[i]*pass_rate*local_per_noinc
          subdat$pass_30[i] = subdat$applicant_30[i]*pass_rate*local_per_noinc
        }else{
          subdat$pass_20[i] = (subdat$applicant_20[i]-admin_inc)*pass_rate*local_per_noinc+
            (admin_inc)*pass_rate*local_per
          subdat$pass_30[i] = subdat$applicant_30[i]*pass_rate*local_per_noinc
        }
        

        # age20[i] = (age20_ago[i]-pass_5yr[i])*(1-dead_rate[1]/10^5)+pass[i]
        # age30[i] = pass_5yr[i]*(1-dead_rate[1]/10^5) + age30_ago[i]*.9*(1-dead_rate[2]/10^5)
        subdat$age20[i] = (subdat$age20_ago[i]*.8)*(1-dead_rate[1]/10^5)+subdat$pass_20[i]
        subdat$age30[i] = (subdat$age20_ago[i]*.2)*(1-dead_rate[1]/10^5) + (subdat$age30_ago[i]*.9 + subdat$pass_30[i])*(1-dead_rate[2]/10^5)
        subdat$age40[i] = subdat$age30_ago[i]*.1*(1-dead_rate[2]/10^5) + subdat$age40_ago[i]*.9*(1-dead_rate[3]/10^5)
        subdat$age50[i] = subdat$age40_ago[i]*.1*(1-dead_rate[3]/10^5) + subdat$age50_ago[i]*.9*(1-dead_rate[4]/10^5)
        subdat$age60[i] = subdat$age50_ago[i]*.1*(1-dead_rate[4]/10^5) + subdat$age60_ago[i]*.8*(1-dead_rate[5]/10^5)
        subdat$age65[i] = subdat$age60_ago[i]*.2*(1-dead_rate[4]/10^5) + subdat$age60_ago[i]*.8*(1-dead_rate[5]/10^5)
        subdat$age70[i] = subdat$age65_ago[i]*.2*(1-dead_rate[5]/10^5) + subdat$age60_ago[i]*.8*(1-dead_rate[6]/10^5)
        
        if(i != k){
          subdat$age20_ago[i+1] = subdat$age20[i]
          subdat$age30_ago[i+1] = subdat$age30[i]
          subdat$age40_ago[i+1] = subdat$age40[i]
          subdat$age50_ago[i+1] = subdat$age50[i]
          subdat$age60_ago[i+1] = subdat$age60[i]
          subdat$age65_ago[i+1] = subdat$age65[i]
          subdat$age70_ago[i+1] = subdat$age70[i]
        }
        # age20_med[i] = age20[i]*.764
        # age30_med[i] = age30[i]*.864
        # age40_med[i] = age40[i]*.901
        # age50_med[i] = age50[i]*.895
        # age60_med[i] = age60[i]*.714
        # age65_med[i] = age65[i]*.714
        # age70_med[i] = age70[i]*.714
        
        subdat$med_young[i] = subdat$age20[i] + subdat$age30[i] + subdat$age40[i] + subdat$age50[i] + subdat$age60[i]
        subdat$med_old[i] = subdat$age65[i] + subdat$age70[i]
        subdat$med_total[i] = subdat$med_young[i] + subdat$med_old[i]
        
        if(local_n == "Total" & i == 2){
          doc_capa = (subdat$total_demand[i])/265/(subdat$med_young[i] + subdat$med_old[i]*old_capa)
          # doc_capa_75 = (subdat$total_demand[i])/265/(subdat$med_young[i] + subdat$med_old[i]*.75)*(1.005)^(i-2)
        }
        
        if(i != 1){
          subdat$doc_capa_med[i] = doc_capa*(1.005)^(i-2)
          subdat$doc_demand_med_tech[i] = subdat$total_demand[i]/265/subdat$doc_capa_med[i]
          subdat$sup_demand_med_tech[i] = (subdat$med_young[i] + subdat$med_old[i]*old_capa) - subdat$doc_demand_med_tech[i]
          
          subdat$sup_demand_med_tech_1000[i] = subdat$sup_demand_med_tech[i]/ subdat$pop_est[i] *1000
          if(subdat$sup_demand_med_tech_1000[i] < 0){
            neg_candi[m,1] <- m
            neg_candi[m,2] <- subdat$sup_demand_med_tech_1000[i]
          }
        }
        
        if(local_n %in% c("Kyungbuk", "Chungnam", "Chungbuk", "Jeonnam", "Jaeju") & i >=2 & i !=k ){
          subdat$local_per[i+1] <- 1/5
        }
        local_all_dat[[m]] <- subdat
        
      }
      
      # if(i >=2 & i != k){
      #   neg_candi <- neg_candi %>% as.data.frame
      #   colnames(neg_candi) <- c("ind", "per1000")
      #   neg_candi_omit <- neg_candi %>% na.omit %>% arrange(per1000) # working
      #   neg_candi_sub <- neg_candi_omit[c(1:5), ]
      #   neg_candi_sub <- neg_candi_sub %>% mutate(perweight = abs(per1000)/sum(abs(neg_candi_omit$per1000)))
      #   for(m in neg_candi_omit[,1]){
      #     subdat <- local_all_dat[[m]]
      #     subdat$local_per[i+1] <- neg_candi_omit[which(neg_candi_omit$ind == m),3]
      #     local_all_dat[[m]] <- subdat
      #   }
      # }
    }
    
    result_dat <- rbindlist(local_all_dat)
    result_dat <- result_dat %>% filter(year != 2017)
    f_n <- paste0("scen_", admin_inc, "_265_0.005_", old_capa, ".csv")
    fwrite(result_dat, paste0("result_local_renewal2/", f_n))
  }
}


