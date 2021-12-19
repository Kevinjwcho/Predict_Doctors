library(data.table)
library(dplyr)
library(stringr)
library(knitr)
library(ggplot2)
library(kableExtra)
# setwd("C:/Users/kevin/Dropbox")





## function
supply_pred<- function(year, med_admin_p_1, med_admin_p_2, med_admin_p_3,
                       admin_set_1, admin_set_2, admin_set_3, dead_rate, pass_rate,
                       age_init, mid_yr = NULL, admin_set_4){
  
  if(is.null(mid_yr)){
    k= year - 2017 + 1
    yr = c(2017:year)
    
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
    
    pass_20 = numeric(k)
    pass_30 = numeric(k)
    
    med_admin_1 = c(med_admin_p_1, rep(admin_set_1, k-length(med_admin_p_1)))
    med_admin_2 = c(med_admin_p_2, rep(admin_set_2, k-length(med_admin_p_2)))
    med_admin_3 = c(med_admin_p_3, rep(admin_set_3, k-length(med_admin_p_3)))
    
    applicant_20 = numeric(k)
    applicant_30 = numeric(k)
    
    fail_20 = numeric(k)
    fail_30 = numeric(k)
    
    
    for(i in 1:k){
      if(applicant_20[i] == 0) {
        applicant_20[i] <- med_admin_1[i]
        applicant_30[i] <- med_admin_2[i] + med_admin_3[i]
      }
      pass_20[i] = applicant_20[i]*pass_rate
      pass_30[i] = applicant_30[i]*pass_rate
      fail_20[i] = applicant_20[i]*(1-pass_rate)
      fail_30[i] = applicant_30[i]*(1-pass_rate)
      
      # age20[i] = (age20_ago[i]-pass_5yr[i])*(1-dead_rate[1]/10^5)+pass[i]
      # age30[i] = pass_5yr[i]*(1-dead_rate[1]/10^5) + age30_ago[i]*.9*(1-dead_rate[2]/10^5)
      age20[i] = (age20_ago[i]*.8)*(1-dead_rate[1]/10^5)+pass_20[i]
      age30[i] = (age20_ago[i]*.2)*(1-dead_rate[1]/10^5) + (age30_ago[i]*.9 + pass_30[i])*(1-dead_rate[2]/10^5)
      age40[i] = age30_ago[i]*.1*(1-dead_rate[2]/10^5) + age40_ago[i]*.9*(1-dead_rate[3]/10^5)
      age50[i] = age40_ago[i]*.1*(1-dead_rate[3]/10^5) + age50_ago[i]*.9*(1-dead_rate[4]/10^5)
      age60[i] = age50_ago[i]*.1*(1-dead_rate[4]/10^5) + age60_ago[i]*.8*(1-dead_rate[5]/10^5)
      age65[i] = age60_ago[i]*.2*(1-dead_rate[4]/10^5) + age60_ago[i]*.8*(1-dead_rate[5]/10^5)
      age70[i] = age65_ago[i]*.2*(1-dead_rate[5]/10^5) + age60_ago[i]*.8*(1-dead_rate[6]/10^5)
      
      age20_ago[i+1] = age20[i]
      age30_ago[i+1] = age30[i]
      age40_ago[i+1] = age40[i]
      age50_ago[i+1] = age50[i]
      age60_ago[i+1] = age60[i]
      age65_ago[i+1] = age65[i]
      age70_ago[i+1] = age70[i]
    } 
  }else{
    k = year - 2017 + 1
    k1= mid_yr - 2017 + 1
    k2= year - mid_yr + 1
    yr1 = c(2017:(mid_yr-1))
    yr2 = c(mid_yr:year)
    yr = c(2017:year)
    
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
    
    pass_20 = numeric(k)
    pass_30 = numeric(k)
    
    # med_admin = c(med_admin_p, rep(admin_set, k1-length(med_admin_p)), rep(mid_admin_set, k2))
    # med_admin_1 = c(med_admin_p_1, rep(admin_set_1, k1-length(med_admin_p_1)), admin_set_1*reduce_rate^seq(1,k2,1))
    med_admin_1 = c(med_admin_p_1, rep(admin_set_1, k1-length(med_admin_p_1)), rep(admin_set_4, k2))
    med_admin_2 = c(med_admin_p_2, rep(admin_set_2, k-length(med_admin_p_2)))
    med_admin_3 = c(med_admin_p_3, rep(admin_set_3, k-length(med_admin_p_3)))
    
    applicant_20 = numeric(k)
    applicant_30 = numeric(k)
    
    fail_20 = numeric(k)
    fail_30 = numeric(k)
    
    
    for(i in 1:k){
      if(applicant_20[i] == 0){
        applicant_20[i] <- med_admin_1[i]
        applicant_30[i] <- med_admin_2[i] + med_admin_3[i] 
      }
      pass_20[i] = applicant_20[i]*pass_rate
      pass_30[i] = applicant_30[i]*pass_rate
      fail_20[i] = applicant_20[i]*(1-pass_rate)
      fail_30[i] = applicant_30[i]*(1-pass_rate)
      
      # age20[i] = (age20_ago[i]-pass_5yr[i])*(1-dead_rate[1]/10^5)+pass[i]
      # age30[i] = pass_5yr[i]*(1-dead_rate[1]/10^5) + age30_ago[i]*.9*(1-dead_rate[2]/10^5)
      age20[i] = (age20_ago[i]*.8)*(1-dead_rate[1]/10^5)+pass_20[i]
      age30[i] = (age20_ago[i]*.2)*(1-dead_rate[1]/10^5) + (age30_ago[i]*.9 + pass_30[i])*(1-dead_rate[2]/10^5)
      age40[i] = age30_ago[i]*.1*(1-dead_rate[2]/10^5) + age40_ago[i]*.9*(1-dead_rate[3]/10^5)
      age50[i] = age40_ago[i]*.1*(1-dead_rate[3]/10^5) + age50_ago[i]*.9*(1-dead_rate[4]/10^5)
      age60[i] = age50_ago[i]*.1*(1-dead_rate[4]/10^5) + age60_ago[i]*.8*(1-dead_rate[5]/10^5)
      age65[i] = age60_ago[i]*.2*(1-dead_rate[4]/10^5) + age60_ago[i]*.8*(1-dead_rate[5]/10^5)
      age70[i] = age65_ago[i]*.2*(1-dead_rate[5]/10^5) + age60_ago[i]*.8*(1-dead_rate[6]/10^5)
      
      age20_ago[i+1] = age20[i]
      age30_ago[i+1] = age30[i]
      age40_ago[i+1] = age40[i]
      age50_ago[i+1] = age50[i]
      age60_ago[i+1] = age60[i]
      age65_ago[i+1] = age65[i]
      age70_ago[i+1] = age70[i]
    } 
  }
  
  result <- cbind(yr, pass_20, pass_30, fail_20, fail_30, age20, age30, age40, age50, age60, age65, age70) %>% as.data.frame
  result <- result %>% mutate(
    age20_med = age20*.764,
    age30_med = age30*.864,
    age40_med = age40*.901,
    age50_med = age50*.895,
    age60_med = age60*.714,
    age65_med = age65*.714,
    age70_med = age70*.714,
    med_total = age20_med+age30_med+age40_med+age50_med+age60_med+age65_med+age70_med,
    med_young = age20_med+age30_med+age40_med+age50_med+age60_med,
    med_old = age65_med+age70_med,
    license_total = age20+age30+age40+age50+age60+age65+age70,
    year = yr
  )
  return(result)
}


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

# path <- c("C:/Users/kevin/Dropbox/SNU_medical/의사추계연구/Macro_trend/origin_data/demand/성별연령별_입원외래방문횟수_2003_2018.xlsx")
path <- c("D:/Dropbox/SNU_medical/의사추계연구/Macro_trend/origin_data/demand/성별연령별_입원외래방문횟수_2003_2018.xlsx")
demand_data <- read_excel_allsheets(path)


for(i in 1:16){
  demand_data[[i]] <- demand_data[[i]] %>% select(-`...1`) %>% mutate_all(as.numeric)%>%
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

demand_18yr_1 <- demand_18yr %>% select(-insurance) %>% mutate(pop_num = out_p/out_per) %>% select(-out_per, -host_per, -year)
demand_18yr_1<- demand_18yr_1 %>% mutate(sex = tstrsplit(sex_age, "_", keep = 2) %>% unlist,
                                         age = tstrsplit(sex_age, "_", keep = 1) %>% unlist)

demand_18yr_2 <- demand_18yr_1 %>% filter(!is.na(sex), age != "total") %>% select(-sex_age)

sub_m<- c(demand_18yr_2[1, 1:3] + demand_18yr_2[3, 1:3], "m", "age0004") %>% unlist
sub_f<- c(demand_18yr_2[2, 1:3] + demand_18yr_2[4, 1:3], "f", "age0004") %>% unlist
# with(demand_18yr_2, sum(out_p[sex == "m" & age %in% c("age00", "age0104")]))

demand_18yr_3 <- rbind(sub_m, sub_f, demand_18yr_2[-c(1:4), ])

str(demand_18yr_3)

demand_18yr_3[,1:3] <- demand_18yr_3[,1:3] %>% mutate_all((as.numeric))

demand_18yr_3 <- demand_18yr_3 %>% mutate(out_per = out_p/ pop_num,
                                          host_per = host_p/pop_num)

# setwd("C:/Users/kevin/Dropbox")
# pop_estimate <- fread("C:/Users/kevin/Dropbox/SNU_medical/의사추계연구/Macro_trend/Total_modeling/demand/pop_estimate.csv")
pop_estimate <- fread("D:/Dropbox/SNU_medical/의사추계연구/Macro_trend/Total_modeling/demand/pop_estimate.csv")

# str(pop_estimate)
colnames(pop_estimate)[1:5] <- c("scean", "sex", "age", "remark", "scale")

# table(pop_estimate$scean)
pop_est1 <- pop_estimate %>% filter(scean == "출산율 현수준 추계(출산율-2018년 출산율 유지 / 기대수명-중위 / 국제순이동-중위)",
                                    sex != "전체",
                                    age != "계") %>% select(-scean, -remark, -scale)

pop_est2 <- pop_est1 %>% reshape2::melt(id = 1:2)
colnames(pop_est2)[3:4] <- c("year", "pop_est")
str(pop_est2)

pop_est_85 <- pop_est2 %>% filter(age %in% c("85-89세", "90-94세", "95-99세", "100세 이상")) %>% group_by(sex, year) %>% summarise(sum(pop_est))

pop_est_85$age <- c("age85")

pop_est_85_2 <- pop_est_85[,c(1,4,2,3)] %>% as.data.frame
colnames(pop_est_85_2)[4] <- "pop_est"

pop_est3 <- rbind(pop_est2 %>% filter(!(age %in% c("85-89세", "90-94세", "95-99세", "100세 이상"))),
                  pop_est_85_2)

pop_est4<- pop_est3 %>% mutate(sex = ifelse(sex == "남자", "m", "f"),
                               age = case_when(
                                 age == "0-4세" ~ "age0004",
                                 age == "5-9세" ~ "age0509",
                                 age == "10-14세" ~ "age1014",
                                 # age == "15-19세" ~ "age1519",
                                 age == "20-24세" ~ "age2024",
                                 age == "25-29세" ~ "age2529",
                                 age == "30-34세" ~ "age3034",
                                 age == "35-39세" ~ "age3539",
                                 age == "40-44세" ~ "age4044",
                                 age == "45-49세" ~ "age4549",
                                 age == "50-54세" ~ "age5054",
                                 age == "55-59세" ~ "age5559",
                                 age == "60-64세" ~ "age6064",
                                 age == "65-69세" ~ "age6569",
                                 age == "70-74세" ~ "age7074",
                                 age == "75-79세" ~ "age7579",
                                 age == "80-84세" ~ "age8084",
                                 age == "age85" ~ "age85",
                                 TRUE ~ "age1519"
                               ),
                               year = tstrsplit(year, " ", keep = 1) %>% unlist %>% as.numeric)


demand_18yr_fin<- demand_18yr_3 %>% select(sex, age, out_per, host_per)

demand_est <- pop_est4 %>% filter(year != 2017) %>%  left_join(demand_18yr_fin) %>%
  mutate(out_p_est = round(pop_est*out_per),
         host_p_est = round(host_per*pop_est)) %>% arrange(year, sex)

demand_est_yr <- demand_est %>% group_by(year) %>% summarise(out_p_est = sum(out_p_est),
                                                             host_p_est = sum(host_p_est))

demand_est_yr1 <- demand_est_yr %>% mutate(total_demand = out_p_est + host_p_est * 3)

demand_est_yr2 <- demand_est_yr1
demand_est_yr2$out_p_est <- format(demand_est_yr2$out_p_est, digits = 10, big.mark = ",")
demand_est_yr2$host_p_est <- format(demand_est_yr2$host_p_est, digits = 10, big.mark = ",")


sup_demand<- function(admin_set_1, admin_set_2, admin_set_3, demand_est_yr, mid_yr = NULL, admin_set_4,
                      prod_rate, old_capa){
  med_admin_p_1 = c(1371, 1371, 1371, 1538, 1538, 2557, 2557, 2889, 2602, 2889, 2889) # initial admitted students.
  med_admin_p_2 = c(1643, 1689, 1689, 1689, 1689, 1242, 1227, 203, 174, 174, 169) # initial admitted college students.
  med_admin_p_3 = c(97, 93, 95, 98, 98, 277, 277, 585, 583, 380, 307)
  # admin_set_1 = 4977
  # admin_set_2 = 80
  # admin_set_3 = 0
  # apply_initial = 3319
  dead_rate = c(36.6, 66.5, 147, 322.4, 684.9, 2092.6)
  pass_rate = .95
  age_init = c(11513, 38001, 31005, 21400, 13404)
  result_sup_4000<- supply_pred(year = 2067, med_admin_p_1, med_admin_p_2, med_admin_p_3,
                                admin_set_1, admin_set_2, admin_set_3, dead_rate, pass_rate,
                                age_init, mid_yr, admin_set_4)
  
  result_4000 <- demand_est_yr %>% left_join(result_sup_4000) %>% select(year, out_p_est, host_p_est, license_total, med_total, med_young, med_old)
  
  result_4000<- result_4000 %>% mutate(doc_capa_med = (out_p_est+host_p_est*3)/265/(med_young + med_old*old_capa))
  result_4000<- result_4000 %>% mutate(doc_demand_100_med_tech = (out_p_est+host_p_est*3)/265/(result_4000$doc_capa_med[1]*(1+prod_rate)^seq(0, by = 1, length.out = (2067 - 2018 + 1))),
                                       sup_demand_100_med_tech = (med_young + med_old*old_capa) - doc_demand_100_med_tech)
  
  # result_4000<- result_4000 %>% mutate(doc_demand_100_med_tech_255 = (out_p_est+host_p_est*3)/255/(result_4000$doc_capa_med[1]*(1+prod_rate)^seq(0, by = 1, length.out = (2067 - 2018 + 1))),
  #                                      sup_demand_100_med_tech_255 = (med_young + med_old*1/2) - doc_demand_100_med_tech_255,
  # )
  return(result_4000)
}


plus_admin <- c(0, 250, 500, 750, 1000, 1250, 1500)
work_days <- c(265)
reduce_per <- c(0, 1)
prod_rate <- c(0.005)
old_capa <- c(0.5, 0.75)
# prod_rate <- c(0.0025)
# prod_rate <- c(0.005)
# prod_rate <- c(0)

setwd("D:/Dropbox/SNU_medical/의사추계연구/Macro_trend/Total_modeling/markdown/mid_final/result_total")


for(i in 1:length(plus_admin)){
  for(j in 1:length(prod_rate)){
    for(k in 1:length(work_days)){
      for(m in 1:length(old_capa)){
      f_n <- paste("scen", plus_admin[i], work_days[k], prod_rate[j], old_capa[m], sep = "_")
      result <- sup_demand(2977+plus_admin[i], 80, 0, demand_est_yr, prod_rate = prod_rate[j], old_capa = old_capa[m])
      sub <- result$sup_demand_100_med_tech
      sub_candi <- which(sub>0)
      if(sub_candi %>% length == 0){
        f_n_new <- paste0(f_n, "_nosurplus.csv")
        fwrite(result, f_n_new)
      }else if(result$year[sub_candi[length(sub_candi)]] < 2040){
        f_n_new <- paste0(f_n, "_nosurplus.csv")
        fwrite(result, f_n_new)
      }else if(result$year[sub_candi[length(sub_candi)]] > 2040 | result$year[sub_candi[1]] > 2040){
        # if(j == 1){
        #   f_n_new <- paste0(f_n, ".csv")
        #   fwrite(result, f_n_new)
        # }
        yr1 <- result$year[which(result$year>2040)]
        yr2 <- yr1[which(yr1 %in% result$year[sub_candi])]
        sur_yr <- yr2[1]
        for(l in 1:length(reduce_per)){
          result<- sup_demand(2977+plus_admin[i], 80, 0, demand_est_yr, mid_yr = (sur_yr-6),
                              2977+(plus_admin[i])*(1-reduce_per[l]),
                              prod_rate = prod_rate[j], old_capa = old_capa[m])
          f_n_new <- paste0(f_n, "_redyr",sur_yr-6,"_red", reduce_per[l], ".csv")
          fwrite(result, f_n_new)
        }
      }else{
        f_n_new <- paste0(f_n, "_supersurplus.csv")
        fwrite(result, f_n_new)
      }
    }
    }
  }
}




## plotting ---------------------------
library(ggplot2)
library(extrafont)
library(RColorBrewer)

# font_import(pattern = 'H2GTRM')
# theme_update(text=element_text(family="H2GTRM"))

# setwd("D:/Dropbox/SNU_medical/의사추계연구/Macro_trend/Total_modeling/markdown/mid_final")
setwd("C:/Users/kevin/Dropbox/SNU_medical/의사추계연구/Macro_trend/mid_range_modeling/mid_final")
path <- "result_total"
f_n <- list.files(path, full.names = T)
f_n_new <- substring(f_n, 14)
f_n_new <- gsub(".csv", "", f_n_new)

spsp5 <- function(x){
  sapply(x %>% strsplit("_"), function(y) y[5])
}

spsp7 <- function(x){
  sapply(x %>% strsplit("_"), function(y) y[7])
}


f_n1 <- f_n %>% as.data.frame %>% mutate(old_capa = apply(f_n_new %>% as.data.frame, 1, function(x) spsp5(x)),
                                         red = apply(f_n_new %>% as.data.frame, 1, function(x) spsp7(x))
)

colors <- brewer.pal(n = 6, name = "Blues")
colors <- colors[-1]

tab1_f_n <- f_n1 %>% filter(old_capa == "0.5", is.na(red) | red == "red0")
list_files <- lapply(tab1_f_n$. %>% as.character, fread)
test <- do.call("cbind", list_files)
# sub <- test[,c(1, 10, 20, 30, 40, 50, 60, 70)]
sub <- test[,c(1, 10*(1:7))]
sub <- sub[,-c(4, 5)] # remove over 1,000 (10/12)

# colnames(sub) <- c("year", "add0", "add1000", "add1250", "add1500", "add250", "add500", "add750")
colnames(sub) <- c("year", "add0", "add1000", "add250", "add500", "add750")

sub2 <- sub %>% melt(id =1)
# sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add0", "add250", "add500", "add750", "add1000", "add1250", "add1500")))
sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add0", "add250", "add500", "add750", "add1000")))

png("plotting_1012/result_prod0.5_nored.png", width = 1000, height = 600)
# theme_update(text=element_text(family="H2GTRM"))
ggplot(sub2, aes(x = year, y = value, color = variable))+geom_line(size = 1) + geom_hline(yintercept = 0)+
  scale_color_manual(name = "", labels = c("증원 0명", "증원 250명", "증원 500명",
                                             "증원 750명", "증원 1000명"),
                       values = colors,
                       guide = guide_legend(reverse = T)) +
  theme_minimal()+ theme(text = element_text(size = 16))+
  ylab("공급 - 수요") + xlab("연도") +ylim(c(-50000, 30000))
dev.off()


tab2_f_n <- f_n1 %>% filter(old_capa == "0.5", red == "red1")
list_files <- lapply(tab2_f_n$. %>% as.character, fread)
test <- do.call("cbind", list_files)
# sub <- test[,c(1, 10, 20, 30)]
sub <- test[,c(1, 10*(1:5))]
sub <- sub[,-c(3, 4)]

# colnames(sub) <- c("year", "add1000", "add1250", "add1500", "add500", "add750")
colnames(sub) <- c("year", "add1000", "add500", "add750")

sub2 <- sub %>% melt(id =1)
# sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add500", "add750", "add1000", "add1250", "add1500")))
sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add500", "add750", "add1000")))

png("plotting_1012/result_prod0.5_red1.png", width = 1000, height = 600)
ggplot(sub2, aes(x = year, y = value, color = variable))+geom_line(size = 1) + geom_hline(yintercept = 0)+
  # scale_color_discrete(name = "", labels = c("증원 1250명", "증원 1500명"),
  #                      guide = guide_legend(reverse = T)) +
  scale_color_manual(name = "", labels = c("증원 500명",
                                           "증원 750명", "증원 1000명"),
                     values = colors[3:7],
                     guide = guide_legend(reverse = T)) +
  theme_minimal()+ theme(text = element_text(size = 16))+
  ylab("공급 - 수요") + xlab("연도") +ylim(c(-50000, 30000))
dev.off()


tab3_f_n <- f_n1 %>% filter(old_capa == "0.75", is.na(red) | red == "red0")
list_files <- lapply(tab3_f_n$. %>% as.character, fread)
test <- do.call("cbind", list_files)
# sub <- test[,c(1, 10, 20, 30)]
sub <- test[,c(1, 10*(1:7))]
sub <- sub[,-c(4, 5)]


# colnames(sub) <- c("year", "add0", "add1000", "add1250", "add1500", "add250", "add500", "add750")
colnames(sub) <- c("year", "add0", "add1000",  "add250", "add500", "add750")

sub2 <- sub %>% melt(id =1)
# sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add0", "add250", "add500", "add750", "add1000", "add1250", "add1500")))
sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add0", "add250", "add500", "add750", "add1000")))


png("plotting_1012/result_prod0.75_nored.png", width = 1000, height = 600)
  ggplot(sub2, aes(x = year, y = value, color = variable))+geom_line(size = 1) + geom_hline(yintercept = 0)+
  scale_color_manual(name = "", labels = c("증원 0명", "증원 250명", "증원 500명",
                                           "증원 750명", "증원 1000명"),
                     values = colors,
                     guide = guide_legend(reverse = T)) +
  theme_minimal()+ theme(text = element_text(size = 16))+
  ylab("공급 - 수요") + xlab("연도") +ylim(c(-50000, 30000))
dev.off()


tab4_f_n <- f_n1 %>% filter(old_capa == "0.75", red == "red1")
list_files <- lapply(tab4_f_n$. %>% as.character, fread)
test <- do.call("cbind", list_files)
# sub <- test[,c(1, 10, 20, 30, 40, 50, 60, 70)]
sub <- test[,c(1, 10*(1:5))]
sub <- sub[,-c(3,4)]

colnames(sub) <- c("year", "add1000",  "add500", "add750")

sub2 <- sub %>% melt(id =1)
# sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add0", "add250", "add500", "add750", "add1000", "add1250", "add1500")))
sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add0", "add250", "add500", "add750", "add1000")))

png("plotting_1012/result_prod0.75_red1.png", width = 1000, height = 600)
ggplot(sub2, aes(x = year, y = value, color = variable))+geom_line(size = 1) + geom_hline(yintercept = 0)+
  scale_color_manual(name = "", labels = c("증원 500명",
                                           "증원 750명", "증원 1000명"),
                     values = colors[3:7],
                     guide = guide_legend(reverse = T)) +
  theme_minimal()+ theme(text = element_text(size = 16))+
  ylab("공급 - 수요") + xlab("연도") +ylim(c(-50000, 30000))
dev.off()

# 
# 
# tab5_f_n <- f_n1 %>% filter(prod == "0.005", red == "red0.5")
# list_files <- lapply(tab5_f_n$. %>% as.character, fread)
# test <- do.call("cbind", list_files)
# # sub <- test[,c(1, 10, 20, 30, 40, 50)]
# sub <- test[,c(1, 12*(1:4))]
# colnames(sub) <- c("year", "add1000", "add1250", "add1500", "add750")
# sub2 <- sub %>% melt(id =1)
# # sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add750", "add1000", "add1250", "add1500")))
# sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add0", "add250", "add500", "add750", "add1000", "add1250", "add1500")))
# 
# png("plotting/result_prod0.5_red0.5.png", width = 1000, height = 600)
# ggplot(sub2, aes(x = year, y = value, color = variable))+geom_line(size = 1) + geom_hline(yintercept = 0)+
#   # scale_color_discrete(name = "", labels = c("증원 750명", "증원 1000명", "증원 1250명", "증원 1500명"),
#   #                      guide = guide_legend(reverse = T)) +
#   scale_color_manual(name = "", labels = c("증원 750명", "증원 1000명", "증원 1250명", "증원 1500명"),
#                      values = colors[4:7],
#                      guide = guide_legend(reverse = T)) +
#   theme_minimal()+ theme(text = element_text(size = 16))+
#   ylab("공급 - 수요") + xlab("연도") + ylim(c(-50000, 30000))
# dev.off()
# 
# 
# 
# tab6_f_n <- f_n1 %>% filter(prod == "0.005", red == "red1")
# list_files <- lapply(tab6_f_n$. %>% as.character, fread)
# test <- do.call("cbind", list_files)
# # sub <- test[,c(1, 10, 20, 30, 40, 50)]
# sub <- test[,c(1, 12*(1:4))]
# colnames(sub) <- c("year", "add1000", "add1250", "add1500", "add750")
# sub2 <- sub %>% melt(id =1)
# # sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add500", "add750", "add1000", "add1250", "add1500")))
# sub2 <- sub2 %>% mutate(variable = factor(variable, levels = c("add0", "add250", "add500", "add750", "add1000", "add1250", "add1500")))
# 
# png("plotting/result_prod0.5_red1.png", width = 1000, height = 600)
# ggplot(sub2, aes(x = year, y = value, color = variable))+geom_line(size = 1) + geom_hline(yintercept = 0)+
#   # scale_color_discrete(name = "", labels = c("증원 500명",
#   #                                            "증원 750명", "증원 1000명", "증원 1250명", "증원 1500명"),
#   #                      guide = guide_legend(reverse = T)) +
#   scale_color_manual(name = "", labels = c("증원 750명", "증원 1000명", "증원 1250명", "증원 1500명"),
#                      values = colors[4:7],
#                      guide = guide_legend(reverse = T)) +
#   theme_minimal()+ theme(text = element_text(size = 16))+
#   ylab("공급 - 수요") + xlab("연도") + ylim(c(-40000, 30000))
# dev.off()


#################
med_admin_p_1 = c(1371, 1371, 1371, 1538, 1538, 2557, 2557, 2889, 2602, 2889, 2889) # initial admitted students.
med_admin_p_2 = c(1643, 1689, 1689, 1689, 1689, 1242, 1227, 203, 174, 174, 169) # initial admitted college students.
med_admin_p_3 = c(97, 93, 95, 98, 98, 277, 277, 585, 583, 380, 307)
admin_set_1 = 2977
admin_set_2 = 80
admin_set_3 = 0
dead_rate = c(36.6, 66.5, 147, 322.4, 684.9, 2092.6)
pass_rate = .95
age_init = c(11513, 38001, 31005, 21400, 13404)

result_sup_3365<- supply_pred(year = 2067, med_admin_p_1, med_admin_p_2, med_admin_p_3,
                              admin_set_1, admin_set_2, admin_set_3, dead_rate, pass_rate,
                              age_init)
png("plotting/fig1.png", width = 500, height = 300)
ggplot(result_sup_3365, aes(x = yr, y = license_total))+geom_line()+
  theme(axis.text.y = element_text(face = "bold", size = 12), axis.text.x = element_text(face = "bold", size = 12))+
  ylab("") + xlab("")
dev.off()



age_sup_3365 <- result_sup_3365 %>% select(yr, age20, age30, age40, age50 ,age60, age65, age70)
age_sup_3365 <- age_sup_3365 %>% melt(id.vars = "yr", variable.name = "age", value.name = "med_number")

png("plotting/fig2.png", width = 500, height = 300)
ggplot(age_sup_3365, aes( x = yr, y= med_number, fill = age))+geom_area(position = 'stack') +
  scale_fill_discrete(name = "", labels = c("20~29세", "30~39세", "40~49세", "50~59세", "60~64세",
                                            "65~69세", "70~74세"))+
  xlab("")+ylab("")+
  theme(axis.text.y = element_text(face = "bold", size = 12), axis.text.x = element_text(face = "bold", size = 12))
dev.off()

# ggplot(age_sup_3365, aes( x = yr, y= med_number, fill = age))+geom_area(position = 'fill') + ggtitle("추계 의사 수 연령 비율")

fwrite(result_sup_3365, "base_supply_estimate.csv")
