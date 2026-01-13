#traitdata 불러오기####
#install.packages(c("data.table", "dplyr", "tidyr", "jsonlite", "curl"))
#install.packages("rtry")
library(rtry)

traitdata <- rtry_import("./data/46171_29122025114036/46171.txt", separator = "\t",
                         encoding = "Latin-1",
                         quote = "",
                         showOverview = TRUE
)

table(traitdata$TraitName)

traitlist <- read.csv("./data/traitlist_260102.csv", header = TRUE)

str(traitdata)
str(traitlist)

library(dplyr)

#필수 trait만 뽑아서 피벗 테이블 만들기####
# 1. 내가 원하는 Trait 이름 리스트 추출
target_traits <- traitlist$Trait.name

# 2. 데이터 필터링 및 테이블 생성
# traitdata에서 TraitName이 target_traits에 포함된 것만 골라냅니다.
trait_summary <- traitdata %>%
  filter(TraitName %in% target_traits) %>%
  # 종(AccSpeciesName)과 특성(TraitName)별로 그룹화하여 개수를 셉니다.
  group_by(AccSpeciesName, TraitName) %>%
  summarise(count = n(), .groups = "drop")

# 3. 보기 편하게 Wide-format(가로 형태) 테이블로 변환
# 종이 행(Row)으로, Trait 이름이 열(Column)로 오게 만듭니다.
trait_table_wide <- trait_summary %>%
  tidyr::pivot_wider(names_from = TraitName, values_from = count, values_fill = 0)

# 결과 확인
print(trait_table_wide)

# 1. 숫자 열(형질들)에 대해 값이 0보다 큰 행의 개수만 카운트
total_row <- trait_table_wide %>%
  summarise(across(where(is.numeric), ~ sum(. > 0))) %>%
  mutate(AccSpeciesName = "Total_Species_Count") # 첫 번째 열 이름을 구분하기 쉽게 변경

# 2. 원래 테이블 아래에 합계 행 붙이기
trait_table_final <- bind_rows(trait_table_wide, total_row)

# 확인
tail(trait_table_final)

#write.csv(trait_table_final, "./data/trait_table_260102.csv", row.names = TRUE)

#종자 무게, SLA, LWC에 대한 정보를 모두 가진 종은 몇 종이나 될까?####
##seed mass
trait_seedmass <- traitdata %>%
  filter(TraitName == "Seed dry mass")

dplyr::n_distinct(trait_seedmass$AccSpeciesName)

seedmasslist <- unique(trait_seedmass$AccSpeciesName)

##SLA
trait_sla <- traitdata %>%
  filter(TraitName %in% c("Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included", "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded", "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded"))

dplyr::n_distinct(trait_sla$AccSpeciesName)

slalist <- unique(trait_sla$AccSpeciesName)

##LWC
trait_lwc <- traitdata %>%
  filter(TraitName %in% c("Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)", "Leaf water content per leaf dry mass (not saturated)"))

dplyr::n_distinct(trait_lwc$AccSpeciesName)

lwclist <- unique(trait_lwc$AccSpeciesName)

str(seedmasslist)
str(slalist)
str(lwclist)

##공통 종목록 뽑기
common_species <- Reduce(intersect, list(seedmasslist, slalist, lwclist))
length(common_species)

all(common_species %in% seedmasslist)
all(common_species %in% slalist)
all(common_species %in% lwclist)

commonlist <- traitdata %>%
  filter(AccSpeciesName %in% common_species) %>%
  distinct(AccSpeciesName, AccSpeciesID)

#write.csv(commonlist, "./data/common_trait_list_260105.csv", row.names = TRUE)

#공통 종 목록에서 분산형과 생활형이 나와있는지 확인하기####
commonlist <- read.csv("./data/common_trait_list_260105.csv", header = TRUE)
commonspeciesid <- commonlist$AccSpeciesID

trait_selected <- traitdata %>%
  filter(AccSpeciesID %in% commonspeciesid) %>%
  filter(TraitName %in% c("Plant growth form", "Dispersal syndrome", "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded", "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included", "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded", "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)", "Leaf water content per leaf dry mass (not saturated)", "Seed dry mass"))

write.csv(trait_selected, "./data/common_selected_trait_260109.csv")

##다시 피벗테이블 만들기
trait_sel_summary <- trait_selected %>%
  group_by(AccSpeciesName, TraitName) %>%
  summarise(count = n(), .groups = "drop")

# 3. 보기 편하게 Wide-format(가로 형태) 테이블로 변환
# 종이 행(Row)으로, Trait 이름이 열(Column)로 오게 만듭니다.
trait_sel_table_wide <- trait_sel_summary %>%
  tidyr::pivot_wider(names_from = TraitName, values_from = count, values_fill = 0)

# 결과 확인
print(trait_sel_table_wide)

# 1. 숫자 열(형질들)에 대해 값이 0보다 큰 행의 개수만 카운트
total_sel_row <- trait_sel_table_wide %>%
  summarise(across(where(is.numeric), ~ sum(. > 0))) %>%
  mutate(AccSpeciesName = "Total_Species_Count") # 첫 번째 열 이름을 구분하기 쉽게 변경

# 2. 원래 테이블 아래에 합계 행 붙이기
trait_sel_table_final <- bind_rows(trait_sel_table_wide, total_sel_row)

# 확인
tail(trait_sel_table_final)

#일반 종 출현정보 불러오기####
#load occurrence data
library(dplyr)

# 파일 경로와 구분 이름 정의
file_info <- data.frame(
  source = c("nfi_5th", "nfi_4th", "nfi_3rd", "nfi_2nd"),
  path = c(
    "D:/data/전국자연환경조사/전국자연환경조사_5차_2019_2021_전국/식물상/전국자연환경조사_5차_식물상_2019_2021_전국.csv",
    "D:/data/전국자연환경조사/전국자연환경조사_4차_2014_2018_전국/식물상/전국자연환경조사_4차_식물상_2014_2018_전국.csv",
    "D:/data/전국자연환경조사/전국자연환경조사_3차_2006_2013_전국/식물상/전국자연환경조사_3차_식물상_2006_2013_전국.csv",
    "D:/data/전국자연환경조사/전국자연환경조사_2차_1997_2005_전국/식물상/전국자연환경조사_2차_식물상_1997_2005_전국.csv"),
  stringsAsFactors = FALSE
)

# 모든 파일 읽어서 하나의 데이터프레임으로 병합 (출처 정보도 함께)
nfi_data <- do.call(rbind, lapply(1:nrow(file_info), function(i) {
  df <- read.csv(file_info$path[i], header = TRUE)
  df$source <- file_info$source[i]
  df
}))

bdplant <- read.csv("D:/data/국립공원/백두대간정밀조사_2012_2022_전국/백두대간정밀조사_식물상_2015_2022_전국/백두대간정밀조사_식물상_2015_2022_전국_수정.csv")
bdveg <- read.csv("D:/data/국립공원/백두대간정밀조사_2012_2022_전국/백두대간정밀조사_식생_2019_2022_전국/백두대간정밀조사_식생_2019_2022_전국.csv")
tjplant <- read.csv("D:/data/국립공원/특정지역정밀조사_2004_2022_전국/식물상/특정지역정밀조사_식물상_2009_2022_전국.csv")
tjveg <- read.csv("D:/data/국립공원/특정지역정밀조사_2004_2022_전국/식생/특정지역정밀조사_식생_2017_2022_전국.csv")
names(nfi_data)

# 필요한 열 정의
cols_needed <- c("id", "한글보통명", "위도", "경도")

# 각 데이터프레임에서 필요한 열 추출
bdplant_sub <- bdplant[, cols_needed]
bdveg_sub   <- bdveg[, cols_needed]
tjplant_sub <- tjplant[, cols_needed]
tjveg_sub   <- tjveg[, cols_needed]
nfi_sub     <- nfi_data[, cols_needed]

# 병합
combined_data <- rbind(
  bdplant_sub,
  bdveg_sub,
  tjplant_sub,
  tjveg_sub,
  nfi_sub
)

names(combined_data) <- c("X", "국명", "decimalLatitude", "decimalLongitude")

combined_data <- combined_data %>%
  filter(!is.na(국명), trimws(국명) != "")

#filtering occurrence records excluding overseas
library(terra)

r <- rast("./data/terrestrial_250509.tif")

combined_sv <- vect(
  combined_data,
  geom = c("decimalLongitude", "decimalLatitude"),
  crs = "EPSG:4326"   # WGS84
)

inside <- !is.na(extract(r, combined_sv)[,1])
combined_land <- combined_sv[inside, ]
combined_land_df <- as.data.frame(combined_land)

ioccurrence <- table(combined_land_df$국명)

#write.csv(ioccurrence, file = "./data/commonspecies_table_260109.csv")

summary(as.numeric(ioccurrence))

ioccurrence_186 <- ioccurrence[ioccurrence >= 186]
summary(as.numeric(ioccurrence_186))

#write.csv(ioccurrence_186, file = "./data/commonspecies_186_table_260109.csv")

#TRY에서 온 자료 열어보기
library(rtry)

commontraitdata <- rtry_import("./data/46438_12012026084334.repair/46438.txt", separator = "\t",
                         encoding = "Latin-1",
                         quote = "",
                         showOverview = TRUE
)

commonspecieslist <- read.csv("./data/traits/commonspecies_list_260112.csv", header = TRUE)
commonsplist <- commonspecieslist$AccSpeciesID

library(dplyr)

commontraits2 <- commontraitdata %>%
  filter(AccSpeciesID %in% commonsplist)

#write.csv(commontraits2, file = "./data/traits/commonspecies_traits_260112.csv")
