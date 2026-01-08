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

#종자 무게, SLA, LWC에 대한 정보를 모두 가진 종은 몇 종이나 될까?
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
