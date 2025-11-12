# 자료 불러오기####
# 우리나라에 해당하는 고도 자료 불러오기
library(terra)

elev <- rast("./data/terrestrial_250509.tif")

plot(elev)

# 기후 자료 불러오기
climlist <- list.files("./data/climate_chelsa_1981-2010", pattern = "\\.tif$", full.names = TRUE)
clim <- rast(climlist)

names(clim) <- c("bio1", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "gsp", "gst", "npp")

names(elev) <- c("elevation")

#좌표계를 m로 계산할 수 있는 5179로 바꾸고, 기후 변수들을 우리나라 모양에 맞게 자르자.

# 좌표계 지정하기
library(terra)

target_crs <- "EPSG:5179"

# 우리나라 모양으로 기후 변수 자르기
cropped_clim <- crop(clim, elev)

masked_clim <- mask(cropped_clim, elev)
masked_clim_5179 <- project(masked_clim, target_crs, res = 1000)

summary(masked_clim_5179)

plot(masked_clim_5179)

#변수 추리기####
library(usdm)

clm_df <- as.data.frame(masked_clim_5179, na.rm = TRUE)

# VIF 계산
vif_result <- vif(clm_df)
print(vif_result)

vif_filtered <- vifstep(clm_df, th = 10)  # threshold는 필요시 조정

# 선택된 변수 이름 확인
selected_vars <- vif_filtered@results$Variables
print(selected_vars)

# 선택된 변수들만 추출한 SpatRaster 생성
clm_var_selected <- subset(masked_clim_5179, selected_vars)

# 상관관계 그림 그리기
clm_df_selected <- as.data.frame(clm_var_selected, na.rm = TRUE)

library(corrplot)

corr_matrix <- cor(clm_df_selected, use = "complete.obs")

corrplot(
  corr_matrix,
  method = "color",        # 색상으로 시각화
  tl.cex = 0.6,            # 변수 이름 크기
  addCoef.col = "black",   # 상관계수 텍스트 색
  number.cex = 0.5,        # 상관계수 숫자 크기
  type = "upper",          # 위쪽 삼각형만 표시 (선택 사항)
  diag = FALSE             # 대각선 제거 (선택 사항)
)

corr_abs <- abs(corr_matrix)

#대각선 제거 (자기 자신과의 상관은 1이므로 제외)
diag(corr_abs) <- 0

library(caret)

high_corr_idx <- findCorrelation(corr_abs, cutoff = 0.7, verbose = TRUE)

#제거 대상 변수 이름
remove_vars <- colnames(corr_abs)[high_corr_idx]

#제거 후 남길 변수만 선택
kept_vars <- setdiff(names(clm_df_selected), remove_vars)
print(kept_vars)

#데이터프레임 필터링####
#SpatRaster에서 필터링
clm_var_filter <- subset(masked_clim_5179, kept_vars)

plot(clm_var_filter)

#환경공간 생성하기####
library(terra)
library(ggplot2)
library(dplyr)

# 그 안의 모든 셀 값을 데이터프레임으로 변환
clm_fil_xy <- as.data.frame(clm_var_filter, xy = TRUE, na.rm = TRUE)

# NA값이 있는 셀 확인
na_per_variable <- colSums(is.na(clm_fil_xy[, -c(1:2)]))
print(na_per_variable)

# NA 제거
clm_fil_xy_nona <- na.omit(clm_fil_xy)

# PCA 수행 (환경 변수만 선택)
clm_only <- clm_fil_xy_nona[, -c(1:2)]

clm_pca_result <- prcomp(clm_only, scale. = TRUE)

#saveRDS(clm_pca_result, file = "pca_result_clm_251107.rds")

#clm_pca_result <- readRDS("pca_result_clm_251107.rds")

clm_pca_df <- as.data.frame(clm_pca_result$x)


#png(filename = "./그림/pca_all_250929.png", width = 2400, height = 2000, res = 300)

# 기후공간 분포 시각화####
ggplot(clm_pca_df, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.4, size = 0.6) +
  labs(title = "대한민국의 기후공간 분포",
       x = "PC1", y = "PC2") +
  theme_minimal() 

#PCA 설명력 확인####

# PCA 결과에서 고유값 (eigenvalues)은 sdev^2
eigenvalues <- clm_pca_result$sdev^2

explained_variance <- eigenvalues / sum(eigenvalues)
cumulative_variance <- cumsum(explained_variance)

pc_var_df <- data.frame(
  PC = paste0("PC", 1:length(explained_variance)),
  Explained = explained_variance,
  Cumulative = cumulative_variance
)

print(pc_var_df)
#1축 설명력 47.2, 2축 설명력 19.1

# 1. PCA 결과에서 rotation 행렬 (각 PC의 loading)
loadings <- clm_pca_result$rotation  # 행: 변수 / 열: PC

# 2. loading^2 하면 각 PC에서의 기여 비율 (분산 비중)
loading_sq <- loadings^2

# 3. 각 PC에서의 상대적 기여율 (총합이 1이 되도록)
pc_contrib <- sweep(loading_sq, 2, colSums(loading_sq), FUN = "/")

# 4. 보기 쉽게 정리
pc_contrib_df <- as.data.frame(pc_contrib)
pc_contrib_df$Variable <- rownames(pc_contrib_df)
pc_contrib_df <- pc_contrib_df[, c("Variable", "PC1", "PC2", "PC3")]
print(pc_contrib_df)

#PCA 기여도 시각화####
library(factoextra)

#png(filename = "./그림/PCA_importance_all_250929.png", width = 1600, height = 1200, res = 300)

fviz_pca_var(
  clm_pca_result,
  col.var = "contrib",  # 기여도 기반 색상
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,          # 텍스트 겹침 방지
  geom = c("arrow", "text"),  # 화살표와 텍스트 모두 출력
  arrowsize = 1.2,            # 화살표 두께 (기본은 0.5)
  labelsize = 4,              # 글씨 크기
  fontface = "bold"           # 글씨 두께 (bold)
)

#기후공간 빈도 확인####
library(dplyr)
library(ggplot2)
library(terra)
library(scales)

# PCA 좌표 가져오기
clm_pca_df <- as.data.frame(clm_pca_result$x)

# 셀 면적 (m²)
clm_cell_area_m2 <- prod(res(clm_var_filter))

# PCA 공간을 격자로 나눠 빈도 계산
pca_bins <- clm_pca_df %>%
  mutate(
    PC1_bin = cut(PC1, breaks = 50),  # bin 개수 조정 가능
    PC2_bin = cut(PC2, breaks = 50)
  ) %>%
  group_by(PC1_bin, PC2_bin) %>%
  summarise(cell_count = n(), .groups = "drop") %>%
  mutate(
    area = cell_count * clm_cell_area_m2,
    area_km2 = area / 1e6,  # km² 변환
    # 빈도(cell_count)를 기준으로 10등분 (quantile 사용)
    # ntile(변수, 등분 수) 함수 사용
    count_decile = as.factor(ntile(cell_count, 10)) 
  )

# 사용할 10가지 색상 벡터 생성
# RColorBrewer Blues 9가지 색상 + 가장 진한 파랑 수동 추가
custom_blues_10 <- c(RColorBrewer::brewer.pal(9, "Blues"), "#000033")

# 시각화 (빈도 10등분, 범주형 색상 사용)
ggplot(pca_bins, aes(x = PC1_bin, y = PC2_bin, fill = count_decile)) +
  geom_tile() +
  scale_fill_manual(
    # 빈도수가 많은(10) 곳에 진한 색상이 오도록 순서를 역전시킵니다.
    values = rev(custom_blues_10), 
    labels = c("하위 10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "상위 10%"),
    na.translate = FALSE # 결측치(빈 셀)는 표시하지 않음
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(x = "PC1", y = "PC2", fill = "빈도 10분위")

#기후공간 지리 지도에 투영####
library(dplyr)

# PCA 점수 + bin 구간 계산
clm_pca_df_bins <- clm_pca_df %>%
  mutate(
    PC1_bin = cut(PC1, breaks = 50),
    PC2_bin = cut(PC2, breaks = 50)
  )

# bin 기준으로 다시 병합 (join)
clm_pca_with_decile <- clm_pca_df_bins %>%
  left_join(pca_bins %>% select(PC1_bin, PC2_bin, count_decile),
            by = c("PC1_bin", "PC2_bin"))

# 원래 clm_fil_xy_nona와 합치기 (1:1 대응)
clm_fil_xy_with_decile <- bind_cols(clm_fil_xy_nona, clm_pca_with_decile["count_decile"])

library(terra)
library(ggplot2)

# 1️⃣ SpatRaster 생성
r_count_decile <- rast(
  clm_fil_xy_with_decile[, c("x", "y", "count_decile")],
  type = "xyz",
  crs = "EPSG:5179"
)

table(clm_fil_xy_with_decile$count_decile)

#writeRaster(r_count_decile, "./clm_decile_251107.tif", filetype = "GTiff", overwrite = TRUE)

# 출현자료 로드####
library(terra)
library(dplyr)

rast_merged_5179 <- rast("./data/large_landcover_5179.tif")

# 종 발생 데이터 로드
occurrence <- read.csv("./data/occurrence241_250916.csv", header = TRUE)
occurrence <- occurrence[,3:6]

# 좌표 변환 및 셀 ID 추출
occurrence_vect <- vect(occurrence, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")
occurrence_vect_5179 <- project(occurrence_vect, "EPSG:5179")

xy_coords <- crds(occurrence_vect_5179)
cell_ids <- cellFromXY(clm_var_filter, xy_coords)
clm_occ_df <- cbind(occurrence, cell = cell_ids)

# 종별 + 셀별로 중복 제거 (random 1개 유지)
set.seed(42)  # 재현 가능하게
clm_thinned_occ <- clm_occ_df %>%
  group_by(국명, cell) %>%
  slice_sample(n = 1) %>%
  ungroup()
#thinning 결과 4871->3087로 줄어듬..

# 환경 변수 추출
thinned_vect <- vect(clm_thinned_occ, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")
thinned_vect_5179 <- project(thinned_vect, "EPSG:5179")

occ_env <- terra::extract(clm_var_filter, thinned_vect_5179)

thinned_with_env <- cbind(clm_thinned_occ, occ_env[,-1])  # 첫번째(ID) 제외

## EPSG:5179를 데이터프레임에 추가####
# 1. thinned_vect_f_5179 객체에서 5179 좌표를 추출합니다.
coords_5179 <- crds(thinned_vect_5179)
colnames(coords_5179) <- c("X_5179", "Y_5179")

# 2. thinned_with_env 데이터프레임에 5179 좌표를 추가합니다.
thinned_with_env <- cbind(thinned_with_env, coords_5179)

# 6. 마스킹 필터링 준비
thinned_vect_f_5179 <- vect(thinned_with_env, geom = c("X_5179", "Y_5179"), crs = "EPSG:5179")

# 래스터에서 값이 1인 위치만 TRUE로 표시 (mask 생성)
rast_mask <- rast_merged_5179 == 1

# 7. 마스크 값 추출 및 필터링
mask_values <- terra::extract(rast_mask, thinned_vect_f_5179)

# 마스크 값이 1이 아니거나 (마스크 바깥) NA인 경우의 인덱스 찾기
idx <- which(mask_values[,2] != 1 | is.na(mask_values[,2]))

# 마스크 바깥에 있는 데이터만 추출하여 thinned_outside_mask에 할당
thinned_outside_mask <- thinned_with_env[idx, ]

# 결과 확인
# 3087 -> 2956 (데이터의 변화는 기존과 동일하게 유지됩니다.)
unique(thinned_outside_mask$국명) # 241 taxa

# 8. 결과 저장 (X_5179, Y_5179 포함)
# 저장된 CSV 파일에는 decimalLongitude, decimalLatitude 외에 X_5179, Y_5179 열이 추가됩니다.

#write.csv(thinned_outside_mask, "./data/thinned_occur_clm_5179_251016.csv", row.names = FALSE)


library(dplyr)
library(terra) # names(clm_var_filter)를 위해 필요

# 출현자료 PCA 투영####

# 1. PCA 투영에 필요한 환경 변수 데이터 준비
env_vars <- names(clm_var_filter)
thinned_env_data_for_pca <- thinned_outside_mask %>%
  select(all_of(env_vars))

# NA가 포함된 행의 인덱스를 저장 (NA 처리 후 원본 데이터 매칭을 위해 사용)
# 환경 변수에 결측치가 없다고 했으므로, 이 단계는 사실상 전체 행을 처리합니다.
rows_to_keep <- which(apply(is.na(thinned_env_data_for_pca), 1, any) == FALSE)
thinned_env_data_no_na <- na.omit(thinned_env_data_for_pca)

# 2. PCA 변환 적용 및 좌표 조정 (Capping)
occ_scaled <- scale(thinned_env_data_no_na, center = clm_pca_result$center, scale = clm_pca_result$scale)

clm_occ_pca <- as.data.frame(occ_scaled %*% clm_pca_result$rotation) 

# pca_bins의 bin 수준(level) 가져오기
pc1_levels <- levels(pca_bins$PC1_bin)
pc2_levels <- levels(pca_bins$PC2_bin)

# clm_occ_pca에 동일한 수준으로 bin 할당
clm_occ_pca_bins <- clm_occ_pca %>%
  mutate(
    PC1_bin = cut(PC1, breaks = unique(c(as.numeric(sub("\\((.+),.*", "\\1", pc1_levels)), 
                                         as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", pc1_levels)))), 
                  include.lowest = TRUE, labels = pc1_levels),
    PC2_bin = cut(PC2, breaks = unique(c(as.numeric(sub("\\((.+),.*", "\\1", pc2_levels)), 
                                         as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", pc2_levels)))), 
                  include.lowest = TRUE, labels = pc2_levels)
  )
# pca_bins와 join
clm_occ_pca_bins <- clm_occ_pca_bins %>%
  left_join(pca_bins %>% select(PC1_bin, PC2_bin, count_decile),
            by = c("PC1_bin", "PC2_bin"))

# Capping으로 인해 NA는 0개가 되었는지 확인
cat("\n[Capping 후 빈도 10분위별 출현지 개수]\n")
print(table(clm_occ_pca_bins$count_decile, useNA = "ifany"))

# 출현정보 기후 공간 내 시각화####
library(dplyr)
library(stringr)

pca_bins <- pca_bins %>%
  mutate(
    PC1_start = as.numeric(str_extract(PC1_bin, "(?<=\\()[^,]+")),
    PC1_end = as.numeric(str_extract(PC1_bin, "(?<=,)[^\\]]+")),
    PC2_start = as.numeric(str_extract(PC2_bin, "(?<=\\()[^,]+")),
    PC2_end = as.numeric(str_extract(PC2_bin, "(?<=,)[^\\]]+")),
    PC1_center = (PC1_start+PC1_end)/2,
    PC2_center = (PC2_start+PC2_end)/2
  )

# ggplot을 사용하여 시각화
ggplot(pca_bins, aes(x = PC1_center, y = PC2_center, fill = count_decile)) + # X, Y에 center 좌표 사용
  geom_tile(aes(width = PC1_end - PC1_start, height = PC2_end - PC2_start)) + # 타일의 너비/높이를 빈 크기에 맞춤
  scale_fill_manual(
    values = rev(custom_blues_10), # 10은 높은 빈도, 1은 낮은 빈도에 매핑
    labels = c("하위 10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "상위 10%"),
    name = "기후 빈도 10분위",
    na.value = "grey80" # pca_bins에 혹시 모를 NA가 있다면 회색으로 표시
  ) +
  # thinned_outside_mask_final의 출현 지점 오버레이
  # inherit.aes = FALSE를 사용하여 geom_tile의 aes 설정을 상속받지 않도록 합니다.
  geom_point(data = clm_occ_pca_bins, 
             aes(x = PC1, y = PC2, color = is.na(count_decile)), # X, Y에 원래 PC 좌표 사용
             inherit.aes = FALSE, 
             size = 1.5, 
             alpha = 0.8,
             shape = 16) + # 원형 점
  coord_fixed(ratio = 1) + # PC1과 PC2 축의 비율을 1:1로 고정하여 왜곡 방지
  scale_color_manual(
    values = c("FALSE" = "black", "TRUE" = "red"), # NA가 아니면 검정색, NA이면 빨간색
    labels = c("TRUE" = "분류되지 않음 (NA)", "FALSE" = "분류됨"),
    name = "출현지 분류 여부"
  ) +
  theme_minimal() +
  theme(
    # 축 텍스트를 제거하지 않고, PC1, PC2의 연속적인 값을 표시하도록 변경
    axis.text = element_text(size = 8), 
    axis.ticks = element_line(),
    panel.grid = element_blank() # 격자선 제거
  ) +
  labs(x = "PC1", y = "PC2", 
       title = "PCA 공간의 기후 빈도 10분위 및 종 출현 지점") +
  guides(fill = guide_legend(order = 1), # 빈도 10분위 범례를 먼저 표시
         color = guide_legend(order = 2)) # 출현지 분류 여부 범례를 나중에 표시

#분위수 출현정보에 넣기####
# count_decile 벡터 생성 (NA 포함 가능)
count_decile_vec <- clm_occ_pca_bins$count_decile

# 원본 데이터프레임에 count_decile 추가 (NA인 행은 NA 유지)
thinned_outside_mask_with_decile <- thinned_outside_mask %>%
  mutate(count_decile = NA)

thinned_outside_mask_with_decile$count_decile[rows_to_keep] <- count_decile_vec
#write.csv(thinned_outside_mask_with_decile, "./data/thinned_occur_clm_w_decile_5179_251016.csv", row.names = FALSE)
