# https://www.moj.go.jp/isa/policies/statistics/toukei_ichiran_touroku.html 都道府県別　国籍・地域別　在留外国人
packages = c('readxl', 'tidyr', 'dplyr')
sapply(packages, library, character.only = TRUE)
github = "https://raw.githubusercontent.com/blacktreeM/welfareJapan/main/foreigner_data/"
file = c('10-99-04-0', '11-99-04-0', '12-12-05-0', '13-12-05-0', '14-12-05-0', '15-12-05-0', '16-12-05-0',
          '17-12-05-0', '18-12-05-0')
(files = paste0(github, file, c(rep('.xls', 3), rep('.xlsx', 6))))
for (file_url in files) { 
  cat("Processing:", file_url, "\n")
  temp_excel_file <- tempfile(fileext = tools::file_ext(file_url))
  download.file(file_url, destfile = temp_excel_file, mode = "wb", method = "wininet")
  filename_only <- basename(file_url)
  filename_without_ext <- tools::file_path_sans_ext(filename_only)
  year_prefix <- substring(filename_without_ext, 1, 2)
  year <- as.numeric(year_prefix)
  data <- read_excel(temp_excel_file, sheet = 1)[-1, -1]
  col_names <- unlist(as.vector(data[1, ]))
  colnames(data) <- col_names
  pref <- data[-1, 1]
  columns_to_keep <- grepl("永住者", colnames(data)) | grepl("配偶者", colnames(data)) |
    grepl("定住者", colnames(data)) | grepl("総数", colnames(data))
  data <- data[-1, columns_to_keep] 
  data <- cbind(pref, data)
  data$year <- 2000 + year 
  colnames(data) <- c('pref', 'total', keep, 'year') 
  df <- rbind(df, data)
  cat("Successfully processed:", file_url, "\n")
  }
keep = c('永住者', '特別永住者', '日本人の配偶者', '永住者の配偶者', '定住者')
table(df$year); head(df)
# 2019-20
files1920 = c('19-12-05.xlsx', '20-12-05.xlsx') 
(files1920 = paste0(github, files1920))
for (file_url in files1920){
  cat("Processing:", file_url, "\n")
  temp_excel_file <- tempfile(fileext = tools::file_ext(file_url))
  download.file(file_url, destfile = temp_excel_file, mode = "wb", method = "wininet")
  filename_only <- basename(file_url)
  filename_without_ext <- tools::file_path_sans_ext(filename_only)
  year_prefix <- substring(filename_without_ext, 1, 2)
  year <- as.numeric(year_prefix)
  data = read_excel(temp_excel_file, sheet = 1)[-1,]; head(data)
  (col_names = unlist(as.vector(data[1, ])))
  colnames(data) = col_names; head(data)
  pref = data[-1,1]
  (columns_to_keep <- grepl("永住者", colnames(data)) | grepl("配偶者", colnames(data)) | 
      grepl("定住者", colnames(data)) | grepl("総数", colnames(data)))
  data = data[-1,columns_to_keep]; head(data)
  data = cbind(pref, data)
  data$year = 2000+year
  colnames(data) = c('pref', 'total', keep, 'year'); head(data)
  df = rbind(data, df)
  cat("Successfully processed:", file_url, "\n")
}
table(df$year); head(df)
files2122 = c('21-12-03-2.xlsx', '22-12-04.xlsx')
(files2122 = paste0(github, files2122))
for (file_url in files2122){
  cat("Processing:", file_url, "\n")
  temp_excel_file <- tempfile(fileext = tools::file_ext(file_url))
  download.file(file_url, destfile = temp_excel_file, mode = "wb", method = "wininet")
  filename_only <- basename(file_url)
  filename_without_ext <- tools::file_path_sans_ext(filename_only)
  year_prefix <- substring(filename_without_ext, 1, 2)
  year <- as.numeric(year_prefix)
  data = read_excel(temp_excel_file, sheet = 1); head(data)
  (col_names = unlist(as.vector(data[1, ])))
  colnames(data) = col_names; head(data)
  data = data %>% filter(substring(市区町村コード, 3,5)=='000' | is.na(市区町村コード)); head(data)# keep only pref and Japan
  pref = data[,2]
  (columns_to_keep <- grepl("永住者", colnames(data)) | grepl("配偶者", colnames(data)) | 
      grepl("定住者", colnames(data)) | grepl("総数", colnames(data)))
  data = data[,columns_to_keep]; head(data)
  data = cbind(pref, data); head(data)
  data$year = 2000+year
  colnames(data) = c('pref', 'total', keep, 'year'); head(data)
  df = rbind(data, df)
  cat("Successfully processed:", file_url, "\n")
}
table(df$year); head(df)
(numerical = setdiff(colnames(df), 'pref'))
df[numerical] = lapply(df[numerical], function(x) as.numeric(x))
foreign = df
unique(foreign$pref)
foreign$pref = gsub('東京都', '東京', foreign$pref)
foreign$pref = gsub('府|県', '', foreign$pref)
foreign$pref = ifelse(foreign$pref=="総数", "全国", foreign$pref)
foreign = foreign %>% filter(!is.na(pref) & !(pref %in% c("未定・不詳", "その他", "その他の合計", "未定・不祥")))                         
unique(foreign$pref)
###### prefecture welfare measures and controls
file = "https://raw.githubusercontent.com/blacktreeM/welfareJapan/main/welfare_pref_new.xlsx"
x = tempfile(fileext = tools::file_ext(file))
download.file(file, destfile = x, mode = "wb", method = "wininet")
length(excel_sheets(x))
data = read_excel(x, sheet = 1)
(year = substring(data[2,2], 1, 4))
var = as.character(data[5, seq(4, ncol(data), 2)]); var # only even numbers columns, starts at 4
data = data[6:nrow(data), c(1, 2, seq(4, ncol(data), 2))]
colnames(data) = c('fips', 'pref', var)
data$year = year
colnames(data)
for (i in 2:49){
  df = read_excel(x, sheet = i)
  year = substring(df[2,2], 1, 4)
  var = as.character(df[5, seq(4, ncol(df), 2)]) 
  df = df[6:nrow(df), c(1, 2, seq(4, ncol(df), 2))]
  colnames(df) = c('fips', 'pref', var)
  df$year = year
  data = bind_rows(data, df); print(i)
}
table(data$year)
colnames(data)
colnames(data) = c('fips', 'pref', 'pop', 'old', 'year', 
                   'welfare_house', 'welfare_pref', 'welfare_city',  'household',
                   'mother', 'income1', 'college', 'unemp', 'income2', 'income3'); summary(data)
numerical = setdiff(colnames(data), 'pref')
data[, numerical] = lapply(data[, numerical], function(x) as.numeric(gsub(',', '', x))); summary(data)
lapply(data[, 3:ncol(data)], function(x) aggregate(x ~ year, data = data, mean))
Data = data
### adding number of people on welfare , not household J1105_生活保護被保護実人員【人
file = "https://raw.githubusercontent.com/blacktreeM/welfareJapan/main/welfare_pref_new_add.xlsx"
x = tempfile(fileext = tools::file_ext(file))
download.file(file, destfile = x, mode = "wb", method = "wininet")
length(excel_sheets(x))
data = read_excel(x, sheet = 1)
(year = substring(data[2,2], 1, 4))
var = as.character(data[5, seq(4, ncol(data), 2)]); var # only even numbers columns, starts at 4
data = data[6:nrow(data), c(1, 2, seq(4, ncol(data), 2))]
colnames(data) = c('fips', 'pref', var)
data$year = year
colnames(data)
for (i in 2:48){
  df = read_excel(x, sheet = i)
  year = substring(df[2,2], 1, 4)
  var = as.character(df[5, seq(4, ncol(df), 2)]) 
  df = df[6:nrow(df), c(1, 2, seq(4, ncol(df), 2))]
  colnames(df) = c('fips', 'pref', var)
  df$year = year
  data = bind_rows(data, df); print(i)
}
table(data$year);colnames(data)
colnames(data) = c('fips', 'pref', 'welfare_pop', 'year')
numerical = setdiff(colnames(data), 'pref')
data[, numerical] = lapply(data[, numerical], function(x) as.numeric(gsub(',', '', x))); summary(data)
lapply(data[, 3:ncol(data)], function(x) aggregate(x ~ year, data = data, mean))
data = data %>% left_join(Data, by = c('fips', 'year', 'pref'))
head(data)
###
data$Pref = NA
data$Pref[data$pref == '北海道'] = 'Hokkaido'
data$Pref[data$pref == '青森県'] = 'Aomori'
data$Pref[data$pref == '岩手県'] = 'Iwate'
data$Pref[data$pref == '宮城県'] = 'Miyagi'
data$Pref[data$pref == '秋田県'] = 'Akita'
data$Pref[data$pref == '山形県'] = 'Yamagata'
data$Pref[data$pref == '福島県'] = 'Fukushima'
data$Pref[data$pref == '茨城県'] = 'Ibaraki'
data$Pref[data$pref == '栃木県'] = 'Tochigi'
data$Pref[data$pref == '群馬県'] = 'Gunma'
data$Pref[data$pref == '埼玉県'] = 'Saitama'
data$Pref[data$pref == '千葉県'] = 'Chiba'
data$Pref[data$pref == '東京都'] = 'Tokyo'
data$Pref[data$pref == '神奈川県'] = 'Kanagawa'
data$Pref[data$pref == '新潟県'] = 'Niigata'
data$Pref[data$pref == '富山県'] = 'Toyama'
data$Pref[data$pref == '石川県'] = 'Ishikawa'
data$Pref[data$pref == '福井県'] = 'Fukui'
data$Pref[data$pref == '山梨県'] = 'Yamanashi'
data$Pref[data$pref == '長野県'] = 'Nagano'
data$Pref[data$pref == '岐阜県'] = 'Gifu'
data$Pref[data$pref == '静岡県'] = 'Shizuoka'
data$Pref[data$pref == '愛知県'] = 'Aichi'
data$Pref[data$pref == '三重県'] = 'Mie'
data$Pref[data$pref == '滋賀県'] = 'Shiga'
data$Pref[data$pref == '京都府'] = 'Kyoto'
data$Pref[data$pref == '大阪府'] = 'Osaka'
data$Pref[data$pref == '兵庫県'] = 'Hyogo'
data$Pref[data$pref == '奈良県'] = 'Nara'
data$Pref[data$pref == '和歌山県'] = 'Wakayama'
data$Pref[data$pref == '鳥取県'] = 'Tottori'
data$Pref[data$pref == '島根県'] = 'Shimane'
data$Pref[data$pref == '岡山県'] = 'Okayama'
data$Pref[data$pref == '広島県'] = 'Hiroshima'
data$Pref[data$pref == '山口県'] = 'Yamaguchi'
data$Pref[data$pref == '徳島県'] = 'Tokushima'
data$Pref[data$pref == '香川県'] = 'Kagawa'
data$Pref[data$pref == '愛媛県'] = 'Ehime'
data$Pref[data$pref == '高知県'] = 'Kochi'
data$Pref[data$pref == '福岡県'] = 'Fukuoka'
data$Pref[data$pref == '佐賀県'] = 'Saga'
data$Pref[data$pref == '長崎県'] = 'Nagasaki'
data$Pref[data$pref == '熊本県'] = 'Kumamoto'
data$Pref[data$pref == '大分県'] = 'Oita'
data$Pref[data$pref == '宮崎県'] = 'Miyazaki'
data$Pref[data$pref == '鹿児島県'] = 'Kagoshima'
data$Pref[data$pref == '沖縄県'] = 'Okinawa'
unique(data$pref)
data = data %>% filter(!is.na(pref) & !(pref %in% c("調査又は集計していないもの",
                                                    "データが得られないもの",
                                                    "数値が秘匿されているもの" )))
data$pref = gsub('東京都', '東京', data$pref)
data$pref = gsub('府|県', '', data$pref)
data$region = NA
data$region = ifelse(data$pref %in% c("北海道", "青森", "岩手", "宮城", "秋田", "山形", "福島"), "東北", data$region)
data$region = ifelse(data$pref %in% c("埼玉", "千葉", "東京", "神奈川"), "南関東", data$region)
data$region = ifelse(data$pref %in% c("茨城", "栃木", "群馬", "山梨", "長野"), "北関東", data$region)
data$region = ifelse(data$pref %in% c("新潟", "富山", "石川", "福井"), "北陸", data$region)
data$region = ifelse(data$pref %in% c("岐阜", "静岡", "愛知", "三重"), "東海", data$region)
data$region = ifelse(data$pref %in% c("滋賀", "京都", "大阪", "兵庫", "奈良", "和歌山"), "近畿", data$region)
data$region = ifelse(data$pref %in% c("鳥取", "島根", "岡山", "広島", "山口"), "中国", data$region)
data$region = ifelse(data$pref %in% c("徳島", "香川", "愛媛", "高知"), "四国", data$region)
data$region = ifelse(data$pref %in% c("福岡", "佐賀", "長崎", "熊本", "大分", "宮崎", "鹿児島", "沖縄"), "九州", data$region)
table(data$region)
data = data %>% filter(year>=2010) %>% right_join(foreign, by = c('year', 'pref'))
table(data$year)
summary(data)
lapply(data[,3:ncol(data)], function(x) aggregate(x ~ year, data = data, min))
# interpolate
if(!require(zoo)) install.packages("zoo"); library(zoo)
subset(data, pref == '北海道', select = c(year, college, household, mother, unemp, income1, income2))
fill_interpolation_and_ends <- function(x) {
  x_interp <- zoo::na.approx(x, na.rm = FALSE, rule = 2) # rule=2 extends to ends if possible
  x_locf_forward <- zoo::na.locf(x_interp, na.rm = FALSE)
  x_final <- zoo::na.locf(x_locf_forward, na.rm = FALSE, fromLast = TRUE)
  return(x_final)
}
data = data %>% 
  arrange(pref, year) %>% # Crucial: Always sort by panel ID and then by year
  group_by(pref) %>%     # Group by your panel ID (e.g., 'pref')
  mutate(across(c(college, household, mother, unemp), fill_interpolation_and_ends),
    income1 = zoo::na.locf(income1, na.rm = FALSE, fromLast = TRUE), # NOCB first
    income1 = zoo::na.locf(income1, na.rm = FALSE),                   # Then LOCF
    income2 = zoo::na.locf(income2, na.rm = FALSE),                   # LOCF first
    income2 = zoo::na.locf(income2, na.rm = FALSE, fromLast = TRUE)  # Then NOCB
  ) %>% ungroup()
subset(data, pref == '北海道', select = c(year, college, household, mother, unemp, income1, income2))
data = data %>% mutate(income = (income1+income2)/2, single_mom = 100*mother/household,
                       welfare = 1000*(welfare_city + welfare_pref)/pop,
                       welfare_share = 100*welfare_pop/pop); summary(data)
data = data %>% mutate(im = 永住者 + 特別永住者 + 日本人の配偶者 + 永住者の配偶者 + 定住者,
                       im_share = 100*im/pop, all_im_share = 100*total/pop, im_share2 = 100*(total-im)/pop); summary(data)
#"#J01101_生活保護被保護実世帯数（月平均一般世帯千世帯当たり）【世帯】"         
#"D3103034_生活保護費（都道府県財政）【千円】"                                  
#"D3203034_生活保護費（市町村財政）【千円】"                                    
save(data, file = 'welfareJapanData.RDa')