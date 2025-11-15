# Tải các gói cần thiết 
if(!require(quantmod)) install.packages("quantmod")
if(!require(tseries)) install.packages("tseries")
if(!require(forecast)) install.packages("forecast")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(moments)) install.packages("moments")
if(!require(WDI)) install.packages("WDI")
if(!require(lubridate)) install.packages("lubridate")
if(!require(lmtest)) install.packages("lmtest")
if(!require(car)) install.packages("car")
if(!require(sandwich)) install.packages("sandwich")
if(!require(DescTools)) install.packages("DescTools")
if(!require(rugarch)) install.packages("rugarch")

# Cài đặt các gói cần thiết 
library(quantmod)
library(tseries)
library(forecast)
library(dplyr)
library(tidyr)
library(ggplot2)
library(moments)
library(WDI)
library(lubridate)
library(lmtest)
library(car)
library(sandwich)
library(DescTools)
library(rugarch)

# 1. Nhập dữ liệu 
# 1.1. Lấy data chỉ số tài chính của 11 ngân hàng 
df_all_banks <- read.csv("df_all_banks.csv", header=TRUE, sep=",")
df_all_banks

# 1.2. Lấy data về các yếu tố vĩ mô 
CCTM <- read.csv("CCTM.csv", header=TRUE, sep=",")
CPI <- read.csv("CPI.csv", header=TRUE, sep=",")
GDP <- read.csv("GDP.csv", header=TRUE, sep=",")
ExchangeRate <- read.csv("ExchangeRate.csv", header=TRUE, sep=",")
close <- read.csv("df_close.csv", header=TRUE, sep=",")
WTI_USD <- read.csv("WTI_USD.csv", header=TRUE, sep=",")
XAU_USD <- read.csv("XAU_USD.csv", header=TRUE, sep=",")

# 2. Tạo dataframe bao gồm các dữ liệu trên 
# 2.1. CCTM 
# Lọc ra những tháng là cuối quý 
CCTM_cl <- CCTM[grepl("Tháng\\s*(3|6|9|12)", CCTM$period), ]

# Thay hàng loạt theo thứ tự, gsub sẽ áp dụng cho toàn bộ chuỗi
CCTM_cl$period <- gsub("Tháng\\s*3",  "Quý 1",  CCTM_cl$period)
CCTM_cl$period <- gsub("Tháng\\s*6",  "Quý 2",  CCTM_cl$period)
CCTM_cl$period <- gsub("Tháng\\s*9",  "Quý 3",  CCTM_cl$period)
CCTM_cl$period <- gsub("Tháng\\s*12", "Quý 4",  CCTM_cl$period)

# Xóa cột không sử dụng 
CCTM_cl$EXP <- NULL
CCTM_cl$IMP <- NULL

# 2.2. CPI 
# Lọc ra những tháng là cuối quý 
CPI_cl <- CPI[grepl("Tháng\\s*(3|6|9|12)", CPI$period), ]

# Thay hàng loạt theo thứ tự, gsub sẽ áp dụng cho toàn bộ chuỗi
CPI_cl$period <- gsub("Tháng\\s*3",  "Quý 1",  CPI_cl$period)
CPI_cl$period <- gsub("Tháng\\s*6",  "Quý 2",  CPI_cl$period)
CPI_cl$period <- gsub("Tháng\\s*9",  "Quý 3",  CPI_cl$period)
CPI_cl$period <- gsub("Tháng\\s*12", "Quý 4",  CPI_cl$period)

# 2.3. ExchangeRate 
ExchangeRate$date <- as.Date(ExchangeRate$period, format = "%d/%m/%Y")

ExchangeRate$year <- as.numeric(format(ExchangeRate$date, "%Y"))
ExchangeRate$month <- as.numeric(format(ExchangeRate$date, "%m"))
ExchangeRate$quarter <- ceiling(ExchangeRate$month / 3)

ends <- aggregate(date ~ year + quarter, data = ExchangeRate, FUN = max)

ExchangeRate_quarter_ends <- merge(
  ends,
  ExchangeRate,
  by = c("year", "quarter", "date")
)

ExchangeRate_quarter_ends <- subset(
  ExchangeRate_quarter_ends,
  select = -c(date, period, month)
)

ExchangeRate_quarter_ends$period <- paste0(
  "Quý ",
  ExchangeRate_quarter_ends$quarter,
  "/",
  ExchangeRate_quarter_ends$year
)

ExchangeRate_cl <- ExchangeRate_quarter_ends[, c("period", "USDVND")]

# 2.4. close 
close$date <- as.Date(close$time, format = "%Y-%m-%d")

close$year <- as.numeric(format(close$date, "%Y"))
close$month <- as.numeric(format(close$date, "%m"))
close$quarter <- ceiling(close$month / 3)

ends <- aggregate(date ~ year + quarter, data = close, FUN = max)

close_quarter_ends <- merge(
  ends,
  close,
  by = c("year", "quarter", "date")
)

close_quarter_ends <- subset(
  close_quarter_ends,
  select = -c(date, time, month)
)

close_quarter_ends$period <- paste0(
  "Quý ",
  close_quarter_ends$quarter,
  "/",
  close_quarter_ends$year
)

close_cl <- close_quarter_ends[, c("period","symbol", "close")]

# 2.5. WTI_USD
WTI_USD$date <- as.Date(WTI_USD$date, format = "%d/%m/%Y")

WTI_USD$year <- as.numeric(format(WTI_USD$date, "%Y"))
WTI_USD$month <- as.numeric(format(WTI_USD$date, "%m"))
WTI_USD$quarter <- ceiling(WTI_USD$month / 3)

ends <- aggregate(date ~ year + quarter, data = WTI_USD, FUN = max)

WTI_USD_quarter_ends <- merge(
  ends,
  WTI_USD,
  by = c("year", "quarter", "date")
)

WTI_USD_quarter_ends <- subset(
  WTI_USD_quarter_ends,
  select = -c(date, month, X.Change)
)

WTI_USD_quarter_ends$period <- paste0(
  "Quý ",
  WTI_USD_quarter_ends$quarter,
  "/",
  WTI_USD_quarter_ends$year
)

WTI_USD_cl <- WTI_USD_quarter_ends[, c("period","WTI_USD")]

# 2.6. XAU_USD
XAU_USD$date <- as.Date(XAU_USD$date, format = "%d/%m/%Y")

XAU_USD$year <- as.numeric(format(XAU_USD$date, "%Y"))
XAU_USD$month <- as.numeric(format(XAU_USD$date, "%m"))
XAU_USD$quarter <- ceiling(XAU_USD$month / 3)

ends <- aggregate(date ~ year + quarter, data = XAU_USD, FUN = max)

XAU_USD_quarter_ends <- merge(
  ends,
  XAU_USD,
  by = c("year", "quarter", "date")
)

XAU_USD_quarter_ends <- subset(
  XAU_USD_quarter_ends,
  select = -c(date, month, X.Change)
)

XAU_USD_quarter_ends$period <- paste0(
  "Quý ",
  XAU_USD_quarter_ends$quarter,
  "/",
  XAU_USD_quarter_ends$year
)

XAU_USD_cl <- XAU_USD_quarter_ends[, c("period","XAU_USD")]


# 2.7. Gộp dữ liệu và làm sạch 
# Gộp dữ liệu từ các dataframe 
data <- df_all_banks %>%
  left_join(CCTM_cl,         by = "period") %>%
  left_join(CPI_cl,          by = "period") %>%
  left_join(ExchangeRate_cl, by = "period") %>%
  left_join(GDP,             by = "period") %>%
  left_join(WTI_USD_cl,      by = "period") %>%
  left_join(XAU_USD_cl,      by = "period") 

data_cl <- data %>%
  left_join(close_cl, by = c("period", "symbol"))

# Kiểm tra thông tin dataframe data_cl 
str(data_cl)

# Thay thế giá trị 
data_cl$period <- gsub("Quý ",  "Q",  data_cl$period)

# Kiểm tra giá trị NaN 
data_cl[!complete.cases(data_cl), ]

# Xóa các dòng có NaN và kiểm tra số dòng còn lại 
df <- na.omit(data_cl)
n <- nrow(df)
print(n)

# Xử lý outliers trong bằng IQR 
# price_to_earning
Q1 <- quantile(df$price_to_earning, 0.25, na.rm = TRUE)
Q3 <- quantile(df$price_to_earning, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

df_clean <- df[df$price_to_earning >= lower_bound & df$price_to_earning <= upper_bound, ]

rownames(df_clean) <- NULL
# 3. Thống kê mô tả và trực quan hóa dữ liệu 
# Thống kê mô tả 
summary(df_clean)

# Ma trận tương quan giữa các biến 
cor(df_clean[, !(names(df_clean) %in% c("symbol", "period"))])

# Phân phối của các biến 
# P/E: Price to Earning
# Box plot 
ggplot(df_clean, aes(y = price_to_earning)) +
  geom_boxplot(fill = "darkblue") +
  labs(title = "Boxplot của P/E", y = "P/E") + 
  theme_classic()
# Histogram
ggplot(df_clean, aes(x = price_to_earning)) +
  geom_histogram(alpha = 0.8, fill = "green", color = "black") +
  labs(title = "Phân phối của P/E", x = "P/E", y = "Frequency") + 
  theme_minimal()
# Density plot 
ggplot(df_clean, aes(x = price_to_earning)) +
  geom_density(fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Mật độ phân bố của P/E", x = "P/E", y = "Density") + 
  theme_minimal() 

# ROA 
# Box plot 
ggplot(df_clean, aes(y = roa)) +
  geom_boxplot(fill = "darkblue") +
  labs(title = "Boxplot của ROA", y = "ROA") + 
  theme_classic()
# Histogram
ggplot(df_clean, aes(x = roa)) +
  geom_histogram(alpha = 0.8, fill = "green", color = "black") +
  labs(title = "Phân phối của ROA", x = "ROA", y = "Frequency") + 
  theme_minimal()
# Density plot 
ggplot(df_clean, aes(x = roa)) +
  geom_density(fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Mật độ phân bố của ROA", x = "ROA", y = "Density") + 
  theme_minimal() 

# P/B: Price to Booking 
# Box plot 
ggplot(df_clean, aes(y = price_to_book)) +
  geom_boxplot(fill = "darkblue") +
  labs(title = "Boxplot của P/B", y = "P/B") + 
  theme_classic()
# Histogram
ggplot(df_clean, aes(x = price_to_book)) +
  geom_histogram(alpha = 0.8, fill = "green", color = "black") +
  labs(title = "Phân phối của P/B", x = "P/B", y = "Frequency") + 
  theme_minimal()
# Density plot 
ggplot(df_clean, aes(x = price_to_book)) +
  geom_density(fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Mật độ phân bố của P/B", x = "P/B", y = "Density") + 
  theme_minimal() 

# TradeBalance
# Box plot 
ggplot(df_clean, aes(y = TradeBalance)) +
  geom_boxplot(fill = "darkblue") +
  labs(title = "Boxplot của Trade Balance", y = "Trade Balance") + 
  theme_classic()
# Histogram
ggplot(df_clean, aes(x = TradeBalance)) +
  geom_histogram(alpha = 0.8, fill = "green", color = "black") +
  labs(title = "Phân phối của Trade Balance", x = "Trade Balance", y = "Frequency") + 
  theme_minimal()
# Density plot 
ggplot(df_clean, aes(x = TradeBalance)) +
  geom_density(fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Mật độ phân bố của Trade Balance", x = "Trade Balance", y = "Density") + 
  theme_minimal() 

# CPI 
# Box plot 
ggplot(df_clean, aes(y = CPI)) +
  geom_boxplot(fill = "darkblue") +
  labs(title = "Boxplot của CPI", y = "CPI") + 
  theme_classic()
# Histogram
ggplot(df_clean, aes(x = CPI)) +
  geom_histogram(alpha = 0.8, fill = "green", color = "black") +
  labs(title = "Phân phối của CPI", x = "CPI", y = "Frequency") + 
  theme_minimal()
# Density plot 
ggplot(df_clean, aes(x = CPI)) +
  geom_density(fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Mật độ phân bố của CPI", x = "CPI", y = "Density") + 
  theme_minimal() 

# Tỷ giá USD/VND 
# Box plot 
ggplot(df_clean, aes(y = USDVND)) +
  geom_boxplot(fill = "darkblue") +
  labs(title = "Boxplot của Tỷ giá USD/VND", y = "USD/VND") + 
  theme_classic()
# Histogram
ggplot(df_clean, aes(x = USDVND)) +
  geom_histogram(alpha = 0.8, fill = "green", color = "black") +
  labs(title = "Phân phối của Tỷ giá USD/VND", x = "USD/VND", y = "Frequency") + 
  theme_minimal()
# Density plot 
ggplot(df_clean, aes(x = USDVND)) +
  geom_density(fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Mật độ phân bố của Tỷ giá USD/VND", x = "USD/VND", y = "Density") + 
  theme_minimal() 

# GDP 
# Box plot 
ggplot(df_clean, aes(y = GDP)) +
  geom_boxplot(fill = "darkblue") +
  labs(title = "Boxplot của GDP", y = "GDP") + 
  theme_classic()
# Histogram
ggplot(df_clean, aes(x = GDP)) +
  geom_histogram(alpha = 0.8, fill = "green", color = "black") +
  labs(title = "Phân phối của GDP", x = "GDP", y = "Frequency") + 
  theme_minimal()
# Density plot 
ggplot(df_clean, aes(x = GDP)) +
  geom_density(fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Mật độ phân bố của GDP", x = "GDP", y = "Density") + 
  theme_minimal() 

# XAU_USD 
# Box plot 
ggplot(df_clean, aes(y = XAU_USD)) +
  geom_boxplot(fill = "darkblue") +
  labs(title = "Boxplot của XAU_USD", y = "XAU_USD") + 
  theme_classic()
# Histogram
ggplot(df_clean, aes(x = XAU_USD)) +
  geom_histogram(alpha = 0.8, fill = "green", color = "black") +
  labs(title = "Phân phối của XAU_USD", x = "XAU_USD", y = "Frequency") + 
  theme_minimal()
# Density plot 
ggplot(df_clean, aes(x = XAU_USD)) +
  geom_density(fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Mật độ phân bố của XAU_USD", x = "XAU_USD", y = "Density") + 
  theme_minimal() 

# WTI_USD
# Box plot 
ggplot(df_clean, aes(y = WTI_USD)) +
  geom_boxplot(fill = "darkblue") +
  labs(title = "Boxplot của WTI_USD", y = "WTI_USD") + 
  theme_classic()
# Histogram
ggplot(df_clean, aes(x = WTI_USD)) +
  geom_histogram(alpha = 0.8, fill = "green", color = "black") +
  labs(title = "Phân phối của WTI_USD", x = "WTI_USD", y = "Frequency") + 
  theme_minimal()
# Density plot 
ggplot(df_clean, aes(x = WTI_USD)) +
  geom_density(fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Mật độ phân bố của WTI_USD", x = "WTI_USD", y = "Density") + 
  theme_minimal() 

# Scatter plot giữa GDP và Close 
ggplot(df_clean, aes(x = GDP, y = close)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color =
                "red") +
  labs(title = "Scatter plot giữa GDP và Giá cổ phiếu (close)",
       x = "GDP",
       y = "Giá cổ phiếu") + 
  theme_minimal()

# Scatter plot giữa P/B và close 
ggplot(df_clean, aes(x = price_to_book, y = close)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color =
                "red") +
  labs(title = "Scatter plot giữa P/B và Giá cổ phiếu (close)",
       x = "P/B",
       y = "Giá cổ phiếu") + 
  theme_minimal()

# Scatter plot giữa USDVND và Close 
ggplot(df_clean, aes(x = USDVND, y = close)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color =
                "red") +
  labs(title = "Scatter plot giữa Tỷ giá (USD/VND) và Giá cổ phiếu (close)",
       x = "USDVND",
       y = "Giá cổ phiếu") + 
  theme_minimal()

# Scatter plot giữa CPI và Close 
ggplot(df_clean, aes(x = CPI, y = close)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color =
                "red") +
  labs(title = "Scatter plot giữa CPI và Giá cổ phiếu (close)",
       x = "CPI",
       y = "Giá cổ phiếu") + 
  theme_minimal()

# Scatter plot giữa WTI_USD và Close 
ggplot(df_clean, aes(x = WTI_USD, y = close)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color =
                "red") +
  labs(title = "Scatter plot giữa Giá dầu (WTI_USD) và Giá cổ phiếu (close)",
       x = "WTI_USD",
       y = "Giá cổ phiếu") + 
  theme_minimal()

# Scatter plot giữa WTI_USD và XAU_USD 
ggplot(df_clean, aes(x = WTI_USD, y = XAU_USD)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color =
                "red") +
  labs(title = "Scatter plot giữa Giá dầu (WTI_USD) và Giá vàng (XAU_USD)",
       x = "Giá dầu",
       y = "Giá vàng") + 
  theme_minimal()

# 4. Mô hình 
# 4.1. Mô hình OLS với các biến ban đầu  
model1 <- lm(close ~ roa + roe + price_to_earning + price_to_book + TradeBalance + CPI + USDVND + GDP + WTI_USD + XAU_USD, data = df_clean)
summary(model1)

#Kiểm tra khoảng tin cậy 
confint(model1)

# Chẩn đoán mô hình 
plot(model1, which = 1)
plot(model1, which = 2)

# Các loại kiểm định 
bptest(model1)
bgtest(model1, order = 2)
dwtest(model1)
vif(model1)

# Mô hình còn xảy ra tự tương quan, HET -> chuyển close thành biến lag D_close và một số biến độc lập 
# Mô hình với biến trễ D_close
df_clean$D_close <- c(NA, diff(df_clean$close))
df_clean$D_GDP <- c(NA, diff(df_clean$GDP))
df_clean$D_WTI_USD <- c(NA, diff(df_clean$WTI_USD))
df_clean$D_XAU_USD <- c(NA, diff(df_clean$XAU_USD))
df_clean$D_USDVND <- c(NA, diff(df_clean$USDVND))
df_clean <- na.omit(df_clean)

# Chạy lại mô hình 1 với biễn phụ thuộc là D_close 
model1 <- lm(D_close ~ roa + roe + price_to_earning + price_to_book + TradeBalance + CPI + USDVND + GDP + WTI_USD + XAU_USD, data = df_clean)
summary(model1)

# Kiểm định HET 
bptest(model1)  
# Kiểm định tự tương quan 
dwtest(model1)
# Kiểm định đa cộng tuyến 
vif(model1)

# Loại biến roe trong mô hình vì có hiện tượng đa cộng tuyến 
df_clean$roe <- NULL 

# Chạy lại mô hình 1 
model1 <- lm(D_close ~ roa + price_to_earning + price_to_book + TradeBalance + CPI + USDVND + GDP + WTI_USD + XAU_USD, data = df_clean)
summary(model1)

# 4.2. Mô hình lấy biến trễ của một số biến độc lập 
# Chạy mô hình 
model2 <- lm(D_close ~ roa + price_to_earning + price_to_book + TradeBalance + CPI + D_USDVND + D_GDP + D_WTI_USD + D_XAU_USD, data = df_clean)
summary(model2)

# Kiểm định HET 
bptest(model2)  # Bị HET -> giải quyết HET bằng Robust Standard Errors ở phần sau 
# Kiểm định tự tương quan 
dwtest(model2)
# Kiểm định đa cộng tuyến 
vif(model2)

# 5. Chọn lựa mô hình và kiểm định 
# Chọn lựa mô hình 
AIC(model1, model2) # model2 có AIC cao hơn -> chọn mô hình 2 

# Kiểm định sự phù hợp của mô hình 
resettest(model2)

# Kiểm định bỏ sót biến 
harvtest(model2) 

# Plot 
plot(model2, which = 1)
plot(model2, which = 2)

# Ước lượng bằng RSE để loại bỏ HET
coeftest(model2, vcov = vcovHC(model2, type = "HC1"))

