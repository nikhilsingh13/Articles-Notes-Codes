# chapter_6.R
# Author: Nikhil Singh
# Date: December 17, 2019
# Reference: https://otexts.com/fpp2/components.html

## Libraries
library(fpp2)
pacman::p_load(seasonal)

## Time-Series Components ----
### Seasonally adjusted data ----
### Moving Averages ----
#### Moving average smoothing
autoplot(elecsales) + xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia")

ma(elecsales, 5)

#### trend-cycle estimates look like

autoplot(elecsales, series = 'Data') +
  autolayer(ma(elecsales, 3), series = '3-MA') +
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia 3-MA") +
  scale_colour_manual(values = c("Data" = "grey50", "3-MA" = "red")
                      , breaks = c("Data", "3-MA"))

autoplot(elecsales, series = 'Data') +
  autolayer(ma(elecsales, 5), series = '5-MA') +
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia 5-MA") + 
  scale_colour_manual(values = c("Data"="grey50", "5-MA"="red"),
                      breaks = c("Data", "5-MA"))

autoplot(elecsales, series = 'Data') +
  autolayer(ma(elecsales, 7), series = '7-MA') +
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia 7-MA") + 
  scale_colour_manual(values = c("Data"="grey50", "7-MA"="red"),
                      breaks = c("Data", "7-MA"))

autoplot(elecsales, series = 'Data') +
  autolayer(ma(elecsales, 9), series = '9-MA') +
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia 9-MA") + 
  scale_colour_manual(values = c("Data"="grey50", "9-MA"="red"),
                      breaks = c("Data", "9-MA"))

#### Moving averages of moving average
ausbeer %>% head

beer2 <- window(ausbeer, start = 1992)
ma4 <- ma(beer2, order = 4, centre = FALSE)
ma2X4 <- ?ma(beer2, order = 4, centre = TRUE)

#### > When a 2-MA follows a moving average of an even order (such as 4), it is called a "centred moving average of order 4". Results are now symmetric.

##### Estimating the trend-cycle with seasonal data
##### > The most common use of centred moving averages is for estimating the trend-cycle from seasonal data.

##### > In general, a 2×m-MA is equivalent to a weighted moving average of order m+1 where all observations take the weight 1/m, except for the first and last terms which take weights 1/(2m). So, if the seasonal period is even and of order m, we use a 2×m-MA to estimate the trend-cycle. If the seasonal period is odd and of order m, we use a m-MA to estimate the trend-cycle. For example, a 2×12-MA can be used to estimate the trend-cycle of monthly data and a 7-MA can be used to estimate the trend-cycle of daily data with a weekly seasonality.

autoplot(elecequip, series = "Data") +
  autolayer(ma(elecequip, 12), series = "12-MA") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_color_manual(values=c("Data"="grey", "12-MA"="red")
                     , breaks = c("Data", "12-MA"))

### Classical decomposition
#### Additive decomposition
#### Multiplicative decomposition
elecequip %>% 
  decompose(type = "multiplicative") %>% 
  autoplot() + 
  xlab("Year") +
  ggtitle("Classical multiplicative decomposition of electrical equipment index")

### X11 decomposition
elecequip %>% 
  seas(x11="") -> fit

autoplot(fit) +
  ggtitle("X11 decomposition of electrical equipment index")

autoplot(elecequip, series = "Data") +
  autolayer(trendcycle(fit), series = "Trend") +
  autolayer(seasadj(fit), series = "Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values = c('gray', "blue", "red")
                      , breaks = c("Data", "Seasonally Adjusted", "Trend"))

fit %>% 
  seasonal() %>% 
  ggsubseriesplot() +
  ylab("Seasonal")

### SEATS decomposition 
#### SEATS - Seasonal Extraction in ARIMA Time Series
elecequip %>% 
  seas() %>% 
  autoplot() + 
  ggtitle("SEATS decomposition of electrical equipment index")

### STL decomposition
#### Seasonal and Trend decomposition using Loess. Loess is a method for estimating nonlinear relationship.

elecequip %>% 
  stl(t.window=13, s.window="periodic", robust = TRUE) %>% 
  autoplot

#### t.window is the number of consecutive observations to be used when estimating the trend-cycle
#### s.window is the number of consecutive years to be used in estimating each value in the seasonal component. 

?mstl

### Measuring strength of trend and seasonality
#### For strongly trended data, the seasonally adjusted data should have much more variation than the remainder component



### Forecasting with decomposition
fit <- stl(elecequip, t.window = 13, s.window = "periodic", robust = TRUE)

fit %>% 
  seasadj %>% 
  naive %>% 
  autoplot() + 
  ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

fit %>% 
  forecast(method="naive") %>% 
  autoplot() +
  ylab("New orders index")

fcast <- stlf(elecequip, method = "naive") 
# this function use mstl() function inside for decomposition with default s.window and t.window values

# ---------------------------------------------------------
### Exercises ---------------------------------------------
#### Q2. -----
##### a
?plastics
autoplot(plastics)

##### b
plastics %>% 
  decompose(type = "multiplicative") %>% 
  autoplot() + 
  xlab("Year") +
  ggtitle("Classical multiplicative decomposition of plastics dataset")

##### d
decomposed_plastics <- plastics %>% decompose(type = "multiplicative")
seasadj_plastics <- decomposed_plastics$x/decomposed_plastics$seasonal
seasadj_plastics %>% 
  autoplot

##### e
plastics %>% str
plastics_w_outlier <- plastics
plastics_w_outlier[20] <- plastics[20]+500 # august,2
plastics_w_outlier

plastic_outlier_decomp <- decompose(plastics_w_outlier, type = "multiplicative")
plastic_o_seasadj <- plastic_outlier_decomp$x/plastic_outlier_decomp$seasonal
plastic_o_seasadj %>% autoplot

#### Q3. -----
retail <- readxl::read_excel('retail.xlsx', skip=1)

retail %>% str
retail %>% dim

retail$A3349335T %>% 
  is.na %>% 
  sum

retail$A3349335T %>% 
  ts(start = 1992, frequency = 12) %>% 
  seas(x11="") -> fit

fit %>% 
  autoplot + 
  ggtitle("X11 decomposition of retail data (A3349335T)")

#### Q5. -----
##### a)
cangas %>% 
  autoplot + 
  ggtitle("Canadian gas production in billions of cubic metres, Jan 60 - Feb 2005")

cangas %>% 
  ggsubseriesplot

cangas %>% 
  ggseasonplot

##### b)
cangas %>% 
  stl(s.window = 'periodic', t.window = NULL, robust = TRUE) %>% 
  autoplot() +
  ggtitle("X11 decomposition of cangas data with seasonal window of all")

cangas %>% 
  stl(s.window = 12, t.window = NULL, robust = TRUE) %>% 
  autoplot() +
  ggtitle("X11 decomposition of cangas data with seasonal window = 12")

cangas %>% 
  stl(s.window = 12, t.window = NULL, robust = TRUE) %>% 
  autoplot() +
  ggtitle("X11 decomposition of cangas data with seasonal window = 6")

##### c)
cangas %>% 
  seas %>% 
  autoplot() + 
  ggtitle("SEATS decomposition of cangas data")

#### Q6. -----
##### a)
bricksq %>% 
  stl(s.window = "periodic", t.window = 30, robust = TRUE) %>% 
  autoplot()

bricksq %>% 
  stl(s.window = 12, t.window = 30, robust = TRUE) %>% 
  autoplot()

##### b) Plot seasonally adjusted data
bricksq %>% 
  stl(s.window = "periodic", t.window = 30, robust = FALSE) %>% 
  seasadj %>% 
  autoplot()

##### c) 
stl_fit <- stl(bricksq, t.window = 30, s.window = "periodic", robust = FALSE)
stl_fit %>% 
  seasadj %>% 
  naive %>% 
  autoplot() +
  ylab("New orders index") +
  ggtitle("Naive Forecast of seasonally adjusted data")
  
stl_fit %>% 
  forecast(method = "naive") %>% 
  autoplot() +
  ylab("New orders index")

##### d) Use stlf() to reseasonalise the results, giving forecasts for the original data

stlf_fit <- stlf(bricksq, method = "naive")
stlf_fit %>% 
  forecast(h=8)

##### e) Do the residuals look uncorrelated?
checkresiduals(stlf_fit)

##### f) Repeat with a robust STL decomposition. Does it make much difference?
bricksq %>% 
  stl(t.window=30, s.window="periodic", robust=TRUE) %>% 
  seasadj %>%
  naive %>% 
  autoplot() +
  ylab("New orders index") +
  ggtitle("Naive forecast of robust seasonally adjusted data")

bricksq %>% 
  stl(t.window=30, s.window="periodic", robust=TRUE) %>% 
  forecast(method = "naive") %>% 
  autoplot() +
  ylab("New orders index") +
  ggtitle("Naive forecast of robust seasonally adjusted data")

##### g) Compare forecasts from stlf() with those from snaive(), using a test set comprising the last 2 years of data. Which is better?
bricksq_training <- window(bricksq, end = c(1992,3))
bricksq_test <- window(bricksq, start = c(1992, 4))

snaive_fit <- snaive(bricksq_training)
stlf_fit <- stlf(bricksq_training)

snaive_forecast <- forecast(snaive_fit, h=8)
stlf_forecast <- forecast(stlf_fit, h=8)

autoplot(bricksq, series = "Data") +
  autolayer(snaive_forecast, PI = FALSE, series = "snaive") +
  autolayer(stlf_forecast, PI = FALSE, series = "stlf") +
  scale_colour_manual(values = c("gray50", "blue", "red")
                      , breaks = c("Data", "snaive", "stlf")) +
  guides(colour = guide_legend(title = "")) +
  ggtitle("Forecast comparison by 2 methods on bricksq data")