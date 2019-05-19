library('olsrr')
library('stats')
library('dplyr')
library('lmtest')
library('gap')
library('tseries')
library('TSA')
library('forecast')
library('xts')
library('data.table')


#1. Готовим данные
#1.1 Ряд GCFC
setwd('C:\\Users\\Sasha\\Documents\\MEGA\\MAGA\\Econometrics\\hw2\\data')
gcfr <- read.csv(file = 'API_NE.GDI.FTOT.CD_DS2_en_csv_v2_10582916.csv', header = FALSE, sep = ',')
gcfr <- gcfr[-c(1, 2, 3), ]
colnames(gcfr) <- c("Country Name","Country Code","Indicator Name","Indicator Code",
                  "1960","1961","1962","1963","1964","1965","1966","1967","1968","1969"
                  ,"1970","1971","1972","1973","1974","1975","1976","1977","1978","1979"
                  ,"1980","1981","1982","1983","1984","1985","1986","1987","1988","1989"
                  ,"1990","1991","1992","1993","1994","1995","1996","1997","1998","1999"
                  ,"2000","2001","2002","2003","2004","2005","2006","2007","2008","2009"
                  ,"2010","2011","2012","2013","2014","2015","2016","2017","2018")
gcf1 <- gcfr[c(80), ]
gcf <- gcf1[-c(1:4)]
gcf <- gcf[,colSums(is.na(gcf)) < nrow(gcf)]
gcf <- gcf[-c(1:5)]
gcf <- gcf[-53]
gcft <- t(gcf)

#1.2 Ряд GDP
gdpr <- read.csv(file = 'API_NY.GDP.MKTP.CD_DS2_en_csv_v2_10576830.csv', header = FALSE, sep = ',')
gdpr <- gdpr[-c(1, 2, 3), ]
colnames(gdpr) <- c("Country Name","Country Code","Indicator Name","Indicator Code",
                    "1960","1961","1962","1963","1964","1965","1966","1967","1968","1969"
                    ,"1970","1971","1972","1973","1974","1975","1976","1977","1978","1979"
                    ,"1980","1981","1982","1983","1984","1985","1986","1987","1988","1989"
                    ,"1990","1991","1992","1993","1994","1995","1996","1997","1998","1999"
                    ,"2000","2001","2002","2003","2004","2005","2006","2007","2008","2009"
                    ,"2010","2011","2012","2013","2014","2015","2016","2017","2018")
gdp1 <- gdpr[c(80), ]
gdp <- gdp1[-c(1:4)]
gdp <- gdp[, colSums(is.na(gdp)) < nrow(gdp)]
gdp <- gdp[-c(1:5)]
gdp <- gdp[-53]
gdpt <- t(gdp)

#1.3 Переводим оба ряда в time series
gdptts <- ts(
  gdpt,
  start = c(1965),
  end = c(2016),
  frequency = 1
)
gcftts <- ts(
  gcft,
  start = c(1965),
  end = c(2016),
  frequency = 1
)

#Дальше пойдет речь о вспомогательных рядах - индексах цен. Нужных для перевода
#из номинальных значений индикаторов в реальные.

#1.4 Ряд CPI
CPI_all <- read.csv(file = 'CPIUKA.csv', header = FALSE, sep = ',', colClasses = 'character')
#Приводим к базовому году - 1965
CPI_actual <- as.data.table(CPI_all[-c(1:757),])
setnames(CPI_actual,c('date','index'))
CPI_level <- as.numeric(CPI_actual[1,index])
CPI <- CPI_actual[,new_index := as.numeric(index)/CPI_level]
CPI

#1.5 Ряд WPP
WPP_all <- read.csv(file = 'WPPIUKA.csv', header = FALSE, sep = ',', colClasses = 'character')
#Приводим к базовому году - 1965
WPP_actual <- as.data.table(WPP_all[-c(1:305),])
setnames(WPP_actual,c('date','index'))
WPP_level <- as.numeric(WPP_actual[1,index])
WPP <- WPP_actual[,new_index := as.numeric(index)/WPP_level]
WPP


#Делим ряд GDP на CPI и GCFC на WPP

CPI <- ts(CPI,
          start = c(1965),
          end = c(2016),
          frequency = 1)
gdptts <- gdptts / CPI[, 'new_index']


WPP <- ts(WPP,
          start = c(1965),
          end = c(2016),
          frequency = 1)
gcftts <- gcftts / WPP[, 'new_index']

#Данные готовы.

#2. тестируем на стационарность ряды
#2.1 ряд GCFC
adf.test(gcftts)
kpss.test(gcftts)
#adf говорит о стационарности, кпсс - обратное. Сл-но, ряд не стационарен

#Посмотрим на график, видим, что ряд очевидно имеет тренд:
plot(gcftts)

#Гоним регрессию, вычитаем тренд
linear_fit <- lm(gcftts ~ time(gcftts))
plot(gcftts - linear_fit$fitted.values + linear_fit$fitted.values[1],
     ylab = "detrended GCF", typ = 'o')
gcf_detr <- gcftts - linear_fit$fitted.values + linear_fit$fitted.values[1]

#Еще раз проверяем на стационарность
adf.test(gcf_detr)
kpss.test(gcf_detr)
#Теперь оба теста говорят о стационарности => ряд стационарен


#2.2 ряд GDP
adf.test(gdptts)
kpss.test(gdptts)
#adf говорит о стационарности, кпсс - обратное. Сл-но, ряд не стационарен

#Посмотрим на график, видим, что ряд очевидно имеет тренд:
plot(gdptts)

#гоним регрессию, вычитаем тренд
linear_fit <- lm(gdptts ~ time(gdptts))
plot(gdptts - linear_fit$fitted.values + linear_fit$fitted.values[1],
     ylab = "detrended GDP", typ = 'o')
gdp_detr <- gdptts - linear_fit$fitted.values + linear_fit$fitted.values[1]

#Еще раз проверяем на стационарность
adf.test(x = gdp_detr, alternative = 'stationary')
kpss.test(gdp_detr)
#Оба теста говорят о стационарности => ряд стационарен.

#3. Переходим к модели

#Взглянем на PACF и ACF:
pacf(gcf_detr)
acf(gcf_detr)

#Переобозначение переменных согласно модели:
investment <- gcf_detr
investment <- investment[-1]

#Создаем матрицу, к-ая состоит из Y_t и Y_(t-1)
gdp_detr_lag <- stats:::lag(gdp_detr, -1)
product <- (ts.union(gdp_detr, gdp_detr_lag))
product <- product[-c(1, 53),]

cor(product)

#тут завожу переменную, на которую делю ряды. Нужно для того, чтобы модель запустилась.
#с большими числами она плохо работает и выдает ошибку. Если поделить то все норм.

denom <- 1000000000
         
#далее делаем много моделей с разной MA() частью:

m1 <- arimax(investment/denom, order = c(1, 0, 0), xreg = product/denom)
m2 <- arimax(investment/denom, order = c(1, 0, 1), xreg = product/denom)
m3 <- arimax(investment/denom, order = c(1, 0, 2), xreg = product/denom)
m4 <- arimax(investment/denom, order = c(1, 0, 3), xreg = product/denom)
m5 <- arimax(investment/denom, order = c(1, 0, 4), xreg = product/denom)
m6 <- arimax(investment/denom, order = c(1, 0, 5), xreg = product/denom)
m7 <- arimax(investment/denom, order = c(1, 0, 6), xreg = product/denom)
m8 <- arimax(investment/denom, order = c(1, 0, 7), xreg = product/denom)



#тестирование на значимость и остатков
#Смотрим на значимость полученных коэф-ов:

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)

coeftest(m1)
coeftest(m2)
coeftest(m3)
coeftest(m4)
coeftest(m5)
coeftest(m6) #Самый лучший по значимости на нужные нам коэф-ты.
coeftest(m7)
coeftest(m8)

#график зависимости лмбды мю и дельты от порядка МА части

fun_lmb <- function(x) {
  m <- x
  lambda = 1 - m$coef['ar1']
  lambda
}

fun_mu <- function(x) {
  m <- x
  lambda = 1 - m$coef['ar1']
  mu = m$coef['gdp_detr'] / lambda
  mu
}

fun_dlt <- function(x) {
  m <- x
  lambda = 1 - m$coef['ar1']
  mu = m$coef['gdp_detr'] / lambda
  delta = m$coef['gdp_detr_lag'] / (lambda * mu) + 1
  delta
}

mas <- list(m1, m2, m3, m4, m5, m6, m7, m8)

all_lmb <- lapply(X = mas, FUN = fun_lmb)
all_mu <- lapply(X = mas, FUN = fun_mu)
all_dlt <- lapply(X = mas, FUN = fun_dlt)

all_lmb

ind_plot <- as.numeric(c(0, 1, 2, 3, 4, 5, 6, 7))

plot(
  x = ind_plot,
  y = all_lmb,
  col = 'blue',
  type = 'b',
  ylim = c(0, 3),
  ylab = 'Значение параметров',
  xlab = 'Порядок MA',
  panel.first = grid(),
  main = 'Зависимость параметров от MA части'
)
lines(x = ind_plot,
      y = all_mu,
      col = 'green',
      type = 'b')
lines(x = ind_plot,
      y = all_dlt,
      col = 'red',
      type = 'b')
legend(
  "topright",
  legend = c('Lambda', 'Mu', 'Delta'),
  text.col = c('blue', 'green', 'red')
)



#Смотрим на остатки у той модели, что нам больше всего понравилась:
m <- m6
acf(m$residuals, main = 'Автокорреляционная функция остатков')
#остатки не коррелированы

plot(density(m$residuals), main = 'Распределение остатков модели', ylab = 'Частота')


plot(m$residuals, main = 'Распределение остатков модели', xlab = 'Время', ylab = 'Остатки')



#4. Дополнительный код
#Код показывает коэф-ты в зависимости от порядка MA() части
fun_coef <- function(x) {
  m <- x
  lambda = 1 - m$coef['ar1']
  mu = m$coef['gdp_detr'] / lambda
  delta = m$coef['gdp_detr_lag'] / (lambda * mu) + 1
  y <- c(lambda, mu, delta)
  names(y) <- c('lambda', 'mu', 'delta')
  y
}

list_of_coefsets <- lapply(FUN = fun_coef,
                           X = list(m1, m2, m3, m4, m5, m6, m7, m8)
)

list_of_coefsets
