library(tidyverse)
library(stringr)
library(lubridate)
library(xts)
library(forecast)
library(tsibble)
library(fable)
library(fabletools)
#library(tsibbledata)
library(feasts)  # potrzebne do sparametryzowania automatycznego doboru lambdy 
library(plotly)


# ustawiamy katalog do którego wrzucimy dane dzienne z liczbą zachorowań na Covid19 w Polsce
setwd('E:/page ds/pages/ts_covid19/hist/')

# dane z https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2#

# najpierw dane historyczne z okresu 3.03-23.11.2020 
covid19 <- read.table('Zakazenia30323112020.csv', header = TRUE, sep=";", quote = "\"")
covid19$Nowe.przypadki <- str_replace_all(covid19$Nowe.przypadki, fixed(" "), "")
covid19$Data <- dmy(covid19$Data)

#pozostawiam jedynie kolumny nowe przypadki oraz z liczbą zgonów
covid19 <- covid19[,c("Data","Nowe.przypadki","Zgony")]

# Pozostałe dni są już prezentowane w trybie dziennym dla każdego województwa 
# Dlatego w pętli wczytam pozostałe dni per plik: za każdym razem dodam do istniejącej tabeli
files <- list.files(pattern = ".csv")

# wywalam z wektora już zaczytane pliki 
files <- files[!files %in% c("Zakazenia30323112020.csv","Zakazenia30323112020.csv.bak", "covid19_dane.csv" )]

tmp <- read.table(files[100], header = TRUE, sep=";", quote = "\"")

for (i in seq_along(files)) {
  tmp <- read.table(files[i], header = TRUE, sep=";", quote = "\"")
  tmp2 <- list(ymd(substr(files[i], 1,8)),  tmp[1,2], tmp[1,4]) # w każdym pliku w pierwszym wierszu jest total Polska
  covid19 <- rbind(covid19, tmp2)
}


# sortowanie po dacie 
covid19 <- covid19[order(covid19$Data),]

#formatowanie typow
covid19$Nowe.przypadki <- as.numeric(covid19$Nowe.przypadki)
covid19$Zgony <- as.numeric(covid19$Zgony)


# czyszczenie z niepotrzebnych obiektow
rm(tmp2)
rm(tmp)


# zapipsuję do pliku plaskiego i z niego będę w przyszlosci odczytywal 
write.csv(covid19,'E:\\page ds\\pages\\ts_covid19\\covid19_dane.csv', row.names = FALSE)
covid19 <- read.csv('E:\\page ds\\pages\\ts_covid19\\covid19_dane.csv')
tail(covid19)

#tworzę oddzielne szeregi dla nowych przypadków oraz dla liczby zgonów
covid19$Data <- as.Date(covid19$Data)
covid19.nowe.przypadki <-  covid19[,c(1,2)]
covid19.zgony <-  covid19[,c(1,3)]


# konwertuję teraz obiekty do formatu xts, przyjaznej formy do obsługi szeregów czasowych
covid19.nowe.przypadki <- xts(x = covid19.nowe.przypadki$Nowe.przypadki, order.by = covid19.nowe.przypadki$Data)
#covid19.zgony <- xts(x = covid19.zgony$Zgony, order.by = covid19.zgony$Data) 


#wywalam pierwszy miesiąc jako niereprezentatywny (brak odpowiedniej liczby testów)
covid19.nowe.przypadki <- covid19.nowe.przypadki['2020-07-31/']
#covid19.zgony <- covid19.zgony['2020-08-01/']

#analizuję wymiar nowych przypadków dziennie: dodaje nazwę serii
names(covid19.nowe.przypadki) <- c('przypadki')

# dzielę teraz zbiór na testowy oraz treningowy. 
# pozostawiam 2 tygodnie na testowanie
covid19.nowe.przypadki.train <- covid19.nowe.przypadki['/2022-07-12']
covid19.nowe.przypadki.test <- covid19.nowe.przypadki['2022-07-13/']


# i wykres szeregu  
covid19.nowe.przypadki.train %>% autoplot() +labs(title= "C19 new cases - train set", 
                                            y = "#cases", x = "Date of research")
tsdisplay(covid19.nowe.przypadki.train)
# na podstawie wykresów autokorelacji (ACF) oraz autokorelacji cząstkowej (PACF) 
# można wstępnie wywnioskować, że będzie to model typu AR. Najpierw zbuduję model 
# bez różnicowania pomimo braku stacjonarności i zobaczę jak będzie sobie radził z prognozowaniem
# pomimo braku spełnienia warunku stacjonarności

mod <- auto.arima(covid19.nowe.przypadki.train)
#ARIMA(1,0,2)
# nim przyglądnę się jak model radzi sobie z prognozowaniem na okresie testowym, wykonam również 
# prognozę po uspokojeniu wariancji, która w okresach kolejnych fal pandemii istotnie różni się 
# od tych z okresów poza falą. W tym celu dokonam transformacji Boxa Coxa z automatycznie dobranym 
# parametrem wygładzenia. Skorzystam z wwbudowanej metody guerrero z pakietu fabletools


Sys.setenv(LANG = "en")

# niestety dane musza byc w postaci tsibble do którego nie można dojść z xts
tmp_lambda <-  covid19[,c(1,2)] %>% as_tsibble()

# podzbiór taki jak dla zbioru testowego
tmp_lambda <- tmp_lambda %>% filter(Data >= '2020-07-31' & Data <= '2022-07-12' ) %>% as_tsibble()

#automatyczny dobór optymalnego parametru
lambda_ <- tmp_lambda %>% 
  features(Nowe.przypadki, features = guerrero) %>%  
    pull(lambda_guerrero)
#0.08634282 
tmp <- box_cox(covid19.nowe.przypadki.train,lambda = lambda_) 


tmp %>% autoplot() +labs(title= "C19 new cases with CoxBox transformation with lambda = 0.0729", 
                         y = "CB(cases)", x = "Date of research")


covid19.nowe.przypadki.train.lambda <- box_cox(covid19.nowe.przypadki.train,lambda = lambda_)


tsdisplay(covid19.nowe.przypadki.train)
tsdisplay(covid19.nowe.przypadki.train.lambda)

# i model na danych z transformacją Boxa-Coxa,
mod.lambda <- auto.arima(covid19.nowe.przypadki.train.lambda)
# ARIMA(3,1,3)

#podsumowania obu modeli 
summary(mod) # ARIMA(1,0,2)
summary(mod.lambda)  # ARIMA(3,1,3)


# wykres prognozy
covid19.nowe.przypadki.test <- ts(covid19.nowe.przypadki.test, start = length(covid19.nowe.przypadki.train)+1) 
fc_mod <- forecast(mod,h = 14)
autoplot(fc_mod) +
  autolayer(covid19.nowe.przypadki.test, series="Test Data") +
  autolayer(fc_mod$mean, series="Forecasts")
# słabo widać ze względu na rozdzielczość ale to co się rzuca to bardzo duży przedział ufności 
# oraz prognoza wydaje się być naiwna tj idzie po maksimach 

# i jeszcze powiększenie tylko okresu testowego
plot_ly(x=1:14) %>%
  add_lines(y = covid19.nowe.przypadki.test, color = I("red"), name = "Test") %>%
  add_lines(y = fc_mod$mean, color = I("green"), name = "Forecast ARIMA(1,0,2)")
# Widać, że model bardzo dobrze poradził sobie z wykryciem trendu ale nie widzi kompletnie weekendowych korekt


# zobaczmy jak radzi sobie model po transformacji Boxa Coxa
fc_mod_lambda <- forecast(mod.lambda,h = 14)


plot_ly(x=1:14) %>%
  add_lines(y = covid19.nowe.przypadki.test, color = I("red"), name = "Test") %>%
  add_lines(y = inv_box_cox(fc_mod_lambda$mean, lambda_), color = I("green"), name = "Forecast ARIMA(3,1,3)")
# tutaj widać, że model wychwytuje cykliczność ale kompletnie nie radzi sobie z trendem

#porównanie obu wykresów na jednym
plot_ly(x=1:14) %>%
  add_lines(y = covid19.nowe.przypadki.test, color = I("red"), name = "Test") %>%
  add_lines(y = inv_box_cox(fc_mod_lambda$mean, lambda_), color = I("green"), name = "Forecast ARIMA Box-Cox transform") %>%
  add_lines(y = fc_mod$mean, color = I("blue"), name = "Forecast ARIMA with no transformations")
# po wykresie widać, że model bez transformacji BC szybciej wychwytuje wzrosty ale oba mocno niedoszacowują wynik 


#Jak widać prognoza modelem bez spełnionej stacjonarności radzi sobie o wiele lepiej!
# Można by się było pokusić o kolejną analizę reszt itp ale ten element omijam i spróbuję jeszcze zbudować model
# wykonując różnicowanie zbioru danych 


#checkresiduals(mod) # nie jest to optymalny model
#covid19.nowe.przypadki.reszty <- residuals(mod)
#qqnorm(covid19.nowe.przypadki.reszty, main = "wykres reszt i test normalności")
#qqline(covid19.nowe.przypadki.reszty)
# rozkład zdecydowanie nie jest normalny, widzimy duże odchylenia na wykresie QQ 
# szczegolnie na wartościach krańcowych -> mozna podrasować model np zmieniając jego typ


# sprawdzenie istotności parametrów modelu a gdy nie są istotne, usunięcie ich z modelu
#coef <- mod$coef
#coef.std <- sqrt(diag(mod$var.coef))

#ratios <- coef / (1.96 * coef.std)
#istotne <- which(abs(ratios) >= 1)
#mod.fixet <- numeric(7)
#mod.fixet[istotne] <- NA

#mod.subset <- Arima(covid19.nowe.przypadki.train, order = c(5,1,2), fixed = mod.fixet)
#mod$aicc - mod.subset$aicc  # to są te same modele bo wszystkie współczynniki sa istotne


# Różnicowanie jednokrotne ponieważ nie zauważyłem żadnej istnotnej zależności na wykresie lag
# pooglądajmy sobie też korelację między okresami, może trzeba zacząc od róznicowania
lag.plot(covid19.nowe.przypadki.train, lags = 15)
# niestety widać korelacją praktycznie z każdym lagiem więc nie ma oczywistego parametru różnicowania


covid19.nowe.przypadki.diff1 <- diff(covid19.nowe.przypadki.train, lag = 1)
tsdisplay(covid19.nowe.przypadki.train)
tsdisplay(covid19.nowe.przypadki.diff1)

# mamy symetrię ale wariancja nie jest jednorodna w czasie więc może spróbuję wykonać transformację Boxa Coxa
# można sprawdzić, że transformacja BC z optymalnym parametrem lambda nie przynosi żadnych korzyści więc ten krok pomijam
#to samo dotyczy kolejnych rzędów różnicowania danych o okresie 1 czy 7 więc ich również nie pokazuje w zestawieniu

#tmp_lambda <-  data.frame(Data = time(covid19.nowe.przypadki.diff1), przypadki = coredata(covid19.nowe.przypadki.diff1)) %>% 
#     as_tsibble()
# podzbiór taki jak dla zbioru testowego
#tmp_lambda <- tmp_lambda %>% filter(Data >= '2020-08-01' & Data <= '2022-07-08' ) %>% as_tsibble()
#automatyczny dobór optymalnego parametru
#lambda_ <- tmp_lambda[2:nrow(tmp_lambda),] %>%  # ponieważ wykonałem róznicowanie, pierwszy element to NA
#  features(przypadki, features = guerrero) %>%  
#  pull(lambda_guerrero)
#0.51237 
#tmp <- box_cox(covid19.nowe.przypadki.diff1,lambda = lambda_) 
#tmp %>% autoplot() +labs(title= "C19 new cases with CoxBox transformation with lambda = guerrero method", 
#                         y = "CB(cases)", x = "Date of research")
#Róznicowanie
#covid19.nowe.przypadki.diff11 <- diff(covid19.nowe.przypadki.diff1)
#tsdisplay(covid19.nowe.przypadki.diff11)
#covid19.nowe.przypadki.diff117 <- diff(covid19.nowe.przypadki.diff11, lags = 7)
#tsdisplay(covid19.nowe.przypadki.diff117)
#tmp_lambda <-  data.frame(Data = time(covid19.nowe.przypadki.diff117), przypadki = coredata(covid19.nowe.przypadki.diff117)) %>% 
#  as_tsibble()
#lambda_ <- tmp_lambda[4:nrow(tmp_lambda),] %>%  # ponieważ wykonałem róznicowanie, pierwszy element to NA
#  features(przypadki, features = guerrero) %>%  
#  pull(lambda_guerrero)
#tmp <- box_cox(covid19.nowe.przypadki.diff117,lambda = lambda_) 
#tsdisplay(tmp)

mod1 <- auto.arima(covid19.nowe.przypadki.diff1)


summary(mod1)
#ARIMA(5,0,2)
fc_mod1 <- forecast(mod1,h = 14)
autoplot(fc_mod1) +
  autolayer(covid19.nowe.przypadki.test, series="Test Data") +
  autolayer(fc_mod1$mean, series="Forecasts")


ost <- coredata(covid19.nowe.przypadki.train[length(covid19.nowe.przypadki.train)])

plot_ly(x=1:14) %>%
  add_lines(y = coredata(covid19.nowe.przypadki.test), color = I("red"), name = "Test") %>%
  add_lines(y = cumsum(fc_mod1$mean) + ost, color = I("orange"), name = "Model after diff Lag=1 (5,0,2)") %>%
  add_lines(y = inv_box_cox(fc_mod_lambda$mean, lambda_), color = I("green"), name = "Forecast ARIMA Box-Cox transform (3,1,3)") %>%
  add_lines(y = fc_mod$mean, color = I("blue"), name = "Forecast ARIMA autmoatic (1,0,2)")
#model po transformacji róznicowania również wychwytuje cykliczność ale słabo radzi sobie z trendem


covForecast <- data.frame(index = 1:14, 
                          dane = coredata(covid19.nowe.przypadki.test),
                          Arima_lag1 = cumsum(fc_mod1$mean) + ost,
                          Arima_bc = inv_box_cox(fc_mod_lambda$mean, lambda_),
                          Arima_auto = fc_mod$mean)


write.csv(covForecast,'E:\\page ds\\pages\\ts_covid19\\covForecast.csv', row.names = FALSE)

coredata(covid19.nowe.przypadki.train)
fitted(mod, h = 1)
inv_box_cox(fitted(mod.lambda, h = 1), lambda_) 

prognoza <- coredata(covid19.nowe.przypadki.diff1)-fc_mod1$residuals
prognoza <- prognoza * - 1
prognoza[1] <-  coredata(covid19.nowe.przypadki.train[1])[,]
cumsum(prognoza)


covTrain <- data.frame(index = 1:712,
                        dane = coredata(covid19.nowe.przypadki.train),
                       Arima_lag1 = cumsum(prognoza),
                       Arima_bc = inv_box_cox(fitted(mod.lambda, h = 1), lambda_) ,
                       Arima_auto = fitted(mod, h = 1))

write.csv(covTrain,'E:\\page ds\\pages\\ts_covid19\\covTrain.csv', row.names = FALSE)
