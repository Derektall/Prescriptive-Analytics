library(data.table)
library(ggplot2)
library(car)
library("margins")
library(corrplot)
library(sandwich)
library(nlme)
library(tidyverse)
library(plm)
library(lmtest)
library(reshape2)
library(plm)
library(ivpanel)
library(MVT)

guns$lvio <- log(guns$vio)
guns$lmur <- log(guns$mur)
guns$lrob <- log(guns$rob)


viobox <- boxplot(lvio~year, data = guns, main = "Violent Crime Rate Over Time",
xlab="Year", ylab="(Log) Violent Crime Rate Per 100,000")
viobox

murbox <- boxplot(lmur~year, data = guns, main = "Murder Rate Over Time",
                  xlab="Year", ylab="(Log) Murder Rate Per 100,000")
murbox

robbox <- boxplot(lrob~year, data = guns, main = "Robbery Rate Over Time",
                  xlab="Year", ylab="(Log) Robbery Rate Per 100,000")
robbox

incarbox <- boxplot(incarc_rate~year, data = guns, main = "Incarceration Rate Over Time",
                  xlab="Year", ylab="Incarceration Rate Per 100,000")
incarbox




mean_vio <- group_by(guns,year) %>%
summarise(vio=mean(vio,na.rm = TRUE))

mean_mur <- group_by(guns,year) %>%
  summarise(mur=mean(mur,na.rm = TRUE))

mean_rob <- group_by(guns,year) %>%
  summarise(rob=mean(rob,na.rm = TRUE))

mean_incarc_rate <- group_by(guns,year) %>%
  summarise(incarc_rate=mean(incarc_rate,na.rm = TRUE))

vioplot <- ggplot(mean_vio, aes(x = year, y = mean_vio$vio)) +
  geom_point(color="red") + geom_line(color="red") +
  xlab("Year") + ylab("Mean Violent Crimes per 100,000 People")
vioplot

murplot <- ggplot(mean_mur, aes(x = year, y = mean_mur$mur)) +
  geom_point(color="blue") + geom_line(color="blue") +
  xlab("Year") + ylab("Mean Murders per 100,000 People")
murplot

robplot <- ggplot(mean_rob, aes(x = year, y = mean_rob$rob)) +
  geom_point(color="dark green") + geom_line(color="dark green") +
  xlab("Year") + ylab("Mean Robberies per 100,000 People")
robplot

incarc_rateplot <- ggplot(mean_incarc_rate, aes(x = year, y = mean_incarc_rate$incarc_rate)) +
  geom_point(color="orange") + geom_line(color="orange") +
  xlab("Year") + ylab("Mean Incarcerated Per 100,000 people")
incarc_rateplot

#Separating States with Shall laws and those without

mean_shallvio <- group_by(guns,shall,year) %>%
  summarise(vio=mean(vio,na.rm = TRUE))

mean_shallmur <- group_by(guns,shall,year) %>%
  summarise(mur=mean(mur,na.rm = TRUE))

mean_shallrob <- group_by(guns,shall,year) %>%
  summarise(rob=mean(rob,na.rm = TRUE))

mean_shallincar<- group_by(guns,shall,year) %>%
  summarise(incar_rate=mean(incar_rate,na.rm = TRUE))

vioshallplot <- ggplot(mean_shallvio, aes(x = year, y = mean_shallvio$vio, color = shall, group = shall)) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Mean Violent Crimes Per 100,000 people")
vioshallplot

Murshallplot <- ggplot(mean_shallmur, aes(x = year, y = mean_shallmur$mur, color = shall, group = shall)) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Mean Murders Per 100,000 people")
Murshallplot

robshallplot <- ggplot(mean_shallrob, aes(x = year, y = mean_shallrob$rob, color = shall, group = shall)) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Mean Robberies Per 100,000 people")
robshallplot

incarshallplot <- ggplot(mean_shallincar, aes(x = year, y = mean_shallincar$incar_rate, color = shall, group = shall)) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Mean Incarcerated Per 100,000 people")
incarshallplot

viomod <- lm(vio ~ ., data=guns)
cooksdvio <- cooks.distance(viomod)



guns2<- guns

qnt <- quantile(guns2$vio, probs=c(.25, .75), na.rm = T)
caps <- quantile(guns2$vio, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(guns2$vio, na.rm = T)
guns2$vio[guns2$vio < (qnt[1] - H)] <- caps[1]
guns2$vio[guns2$vio > (qnt[2] + H)] <- caps


qnt2 <- quantile(guns2$mur, probs=c(.25, .75), na.rm = T)
caps2 <- quantile(guns2$mur, probs=c(.05, .95), na.rm = T)
H2 <- 1.5 * IQR(guns$mur, na.rm = T)
guns2$mur[guns2$mur < (qnt2[1] - H2)] <- caps2[1]
guns2$mur[guns2$mur > (qnt2[2] + H2)] <- caps2[2]

qnt3 <- quantile(guns2$rob, probs=c(.25, .75), na.rm = T)
caps3 <- quantile(guns2$rob, probs=c(.05, .95), na.rm = T)
H3 <- 1.5 * IQR(guns2$rob, na.rm = T)
guns2$rob[guns2$rob < (qnt3[1] - H3)] <- caps3[1]
guns2$rob[guns2$rob > (qnt3[2] + H3)] <- caps3[2]


qnt4 <- quantile(guns2$incarc_rate, probs=c(.25, .75), na.rm = T)
caps4 <- quantile(guns2$incarc_rate, probs=c(.05, .95), na.rm = T)
H4 <- 1.5 * IQR(guns2$incarc_rate, na.rm = T)
guns2$incarc_rate[guns2$incarc_rate < (qnt4[1] - H4)] <- caps4[1]
guns2$incarc_rate[guns2$incarc_rate > (qnt4[2] + H4)] <- caps4[2]

vioshallplot <- ggplot(mean_shallvio, aes(x = year, y = mean_shallvio$vio, color = shall, group = shall)) +
  geom_point() + geom_line() +
  ggtitle("Mean Violent Crimes Over Time")
  xlab("Year") + ylab("Mean Violent Crimes Per 100,000 people")
vioshallplot

Murshallplot <- ggplot(mean_shallmur, aes(x = year, y = mean_shallmur$mur, color = shall, group = shall)) +
  geom_point() + geom_line() +
  ggtitle("Mean Murder Rates Over Time") +
  xlab("Year") + ylab("Mean Murders Per 100,000 people")
Murshallplot

robshallplot <- ggplot(mean_shallrob, aes(x = year, y = mean_shallrob$rob, color = shall, group = shall)) +
  geom_point() + geom_line() +
  ggtitle("Mean Robberies Over Time") +
  xlab("Year") + ylab("Mean Robberies Per 100,000 people")
robshallplot

incarshallplot <- ggplot(mean_shallincar, aes(x = year, y = mean_shallincar$incar_rate, color = shall, group = shall)) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Mean Incarcerated Per 100,000 people")
incarshallplot

##OUtliers Removed

mean_vio2 <- group_by(guns2,year) %>%
  summarise(vio=mean(vio,na.rm = TRUE))

mean_mur2 <- group_by(guns2,year) %>%
  summarise(mur=mean(mur,na.rm = TRUE))

mean_rob2 <- group_by(guns2,year) %>%
  summarise(rob=mean(rob,na.rm = TRUE))

mean_incarc_rate2 <- group_by(guns2,year) %>%
  summarise(incarc_rate=mean(incarc_rate,na.rm = TRUE))

vioplot2 <- ggplot(mean_vio2, aes(x = year, y = mean_vio2$vio)) +
  geom_point(color="red") + geom_line(color="red") +
  xlab("Year") + ylab("Mean Violent Crimes per 100,000 People")
vioplot2

murplot2 <- ggplot(mean_mur2, aes(x = year, y = mean_mur2$mur)) +
  geom_point(color="blue") + geom_line(color="blue") +
  xlab("Year") + ylab("Mean Murders per 100,000 People")
murplot2

robplot2 <- ggplot(mean_rob2, aes(x = year, y = mean_rob2$rob)) +
  geom_point(color="dark green") + geom_line(color="dark green") +
  xlab("Year") + ylab("Mean Robberies per 100,000 People")
robplot2

incarc_rateplot2 <- ggplot(mean_incarc_rate2, aes(x = year, y = mean_incarc_rate2$incarc_rate)) +
  geom_point(color="orange") + geom_line(color="orange") +
  xlab("Year") + ylab("Mean Incarcerated Per 100,000 people")
incarc_rateplot2

mean_shallvio2 <- group_by(guns2,shall,year) %>%
  summarise(vio=mean(vio,na.rm = TRUE))

mean_shallmur2 <- group_by(guns2,shall,year) %>%
  summarise(mur=mean(mur,na.rm = TRUE))

mean_shallrob2 <- group_by(guns2,shall,year) %>%
  summarise(rob=mean(rob,na.rm = TRUE))

mean_shallincar2<- group_by(guns2,shall,year) %>%
  summarise(incarc_rate=mean(incarc_rate,na.rm = TRUE))

vioshallplot2 <- ggplot(mean_shallvio2, aes(x = year, y = mean_shallvio2$vio, color = shall, group = shall)) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Mean Violent Crimes Per 100,000 people")
vioshallplot2

Murshallplot2 <- ggplot(mean_shallmur2, aes(x = year, y = mean_shallmur2$mur, color = shall, group = shall)) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Mean Murders Per 100,000 people")
Murshallplot2

robshallplot2 <- ggplot(mean_shallrob2, aes(x = year, y = mean_shallrob2$rob, color = shall, group = shall)) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Mean Robberies Per 100,000 people")
robshallplot2

incarshallplot2 <- ggplot(mean_shallincar2, aes(x = year, y = mean_shallincar2$incarc_rate, color = shall, group = shall)) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Mean Incarcerated Per 100,000 people")
incarshallplot2

corgun <- round(cor(guns),2)

meltcorgun <- melt(corgun)

ggplot(data = meltcorgun, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                     midpoint = 0, limit = c(-1,1), space = "Lab", 
                     name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))
vioshallbox <- boxplot(vio~shall, data = guns, main = "Violent Crime With/Without",
                  xlab="Shall", ylab="Violent Crime Rate Per 100,000")
vioshallbox

## Demographic Variable Analysis

vioincarplot2 <- ggplot(guns, aes(x = incarc_rate, y = vio)) +
  geom_point(color="gold") + 
  xlab("Incarceration Rate Per 100,000 People") + ylab("Violent Crimes per 100,000 People") +
  ggtitle("Violent Crime Vs. Incarceration Rate")
vioincarplot2

viodensplot2 <- ggplot(guns, aes(x = density, y = vio)) +
  geom_point(color="brown") + 
  xlab("Population Density") + ylab("Violent Crimes per 100,000 People") +
  ggtitle("Violent Crime Vs. Population Density")
viodensplot2

viopopplot2 <- ggplot(guns, aes(x = pop, y = vio)) +
  geom_point(color="sky blue") + 
  xlab("Population") + ylab("Violent Crimes per 100,000 People") +
  ggtitle("Violent Crime Vs. Population")
viopopplot2

viobplot2 <- ggplot(guns, aes(x = pb1064, y = vio)) +
  geom_point(color="grey") + 
  xlab("Percentage of Black Persons In Population") + ylab("Violent Crimes per 100,000 People") +
  ggtitle("Violent Crime Vs. Black Population Percentage")
viobplot2

viowplot2 <- ggplot(guns, aes(x = pw1064, y = vio)) +
  geom_point(color="black") + 
  xlab("Percentage of White Persons In Population") + ylab("Violent Crimes per 100,000 People") +
  ggtitle("Violent Crime Vs. White Population Percentage")
viowplot2

guns$lavginc <- log(guns$avginc)

vioincplot2 <- ggplot(guns, aes(x = avginc, y = vio)) +
  geom_point(color="pink") + 
  xlab("Log of Average Income") + ylab("Violent Crimes per 100,000 People") +
  ggtitle("Violent Crime Vs. Log Average Income")
vioincplot2

viomplot2 <- ggplot(guns, aes(x = lavginc, y = vio)) +
  geom_point(color="orange") + 
  xlab("Percentage of Young Males Among Populations") + ylab("Violent Crimes per 100,000 People") +
  ggtitle("Violent Crime Vs. Percentage of Young Males Among Populations")
viomplot2

## Hausman Test

guns$year_state <- paste(guns$year,guns$stateid,sep = "_")

guns_plm <- pdata.frame(guns, index = c("stateid","year_state"))

murfe <- plm(mur~incarc_rate+avginc+density+shall, data=guns_plm, model = "within")

murre <- plm(mur~incarc_rate+avginc+density+shall, data=guns_plm, model = "random")

phtest(gunsfe, gunsre, method = "aux")

viofe <- plm(vio~incarc_rate+avginc+density+shall, data=guns_plm, model = "within")

viore <- plm(vio~incarc_rate+avginc+density+shall, data=guns_plm, model = "random")

phtest(viofe, viore, method = "aux")

robfe <- plm(rob~incarc_rate+avginc+density+shall, data=guns_plm, model = "within")

robre <- plm(rob~incarc_rate+avginc+density+shall, data=guns_plm, model = "random")

phtest(robfe, robre, method = "aux")

## Adding in pw1064

murwfe <- plm(mur~incarc_rate+avginc+density+shall+pw1064, data=guns_plm, model = "within")

murwre <- plm(mur~incarc_rate+avginc+density+shall+pw1064, data=guns_plm, model = "random")

phtest(murwfe, murwre, method = "aux")

viowfe <- plm(vio~incarc_rate+avginc+density+shall+pw1064, data=guns_plm, model = "within")

viowre <- plm(vio~incarc_rate+avginc+density+shall+pw1064, data=guns_plm, model = "random")

phtest(viowfe, viowre, method = "aux")

robwfe <- plm(rob~incarc_rate+avginc+density+shall+pw1064, data=guns_plm, model = "within")

robwre <- plm(rob~incarc_rate+avginc+density+shall+pw1064, data=guns_plm, model = "random")

phtest(robwfe, robwre, method = "aux")

##Adding in pb1064

murbfe <- plm(mur~incarc_rate+avginc+density+shall+pb1064, data=guns_plm, model = "within")

murbre <- plm(mur~incarc_rate+avginc+density+shall+pb1064, data=guns_plm, model = "random")

phtest(murbfe, murbre, method = "aux")

viobfe <- plm(vio~incarc_rate+avginc+density+shall+pb1064, data=guns_plm, model = "within")

viobre <- plm(vio~incarc_rate+avginc+density+shall+pb1064, data=guns_plm, model = "random")

phtest(viobfe, viobre, method = "aux")

robbfe <- plm(rob~incarc_rate+avginc+density+shall+pb1064, data=guns_plm, model = "within")

robbre <- plm(rob~incarc_rate+avginc+density+shall+pb1064, data=guns_plm, model = "random")

phtest(robbfe, robbre, method = "aux")


summary(robbfe)
summary(murbre)
summary(viobfe)

summary(robwfe)
Summary(murwfe)
summary(viowfe)

summary(viofe)
summary(murre)
summary(robre)

murfe4 <- plm(mur~pop+shall+ldensity+incarc_rate+lavginc+pm1029+pw1064, data = guns_plm, model = "within")

guns_plm$ldensity <- log(guns_plm$density)

coeftest(murfe4, vcov. = vcovHC, type = "HC1")

viofe4 <- plm(vio~pop+shall+ldensity+incarc_rate+lavginc+pm1029+pw1064, data = guns_plm, model = "within")

coeftest(viofe4, vcov. = vcovHC, type = "HC1")

robfe4 <- plm(rob~pop+shall+ldensity+incarc_rate+lavginc+pm1029+pw1064, data = guns_plm, model = "within")

coeftest(robfe4, vcov. = vcovHC, type = "HC1")

## Interaction Variables

murfe4 <- plm(mur~pop+shall+ldensity+incarc_rate+lavginc+pm1029+pw1064, data = guns_plm, model = "within")

murhtfe4 <- plm(mur~pop+shall+ldensity+incarc_rate+lavginc+pm1029+pw1064, data = guns_plm, random.method = "ht", model = "within")

coeftest(murhtfe4, vcov. = vcovHC, type = "HC1")
summary(murhtfe4)

guns_plm$ldensity <- log(guns_plm$density)

coeftest(murfe4, vcov. = vcovHC, type = "HC1")
summary(murfe4)

murerrorfe <- residuals(murfe4)

plot(murerrorfe)

murerrorfe <- ggplot(guns_plm, aes(x = year, y = mean_shallmur$mur, color = shall, group = shall)) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Mean Murders Per 100,000 people")
Murshallplot


viofe4 <- plm(vio~pop+shall+ldensity+incarc_rate+lavginc+pm1029+pw1064+I(pm1029*pw1064), data = guns_plm, model = "within")

coeftest(viofe4, vcov. = vcovHC, type = "HC1")

robfe4 <- plm(rob~pop+shall+ldensity+incarc_rate+lavginc+pm1029+pw1064+I(pm1029*pw1064), data = guns_plm, model = "within")

coeftest(robfe4, vcov. = vcovHC, type = "HC1")

summary(robfe4)
margins(murfe4)

robfe5 <- plm(rob~pop+shall+ldensity+incarc_rate+lavginc+pm1029+pw1064, data = guns_plm, model = "within")
summary(robfe5)

guns_plm$vio <- guns_plm$vio

guns_plm$d77 <- guns_plm$year == 1977
guns_plm$d78 <- guns_plm$year == 1978
guns_plm$d79 <- guns_plm$year == 1979
guns_plm$d80 <- guns_plm$year == 1980
guns_plm$d81 <- guns_plm$year == 1981
guns_plm$d82 <- guns_plm$year == 1982
guns_plm$d83 <- guns_plm$year == 1983
guns_plm$d84 <- guns_plm$year == 1984
guns_plm$d85 <- guns_plm$year == 1985
guns_plm$d86 <- guns_plm$year == 1986
guns_plm$d87 <- guns_plm$year == 1987
guns_plm$d88 <- guns_plm$year == 1988
guns_plm$d89 <- guns_plm$year == 1989
guns_plm$d90 <- guns_plm$year == 1990
guns_plm$d91 <- guns_plm$year == 1991
guns_plm$d92 <- guns_plm$year == 1992
guns_plm$d93 <- guns_plm$year == 1993
guns_plm$d94 <- guns_plm$year == 1994
guns_plm$d95 <- guns_plm$year == 1995
guns_plm$d96 <- guns_plm$year == 1996
guns_plm$d97 <- guns_plm$year == 1997
guns_plm$d98 <- guns_plm$year == 1998
guns_plm$d99 <- guns_plm$year == 1999

summary(viowfe)

viowfe <- plm(lvio~incarc_rate+avginc+pop+density+shall+pw1064+pm1029, data=guns_plm, model = "within")
summary(viowfe)
viowfe2 <- plm(lvio~incarc_rate+pop+density+shall+pw1064+pm1029, data=guns_plm, model = "within")
summary(viowfe2)
viowfe3 <- plm(lvio~pop+density+shall+pw1064+pm1029, data=guns_plm, model = "within")
summary(viowfe3)
viowfe4 <- plm(lvio~pop+shall+pw1064+pm1029, data=guns_plm, model = "within")
summary(viowfe4)

res1 <- viowfe$residuals
n1 <- nrow(viowfe$model)
w1 <- rep(1,n1)

ll1 <- -.5 * (sum(log(w1)) - n1 * (log(2*pi) + 1 - log(n1) + log(sum(w1 * res1^2))))
length(viowfe$coefficients)

BIC_viowfe <- -2 * ll1 + log(n1) * 8

##Time Fixed Effects

res2 <- viowtimefe$residuals
n2 <- nrow(viowtimefe$model)
w2 <- rep(1,n2)



ll2 <- -.5 * (sum(log(w2)) - n2 * (log(2*pi) + 1 - log(n2) + log(sum(w2 * res2^2))))
length(viowtimefe$coefficients)

BIC_viowtimefe <- -2 * ll2 + log(n2) * 8

## BIC Rob

robwfe <- plm(lrob~incarc_rate+avginc+pop+density+shall+pw1064+pm1029, data=guns_plm, model = "within")
robwtimefe <- plm(lrob~incarc_rate+avginc+density+pop+shall+pw1064+pm1029+d77+d78+d79+d80+d81+d82+d83+d84+d85+d86+d87+d88+d89+d90+d91+d92+d93+d94+d95+d96+d97+d98+d99, data=guns_plm, model = "within")

res1 <- robwfe$residuals
n1 <- nrow(robwfe$model)
w1 <- rep(1,n1)

ll1 <- -.5 * (sum(log(w1)) - n1 * (log(2*pi) + 1 - log(n1) + log(sum(w1 * res1^2))))
length(robwfe$coefficients)

BIC_robwfe <- -2 * ll1 + log(n1) * 6

## BIC Rob Time

res2 <- robwtimefe$residuals
n2 <- nrow(robwtimefe$model)
w2 <- rep(1,n2)

ll2 <- -.5 * (sum(log(w2)) - n2 * (log(2*pi) + 1 - log(n2) + log(sum(w2 * res2^2))))
length(robwtimefe$coefficients)

BIC_robtimewfe <- -2 * ll2 + log(n2) * 8

