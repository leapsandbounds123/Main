library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(hrbrthemes)

#Unemployment rate data set-up
unemployed <- read_csv("Monthly_Unemployment.csv")
colnames(unemployed)[5] <- "Province_State"
colnames(unemployed)[37:41] <- c("2020-05-01", "2020-04-01", "2020-03-01", "2020-02-01", "2020-01-01")
unemployed <- unemployed %>%
  select(Province_State, "2020-05-01", "2020-04-01", "2020-03-01", "2020-02-01", "2020-01-01") 
unemployed$Province_State <- factor(unemployed$Province_State)
unemployed_long <- unemployed %>% 
  gather(month, unemployed_num, "2020-05-01":"2020-01-01", factor_key=TRUE)
unemployed_long <- mutate(unemployed_long, month = (as.Date(month)))

#Confirmed cases data set-up
confirmed <- read_csv("https://raw.githubusercontent.com/leapsandbounds123/Main/master/.github/workflows/1_county_level_confirmed_cases(1).csv")
JHU <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
JHU_long <- JHU %>%
  select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Combined_Key) %>%
  filter(!(Province_State %in% 
             c("American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands"))) %>%
  pivot_longer(-(`Province_State`:Long_), 
               names_to = "Date", 
               values_to = "confirmed") %>%
  mutate(Date = (mdy(Date))) %>%
  group_by(Province_State, month=floor_date(Date, "month")) %>%
  summarize(confirmed=sum(confirmed))

#Population density set-up
popden <- read_csv("popdensity.csv")
popden <- select(popden, States, Pop_density, POPESTIMATE2019)
colnames(popden)[1] <- "Province_State"

#Join the above three datasets 
JHU_long %>%
  inner_join(unemployed_long, by = c("Province_State", "month")) %>%
  print() -> df
df1 <- group_by(df, Province_State) %>% 
  summarize(confirmed = sum(confirmed), unemploy = sum(unemployed_num))
df1 <- left_join(df1, popden, by = "Province_State")
df1 <- na.omit(df1)
df1 <- mutate(df1, unemployed_rt = unemploy/POPESTIMATE2019)

#Top-10 confirmed cases states  
top_10 <- df %>% 
  group_by(Province_State) %>%
  summarize(total = sum(confirmed)) %>%
  arrange(desc(total)) %>%
  slice(1:10)

#Data visualization
df %>%
  filter(Province_State %in% top_10$Province_State) %>%
  ggplot() +
  geom_bar(aes(fill=Province_State, y=unemployed_num*2, x=month), 
           position="stack", stat="identity") +
#  geom_bar(mapping = aes(x = month, y = unemployed_num), stat = "identity") +
  geom_line(mapping = aes(x = month, y = confirmed, color = Province_State)) +
  scale_y_continuous(
    name = "Confirmed Cases",
    sec.axis = sec_axis(~./2, name="Unemployment population")
  ) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8))

#Top 10 Confirmed Cases vs. Unemployment Population
df1 %>%
  arrange(desc(confirmed)) %>%
  slice(1:10) %>%
#  filter(Province_State %in% c("California", "New York")) %>%
  ggplot() +
  geom_bar(mapping = aes(x = Province_State, y = unemploy*3), stat = "identity") +
  geom_point(mapping = aes(x = Province_State, y = confirmed), col = "forestgreen") + 
  scale_y_continuous(
    name = "Confirmed Cases",
    sec.axis = sec_axis(~./3, name = "Unemployment population")
  ) +
  theme(
    axis.title.y = element_text(color = "forestgreen", size=13),
    axis.title.y.right = element_text(color = "black", size=13)
  ) +
  ggtitle("Top 10 Confirmed Cases vs. Unemployment Population")

#Top 10 Confirmed Cases vs. Population Density
df1 %>%
  arrange(desc(confirmed)) %>%
  slice(1:10) %>%
  #  filter(Province_State %in% c("California", "New York")) %>%
  ggplot() +
  geom_bar(mapping = aes(x = Province_State, y = Pop_density*10000), 
           stat = "identity") +
  geom_point(mapping = aes(x = Province_State, y = confirmed), 
             col = "forestgreen") + 
  scale_y_continuous(
    name = "Confirmed Cases",
    sec.axis = sec_axis(~./10000, name = "Population Density")
  ) +
  theme(
    axis.title.y = element_text(color = "forestgreen", size=13),
    axis.title.y.right = element_text(color = "black", size=13)
  ) +
  ggtitle("Top 10 Confirmed Cases vs. Population Density")

#Linear regression with features: unemployment population and population density
plot(log(df1$unemploy), log(df1$confirmed))
plot(log(df1$Pop_density), log(df1$confirmed))
y <- log(df1$confirmed)
x1 <- log(df1$unemploy)
x2 <- log(df1$Pop_density)
lm1 <- lm(y ~ x1 + x2)
summary(lm1) #Coefs are positive meaning that there is a positive association 
             #between those two features and the confirmed cases.
             #Moreover, p-values for two features are both significant.
df_clean <- df1 %>%
  slice(-c(9, 33))
plot(predict(lm1, data.frame(x1 = df_clean$unemploy, x2 = df_clean$Pop_density)),
     df_clean$confirmed, 
     pch = 20, col = "lightblue", 
     xlab = "Fitted confirmed cases", ylab = "Observed confirmed cases",
     main = "Fitted vs. Observed (after excluding the outliers)")
plot(lm1)

#The following codes are trying to visualize and analyze the relationship 
#between air quality (CO density) and the confirmed cases in CA and NY.
#However, the observations in those two states are too small to be informative.
#We decide not to include this air quality feature into our project.
CONY <- read_csv("CONY.csv")
View(CONY)
colnames(CONY)[5] <- "CO"
CONY <- group_by(CONY, COUNTY) %>% summarize(CO = mean(CO))

CO <- read_csv("CO.csv")
colnames(CO)[5] <- "CO"
CO <- group_by(CO, COUNTY) %>% summarize(CO = mean(CO))
View(CO)

NY <- JHU %>%
  select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key) %>%
  filter(Province_State %in% "New York") %>%
  pivot_longer(-(`Admin2`:Long_), 
               names_to = "Date", 
               values_to = "confirmed") %>%
  mutate(Date = (mdy(Date))) %>%
  group_by(Admin2) %>%
  summarize(confirmed=sum(confirmed))
colnames(NY)[1] <- "COUNTY"
CONY <- left_join(CONY, NY, by = "COUNTY")
View(CONY)
plot(CONY$CO, log(CONY$confirmed + 1))

CA <- JHU %>%
  select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key) %>%
  filter(Province_State %in% "California") %>%
  pivot_longer(-(`Admin2`:Long_), 
               names_to = "Date", 
               values_to = "confirmed") %>%
  mutate(Date = (mdy(Date))) %>%
  group_by(Admin2) %>%
  summarize(confirmed=sum(confirmed))
colnames(CA)[1] <- "COUNTY"
CO <- left_join(CO, CA, by = "COUNTY")

#Since the data for CA is too small, we combine it with the data of New York State.
a <- rbind(CONY, CO)
CO <- full_join(CONY, CO, by = "COUNTY", copy = T)
plot(CO$CO, log(CONY$confirmed + 1))









