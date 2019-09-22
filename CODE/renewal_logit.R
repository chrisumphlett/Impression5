library(tidyverse)
library(lubridate)

# utilize the initial data set up from the renewal rate code, but set cutoff date to Oct 2016
dat2 <- read_csv("DATA\\newest_renewal_data.csv") %>%
  rename(member_id = `Constituent ID`,
         joined_date = `Current Joined On Date`,
         renewal_date = `History All Renewed On Dates`,
         curr_expiration_date = `Current Expires On`,
         curr_member_category = `Current Category`,
         member_category = `History Membership Category`,
         expiration_date = `History Expires On`) %>%
  mutate(renewal_date = as.Date(renewal_date, format = "%m/%d/%Y"),
         curr_expiration_date = as.Date(curr_expiration_date, format = "%m/%d/%Y"),
         expiration_date = as.Date(expiration_date, format = "%m/%d/%Y"),
         member_category = as.factor(member_category),
         joined_date = as.Date(joined_date, format = "%m/%d/%Y")) %>%
  select(member_id, joined_date, renewal_date, curr_expiration_date, expiration_date, member_category) %>%
  arrange(member_id, expiration_date)

cutoff_date <- floor_date(Sys.Date() - months(0), "month") - 1

renewals2<-dat2 %>%
  group_by(member_id) %>%
  mutate(next_renewal = lead(renewal_date, order_by=member_id),
         future_expir = ifelse(expiration_date > Sys.Date(),1,0),
         member_is_numeric = ifelse(!grepl("\\D", member_id),1,0),
         CountInst = n(), 
         membership_year = row_number(),
         renewal_timing = as.numeric(difftime(next_renewal, expiration_date , units = "days")),
         renewed = if_else(is.na(renewal_timing) | renewal_timing > 183, 0, 1),
         member_category2 = as.factor((if_else(expiration_date >= "2018-10-31",
                                   paste0(member_category, " - 2018"),
                                   as.character(member_category))))) %>%
  filter(member_is_numeric==1,
         !is.na(member_id),
         !member_category %in% c("Corporate", "SED", "SED - 2018"),
         expiration_date >= '2016-10-31' & expiration_date <= cutoff_date) %>%
  select(-member_is_numeric) %>%
  arrange(member_id, renewal_date)

# data on when there were travelling exhibits
expiration_date <- seq.Date(from = as.Date("2016-11-01"), by = "month", length.out = 36) - 1
exhibits <- as.data.frame(expiration_date) %>%
  mutate(hotwheels = as.factor(if_else(expiration_date <= "2016-12-31", 1, 0)),
         curiousg = as.factor(if_else(expiration_date > "2016-12-31" & expiration_date <= "2017-05-31",
                                      1, 0)),
         clifford = as.factor(if_else(expiration_date > "2017-09-30" & expiration_date <= "2017-12-31",
                                      1, 0)),
         trains = as.factor(if_else(expiration_date >= "2018-02-28" & expiration_date <= "2018-03-31"
                                    | expiration_date >= "2019-02-28" & expiration_date <= "2019-03-31",
                                      1, 0)),
         dinos = as.factor(if_else(expiration_date >= "2018-07-31" & expiration_date <= "2018-12-31",
                                      1, 0)),
         flow = as.factor(if_else(expiration_date >= "2017-07-31", 1, 0))#,
         # smash = as.factor(if_else(expiration_date >= "2019-08-31", 1, 0))
#          iceage = as.factor(if_else(expiration_date >= "2018-09-31" & expiration_date <= "2018-12-31",
#                                    1, 0)),
)

renewals3 <- renewals2 %>%
  left_join(exhibits) %>%
  mutate(exhibit_f = as.factor(if_else(as.numeric(hotwheels) + as.numeric(curiousg) + as.numeric(clifford) + as.numeric(trains) + as.numeric(dinos) >= 1,
                             1,0)),
         month_fctr = as.factor(as.character(month(expiration_date))),
         yyyy = year(expiration_date),
         first_renewal_f = as.factor(if_else(membership_year == 1, 1, 0))) %>%
  filter(expiration_date <= "2019-02-28")

options(scipen = 999)
mylogit <- glm(renewed ~ month_fctr + yyyy + 
                 #flow + # "glm.fit: algorithm did not converge" - if want to include flow may need to convert to numeric and look at correlations 
                 hotwheels + curiousg + clifford + trains + dinos +
                 # exhibit_f +
                 # member_category +
                 member_category2 +
                 membership_year +
                 first_renewal_f
                , data = renewals3, family = "binomial")
summary(mylogit)

oddsratios<-exp(cbind(OR = coef(mylogit), confint(mylogit)))
oddsratios


#won't work, can't get some of the columns to convert to factors and this requires fctr or num
#plot(effects::allEffects(mylogit))
