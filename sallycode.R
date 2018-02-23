library(dplyr)
library(tidyr)
library(mice)
library(Amelia)
library(readr)
library(janitor)
library(lubridate)
library(ggplot2)
library(car)
library(multcomp)
library(ez)
# import and clean
sallydata <- read_csv("C:/Users/Branly Mclanbry/Desktop/sally.csv") %>% janitor::clean_names()
#cleaning and imputing shit
dat <- sallydata %>%
  separate(id, into = c('id', 'nothing'), sep = 1) %>%
  mice(m = 10, maxit = 10, method = 'pmm', seed = 1619) %>%
  mice::complete()
#making some new variables.
dat <- dat %>% 
  mutate(time = case_when(session == 1 ~ ymd(20170403),
                          session == 2 ~ ymd(20170405),
                          session == 3 ~ ymd(20170407),
                          session == 4 ~ ymd(20170410),
                          session == 5 ~ ymd(20170412),
                          session == 6 ~ ymd(20170414),
                          session == 7 ~ ymd(20170417),
                          session == 8 ~ ymd(20170419),
                          session == 9 ~ ymd(20170421),
                          session == 10 ~ ymd(20170424),
                          session == 11 ~ ymd(20170425),
                          session == 12 ~ ymd(20170401)), 
    recent_exercise = case_when(x12hoursago == 1 ~ "yes",
                                x12hoursago == 2 ~ "no"),
    time_factor = as.factor(session),
    time_test = ts(time),
    avg_bdnf = mean(avgbdnf),
    upper = avg_bdnf + (avgbdnf * .15),
    lower = avg_bdnf - (avgbdnf * .15),
    intensity = as.factor(intensity),
    sex = as.factor(sex),
    avgbdnf = round(avgbdnf,2),
    id = as.factor(id))

#hypothesis 1
ggplot(dat, aes(x = time, y = avgbdnf)) + geom_line(aes(color = id))
#talk to Josue?
# does each individual fall within 15% of mean?
avg_bdnf <- mean(dat$avgbdnf)
upper <- avg_bdnf + (avg_bdnf * .15)
lower <- avg_bdnf - (avg_bdnf * .15)

dat %>%
  mutate(
    within_range = case_when(avgbdnf <= upper & avgbdnf >= lower ~ 1,
                             TRUE ~ 0)) %>%
  summarize(total_n = n(), within_15 = sum(within_range)) # gives you total observations, and how many within 15% of mean

# hypothesis 2
t.test(avgbdnf~recent_exercise,dat)

# does BMI and BDNF correlate? Nope.
cor.test(dat$bmi,dat$avgbdnf)

#does mentrual cycle and BMI correlate? Yup. 
cor.test(dat$bmi,dat$menstrul)

#correlation between gender and bfnd averages over time?
t.test(avgbdnf~sex,dat)

sexytime<-ezBoot(dat, avgbdnf, id, within = .(time_factor,sex) , between = sex, resample_within = FALSE)
p = ezPlot2(sexytime, x = time_factor, split = sex)

#first a plot of the time levels
  ezPlot2(
    preds = sexytime
    , x = time_factor)

#now a table of the difference
  ezPlot2(
    preds = sexytime
    , diff = time_factor
      , do_plot = FALSE
    )

#first a plot of the  sex levels
  ezPlot2(
    preds = sexytime
    , x = sex)

#now a table for the difference between the first two levels
  ezPlot2(
    preds = sexytime
    , diff = sex
    , do_plot = FALSE)

  
# Look at the Hour X Condition interaction

  #first a plot of all cells
  ezPlot2(
    preds = sexytime
    , x = sex
    , split = time_factor)

#Let's look at sex and time. 
  ezPlot2(
    preds = sexytime
    , x = sex
    , diff = time_factor,
    do_plot = FALSE)

#compare the first levels of sex on their time effect
  ezPlot2(
    preds = sexytime
    , diff = .(time_factor,sex)
    , do_plot = FALSE)

#does intensity predict avgbdnf? F Yea brother. 
aov <- aov(avgbdnf~intensity,dat)
Anova(aov, type = 3)
TunakTunakTunDADADA <- glht(aov,linfct=mcp(intensity = "Tukey"))
summary(TunakTunakTunDADADA)


##########################

library(ggplot2)

# format data properly
runs <- bdnf.t %>%
  select(id, session, run_1, run_2) %>%
  filter(!is.na(run_1) & !is.na(run_2)) %>%
  gather(key = run_time, value = run_bdnf, run_1, run_2)

# line graph run 1 vs run 2
runs %>%
  group_by(session, run_time) %>%
  summarize(avg_bdnf = mean(run_bdnf)) %>%
  ggplot(aes(x = as.factor(session), y = avg_bdnf, color = run_time, group = run_time)) +
  geom_line()

# bar chart for BDNF by exercise intensity
bdnf.t %>%
  mutate_at(.vars = vars(intensity), .funs = factor) %>%
  group_by(intensity) %>%
  summarize(avgbdnfm = mean(avgbdnf)) %>%
  filter(intensity != 'none') %>%
  ggplot(aes(x = intensity, y = avgbdnfm, fill = intensity)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin = avgbdnfm - sd(avgbdnfm), ymax = avgbdnfm + sd(avgbdnfm)), width = 0.2)


# bar chart for type of exercise
bdnf.t %>%
  mutate_at(.vars = vars(type), .funs = factor) %>%
  mutate(new_type = case_when(type == 'resistance' ~ 'anaerobic',
                              type == 'hitt' ~ 'anaerobic',
                              type == 'none' ~ 'none',
                              TRUE ~ 'aerobic')) %>%
  group_by(new_type) %>%
  summarize(avgbdnfm = mean(avgbdnf)) %>%
  ggplot(aes(x = new_type, y = avgbdnfm, fill = new_type)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin = avgbdnfm - sd(avgbdnfm), ymax = avgbdnfm + sd(avgbdnfm)), width = 0.2)


write_csv(dat, "sallydat.csv")

