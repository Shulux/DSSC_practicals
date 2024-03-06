x <- c(5.4314, 10341, 33141.4314)

signif(x, digits = 3)



X <- data.frame(var1 = rnorm(50, 5),
                var2 = rnorm(50),
                var3 = rnorm(50),
                var4 = rnorm(50),
                var5 = as.factor(rep(c("Low", "High"), 25)))

str(x)

library(dplyr)

X2 <- X |>

  filter(var1 > 5) |>

  group_by(var5) |>

  summarise(average = mean(var2)) |>

  arrange(desc(average))


games <- data.frame(Home = c("Arsenal", "Brentford", "Burnley", "Chelsea"),
                    Away = c("Everton", "Leeds", "Newcastle", "Watford"),
                    HomeGoals = c(5, 1, 1, 2),
                    AwayGoals = c(1, 2, 2, 1))


rm(list = ls())

data("baseball", package = "plyr")

??baseball

str(baseball)

dim(baseball)

for (name in names(baseball)) {
  print(paste(name, sum(is.na(baseball$name))))
}

na.counts <- colSums(is.na(baseball))

max(na.counts)


baseball |>
  filter(id == "dennyje01" & year == 1891)

no.bat <- baseball |>
  group_by(id) |>
  summarise(ab_total = sum(ab)) |>
  filter(ab_total == 0)

length(no.bat$id)

n_distinct(baseball |>
  filter(year <= 1879 & year >= 1870) |>
  group_by(id) |>
  select(id))

decades_bots = seq(1850, 2020, 10)

for (decade in decades_bots) {
  print(paste(decade, n_distinct(baseball |>
               filter(year <= (decade+9) & year >= decade) |>
               group_by(id) |>
               select(id))))
}


team.counts <- baseball |>
  group_by(id) |>
  summarise(team_count = n())

min(team.counts$team_count)
max(team.counts$team_count)

nrow(team.counts |>
  filter(team_count >= 10))


mean.hr.id.year <- baseball |>
  group_by(year) |>
  summarise(mean_home_runs = mean(hr))


mean.hr.id.year[which.max(mean.hr.id.year$mean_home_runs),]
