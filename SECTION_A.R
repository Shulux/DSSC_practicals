## SECTION A

library("dplyr")

x <- c(5, 10, NA, 2)

mean(x)

mean(x, na.rm = TRUE)


X <- data.frame(var1 = c(rnorm(50, 5)),
                var2 = c(rnorm(50, 10)),
                var3 = c(rnorm(50)),
                var4 = c(rnorm(50)),
                var5 = as.factor(rep(c("Low", "High"), 25)))

X |>
  filter(var1 > 5) |>
  group_by(var5) |>
  summarise(average = mean(var2)) |>
  arrange(desc(average))

??tidyr::seperate


data("txhousing", package = "ggplot2")

str(txhousing)

??ggplot2::txhousing

txhousing |>
  arrange(desc(year))

city.counts <- txhousing |>
  group_by(city) |>
  summarise(count = n())

all(city.counts$count == 187)

txhousing |>
  filter(city == "Port Arthur" & year == 2007 & month == 6) |>
  select(sales)


txhousing.withmean <- txhousing |>
  mutate(mean = volume / sales)


# median > mean --> count.large + 1
# median < mean --> count.small + 1

count.mean.larger.median <-  0
count.mean.less.median <-  0

mean.median <- txhousing.withmean |>
  select(mean, median)


adj.mean.median <- mean.median |>
  mutate(larger_mean = ifelse(mean > median, 1, 0))

sum(adj.mean.median$larger_mean, na.rm = TRUE) / (sum(!is.na(adj.mean.median$larger_mean)))
## 99% have a larger mean


txhousing |>
  group_by(year) |>
  summarise(total_sales = sum(sales, na.rm = TRUE)) |>
  arrange(desc(total_sales))
