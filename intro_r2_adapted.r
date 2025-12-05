# Introduction to R - R Ladies Vienna (3.12.2025)
# by Patricia Puchhammer

# ------------------------------------------------------------
# 0) Setup
# ------------------------------------------------------------
install.packages("tidyverse")

library(dplyr)
library(ggplot2)

data(mtcars) # from datasets

# ------------------------------------------------------------
# 1) Warm-up: Explore dataset
# ------------------------------------------------------------
head(mtcars)
str(mtcars)
summary(mtcars)
?mtcars

# ------------------------------------------------------------
# 2) Introduction to dplyr
# ------------------------------------------------------------

# 2.1 Select variables

mtcars %>%
  select(mpg, cyl, hp)


select(mtcars, mpg, cyl, hp)

mtcars %>%
  select(mpg:wt)

mtcars %>%
  select(-disp, -hp)

mtcars %>% select(mpg:wt, -cyl)

# 2.2 Filter rows
mtcars %>%
  filter(cyl == 4, mpg > 25, wt > 3)

# 2.3 Create new variables
mtcars %>%
  mutate(hp_per_liter = hp / disp, one = 1, wt = log(wt))

head(mtcars)
mtcars_fact <- mtcars %>%
  mutate(am = factor(am, 
                     levels = c(0,1), 
                     labels = c("automatic", "manual")),
         vs = factor(vs, 
                     levels = c(0,1), 
                     labels = c("V-shaped", "straight")))
head(mtcars_fact)
str(mtcars_fact)

# 2.4 Sort data
mtcars %>%
  arrange(desc(cyl), mpg)

mtcars %>%
  arrange(desc(mpg))

# 2.5 Group and summarize
mtcars %>%
  group_by(cyl)

mtcars %>%
  group_by(cyl, vs) %>%
  summarise(
    mean_mpg = mean(mpg),
    sd_mpg   = sd(mpg),
    n        = n()
  )

# Mini-Exercise Solution (5-10min): 
# Compare fuel efficiency [mpg] by all combinations of transmission type [am] and engine type [vs] and 
# sort the groups by the mean of mpg. Show min, max, mean, sd, and n per group.

mtcars %>%
  mutate(am = factor(am, 
                     levels = c(0,1), 
                     labels = c("automatic", "manual")),
         vs = factor(vs, 
                     levels = c(0,1), 
                     labels = c("V-shaped", "straight"))) %>%
  group_by(am, vs) %>%
  summarise(mean_mpg = mean(mpg),
            min_mpg = min(mpg),
            max_mpg = max(mpg),
            sd_mpg = sd(mpg),
            n = n()) %>%
  arrange(mean_mpg)





# ------------------------------------------------------------
# 3) Introduction to ggplot2
# ------------------------------------------------------------

mtcars_fact <- mtcars %>%
  mutate(am = factor(am, 
                     levels = c(0,1), 
                     labels = c("automatic", "manual")),
         vs = factor(vs, 
                     levels = c(0,1), 
                     labels = c("V-shaped", "straight")))


# 3.1 Scatterplot
ggplot(data, mapping = aes())

ggplot(mtcars_fact, aes(x = wt, y = mpg))

ggplot(mtcars_fact, aes(x = wt, y = mpg)) +
  geom_point()

# 3.2 Scatterplot with color and size
ggplot(mtcars_fact, aes(wt, mpg, color = gear, size = wt)) +
  geom_point(shape = 15, alpha = 0.3) +
  scale_color_gradient(low = "blue", high = "red")
  #scale_color_distiller(palette = "Reds")

# 3.3 Boxplot
ggplot(mtcars_fact, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot()

ggplot(mtcars_fact, aes(x = cyl, y = mpg, group = cyl, fill = factor(cyl))) +
  geom_boxplot()

# 3.4 Bar chart
ggplot(mtcars_fact, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar()

# 3.5 Faceting
ggplot(mtcars_fact, aes(wt, mpg)) +
  geom_point() +
  facet_wrap(vars(cyl))


# 3.6 Labels + themes
ggplot(mtcars_fact, aes(wt, mpg, col = vs, shape = vs, fill = vs)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", 
              alpha = 0.15, 
              linewidth = 0.75,
              se = TRUE) +
  labs(
    title = "Fuel Efficiency vs. Weight",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon"
  ) +
  scale_color_brewer(name = "Engine", palette = "Dark2") + 
  scale_fill_brewer(name = "Engine", palette = "Dark2") + 
  scale_shape_discrete(name = "Engine") + 
  theme_void() +
  theme()

? scale_color_brewer 
? theme_minimal



# ------------------------------------------------------------
# 4) Combined dplyr + ggplot2 example
# ------------------------------------------------------------
summary_data <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg))

# Bar chart of average mpg by cylinders
ggplot(summary_data, aes(factor(cyl), mean_mpg)) +
  geom_col() +
  labs(
    title = "Average MPG by Number of Cylinders",
    x = "Cylinders",
    y = "Average MPG"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 5) Exercises
# ------------------------------------------------------------
# - Filter high-consumption cars [mpg] and plot them
# - Explore relationship between weight [wt] and horsepower [hp]
# - Faceted plots by gear [gear] or cylinder [cyl]
# - Scatterplot with color by displacement [disp]
# - Histogram (geom_histogram) of weight [wt] by transmission [am]


mt_filt <- mtcars %>% 
  filter(mpg >= 22.80)

ggplot(mtcars, aes(mpg, group = cyl, fill = factor(cyl))) +
  geom_histogram(#position = "identity"
    )+
  facet_wrap(~cyl)


summary(mtcars)








# ------------------------------------------------------------
# 5) Functions, conditions and loops
# ------------------------------------------------------------

## ----if_else------------------------------------------------------------------
if (3 > 5) {
  print("That's true!") 
} else {
  print("That's false")
}

## ---- for loops---------------------------------------------------------------
for (i in 1:3){
  print(paste("count:", i))
}

## ----functions----------------------------------------------------------------
my_ggplot <- function(df, color) {
  
  if("mpg" %in% colnames(df) & "hp" %in% colnames(df)){
    ggplot(df) + 
      geom_point(aes(x = mpg, 
                     y = hp), 
                 col = color)
  } else {
    print("The input df does not have mpg and hp as column names.")
  }
}

my_ggplot(mtcars, "lightblue")
my_ggplot(iris, "green")  # iris from package datasets
