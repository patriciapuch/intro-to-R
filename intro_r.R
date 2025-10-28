## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

## ----basic_commands-----------------------------------------------------------
3 + 4
3 - 7
2 * 20
4/2
10^3
sqrt(10)

## ----comparisons--------------------------------------------------------------
3 < 5
3 > 5
3 <= 5 
3 >= 5
3 == 5     # Attention: double equal sign
3 != 5

## ----assignment---------------------------------------------------------------
x <- 3
y = 4
5 -> z # works, but not recommended

## ----atomic_vectors-----------------------------------------------------------
pyth_triple <- c(x, y, z)
?c
# indexing with boolean (condition)
pyth_triple[pyth_triple >= 4]

## ----sequences----------------------------------------------------------------
seq_incr <- seq(1, 5, by = 1)
seq_decr <- 10^seq(4, 0, by = -1)

## ----factors------------------------------------------------------------------
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1)
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
y2 <- factor(x2, levels = month_levels)
sort(y1)

## ----matrices-----------------------------------------------------------------
mat <- matrix(0, nrow = 5, ncol = 5)
diag(mat) <- c(seq(1, 2, by = 1), seq(4, 2, by = -1))
mat

## ----data_frames--------------------------------------------------------------
v_char <- paste0("char", seq(1, 10, 1))
v_num <- seq(0.01, 0.1, length.out = 10)
v_bool <- c(rep(TRUE, 4), rep(FALSE, 6))
df <- data.frame(col1 = v_char,
                 col2 = v_num,
                 col3 = v_bool)
df$col1[df$col3] #indexing
df$col2[df$col2 > 0.05]

## ----lists--------------------------------------------------------------------
my_list <- list(matrix = mat, 
                vector = pyth_triple, 
                fact = y2, 
                bool = TRUE)

## ----installation, eval = FALSE-----------------------------------------------
# install.packages("tidyverse")

## ----load_packages------------------------------------------------------------
library(tidyverse)
citation("tidyverse")

## ----help_packages------------------------------------------------------------
help(package = "tidyverse")

## ----data_import--------------------------------------------------------------
?read.table

# getwd()
# setwd("C:/Users/puchhammer/Documents/Konferenzen Workshops/R_Ladies2025_IntroToR/intro-to-R-main/")

path1 <- file.path("iris", "iris_1.csv")
path2 <- file.path("iris", "iris_2.csv")
path3 <- file.path("iris", "iris_3.txt")
path4 <- file.path("iris", "iris_4.txt")

iris1 <- read.csv(path1, header = T)
iris2 <- read.csv2(path2, dec = ".")
iris3 <- read.table(path3, header = T)
iris4 <- read.table(path4)

head(iris1) # look at data



## ----data_proc----------------------------------------------------------------
?iris

# selecting and naming columns
iris_new <- iris1[,2:6]
colnames(iris_new) <- c("sepal_l", "sepal_w", "petal_l", "petal_w", "species")

# missing values
iris_new[5,2] <- NA
mean(iris_new[,2], na.rm = TRUE)
iris_new <- na.omit(iris_new)
mean(iris_new[,2])

# modifying columns
iris_new$total_l <- iris_new$sepal_l + iris_new$petal_l
iris_new$total_w <- iris_new$sepal_w + iris_new$petal_w
iris_new$species <- factor(iris_new$species, levels = c( "setosa", "versicolor", "virginica"))
head(iris_new)

## ----data_exp-----------------------------------------------------------------
?write.table

write.csv(iris_new, file = file.path("iris", "iris_mod.csv"))

save(iris_new, file = file.path("iris", "iris_mod.Rdata"))
load(file = file.path("iris", "iris_mod.Rdata"))

## ----basic_plots--------------------------------------------------------------
plot(iris_new$sepal_l, iris_new$petal_l, 
     type = "p", 
     lty = 1, 
     col = iris_new$species,
     xlab = "Sepal Length", ylab = "Petal Length")
points(iris_new$sepal_l[100], iris_new$petal_l[100], 
       pch = 19,
       col = iris_new$species[100])
legend("bottomright", fill = c("black", "red", "green"), legend = levels(iris_new$species))
abline(v = iris_new$sepal_l[100], lty = 2)
abline(h = iris_new$petal_l[100], lty = 3)
abline(coef = c(0.6, 0.75), col = "green")

## ----histogram----------------------------------------------------------------
hist(iris_new$petal_w, 
     breaks = 10,
     main = "Example for histogram",
     xlab = "Petal Width",
     col = "lightblue", lty = 2)
abline(v = 0.7, lty = 1, col = "red")

## -----------------------------------------------------------------------------
pairs(iris_new[,1:2], 
      col = iris_new$species, 
      pch = as.numeric(iris_new$species),
      labels = c("Sepal Length", "Sepal Width"))

## ----plots_export-------------------------------------------------------------


## ----ggplots------------------------------------------------------------------
library(ggplot2)
ggplot(data = iris_new,
       aes(x = total_w, 
           y = total_l, 
           groups = species,
           col = species)) + 
  geom_point() +
  geom_line(aes(linetype = species)) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Total Width") + ylab("Total Length") +
  labs(col = "Species", linetype = "Species") +
  scale_color_brewer(palette = "Set1") + 
  theme_light()

? scale_color_brewer 
? theme_light

## ----if_else------------------------------------------------------------------
if (3 > 5) {
  print("That's true!") 
  } else {
  print("That's false")
  }

## ----loops--------------------------------------------------------------------
for (i in 1:3){
  print(paste("count:", i))
}

j <- 0
while (j < 4){
  print(j)
  j <- j + 1
}

## ----functions----------------------------------------------------------------
my_plot <- function(x, y, color, linetype) {
  plot(x, y, type = "l", col = color, lty = linetype)
}

my_plot(seq(1,10,1), seq(1,10,1)^2, 5, 2)

