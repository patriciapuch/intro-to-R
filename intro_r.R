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
z <- y + 1

## ----atomic_vectors-----------------------------------------------------------
pyth_triple <- c(x, y, z)
?c
# indexing with boolean (condition)
pyth_triple[pyth_triple >= 4]

## ----factors------------------------------------------------------------------
x1 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1)
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
y1
sort(y1)

## ----matrices-----------------------------------------------------------------
mat <- matrix(0, nrow = 5, ncol = 5)
diag(mat) <- c(seq(1, 2, by = 1), seq(4, 2, by = -1))
mat

## ----data_frames--------------------------------------------------------------
v_char <- c("Lucy", "Stefanie", "Florian", "Michelle", "Jacky", "Anna", "Pia", "Markus")
v_num <- c(18, 20, 22, 30, 29, 50, 38, 25)
v_bool <- c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)
df <- data.frame(name = v_char,
                 age = v_num,
                 female_names = v_bool)
df$name[df$female_names] #indexing
df$age[df$age > 25]

## ----lists--------------------------------------------------------------------
my_list <- list(matrix = mat, 
                vector = pyth_triple, 
                fact = y1, 
                bool = TRUE)

## ----installation, eval = FALSE-----------------------------------------------
# install.packages("tidyverse")

## ----load_packages------------------------------------------------------------
library(tidyverse)

## ----help_packages------------------------------------------------------------
help(package = "tidyverse")

## ----data_import--------------------------------------------------------------
?read.table

path1 <- file.path("iris", "iris_1.csv")
path2 <- file.path("iris", "iris_2.csv")
path3 <- file.path("iris", "iris_3.txt")
path4 <- file.path("iris", "iris_4.txt")

iris1 <- read.csv(path1, header = F)
iris2 <- read.csv2(path2, dec = ",")
iris3 <- read.table(path3, header = T)
iris4 <- read.table(path4, header = T)

head(iris1) # look at data

## ----data_proc----------------------------------------------------------------
iris1 <- read.csv(path1, header = T)
?iris

# selecting and naming columns
iris_new <- iris1[,2:6]
colnames(iris_new) <- c("sepal_l", "sepal_w", "petal_l", "petal_w", "species")

# missing values
iris_new[5,2] <- NA
mean(iris_new[,2])
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
rm(iris_new)
load(file = file.path("iris", "iris_mod.Rdata"))

## ----basic_plots--------------------------------------------------------------
plot(iris_new$sepal_l, iris_new$petal_l, 
     type = "p", 
     col = iris_new$species,
     xlab = "Sepal Length", ylab = "Petal Length")
points(iris_new$sepal_l[100], iris_new$petal_l[100], 
       pch = 19,
       col = iris_new$species[100])
legend("bottomright", 
       fill = c("black", "red", "green"), 
       legend = levels(iris_new$species))
abline(v = iris_new$sepal_l[100], lty = 2)
abline(h = iris_new$petal_l[100], lty = 3)
abline(coef = c(0.6, 0.75), col = "green")

## -----------------------------------------------------------------------------
pairs(iris_new[,1:4], 
      col = iris_new$species, 
      pch = as.numeric(iris_new$species),
      labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"))

## ----plots_export-------------------------------------------------------------


## ----ggplots------------------------------------------------------------------
library(ggplot2)

ggplot(data = iris_new,
       aes(x = total_w, 
           y = total_l, 
           groups = species,
           col = species)) + 
  geom_point(aes(pch = species), 
             size = 2, alpha = 0.5) +
  geom_smooth(aes(fill = species), 
              method = "lm", 
              alpha = 0.2, linewidth = 0.75) +
  xlab("Total Width") + ylab("Total Length") +
  labs(col = "Species", 
       pch = "Species", 
       fill = "Species") +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_light()

? scale_color_brewer 
? theme_light

# Now its time to make your own plots with ggplot2 :)

? geom_histogram # aes(fill= _, x or y = _)
? geom_boxplot # aes(fill= _, x or y = _)
? geom_qq # aes(sample=_)
? geom_text  # aes(x=_, y=_, label=_)

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

