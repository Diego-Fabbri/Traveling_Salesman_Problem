#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Traveling Salesman Problem")

# Import lpSolve package
library(lpSolve)

#Import required packages (ompr)
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)
library(ggplot2)

#Set problem size
n <- 5

#Set City names (for plot)
city_names <- c("Rome","Milan","Naples","Cosenza", "Turin")

#Set coordinates
set.seed(1234)

xPos <- vector(mode="numeric", length = n)
yPos <- vector(mode="numeric", length = n)

for(i in 1:n) {
  xPos[i] <- sample(seq(0, 1, 0.01), 1) * 100 + 1
  yPos[i] <- sample(seq(0, 1, 0.01), 1) * 100 + 1
}

#Set costs (euclidean distances)
c <- array(dim = c(n, n))

for (i in 1:n) {
  for (j in 1:n) {
    c[i, j] <- sqrt((xPos[j] - xPos[i]) ^ 2 + (yPos[j] - yPos[i]) ^ 2)
    
  }
}

#Plot cities
cities <- data.frame(id = 1:n, x = xPos, y = yPos)

ggplot(cities, aes(x= xPos, y= yPos, label=city_names)) + 
  geom_point(size = 2,alpha = 0.6) +
  theme_bw()+
  geom_text(aes(label = city_names), hjust=0.6, vjust=2)

#Build Model
Model <- MIPModel() %>%
  add_variable(x[i,j], i = 1:n, j = 1:n, type = "binary") %>% #define variables
  set_bounds(x[i, i], ub = 0, i = 1:n) %>% #define x variables' bounds
  add_variable(u[i], i = 1:n, lb = 1, ub = n) %>%
  set_objective(sum_expr(c[i, j] * x[i, j], i = 1:n, j = 1:n), "min") %>% #define objective
  add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>% #define constraints
  add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
  add_constraint(u[i] >= 2, i = 2:n) %>% 
  add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n) %>% 
  solve_model(with_ROI(solver = "symphony", verbosity = 1))

#Model summary
##Status
print(paste("Model status is:", Model$status))

##Objective Function
print(paste("Objective value:", objective_value(Model)))

#Variables
for (a in 1:n) {
  for (b in 1:n) {
    tmp_x <- get_solution(Model, x[i, j]) %>%
      filter(variable == "x", i == a, j == b) %>%
      select(value)
    
    
    if (tmp_x != 0) {
      print(paste("--->x[", a, ",", b , "] =", tmp_x))
    }
  }
}

