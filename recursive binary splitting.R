#Recursive binary splitting, for decision (regression) trees
#Designed based off of description of Decision Trees in:
### Introduction to Statistical Learning by James, Witten, Hastie, Tibshirani
### pg 306-307.
###Purpose is for improving understanding of regression trees, rather than for improved efficiency of current implementations. 

library(magrittr)
library(ggplot2)
library(dplyr)

#generating simple regression/classification problem
x1 <- rnorm(50, mean = 2, sd = 1)
x2 <- rnorm(50, mean = 5, sd = 1)
x <- c(x1, x2)
y1 <- rnorm(50, mean = 2, sd = 0.5)
y2 <- rnorm(50, mean = 5, sd = 0.7)
y <- c(y1, y2)
data_m <- matrix(c(y, x), ncol = 2)
plot(data_m)

#values are ordered, then an identity matrix for each successive grouping is supplied for every variable.
region_separater <- function(data.matrix){
  #separate into two regions
  # data.matrix <- data_m
  number_variables <- ncol(data.matrix)
  number_obs <- nrow(data.matrix)
  #id matrix to be rearranged
  m2 <- matrix(0, #square matrix 
               ncol = number_obs, 
               nrow = number_obs)
  m2[upper.tri(m2, diag = TRUE)] <- 1
  #empty list of region IDs
  list_regions_id <- list()
  for(i in 1:number_variables){
    data_vec <- data.matrix[,i]
    data_vec_ordered <- data_vec %>% rank()
    m3 <- m2[data_vec_ordered,]
    list_regions_id[[i]] <- m3
  }
  return(list_regions_id)
}

#produces rss for each region, given a temporary matrix
###first column of temp matrix are values, second is 1/0 or in a region or not
rss_producer <- function(temp_matrix){
  #gives the matrix of data values and region IDs
  # temp_matrix <- temp_ex
  r1 <- temp_matrix[temp_matrix[,2] > 0,]
  if(!is.matrix(r1)){ r1 <- t(r1)}
  u_r1 <- rep(r1[,1] %>% mean(), nrow(r1))
  r1_df <- cbind(r1, u_r1)
  r1_df_a <- (r1_df[,1] - r1_df[,3])^2 %>% sum()
  
  r2 <- temp_matrix[temp_matrix[,2] < 1,]
  if(!is.matrix(r2)){ r2 <- t(r2)}
  u_r2 <- rep(r2[,1] %>% mean(), nrow(r2))
  r2_df <- cbind(r2, u_r2)
  r2_df_a <- (r2_df[,1] - r2_df[,3])^2 %>% sum()
  
  rss_value <- r1_df_a + r2_df_a
  return(rss_value)
}

rss_tree <- function(data.vector, region_id_list, data.column.i){
  #find the mean of r1, and mean of r2
  rss_vector <- vector()
  for(i in 1:length(data.vector)){
    temp_matrix_i <- cbind(data.vector, region_id_list[[data.column.i]][,i])
    rss_i <- rss_producer(temp_matrix = temp_matrix_i)
    rss_vector[i] <- rss_i
  }
  return(rss_vector)#RSS for every cut separate half-planes of regions. 
}

optimal_regions_byVar <- function(data.matrix){
  #vars = a vector of parameters to be minized.
  #data.matrix = matrix of data, where columns are predictor variables (j)
  # data.matrix <- test_matrix
  number_variables <- ncol(data.matrix)
  number_obs <- nrow(data.matrix)
  region_id_list_i <- region_separater(data.matrix)
  #finding the rss estimates for every variable
  combined_lists <- list()
  for(i in 1:number_variables){
    rss_values <- rss_tree(data.vector = data.matrix[,i],
                           region_id_list = region_id_list_i,
                           data.column.i = i)
    rss_min <- which(rss_values == min(rss_values))#RSS Index
    if(length(rss_min) > 1){rss_min <- rss_min[1]}
    optimal_region_id <- region_id_list_i[[i]][,rss_min]#Region IDs for optimal
    # if(ncol(optimal_region_id)>1){optimal_region_id <- optimal_region_id[,1]}
    data_vector_i <- data.matrix[,i]
    optimal_data_bin <- cbind(data_vector_i, 
                              optimal_region_id)
    combined_opt <- list(RSS_min_value = rss_values[rss_min], 
                         RSS_min_value_index = rss_min,
                         Optimal_Data_Bin = optimal_data_bin,
                         Optimal_variable_index = NA)
    combined_lists[[i]] <- combined_opt
  }
  return(combined_lists)
}

best_region_selection <- function(combined_lists_i, no_vars = number_variables){
  rss_vector <- numeric()
  for(i in 1:no_vars){
    rss_i <- combined_lists_i[[i]][[1]]
    rss_vector[i] <- rss_i
  }
  rss_min <- which(rss_vector == rss_vector %>% min())
  finale <- combined_lists_i[[rss_min]]
  finale["Optimal_variable_index"] <- rss_min
  return(finale)
}

binary_split <- function(data.matrix){
  if(class(data.matrix) != "matrix"){ 
    stop("Input variable must be a matrix object.")}
  regions_byVar <- optimal_regions_byVar(data.matrix)
  best_region <- best_region_selection(combined_lists_i = regions_byVar, 
                                       no_vars = ncol(data.matrix))
  #formatting result
  list_result <- list()
  list_result[["data_matrix"]] <- data.matrix
  list_result[["optimal_variable_index"]] <- best_region$Optimal_variable_index
  list_result[["optimal_region_ids"]] <- best_region$Optimal_Data_Bin[,"optimal_region_id"]
  return(list_result)
}

#testing functionality with iris dataset on each component. 
iris_regions <- region_separater(iris2)
iris_rss_tree <- rss_tree(iris2[,1], region_id_list = iris_regions, 1)
iris_opt <- optimal_regions_byVar(iris2)
iris_best_regions <- best_region_selection(combined_lists_i = iris_opt, no_vars = 2)

#final compiled function
iris_tree <- binary_split(data.matrix = iris[,1:2])
plot(iris_tree[[1]], 
     pch = iris_tree[[3]])

data_tree <- binary_split(data.matrix = data_m)
plot(data_tree[[1]],
     pch = data_tree[[3]])


