####################################################
# Author: Nikhil Singh
# Date: December 02, 2019
# Details: Coded Two layer neural net from scratch
# Input: 3-input X-OR gate
# Output: 3-input X-OR gate
####################################################

# Removing all pre-declared variables
rm(list=ls())

# Declaring input matrix -----
X = matrix(c(0,0,0,0,0,1,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,1,1,1)
           , nrow = 8
           , ncol = 3)

X
# Declaring output matrix -----
y = matrix(c(0,1,1,0,1,0,0,1)
           , nrow = 8
           , ncol = 1)

y
# Initializing basic arguments -----
epoch = 500
learning_rate = 0.1
ip_neurons = ncol(X)
hidden_neurons = ncol(X)
op_neurons = ncol(y)

set.seed(123)

# Function to initialize weights and biases -----
initialize_weight_bias <- function(X, ip_neurons, hidden_neurons
                                   , op_neurons){
  ## for hidden layer
  hidden_weights <- matrix(rnorm(ip_neurons*hidden_neurons)
                           , ip_neurons
                           , hidden_neurons)
  hidden_bias <- matrix(rep(runif(hidden_neurons), nrow(X))
                        , nrow = nrow(X)
                        , byrow = FALSE)
  
  ## for output layer
  output_weights <- matrix(rnorm(hidden_neurons*op_neurons)
                           , nrow = hidden_neurons
                           , ncol = op_neurons)
  
  output_bias <- matrix(rep(runif(op_neurons), nrow(X))
                        , nrow = nrow(X)
                        , byrow = FALSE)
  
  list_returning_wt_bias <- list("hidden_weights" = hidden_weights
                                 , "hidden_bias" = hidden_bias
                                 , "output_weights" = output_weights
                                 , "output_bias" = output_bias)
  
  return(list_returning_wt_bias)
}

# sigmoid function
sigmoid <- function(x){
  return(1/(1+exp(-x)))
}

# sigmoid derivative
sigmoid_derivative <- function(x){
  return(x*(1-x))
}

# forward propogation
feed_forward <- function(X, list_weights_bias){
  
  hidden_weights = list_weights_bias$hidden_weights
  hidden_bias = list_weights_bias$hidden_bias
  output_weights = list_weights_bias$output_weights
  output_bias = list_weights_bias$output_bias
  
  hidden_layer_processing = (X %*% hidden_weights) + hidden_bias
  hidden_layer_activation = sigmoid(hidden_layer_processing)
  
  output_layer_processing = (hidden_layer_activation %*% 
                               output_weights) + output_bias
  output_layer_activation = sigmoid(output_layer_processing)
  
  list_activation <- list("hidden_layer_activation" = hidden_layer_activation
                          , "output_layer_activation" = output_layer_activation)
  
  return(list_activation)
}

calculate_error <- function(y, output_layer_activation){
  error = y - output_layer_activation
  return(error)
}

# backpropogation
backpropogation <- function(error
                            , list_activation
                            , list_weights_bias){
  
  hidden_layer_activation = list_activation$hidden_layer_activation
  output_layer_activation = list_activation$output_layer_activation
  
  hidden_weights = list_weights_bias$hidden_weights
  hidden_bias = list_weights_bias$hidden_bias
  output_weights = list_weights_bias$output_weights
  output_bias = list_weights_bias$output_bias
  
  # moving in reverse direction now
  
  ## calculating gradients
  slope_output_layer <- sigmoid_derivative(output_layer_activation)
  slope_hidden_layer <- sigmoid_derivative(hidden_layer_activation)
  
  der_output_layer <- error*slope_output_layer 
  hidden_layer_error <- der_output_layer %*% t(output_weights)
  
  der_hidden_layer <- hidden_layer_error*slope_hidden_layer
  
  ## updating weights & bias on all levels
  output_weights_updated <- 
    output_weights + learning_rate*(t(hidden_layer_activation)%*% der_output_layer)
  output_bias_updated <- output_bias + learning_rate*(rowSums(der_output_layer))
  
  hidden_weights_updated <- 
    hidden_weights + learning_rate * (t(X) %*% der_hidden_layer)
  hidden_bias_updated <- hidden_bias + learning_rate*(rowSums(der_hidden_layer))
  
  list_upd_wts_bias <- list("hidden_weights" = hidden_weights_updated
                            , "hidden_bias" = hidden_bias_updated
                            , "output_weights" = output_weights_updated
                            , "output_bias" = output_bias_updated)

  return(list_upd_wts_bias)
}

list_weights_bias <- 
  initialize_weight_bias(X, ip_neurons, hidden_neurons, op_neurons)

for(i in 1:epoch){
  list_activation <- feed_forward(X, list_weights_bias)
  error <- calculate_error(y, list_activation$output_layer_activation)
  list_weights_bias <- backpropogation(error, list_activation, list_weights_bias)
}

accuracy = paste0(mean(ifelse(y==ifelse(list_activation$output_layer_activation>0.5
                               ,1
                               ,0)
                     ,1
                     ,0))*100, "%")

cat("Output from NN: ",list_activation$output_layer_activation)
cat("Accuracy = ",accuracy)