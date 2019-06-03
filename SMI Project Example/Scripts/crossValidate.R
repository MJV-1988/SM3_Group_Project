
crossValidate <- function( formula, data, k ) {
  
  folds <- crossv_kfold( data, k )
  models <- map( folds$train, ~lm( formula, data = . ) )
  get_pred <- function( model, test_data ){ return( add_predictions( as.data.frame(test_data), model ) ) }
  results <- map2_df( models, folds$test, get_pred, .id = "Fold" )
  MSE <- results %>%
    group_by(Fold) %>%
    summarise( MSE = mean( ( popularity - pred )^2 ), n=n() )
  CV <- sum( MSE$MSE * MSE$n ) / sum( MSE$n )
  return(CV)
  
}