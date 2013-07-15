require( "stats", quietly = TRUE )

user_func <- function() {
  sample(1:100, 10)
}

fib <- function( n ) {
  if( n < 2 )
    return( 1 )
  return( fib( n - 1 ) + fib( n - 2 ) )
}

SomeDF <- function () {
  return(data.frame(abc=1:5, foo=6:10, bar=11:15))
}
