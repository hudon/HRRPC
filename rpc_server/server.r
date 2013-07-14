library("rjson")

#load in any extra sources
source_files <- Sys.getenv( "R_SERVER_SOURCE" )
if( source_files != "" ) {
  source_files <- strsplit( source_files, ":" )[[1]]
  for( s in source_files )
    source( s )
}

hrrpcVer <- "0.0.0"

#rpc is an R object corresponding to the parsed JSON-RPC call
#returns: a JSON string with the results or error
do.rpc <- function( rpc ) {
  makeErrorResult <- function (code, msg) {
    return(list(HRRPC=hrrpcVer, error=list(code=code, message=msg)))
  }

  isRPCProtocol <- function (rpc) {
    if (!is.list(rpc)) return(FALSE)
    if (is.null(rpc$method)) return(FALSE)
    if (is.null(rpc$params)) return(FALSE)
    return(TRUE)
  }

  ## The given object must provide a specific interface
  if (!isRPCProtocol(rpc)) {
    rpc_result <- makeErrorResult(-2, 'Invalid RPC format. Provide a "method" property with a string value and a "params" property')

  } else {

    rpc$params <- as.list(rpc$params)
    result <- try(do.call(rpc$method, rpc$params), silent=TRUE)

    if(class(result) == "try-error") {
      rpc_result <- makeErrorResult(-3, "Internal RPC error")

    } else {
      #RPC call suceeded
      rpc_result <- list(HRRPC=hrrpcVer, result=result)
    }
  }

  #return the JSON string
  ret <- toJSON(rpc_result)
  ret <- paste(ret, "\n", sep="")
  return(ret)
}

while (TRUE) {
  line <- readLines(n=1)
  if (length(line) == 0) break

  tryCatch({
    rpc <- fromJSON(line)
    ret <- do.rpc(rpc)
    cat(ret)
  }, error=function(e) {
    cat(paste('{"HRRPC":"',
              hrrpcVer,
              '", "error": {"code": -1, "message": "Invalid JSON"}}\n', sep=""))
  })
}

#must quit here - otherwise, we get dropped into an R shell
q()
