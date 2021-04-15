#' search_pmc
#'
#' @param query a string of character
#' @param resultType a string of character
#' @param synonym a string of character
#' @param callback a string of character
#' @param waitTime a number
#'
#' @examples
#' search_pmc("pinkeye")
#'
#' @import jsonlite tidyr
#' @export
#'

search_pmc <- function(query,
                      resultType = "lite",
                      synonym = "FALSE",
                      callback ="",
                      waitTime = 0.1) {
  endpoint = "search"
  pageSize = "1000"
  result_json <- getJson(query = query,
                          endpoint = endpoint,
                          resultType = resultType,
                          synonym = synonym,
                          cursorMark = "*",
                          pageSize = pageSize,
                          callback =callback,
                          waitTime = waitTime)

  totalCount <- result_json$hitCount
  nloops <- ceiling(totalCount / as.numeric(pageSize))
  nextCursorMark <- result_json$nextCursorMark

  resultList <- result_json$resultList$result
  resultLong <- do.call(rbind, lapply(resultList, function(x){
     return(data.frame(id= x["id"], varname = names(x), varvalue = as.character(x), stringsAsFactors = F))
  }))
  metaDataLong <- resultLong
  for(i in 1:nloops){
    print(paste0(i,"/",nloops))
    if(i <= 1) next
    result_json <- getJson(query = query,
                           endpoint = endpoint,
                           resultType = resultType,
                           synonym = synonym,
                           cursorMark = nextCursorMark,
                           pageSize = pageSize,
                           callback =callback,
                           waitTime = waitTime)

    nextCursorMark <- result_json$nextCursorMark

    resultList <- result_json$resultList$result
    resultLong <- do.call(rbind, lapply(resultList, function(x){
      return(data.frame(id= x["id"], varname = names(x), varvalue = as.character(x), stringsAsFactors = F))
    }))

    metaDataLong <- rbind(metaDataLong, resultLong)
  }

  metaData <- tidyr::spread(metaDataLong, key="varname", value = "varvalue")

  return(metaData)
}


