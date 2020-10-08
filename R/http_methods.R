#' getAPIlink
#' Return European api links with parameters
#'
#' @param baseUrl a string of characters, default is "https://www.ebi.ac.uk/europepmc/webservices/rest/"
#' @param endpoint a string of characters, valid endpoint of NCBI Utilities, e.g. "search",
#' @param query a string of characters. Entrez text query.
#' @param resultType a string of characters.
#' @param synonym a string of characters.
#' @param cursorMark a string of characters.
#' @param pageSize a string of characters of an integer 0-1000
#' @param format a string of characters.
#' @param email a string of characters.
#' @param callback a string of characters.
#'
#' @import stats
#' @return link
#' @export
#'
#' @examples
#' getAPIlink(query = "pinkeye")
#'
getAPIlink <- function(query,
                       baseUrl = "https://www.ebi.ac.uk/europepmc/webservices/rest/",
                       endpoint = "search",
                       resultType = "lite",
                       synonym = "FALSE",
                       cursorMark = "*",
                       pageSize = "25",
                       format = "json",
                       email = "shihikoo@gmail.com",
                       callback ="all"
) {
  baseUrl <- paste0(baseUrl, endpoint, "?")

  query <- ifelse(query != "", paste0("query=",query),NA)
  resultType <- ifelse(resultType != "", paste0("resultType=",resultType),NA)
  synonym <- ifelse(synonym != "", paste0("synonym=",synonym),NA)
  cursorMark <- ifelse(cursorMark != "", paste0("cursorMark=",cursorMark),NA)
  pageSize <- ifelse(pageSize != "",  paste0("pageSize=",pageSize),NA)
  format <- ifelse(format != "", paste0("format=",format),NA)
  email <- ifelse(email != "", paste0("email=",email),NA)
  callback <- ifelse(callback != "", paste0("callback=",callback),NA)

  paras <- paste0(stats::na.omit(c(query, resultType,synonym, cursorMark, pageSize,format, email, callback)), collapse = "&")

  link <- paste0(baseUrl, paras)
  return(link)
}

#' getContentByPostLink
#'
#' @param link a string of characters
#' @param waitTime a number. Waiting of the program
#'
#' @return a string of characters of the returned content
#' @export
#'
#' @examples
#' link <- getAPIlink(query = "pinkeye")
#' content <- getContentByPostLink(link, 0.3)
#'
#' @import httr stringr
#'
getContentByPostLink <- function(link, waitTime = 0.3) {
  link <- gsub("/search?","/searchPOST?",link, fixed = T)
  content <- NULL
  attampt <- 0
  while (is.null(content) & attampt < 5) {
    tryCatch({
      baselink <- stringr::str_split(link,"[?]")[[1]][1]
      paras <- stringr::str_split(link,"[?]")[[1]][2]
      # paralist <- stringr::str_split(stringr::str_split(paras,"[&]")[[1]], "=")
      # fineParalist <- lapply(paralist, function(x) x[[2]])
      # names(fineParalist) <- lapply(paralist, function(x) x[[1]])
      # print("Send Post request")
      # print(fineParalist)
      Sys.sleep(waitTime*attampt)
      r0 <- httr::POST(as.character(baselink), body = paras)
      # print("Receive Post request")
      content <- httr::content(r0, "text", encoding = "UTF-8")
    }, error = function(e) {
      print(e)
    })
    attampt <- attampt + 1
  }
  return(content)
}

#' getContentByGetLink
#'
#' @param link a string of characters
#' @param waitTime a number. Waiting of the program
#'
#' @return a string of characters of the returned content
#' @export
#'
#' @examples
#' link <- "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=pinkeye"
#' content <- getContentByGetLink(link, 0.3)
#'
#' @import httr
#'
getContentByGetLink <- function(link, waitTime = 0.3) {
  # httr::set_config(httr::config(http_version = 0))
  content <- NULL
  attampt <- 0

  link <- gsub("#","%23", gsub("\"","%22", gsub(" ", "+", link)))

  while (is.null(content) & attampt < 10) {
    tryCatch({
      # print("Send GET request")
      iwaitTime <- waitTime*(attampt+1)
      Sys.sleep(iwaitTime)
      r0 <- httr::GET(link)
      # print("Receive GET request")
      content <- httr::content(r0, "text", encoding = "UTF-8")
      # print(content)
    }, error = function(e) {
      print(e)
    })
    attampt <- attampt + 1
  }
  return(content)
}

#' getJson
#'
#' @param endpoint a string of characters, valid endpoint of NCBI Utilities, e.g. "search",
#' @param query a string of characters. Entrez text query.
#' @param resultType a string of characters.
#' @param synonym a string of characters.
#' @param cursorMark a string of characters.
#' @param pageSize a string of characters.
#' @param callback a string of characters.
#' @param waitTime a number to wait in each call

#'
#' @return a XMLInternalDocument
#' @export
#'
#' @import jsonlite
#'
#' @examples
#' getJson(query = "pinkeye")
#'
#'
getJson <-
  function(query,
           endpoint = "search",
           resultType = "lite",
           synonym = "FALSE",
           cursorMark = "*",
           pageSize = "1",
           callback ="",
           waitTime = 0.1
           ) {
    format <- "json"
    link <- getAPIlink(query = query, endpoint = endpoint, resultType = resultType,  synonym = synonym, cursorMark =cursorMark, pageSize =pageSize, format = format, callback = callback)
    # print(link)

    if(nchar(link) > 500) content <- getContentByPostLink(link, waitTime) else content <-  getContentByGetLink(link, waitTime)
    if(is.null(content)) return(NULL)
  # this part is because the returned json is a valid json wraped in all()
    content <- gsub("^all[(]", "", content)
    content <- gsub("[)]$", "", content)

    if(jsonlite::validate(content) == FALSE) return(NULL)

    result_json <- jsonlite::parse_json(content)

    return(result_json)
  }

#' getDoc
#'
#' @param endpoint a string of characters, valid endpoint of NCBI Utilities, e.g. "search",
#' @param query a string of characters. Entrez text query.
#' @param resultType a string of characters.
#' @param synonym a string of characters.
#' @param cursorMark a string of characters.
#' @param pageSize a string of characters.
#' @param waitTime a number.
#' @param callback a string of characters.

#' @return a XMLInternalDocument
#' @export
#'
#' @import xml2
#'
#' @examples
#' getDoc(query = "pinkeye")
#'
getDoc <-
  function( query,
            endpoint = "search",
            resultType = "lite",
            synonym = "FALSE",
            cursorMark = "*",
            pageSize = "1",
            callback ="",
            waitTime = 0.1) {
    format <- "XML"
    link <- getAPIlink(query = query, endpoint = endpoint, resultType = resultType,  synonym = synonym, cursorMark =cursorMark, pageSize =pageSize, format = format, callback = callback)
    # The waiting time to retrive data from the API. Default is set to 0.4 to ensure less than 3 API calling.
    # print(link)
    if(nchar(link) > 500) content <- getContentByPostLink(link, waitTime) else content <- getContentByGetLink(link, waitTime)

    if(is.null(content)) return(NULL)

    doc <- xml2::read_xml(content, encoding = "UTF-8", useInternalNodes = TRUE, trim = FALSE)

    return(doc)
  }

#' retriveXmlNodeValuefromDoc
#'
#' @param doc the parsed XML file
#' @param nodePosition the node position of the xml file
#'
#' @return the values of the node
#' @export
#' @examples  doc <- getDoc("pinkeye")
#' retriveXmlNodeValuefromDoc(doc, "//pmid")
#'
#' @import xml2
#'
retriveXmlNodeValuefromDoc <- function(doc, nodePosition) {
  nodes <- xml2::xml_find_all(doc, nodePosition)
  if (length(nodes) == 0) return(NA)
  results <- gsub("\t"," ",gsub("\n","",sapply(nodes, xml2::xml_text), fixed = T), fixed = T)
  results[which(results == "NA")] <- NA

  return(results)
}

#' cleanDoi
#'
#' @param doi a doi string
#'
#' @return the values of the node
#' @export
#' @examples
#' cleanDoi("https://doi.org/10.1212/01.wnl.0000260060.60870.89")
#'

cleanDoi <-function(doi ){
  doi <- tolower(doi)
  doi <- gsub("doi:", "", doi)
  doi <- gsub("https://", "", doi)
  doi <- gsub("http://", "", doi)
  doi <- gsub("dx.doi.org/","", doi)
  doi <- gsub("doi.org/","", doi)
  return(doi)
}

