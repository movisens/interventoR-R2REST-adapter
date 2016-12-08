#' @import httr
#' @import logging
#' @import digest
#'
NULL

sendInterventorEMail_ <- function(message_content, studyXSId, probandXSID, studyApiKey, interventorUrl, sendInjection = sendByPostRequest){
  secretToken <- generateAPIPermissionToken(probandXSID, message_content, studyApiKey)
  logdebug(paste("Sending Message via Interventor", paste('probandXSID', probandXSID, sep=':'), paste('content', message_content, sep=':'), paste('token', secretToken, sep=':')), sep=' - ', logger = 'interventor_adapter')
  apiOperationAdr <- interventorMessengerAPIPath(studyXSId, probandXSID, message_content, secretToken, interventorUrl)
  result <- sendInjection(apiOperationAdr, message_content)
  if(result$status_code == 200){
    loginfo(paste('Message sent: ', paste('probandXSID', probandXSID, sep=':'), paste('content', message_content, sep=':')), sep=' - ')
    'success_sending'
  }
  else{
    probandID <- paste(paste('studyXSID', studyXSId, sep=':'), paste('probandXSID', probandXSID, sep=':'), sep='x')
    content <- paste('content', message_content, sep=':')
    details <- paste('Result code', result$status_code, sep=':')
    logerror(paste('Failed sending message:', paste(probandID, content, details, sep=' - ')))
    'error_sending'
  }
}

sendByPostRequest <- function(apiOperationAdr, message_content){
  logdebug(paste('Requesting interventor API operation: POST: ', apiOperationAdr), logger = 'interventor_adapter')
  reqHeaders <- add_headers('Content-Type'='text/plain;charset=UTF-8')
  req <- POST(url = apiOperationAdr, body = message_content, reqHeaders)
  result <- content(req, as = "text", encoding = "UTF-8")
  logdebug(paste('json result', result, sep=':'), logger = 'interventor_adapter')
  list(status_code = status_code(req), body = result)
}

interventorMessengerAPIPath <- function(studyXSId, probandXSID, content, token, interventorUrl){
  operationURL <- paste0(interventorUrl, conf.interventor.messagingAPI.sendMessagePath)
  debugFormatted <- tolower(conf.interventor.messagingAPI.debugging)  #The intervention server will only parse the lower cased string correctly
  apiCallParams = paste(paste('studyXSID', studyXSId, sep = '='), paste('probandXSID', probandXSID, sep = '='), paste('token', token, sep = '='), paste('debugging', debugFormatted, sep = '='), sep = '&')
  apiOperationCall <- paste(operationURL, apiCallParams, sep = '?')
  apiOperationCall
}


#' Send a message to a proband's medical attendant via e-mail
#'
#' @param message_content the content of the e-mail
#' @param studyXSId the xs-study id
#' @param probandXSID xs-proband id
#' @param studyApiKey the xs study api secret key
#' @param interventorUrl the interventor server's url
#' @export
#'
sendInterventorEMail <- function(message_content, studyXSId, probandXSID, studyApiKey, interventorUrl){
  sendInterventorEMail_(message_content, studyXSId, probandXSID, studyApiKey, interventorUrl)
}
