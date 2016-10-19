library(logging)
library(httr)

source('R/APIPermissionToken.R')
source('R/config.R')

sendInterventorEMail <- function(message_content, studyXSId, probandXSID, sendInjection = sendByPostRequest){
  secretToken <- generateAPIPermissionToken(probandXSID, message_content, conf.studyAPIKey)
  logdebug(paste("Sending Message via Interventor", paste('probandXSID', probandXSID, sep=':'), paste('content', message_content, sep=':'), paste('token', secretToken, sep=':')), sep=' - ')
  apiOperationAdr <- interventorMessengerAPIPath(studyXSId, probandXSID, message_content, secretToken)
  result_code <- sendInjection(apiOperationAdr, message_content)
  if(result_code == 200){
    loginfo(paste('Message sent: ', paste('probandXSID', probandXSID, sep=':'), paste('content', message_content, sep=':')), sep=' - ')
    'success_sending'
  }
  else{
    logerror(paste('Failed sending message: ', paste('probandXSID', probandXSID, sep=':'), paste('content', message_content, sep=':')), sep=' - ')
    'error_sending'
  }
}

sendByPostRequest <- function(apiOperationAdr, message_content){
  logdebug(paste('Requesting interventor API operation: POST: ', apiOperationAdr))
  reqHeaders <- add_headers('Content-Type'='text/plain;charset=UTF-8')
  req <- POST(url = apiOperationAdr, body = message_content, reqHeaders)
  result <- content(req, as = "text", encoding = "UTF-8")
  logdebug(paste('json result', result, sep=':'))
  status_code(req)
}

interventorMessengerAPIPath <- function(studyXSId, probandXSID, content, token){
  operationURL <- paste0(conf.alerts.interventorUrl, conf.alerts.interventor_messagingAPI.sendMessagePath)
  debugFormatted <- tolower(conf.alerts.debugging)  #The intervention server will only parse the lower cased string correctly
  apiCallParams = paste(paste('studyXSID', studyXSId, sep = '='), paste('probandXSID', probandXSID, sep = '='), paste('token', token, sep = '='), paste('debugging', debugFormatted, sep = '='), sep = '&')
  apiOperationCall <- paste(operationURL, apiCallParams, sep = '?')
  apiOperationCall
}
