library('assertthat')
library('testthat')


test_that('test_sendInterventorEMail_interventorAPIOperationReturningSuccessfully_success', {
  # build
  # operate
  sent <<- FALSE
  message <- 'theMessage'
  studyXSId <- 1
  probandXSID <- 2
  sendRequestMock <- function(apiOperationAdr, message_content){
    sent <<- TRUE
    200
  }
  result <- sendInterventorEMail(message, studyXSId, probandXSID, sendRequestMock)

  # check
  assert_that(sent)
  assert_that(result == 'success_sending')
})

test_that('test_sendInterventorEMail_interventorAPIOperationThrowingAnException_fail', {
  # build
  # operate
  sent <<- FALSE
  message <- 'theMessage'
  studyXSId <- 1
  probandXSID <- 2
  sendRequestMock <- function(apiOperationAdr, message_content){
    404
  }
  result <- sendInterventorEMail(message, studyXSId, probandXSID, sendRequestMock)

  # check
  assert_that(result == 'error_sending')
})

#disabled
integrationTestWithInterventorStagingServer_sendInterventorEMailWithDebugParamSet_interventorCorrectlyOperating_successResultFromInterventoR <- function(){
  saved_conf.alerts.debugging <- conf.alerts.debugging
  tryCatch({
    # build
    conf.alerts.debugging <<- TRUE
    integrationTestWithInterventorStagingServer_sendInterventorEMailWithDebugParamSet_interventorCorrectlyOperating_successResultFromInterventoR_()
  }, finally = {
    conf.alerts.debugging <<- saved_conf.alerts.debugging
  })
}
