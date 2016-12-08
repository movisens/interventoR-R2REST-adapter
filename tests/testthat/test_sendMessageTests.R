#' @import logging
#'
NULL

test_that('test_sendInterventorEMail_interventorAPIOperationReturningSuccessfully_success', {
  # build
  # operate
  sent <<- FALSE
  message <- 'theMessage'
  studyXSId <- 1
  probandXSID <- 2
  studyApiKey <- conf.test.interventor.studyAPIKey
  interventorUrl <- conf.test.interventor.url

  sendRequestMock <- function(apiOperationAdr, message_content){
    sent <<- TRUE
    list(status_code = 200)
  }
  result <- sendInterventorEMail_(message, studyXSId, probandXSID, studyApiKey, interventorUrl, sendRequestMock)

  # check
  expect_true(sent)
  expect_equal(result, 'success_sending')
})

test_that('test_sendInterventorEMail_interventorAPIOperationThrowingAnException_fail', {
  # build
  # operate
  sent <<- FALSE
  message <- 'theMessage'
  studyXSId <- 1
  probandXSID <- 2
  studyApiKey <- conf.test.interventor.studyAPIKey
  interventorUrl <- conf.test.interventor.url

  sendRequestMock <- function(apiOperationAdr, message_content){
    list(status_code = 404)
  }
  result <- sendInterventorEMail_(message, studyXSId, probandXSID, studyApiKey, interventorUrl, sendRequestMock)

  # check
  expect_equal(result, 'error_sending')
})
