library(digest)

generateAPIPermissionToken <- function(probandXSID, message_content, studyApiKey){
  randomUpdate <- paste0(enc2utf8(message_content), probandXSID, studyApiKey)
  token <- digest(randomUpdate, algo="sha256", serialize=FALSE)
  token
}
