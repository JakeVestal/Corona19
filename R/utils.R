#' Refresh all data in the Corona19 Package.
#' 
refresh_corona_data <- function(){
  
  system("sudo git fetch upstream")
  system("sudo git rebase upstream/master")
  
  print(getwd())
  
  "yolo"
}
