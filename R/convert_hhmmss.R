#' Convert hh:mm:ss to hours
#' @param hhmmss Either a string with "hh:mm:ss" or a decimal number of hours
#' @param forward Default is TRUE, thus performs the hh:mm:ss conversion to decimal. FALSE would do the opposite conversion.
#' @keywords biohazardCleanUp
#' @examples
#' convert_hhmmss("07:56:08") / convert_hhmmss("09:46:22")
#' 0.8040201 * convert_hhmmss("28:23:41")
#' convert_hhmmss(23.09548, forward=FALSE)


convert_hhmmss = function(hhmmss, forward=TRUE) {
  if (forward) {
    nums = unlist(lapply(strsplit(hhmmss, ":")[[1]], as.integer))
    nums[1] + (nums[2]/60) + (nums[3]/120)
  } else {
    hours = as.integer(hhmmss)
    decimal = hhmmss - hours
    minutes = decimal*60
    decimal = minutes - as.integer(minutes)
    seconds = decimal*120
    if (hours<10) {
      hours = round(hours)
      hours = paste0("0", hours)
    } else {
      hours = round(hours)
    }
    if (minutes<10) {
      minutes = round(minutes)
      minutes = paste0("0", minutes)
    } else {
      minutes = round(minutes)
    }
    if (seconds<10) {
      seconds = round(seconds)
      seconds = paste0("0", seconds)
    } else {
      seconds = round(seconds)
    }
    paste(hours,minutes,seconds,sep=":")
  }
}