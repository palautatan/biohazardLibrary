#' Create Folders
#' 
#' Eventually, this should work for several different functionalities.
#' @param source The directory you wish to copy folders from.
#' @param copy_path Optional. The path where you want the copy to go to. Otherwise, it will just go to the directory above your source directory.
#' @keywords biohazardCleanUp
#' @examples
#' create_folders(getwd())

create_folders = function(source, copy_path=FALSE){
  # GET THE FILES FROM THE SOURCE
  source_files = list.files(source)
  
  # PARSE THE FILES TO ONLY GET FOLDERS
  new_list = c()
  for (i in 1:length(source_files)) {
    if (grepl("\\.", source_files[i]) == FALSE) {
      new_list = c(new_list, source_files[i])
    }
  }
  
  # CREATE NEW PARENT DIRECTORY
  if (copy_path == FALSE) {
    copy_path = paste(source, "copy")
  }
  dir.create(copy_path)
  
  # CREATE THE SUBFOLDERS
  for (i in 1:length(new_list)) {
    dir.create(paste(copy_path, "/", new_list[i], sep=""))
  }
  
  return(new_list)
}