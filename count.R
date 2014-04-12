#count.R

count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  ## Extract causes of death
  ## Return integer containing count of homicides for that cause

  #if (cause == NULL 
  #if(! cause %in% c("[Aa]sphyxiation", "[Bb]lunt [Ff]orce", "[Oo]ther", "[Ss]hooting", "[Ss]tabbing", "[Uu]nknown")) stop("invalid cause")
  if(! cause %in% c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")) stop("invalid cause")

  if (cause == "asphyxiation") {cause = "[Aa]sphyxiation"}
  if (cause == "blunt force") {cause = "[Bb]lunt [Ff]orce"}
  if (cause == "other") {cause = "[Oo]ther"}
  if (cause == "shooting") {cause = "[Ss]hooting"}
  if (cause == "stabbing") {cause = "[Ss]tabbing"}
  if (cause == "unknown") {cause = "[Uu]nknown"}
  
  homicides = readLines("homicides.txt")
  return_num = length(grep(paste("[Cc]ause:",cause),homicides))
  
  return(return_num)

}
