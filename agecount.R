#agecount.R

agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  ## Read "homicides.txt" data file
  ## Extract ages of victims; ignore records where no age is
  ## given
  ## Return integer containing count of homicides for that age
  
  #if (age == NULL) stop("NULL age")
  
  homicides = readLines("homicides.txt")
  age = as.character(age)
  search_string = paste(" ", age, " years old</dd>",sep = "")
  return_num = length(grep(search_string,homicides))
  
  return(return_num)
}
