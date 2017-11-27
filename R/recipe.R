#' Getting Recipes
#'
#' @description
#' Given a list of ingredients, what kind of dish can one make? \code{getRecipes} is a function that
#' takes in a list of \code{k} ingredients, an optional dish name of interest, and a minimum number of ingredients \code{j}
#' to use from the provided list. Using the Recipe Puppy API, getRecipes returns the first 100
#' recipes that include at least \code{j} of the \code{k} ingredients, providing the user with a corresponding link and additional ingredients required
#' for each recipe.
#'
#' @param ingredients a list of ingredients, where each one is a string
#' @param search an optional string that indicates dish name.
#' @param minNumIngred a positive integer specifying the minimum number of ingredients from \code{ingredients}.
#'  Note that this cannot exceed the number of ingredients provided.
#'
#' @return \code{getRecipes} returns a \code{data.frame}, which is in ascending order by the number of additional ingredients needed.
#' The \code{data.frame} has the following columns:
#' \itemize{
#' \item \code{recipe} is a column of strings containing the name of the recipe.
#'
#' \item \code{link} is a column of string of the HTTP link to the complete recipe
#'
#' \item \code{all.ingre} is a column of string of the full list of ingredients needed
#'
#' \item \code{add.ingre} is a column of string of only the additional ingredients needed
#' (those not provided by the user)
#'
#' \item \code{num} is a column of positive integer indicating the number of additional
#' ingredients needed
#' }
#'
#' @import jsonlite
#' @import stringr
#' @import utils
#' @import curl
#'
#' @export
#'
#' @details
#'
#' \code{getRecipes} takes in user-inputted strings for ingredients and an optional search term
#' to provide a list 100 of recipes. There is also an option for the user to enter the minimum number
#' of ingredients from the provided list required for a recipe.
#'
#' The function uses the Recipe Puppy API, and uses \code{\link[jsonlite]{fromJSON}} from the
#' \code{jsonlite} package to extract text. (An internet connection is required for this function to work.)
#'
#' For example, in the default output, we are looking for any recipe that must contain at least 2
#' of the ingredients from the list of chicken, onions, tomato, and green beans. This contains
#' the 4 choose 2 combinations (chicken & onions, chicken & tomato, chicken & green beans, etc.)
#'
#' To check these combinations, the function checks for the minimum number appearances over
#' all combinations if only 1 pairing of 2 is found in the recipe. This is \code{2*(k-1)}.
#' For example, with 4 ingredients, and 1 pairing of chicken & onions. Out of the  combinations
#' of all pairings, chicken and onions appears a total of 6 times, which is 2*(4-1)=2*3.
#'
#' For users with less of an idea of what to cook, the function also accepts a single ingredient.
#' The minNumIngred value is set to 1, and the construction of combinations is ignored.
#'
#' This function does not address recipes that comprise of only a single ingredient (as recipes
#' usually involve more than one ingredient).
#'
#' @examples
#' getRecipes(ingredients="beef")
#' getRecipes(ingredients=c("onions", "cheese", "garlic"), search="burger", minNumIngred=2)
#' getRecipes(ingredients=c("onions", "cheese", "green beans", "garlic", "tomato"), search="pasta", minNumIngred=2)
#'
#'\dontrun{
#'getRecipes(ingredients=c(5, "pound turkey"))
#'}
#' @references
#' \url{http://www.recipepuppy.com/about/api}
#'
#' \url{http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/index.html}
#'
#' \url{http://r-pkgs.had.co.nz/}
#'
#' \url{http://stackoverflow.com/questions/2098368/how-do-i-concatenate-a-vector-of-strings-character-in-r}
#'
#' \url{http://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r}
#'
#' \url{http://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r}
#'
#'
getRecipes = function(ingredients = c("chicken", "onions", "tomato", "green beans"),
                      search = "",
                      minNumIngred = 2) {
  #Checks that user can be connected to recipepuppy
  if (!isTRUE(curl::has_internet())) stop("No internet connection. Find a suitable connection before using getRecipes()")

  #Check for invalid inputs

  if (length(ingredients)==0) stop("Must enter at least 1 ingredient")
  if (length(ingredients)==1) minNumIngred=1

  if (!is.character(ingredients) | sum(is.na(ingredients))>0 |
      sum(!is.na(as.numeric(ingredients)))>0){
    stop("ingredients must be a string")
  }

  if (!is.character(search) | sum(is.na(search)>0) |
      sum(!is.na(as.numeric(search)))>0){
    stop("Optional search term must be a string")
  }

  if ((!is.double(minNumIngred)) | (is.null(minNumIngred))) {
    stop("minNumIngred must be a positive integer")
  }
  if ((sum(is.na(minNumIngred))>0) | (minNumIngred < 1) |
      (minNumIngred-as.integer(minNumIngred)!=0)) {
    stop("minNumIngred must be a positive integer")
  }

  if (minNumIngred > length(ingredients)){
    stop("minNumIngred cannot exceed number of ingredients provided")
  }

  #if user enters na, remove from the ingredients list

  ingredients=ingredients[which(!is.na(ingredients))]
  totcomb = matrix(ingredients)
  #generate all unique combinations of ingredients with groups size minNumIngred
  totcomb = utils::combn(totcomb, minNumIngred)

  #minimum instances of a single pairing. If only 1 ingredient provided, set equal to 1.
  mincomb = 2*(length(ingredients))
  if (minNumIngred==1) mincomb=1

  #reformat user inputs into url
  ing = sapply(1:length(ingredients), function(i) {
    stringr::str_replace_all(stringr::str_trim(ingredients[i]), "[:space:]", "\\+")
  })
  ing = stringr::str_c("i=", paste(ing, collapse = ','))
  keyword = stringr::str_c("q=", stringr::str_replace_all(search, "[:space:]", "\\+"))

  #initialize table of recipes
  info = as.data.frame(matrix(nrow = 0, ncol = 3))
  i = 0

  while(nrow(info)<100){
    i=i+1
    url = stringr::str_c(
      "http://www.recipepuppy.com/api/?", ing, "&", keyword, "&minIngs=",
      as.character(minNumIngred),"&p=", i
    )
    temp = tryCatch({
      jsonlite::fromJSON(url)$results[, -4]},
      error = function(e) {
        message("Reached end of results!")
        return(NULL)
      }
    )

    #if no recepies found, stop function
    if (is.null(temp)){
      if (nrow(info)==0){
        stop("No recipes found!")
      }
      break
    }

    index=vector(length=10)
    if (length(ingredients)>0){
      for (j in 1:10) {
        if ((sum(stringr::str_detect(temp$ingredients[j], totcomb)) >= mincomb)==TRUE) {
          index[j] = TRUE
        }
      }
      temp=temp[which(index==TRUE),]
      info=rbind(info, temp)
    }
  }
  #remove extra blank space in recipe names
  info[,1]=stringr::str_trim(info[,1])

  #generate column of additional ingredients
  #for multiword ingredients, like green beans, @ replacement only near
  #columns avoids confusion with "internal" space
  add.ingre=sapply(1:nrow(info), function(i){
    ingextr=sapply(1:length(ingredients), function(i){
      stringr::str_c("@",stringr::str_trim(ingredients[i]), "@")
    })
    ingextr = stringr::str_c(paste(ingextr, collapse = '|'))

    temp2=stringr::str_c("@",info$ingredients[i],"@")
    temp2=stringr::str_replace_all(temp2, ", ", "@,@")
    temp2=stringr::str_replace_all(temp2, ingextr, "")
    temp2=stringr::str_replace_all(temp2, "@,+@",", ")
    temp2=stringr::str_replace_all(temp2, ",+",",")
    temp2=stringr::str_replace(temp2, ",$","")
    temp2=stringr::str_replace_all(temp2, ",$","")
    if (!is.na(stringr::str_locate(temp2,",")[1,1]) &&
        stringr::str_locate(temp2,",")[1,1]==1){
      temp2=stringr::str_sub(temp2, start=2)
    }
    return(stringr::str_replace_all(temp2, "@",""))
  })

  #convert and combine into single data frame
  add.ingre=as.data.frame(add.ingre, stringsAsFactors=FALSE)
  add.ingre$num=stringr::str_count(add.ingre[,1], ",")+1
  recipes=cbind(info, add.ingre)
  recipes=recipes[order(recipes$num),]
  colnames(recipes)=c("recipe","link","all.ingre", "add.ingre","num")

  #since each page returns 10, possible to have 101-109 recipes, this only take first 100
  recipes=recipes[1:100,]
  return(recipes)
}
