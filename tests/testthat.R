library(testthat)
library(project)

test_check("project")

#getRecipes checks
####################
test_that("getRecipes: Invalid inputs", {
  expect_error(getRecipes(ingredients=1),"ingredients must be a string")
  expect_error(getRecipes(ingredients=1.2),"ingredients must be a string")
  expect_error(getRecipes(ingredients=NA),"ingredients must be a string")
  expect_error(getRecipes(ingredients=c("chicken", NA)),"ingredients must be a string")
  expect_error(getRecipes(ingredients=c(3, "pound", "chicken")),"ingredients must be a string")
  expect_error(getRecipes(ingredients=c("10", "pound", "chicken")),"ingredients must be a string")
  expect_error(getRecipes(ingredients=c("chicken", 3)),"ingredients must be a string")
  expect_error(getRecipes(ingredients=c("chicken", "3")),"ingredients must be a string")

  expect_error(getRecipes(ingredients=NULL),"Must enter at least 1 ingredient")

  expect_error(getRecipes(search=1),"Optional search term must be a string")
  expect_error(getRecipes(search=5.5),"Optional search term must be a string")
  expect_error(getRecipes(search=NULL),"Optional search term must be a string")
  expect_error(getRecipes(search=NA),"Optional search term must be a string")
  expect_error(getRecipes(search=c("pasta", NA)),"Optional search term must be a string")
  expect_error(getRecipes(search=c(10, "piece chicken nuggets")),
               "Optional search term must be a string")


  expect_error(getRecipes(minNumIngred=NA),"minNumIngred must be a positive integer")
  expect_error(getRecipes(minNumIngred=NULL),"minNumIngred must be a positive integer")
  expect_error(getRecipes(minNumIngred=c(3,NA)),"minNumIngred must be a positive integer")
  expect_error(getRecipes(minNumIngred="string"),"minNumIngred must be a positive integer")
  expect_error(getRecipes(minNumIngred=c(3,"string")),"minNumIngred must be a positive integer")

  expect_error(getRecipes(minNumIngred=2.2),"minNumIngred must be a positive integer")
  expect_error(getRecipes(minNumIngred=-2.2),"minNumIngred must be a positive integer")
  expect_error(getRecipes(minNumIngred=0),"minNumIngred must be a positive integer")
  })

test_that("getRecipes: Output checks", {
  expect_s3_class(getRecipes(),"data.frame")
  expect_equal(nrow(getRecipes()),100)
  expect_type(getRecipes()$recipe, "character")
  expect_type(getRecipes()$link, "character")
  expect_type(getRecipes()$all.ingre, "character")
  expect_type(getRecipes()$add.ingre, "character")
  expect_type(getRecipes()$num, "double")

  expect_identical(getRecipes(),getRecipes(ingredients = c("chicken", "onions", "tomato", "green beans"),
                                           search = "",
                                           minNumIngred = 2))

  expect_identical(getRecipes(), getRecipes(ingredients = c("chicken", "onions", "tomato", "green beans", NULL)))

  expect_error(getRecipes(ingredients=c("chicken", "sugar", "chocolate",
                                        "beef", "black pepper", "vegetable oil"),
                          search="pasta",
                          minNumIngred=6), "No recipes found!")

  expect_error(getRecipes(ingredients="chickennn"), "No recipes found!")
  expect_error(getRecipes(ingredients=c("chicken", "eeeeeeeegs")), "No recipes found!")


})


##regsim checks
##
###############
test_that("regsim: Invalid Inputs",{
  expect_error(regsim(reps="hi"), "parameters must be numeric")
  expect_error(regsim(reps=NA), "parameters must be numeric")
  expect_error(regsim(reps=NULL), "parameters must be numeric")
  expect_error(regsim(reps=c(3,"uniform")), "parameters must be numeric")
  expect_error(regsim(reps=c("uniform", 3)), "parameters must be numeric")
  expect_error(regsim(reps=c(3,NA)), "parameters must be numeric")
  expect_error(regsim(reps="3"), "parameters must be numeric")

  expect_error(regsim(alph="hi"), "parameters must be numeric")
  expect_error(regsim(alph=NA), "parameters must be numeric")
  expect_error(regsim(alph=NULL), "parameters must be numeric")
  expect_error(regsim(alph=c(3,"uniform")), "parameters must be numeric")
  expect_error(regsim(alph=c("uniform", 3)), "parameters must be numeric")
  expect_error(regsim(alph=c(3,NA)), "parameters must be numeric")
  expect_error(regsim(alph="3"), "parameters must be numeric")

  expect_error(regsim(ns="hi"), "parameters must be numeric")
  expect_error(regsim(ns=NA), "parameters must be numeric")
  expect_error(regsim(ns=NULL), "parameters must be numeric")
  expect_error(regsim(ns=c(3,"uniform")), "parameters must be numeric")
  expect_error(regsim(ns=c("uniform", 3)), "parameters must be numeric")
  expect_error(regsim(ns=c(3,NA)), "parameters must be numeric")
  expect_error(regsim(ns="3"), "parameters must be numeric")

  expect_error(regsim(betaall="hi"), "parameters must be numeric")
  expect_error(regsim(betaall=NA), "parameters must be numeric")
  expect_error(regsim(betaall=NULL), "parameters must be numeric")
  expect_error(regsim(betaall=c(3,"uniform")), "parameters must be numeric")
  expect_error(regsim(betaall=c("uniform", 3)), "parameters must be numeric")
  expect_error(regsim(betaall=c(3,NA)), "parameters must be numeric")
  expect_error(regsim(betaall="3"), "parameters must be numeric")

  expect_error(regsim(reps=3.3), "reps must be a single positive integer")
  expect_error(regsim(reps=0), "reps must be a single positive integer")
  expect_error(regsim(reps=-1), "reps must be a single positive integer")
  expect_error(regsim(reps=c(3,3)), "reps must be a single positive integer")
  expect_error(regsim(reps=c(3,-3)), "reps must be a single positive integer")

  expect_error(regsim(ns=c(3,3.3)), "ns must be positive and integer")
  expect_error(regsim(ns=c(-3,3.3)), "ns must be positive and integer")

  expect_error(regsim(err.dist=NA), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(regsim(err.dist=3), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(regsim(err.dist=NULL), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(regsim(err.dist=c("hi", "bye")), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(regsim(err.dist=c("t",NA)), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(regsim(err.dist=c("t","uniform")), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(regsim(err.dist=c("t",3)), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(regsim(err.dist="unifrom", min=3,max=3),"err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(regsim(err.dist="uniformm", min=3,max=3),"err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(regsim(err.dist="tt", df=2),"err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(regsim(err.dist="normal", min=3,max=3),"err.dist must be \"t\", \"uniform\", or \"laplace\"")

  expect_error(regsim(err.dist="uniform", rate=3), "... parameters must match err.dist")
  expect_error(regsim(err.dist="uniform", min=3, rate=3), "... parameters must match err.dist")
  expect_error(regsim(err.dist="uniform", min=c(-3,3)), "... parameters must match err.dist")
  expect_error(regsim(err.dist="uniform", min=-3, max=3, sig=3), "... parameters must match err.dist")
  expect_error(regsim(err.dist="uniform", min=-3, max=3, rate=3), "... parameters must match err.dist")

  expect_error(regsim(err.dist="t", rate=-2), "... parameters must match err.dist")
  expect_error(regsim(err.dist="t", min=-3, max=c(3,4)), "... parameters must match err.dist")
  expect_error(regsim(err.dist="t", min=-3, max=4), "... parameters must match err.dist")
  expect_error(regsim(err.dist="t", min=-3, rate=4), "... parameters must match err.dist")
  expect_error(regsim(err.dist="t", rate=4), "... parameters must match err.dist")
  expect_error(regsim(err.dist="t", min="-3", rate="4"), "... parameters must match err.dist")
  expect_error(regsim(err.dist="t", sig=4), "... parameters must match err.dist")
  expect_error(regsim(err.dist="t", min=-3, max=3, rate=4), "... parameters must match err.dist")

  expect_error(regsim(err.dist="laplace", min=3, rate=3), "... parameters must match err.dist")
  expect_error(regsim(err.dist="laplace", min=3, max=3, rate=2), "... parameters must match err.dist")
  expect_error(regsim(err.dist="laplace", rate="3"), "... parameters must match err.dist")

  expect_error(regsim(err.dist="uniform", min=-3, max=c(3,4)), "min and max must be each a single number")
  expect_error(regsim(err.dist="t", df=-2), "df must be single positive number")
  expect_error(regsim(err.dist="t", df=0), "df must be single positive number")
  expect_error(regsim(err.dist="laplace", rate=-2), "rate must be single positive number")
  expect_error(regsim(err.dist="laplace", rate=0), "rate must be single positive number")
  expect_error(regsim(err.dist="laplace", rate=c(3,5)))#, "rate must be single number")

})

test_that("regsim: output checks", {

  expect_equal(length(regsim()),2)
  expect_equal(length(regsim()$norm.err),2)
  expect_equal(length(regsim()$oth.err),2)
  expect_equal(length(regsim(err.dist="t",df=3.5)), 2)
  expect_equal(length(regsim(err.dist="laplace",rate=3.5)), 2)
  expect_equal(length(regsim(err.dist="uniform",min=-3, max=3)), 2)
  expect_type(regsim(), "list")
  expect_type(regsim()$norm.err, "list")
  expect_type(regsim()$oth.err, "list")
  expect_s3_class(regsim()$norm.err$pow, "data.frame")
  expect_s3_class(regsim()$norm.err$est, "data.frame")
  expect_s3_class(regsim()$oth.err$pow, "data.frame")
  expect_s3_class(regsim()$oth.err$est, "data.frame")

})

##plotRegsim tests
##
##Since plotRegsim inputs depend on regsim, only a sample of the input tests done with regsim
#are used again in plotRegsim

###
test_that("plotRegsim: Invalid Inputs",{
  expect_error(plotRegsim(reps=NA), "parameters must be numeric")
  expect_error(plotRegsim(reps=NULL), "parameters must be numeric")
  expect_error(plotRegsim(reps=c(3,"uniform")), "parameters must be numeric")
  expect_error(plotRegsim(reps=c("uniform", 3)), "parameters must be numeric")
  expect_error(plotRegsim(reps=c(3,NA)), "parameters must be numeric")
  expect_error(plotRegsim(reps="3"), "parameters must be numeric")

  expect_error(plotRegsim(alph="hi"), "parameters must be numeric")
  expect_error(plotRegsim(alph=NA), "parameters must be numeric")
  expect_error(plotRegsim(alph=NULL), "parameters must be numeric")
  expect_error(plotRegsim(alph=c(3,"uniform")), "parameters must be numeric")
  expect_error(plotRegsim(alph=c("uniform", 3)), "parameters must be numeric")
  expect_error(plotRegsim(alph=c(3,NA)), "parameters must be numeric")
  expect_error(plotRegsim(alph="3"), "parameters must be numeric")

  expect_error(plotRegsim(ns="hi"), "parameters must be numeric")
  expect_error(plotRegsim(ns=NA), "parameters must be numeric")
  expect_error(plotRegsim(ns=NULL), "parameters must be numeric")
  expect_error(plotRegsim(ns=c(3,"uniform")), "parameters must be numeric")
  expect_error(plotRegsim(ns=c("uniform", 3)), "parameters must be numeric")
  expect_error(plotRegsim(ns=c(3,NA)), "parameters must be numeric")
  expect_error(plotRegsim(ns="3"), "parameters must be numeric")

  expect_error(plotRegsim(betaall="hi"), "parameters must be numeric")
  expect_error(plotRegsim(betaall=NA), "parameters must be numeric")
  expect_error(plotRegsim(betaall=NULL), "parameters must be numeric")
  expect_error(plotRegsim(betaall=c(3,"uniform")), "parameters must be numeric")
  expect_error(plotRegsim(betaall=c("uniform", 3)), "parameters must be numeric")
  expect_error(plotRegsim(betaall=c(3,NA)), "parameters must be numeric")
  expect_error(plotRegsim(betaall="3"), "parameters must be numeric")

  expect_error(plotRegsim(reps=3.3), "reps must be a single positive integer")
  expect_error(plotRegsim(reps=0), "reps must be a single positive integer")
  expect_error(plotRegsim(reps=-1), "reps must be a single positive integer")
  expect_error(plotRegsim(reps=c(3,3)), "reps must be a single positive integer")
  expect_error(plotRegsim(reps=c(3,-3)), "reps must be a single positive integer")

  expect_error(plotRegsim(ns=c(3,3.3)), "ns must be positive and integer")
  expect_error(plotRegsim(ns=c(-3,3.3)), "ns must be positive and integer")

  expect_error(plotRegsim(err.dist=NA), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(plotRegsim(err.dist=3), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(plotRegsim(err.dist=NULL), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(plotRegsim(err.dist=c("hi", "bye")), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(plotRegsim(err.dist=c("t",NA)), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(plotRegsim(err.dist=c("t","uniform")), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(plotRegsim(err.dist=c("t",3)), "err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(plotRegsim(err.dist="unifrom", min=3,max=3),"err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(plotRegsim(err.dist="uniformm", min=3,max=3),"err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(plotRegsim(err.dist="tt", df=2),"err.dist must be \"t\", \"uniform\", or \"laplace\"")
  expect_error(plotRegsim(err.dist="normal", min=3,max=3),"err.dist must be \"t\", \"uniform\", or \"laplace\"")

  expect_error(plotRegsim(err.dist="uniform", rate=3), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="uniform", min=3, rate=3), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="uniform", min=c(-3,3)), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="uniform", min=-3, max=3, sig=3), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="uniform", min=-3, max=3, rate=3), "... parameters must match err.dist")

  expect_error(plotRegsim(err.dist="t", rate=-2), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="t", min=-3, max=c(3,4)), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="t", min=-3, max=4), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="t", min=-3, rate=4), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="t", rate=4), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="t", min="-3", rate="4"), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="t", sig=4), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="t", min=-3, max=3, rate=4), "... parameters must match err.dist")

  expect_error(plotRegsim(err.dist="laplace", min=3, rate=3), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="laplace", min=3, max=3, rate=2), "... parameters must match err.dist")
  expect_error(plotRegsim(err.dist="laplace", rate="3"), "... parameters must match err.dist")

  expect_error(plotRegsim(err.dist="uniform", min=-3, max=c(3,4)), "min and max must be each a single number")
  expect_error(plotRegsim(err.dist="t", df=-2), "df must be single positive number")
  expect_error(plotRegsim(err.dist="t", df=0), "df must be single positive number")
  expect_error(plotRegsim(err.dist="laplace", rate=-2), "rate must be single positive number")
  expect_error(plotRegsim(err.dist="laplace", rate=0), "rate must be single positive number")
  expect_error(plotRegsim(err.dist="laplace", rate=c(3,5)))#, "rate must be single number")

})

test_that("plotRegsim Output",{

  expect_type(plotRegsim(), "list")
  expect_equal(length(plotRegsim()),6)

})
