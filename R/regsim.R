#' Assumption Violation Simulation
#'
#' @description
#' One of the assumptions for linear regression is normally distributed errors. If
#' this is not met, the standard errors can be invalidated. This function runs a
#' simulation with a normal distribution and with non-normal error distribution,
#' and returing the distributions of various estimates of parameters, such as the
#' power of the test.
#'
#' @param reps positive integer of number of repetitions of simulation
#' @param ns list of positive integers of sample sizes
#' @param betaall list of different \eqn{\beta} values to test
#' @param alph a double between 0 and 1, representing the siginificance level \eqn{\alpha}
#'     to test. Default is 0.05.
#' @param err.dist a string indicating what distribution to use for errors. The current build
#'     supports \code{uniform}, \code{t}, \code{laplace}
#' @param ... additional parameters for the err.dist. See details for more information.
#'
#' @return
#' \code{regsim()} returns a list that contains two lists \code{norm.err} and
#' \code{oth.err}. Each of these lists contains two data.frames. Both contain the
#' two columns of sample sizes \code{n} and parameter \code{beta}.
#'
#' The first data.frame \code{pow} contains a 3rd column of
#' \code{power}, estimates of power. This data.frame is intended for examining power of the
#' test. In summary, \code{pow} contains
#'\itemize{
#'\item \code{n}
#'\item \code{beta}
#'\item \code{power} Estimate of power
#'}
#'The second data.frame \code{est} contains a 2 additional columns for estimates
#'of beta, \code{beta.hat}, and the coefficient of determination, r^2 values.
#'In summary, \code{est} contains
#'\itemize{
#'\item \code{n}
#'\item \code{beta}
#'\item \code{beta.hat} Estimate of beta
#'\item \code{r.sq} R^2 values
#'}
#'
#' @import
#' stats
#'
#' @export
#'
#' @details
#' It is important to check assumptions before performing linear regression. This function
#' runs a simulation twice, once with normal errors and once with non-normal errors. The
#' current build supports \code{uniform}, \code{t}, and \code{laplace} as non-normal
#' distributions. The default is \code{t} with degrees of freedom 10. Since there is
#' no built in laplace function, we generate values from a difference of two exponential
#' distributions, which is equivalent to a laplace distirbution.
#'
#' To estimate \code{beta}, we use \eqn{beta.hat = cov(x,y)/var(x)}.
#'
#' To calculate r^2, we use \deqn{1 - (SSR / SST)}
#' where SSE is the residuals sum of squares and SST is the total sum of squares.
#' SSR is extracted using \code{.lm.fit()} from \code{\link[stats]{lm.fit}} function.
#'
#' Hypothesis testing with simple linear regression is equivalent to anova. However, since
#' running \code{anova} requires the argument of a model (also using \code{lm}), it is faster to
#' calculate the p-value by using \code{pf} and using the F-statistic calculated as
#' \deqn{r^2 / (1-r^2) * (n-1)} This is derived from the relation between r^2 and the
#' F-statistic \deqn{r^2/(1-r^2)*df_2/df_1} where df_1 is the degrees of freedom for the
#' number of predictors, and df_2 is the degrees of freedom for the full model.
#'
#' The three distributions that the user can check for assumption violation are
#' \code{t}, \code{uniform}, \code{laplace}.
#' \itemize{
#' \item For \code{t}, the additional argument is \code{df}, which must be positive.
#' \item For \code{uniform}, the additional arguments are \code{min} and \code{max}.
#' \item For \code{laplace}, the additional argument is the \code{rate}, which must be positive.
#' }
#' See examples for syntax.
#'
#' @examples
#'
#'regsim(err.dist = "uniform", min=-1,max=1)
#'regsim(err.dist = "t", df=25)
#'regsim(err.dist = "laplace", rate=3)
#'
#' @seealso
#'
#' \link[stats]{lm.fit}
#'
#' \link{plotRegsim}
#'
#'
#' @references
#' \url{http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/index.html}
#'
#' \url{http://r-pkgs.had.co.nz/}
#'
#' \url{https://en.wikipedia.org/wiki/Laplace_distribution#Generating_random_variables_according_to_the_Laplace_distribution}
#'
#' \url{https://stats.stackexchange.com/questions/56881/whats-the-relationship-between-r2-and-f-test}
#'
#' \url{https://en.wikipedia.org/wiki/F-test}
#'
#' \url{http://stackoverflow.com/questions/32585753/r-swap-two-variables-without-using-a-third}
#'
#' \url{http://www.deanbodenham.com/learn/r-package-documentation.html}
regsim = function(reps = 100,
                  ns = 100,
                  betaall = seq(-1,1,0.1),
                  alph = 0.05,
                  err.dist = "t",
                  ...) {
#check invalid inputs
  if(!is.numeric(reps) | !is.numeric(ns) | !is.numeric(betaall) | !is.numeric(alph)|
     sum(is.na(reps))>0 | sum(is.na(ns))>0 | sum(is.na(betaall))>0 | sum(is.na(alph))>0)
    stop("parameters must be numeric")

  if ((length(reps)!=1) | (reps-as.integer(reps)!=0) | (reps<=0))
    stop("reps must be a single positive integer")

  if((sum(ns<=0)>0) | sum(ns-as.integer(ns)!=0)>0)
    stop("ns must be positive and integer")

  if ((length(alph)!=1) | alph>1 | alph < 0 | sum(is.na(alph))>0)
    stop("alpha must be a single value between 0 and 1")

  if (!is.character(err.dist) | (length(err.dist)!=1) |
      (err.dist!="t" && err.dist!="uniform" && err.dist!="laplace"))
    stop("err.dist must be \"t\", \"uniform\", or \"laplace\"")

  arg = list(...)
  #default parameter
  if (length(arg)==0) arg=list(df=10)

  if (((err.dist=="uniform") && ((sum((names(arg)!=c("min", "max"))>0)) |
                                 (length(arg)!=2) |
                                 (!is.numeric(unlist(arg)))
  )) |

  ((err.dist=="laplace") && ((names(arg)!="rate") |
                             (length(arg)!=1) |
                             (!is.numeric(unlist(arg)))
  )) |

  (err.dist=="t" && ((names(arg)!="df") |
                     (length(arg)!=1) |
                     (!is.numeric(unlist(arg)))
  )))
  stop("... parameters must match err.dist")

  if((err.dist=="uniform") && (length(arg)==2) &&
     ((length(arg[[1]])!=1) | (length(arg[[2]])!=1)))
    stop("min and max must be each a single number")

  if((err.dist=="laplace") && (length(arg)==1) && ((length(unlist(arg))!=1) |
                                                   (arg<=0)))
    stop("rate must be single positive number")

  if((err.dist=="t") && (length(arg)==1) && ((length(unlist(arg))!=1) |
                                             (arg<=0)))
    stop("df must be single positive number")

  #initialize values needed
  pow.norm=matrix(nrow=0, ncol=3)
  pow.oth=pow.norm
  est.norm=matrix(nrow=0, ncol=4)
  est.oth=est.norm

  #vectorize estimates before going through loop

  bhat.norm=vector(length=reps*length(ns)*length(betaall))
  rsq.norm=vector(length=reps*length(ns)*length(betaall))
  bhat.oth=vector(length=reps*length(ns)*length(betaall))
  rsq.oth=vector(length=reps*length(ns)*length(betaall))
  maxj=length(betaall)

  #simulation, loops through sample sizes ns, beta values betaall,
  #and saves b.hat, rsq for both the normal error regression and non-normal error regression.
  #save.norm and save.oth are boolean values that check whether or not the test is signficant.
  #the mean (probability of getting signfiicant value) is saved outside the repetition loop

  for (k in seq_along(ns)) {
    n = ns[k]
    for (j in seq_along(betaall)) {
      beta = betaall[j]
      save.norm = vector(length = reps)
      save.oth = vector(length = reps)
      for (i in 1:reps) {
        x = as.matrix(rnorm(n))
        y = beta * x + rnorm(n)
        se = sd(y) / sqrt(n)
        bhat.norm[(k-1)*maxj+(j-1)*reps+i] = cov(x, y) / var(x)
        rsq.norm[(k-1)*maxj+(j-1)*reps+i] = 1 - sum((stats::.lm.fit(cbind(1, x), y)$residuals) ^ 2) /
          sum((y - sum(y) / n) ^ 2)
        save.norm[i] = pf(rsq.norm[(k-1)*maxj+(j-1)*reps+i] / (1 - rsq.norm[(k-1)*maxj+(j-1)*reps+i]) * (n - 1), df1 = 1,
                          df2 = n - 1, lower.tail = FALSE
        ) < alph

        #uniform
        if ((length(arg)==2) && err.dist=="uniform") {
          se = (arg[[2]] - arg[[1]]) ^ 2 / 12
          #if user puts max first instead of second
          if (arg[[1]]>arg[[2]]){
            arg[[1]]=arg[[1]]+arg[[2]]
            arg[[2]]=arg[[1]]-arg[[2]]
            arg[[1]]=arg[[1]]-arg[[2]]
          }
          eps.dist= runif(n, min=arg[[1]], max=arg[[2]])
        }
        else if (length(arg)==1){
          if (err.dist=="laplace") {
            se=2 * arg[[1]] ^ 2
            eps.dist=rexp(n, rate = arg[[1]]) - rexp(n, rate = arg[[1]])
          }
          else if (err.dist=="t"){
            se=arg[[1]]/(arg[[1]]-2)
            eps.dist = rt(n, df = arg[[1]])
          }
        }
        y = beta * x + eps.dist
        bhat.oth[(k-1)*maxj+(j-1)*reps+i] = cov(x, y) / var(x)
        rsq.oth[(k-1)*maxj+(j-1)*reps+i] = 1 - sum((stats::.lm.fit(cbind(1, x), y)$residuals) ^ 2) /
          sum((y - sum(y) / n) ^ 2)
        save.oth[i] = pf(rsq.oth[(k-1)*maxj+(j-1)*reps+i] / (1 - rsq.oth[(k-1)*maxj+(j-1)*reps+i]) * (n - 1), df1 = 1,
                         df2 = n - 1, lower.tail = FALSE
        ) < alph
        est.oth = rbind(est.oth, c(n, beta,
                                   bhat.oth[(k-1)*maxj+(j-1)*reps+i],
                                   rsq.oth[(k-1)*maxj+(j-1)*reps+i]))
        est.norm = rbind(est.norm, c(n, beta, bhat.norm[(k-1)*maxj+(j-1)*reps+i],
                                     rsq.norm[(k-1)*maxj+(j-1)*reps+i] ))
      }
      pow.oth = rbind(pow.oth, c(n, beta, mean(save.oth)))
      pow.norm = rbind(pow.norm, c(n, beta, mean(save.norm)))
    }
  }

  #convert results into a data frame
  pow.norm=as.data.frame(pow.norm)
  pow.oth=as.data.frame(pow.oth)
  est.norm=as.data.frame(est.norm)
  est.oth=as.data.frame(est.oth)

  colnames(pow.norm)=c("n","beta","power")
  colnames(pow.oth)=colnames(pow.norm)
  colnames(est.norm)=c("n","beta","beta.hat","r.sq")
  colnames(est.oth)=colnames(est.norm)
  norm.err=list(pow=pow.norm, est=est.norm)
  oth.err=list(pow=pow.oth, est=est.oth)

  results=list(norm.err=norm.err, oth.err=oth.err)
  return(results)
}

#' Plotting Parameters
#'
#' @description
#' Visualizing the relation between different parameters and estimates is useful. This function
#' plots estimates of slope, power, and r^2 from a simple linear regression model that is
#' checked for assumption violation via \code{regsim}. These plots are
#' created by \code{ggplot}.
#'
#' @param reps positive integer of number of repetitions of simulation
#' @param ns list of positive integers of sample sizes
#' @param betaall list of different \eqn{\beta} values to test
#' @param alph a double between 0 and 1, representing the siginificance level \eqn{\alpha}
#'     to test. Default is 0.05.
#' @param err.dist a string indicating what distribution to use for errors. The current build
#'     supports \code{uniform}, \code{t}, \code{laplace}
#' @param ... additional parameters for the err.dist. See details for more information.
#'
#' @return
#' \code{plotRegsim} returns a list of 6 ggplot objects. The first three are plots related to
#' having a normally distributed error, while the remaining are plots related to the error
#' distribution provided by the user. Each plot colors the data based on sample size. Specifically
#'
#' \enumerate{
#'
#' \item Power vs. beta, normal errors
#'
#' \item Beta.hat vs. beta, normal errors
#'
#' \item r^2 vs. beta, normal errors
#'
#' \item Power vs. beta, uniform/t/laplace errors
#'
#' \item Beta.hat vs. beta, uniform/t/laplace errors
#'
#' \item r^2 vs. beta, uniform/t/laplace errors
#'
#' }
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' plotRegsim(err.dist = "laplace", rate=3)
#'
#' @seealso
#'
#' \link[project]{regsim}
#'
#' \link[ggplot2]{ggplot}
#'
#'
plotRegsim=function(reps = 100,
                    ns = c(10,25,100),
                    betaall = seq(-1,1,0.1),
                    err.dist = "t",
                    alph=0.05,
                    ...){

  results=regsim(reps=reps,ns=ns,betaall=betaall,alph=alph, err.dist,...)

  #make sample size as a factor for plotting
  results$norm.err$pow$n = as.factor(results$norm.err$pow$n)
  results$oth.err$pow$n = as.factor(results$oth.err$pow$n)
  results$norm.err$est$n =as.factor(results$norm.err$est$n)
  results$oth.err$est$n =as.factor(results$oth.err$est$n)

  p1=ggplot2::ggplot(results$norm.err$pow, ggplot2::aes(x = beta, y = power,group = n, color = n)) +
    ggplot2::geom_line() + ggplot2::ggtitle("Power vs. Beta, Normal Error")

  p2=ggplot2::ggplot(results$norm.err$est, ggplot2::aes(x = beta, y = beta.hat,group = n, color = n)) +
    ggplot2::geom_jitter(size=0.85, alpha=0.75) + ggplot2::geom_point(size=0.85, alpha=0.75) +
    ggplot2::geom_abline(slope=1, intercept=0) +
    ggplot2::ggtitle("Beta.hat vs. Beta, Normal Error")

  p3=ggplot2::ggplot(results$norm.err$est, ggplot2::aes(x = beta, y = r.sq,group = n, color = n)) +
    ggplot2::geom_point(size=0.85, alpha=0.75) + ggplot2::geom_jitter(size=0.85, alpha=0.75) + ggplot2::ggtitle("r^2 vs. Beta, Normal Error")

  p4=ggplot2::ggplot(results$oth.err$pow, ggplot2::aes(x = beta, y = power,group = n, color = n)) +
    ggplot2::geom_line() + ggplot2::ggtitle(paste("Power vs. Beta, ", err.dist, " error"))

  p5=ggplot2::ggplot(results$oth.err$est, ggplot2::aes(x = beta, y = beta.hat,group = n, color = n)) +
    ggplot2::geom_point(size=0.85, alpha=0.75) + ggplot2::geom_jitter(size=0.85, alpha=0.75) + ggplot2::geom_abline(slope=1, intercept=0) +
    ggplot2::ggtitle(paste("Beta.hat vs. Beta, ", err.dist, " error"))

  p6=ggplot2::ggplot(results$oth.err$est, ggplot2::aes(x = beta, y = r.sq,group = n, color = n)) +
    ggplot2::geom_point(size=0.85, alpha=0.75) + ggplot2::geom_jitter(size=0.85, alpha=0.75) +
    ggplot2::ggtitle(paste("r^2 vs. Beta, ", err.dist, " error"))

  return(list(p1,p2,p3,p4,p5,p6))
}
