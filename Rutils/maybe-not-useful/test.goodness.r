#==========================================================================================#
#==========================================================================================#
#     This function computes several metrics to test the goodness of fit.  The list is     #
# not complete, contributions are welcome.                                                 #
#                                                                                          #
#  INPUT:                                                                                  #
#  ~ x.mod          -- the modelled values of x                                            #
#  ~ x.obs          -- the observed values of x                                            #
#  ~ n.parameters   -- number of parameters (if the number of parameters is unknown, we    #
#                      don't assume any parameters).                                       #
#  ~ out.dfr        -- output as a data frame? (FALSE returns a list).                     #
#------------------------------------------------------------------------------------------#
test.goodness <<- function(x.mod,x.obs,x.sigma=NULL,n.parameters=NULL,out.dfr=FALSE){


   #---- Crash if the x.mod and x.obs don't have the same size and class. -----------------#
   dlength = (length(x.mod) - length(x.obs)) != 0
   dclass  = typeof(x.mod) != typeof(x.obs)
   if (dlength || dclass){
      stop (" x.mod and x.obs must have the same size and class","\n")
   }#end if
   #---------------------------------------------------------------------------------------#


   #---- Crash if the x.sigma and x.obs don't have the same size and class. ---------------#
   if (! is.null(x.sigma)){
      dlength = (length(x.mod) - length(x.sigma)) != 0
      dclass  = typeof(x.mod) != typeof(x.sigma)
      if (dlength || dclass){
         stop (" x.mod and x.sigma must have the same size and class","\n")
      }#end if
      #------------------------------------------------------------------------------------#


      #----- Find associated weights, and re-create the vectors x.mod and x.obs. ----------#
      x.wgt      = ifelse(x.sigma %>% 0, 1. / x.sigma^2, 0)
      sel        = x.wgt %>% 0
      x.obs.orig = x.obs
      x.mod.orig = x.mod
      x.obs      = x.obs[sel] * sqrt(x.wgt[sel])
      x.mod      = x.mod[sel] * sqrt(x.wgt[sel])
      #------------------------------------------------------------------------------------#
   }#end if (! is.null(x.sigma)
   #---------------------------------------------------------------------------------------#



   #---- Find some general variables that will be used during this function. --------------#
   sel      = is.finite(x.mod) & is.finite(x.obs)
   n.ok     = sum(sel)
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Find the number of degrees of freedom for both total and explained variance.      #
   #---------------------------------------------------------------------------------------#
   df.tot = n.ok - 1
   if (is.null(n.parameters)){
      df.err = n.ok - 1
   }else{
      df.err = n.ok - n.parameters
   }#end if
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Check whether we have enough valid observations before proceeding.                #
   #---------------------------------------------------------------------------------------#
   if (df.err > 1){
      #------------------------------------------------------------------------------------#
      #      Find the total and model residuals.                                           #
      #------------------------------------------------------------------------------------#
      x.mod.ok = x.mod[sel]
      x.obs.ok = x.obs[sel]
      x.res.ok = x.obs.ok - x.mod.ok
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #      Find the four moments of the distributions.                                   #
      #------------------------------------------------------------------------------------#
      obs.moment = c( mean     = mean(x.obs.ok), variance = var (x.obs.ok)
                    , skewness = skew(x.obs.ok), kurtosis = kurt(x.obs.ok) )
      mod.moment = c( mean     = mean(x.mod.ok), variance = var (x.mod.ok)
                    , skewness = skew(x.mod.ok), kurtosis = kurt(x.mod.ok) )
      res.moment = c( mean     = mean(x.res.ok), variance = var (x.res.ok)
                    , skewness = skew(x.res.ok), kurtosis = kurt(x.res.ok) )
      #------------------------------------------------------------------------------------#




      #------------------------------------------------------------------------------------#
      #     Find the mean bias, standard deviation of the residuals, and the support for   #
      # the errors being normally distributed around the mean.                             #
      #------------------------------------------------------------------------------------#
      bias         = -res.moment["mean"]
      sigma        = sqrt(res.moment["variance"])
      names(bias)  = NULL
      names(sigma) = NULL
      lsq.lnlike   = sum(dnorm(x.res.ok,mean=0,sd=sigma,log=TRUE))
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #     Find the mean square error of the estimator, and the root mean square error.   #
      #------------------------------------------------------------------------------------#
      mse         = bias^2 + sigma^2
      rmse        = sqrt(mse)
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #     Find the coefficient of determination (R2).  If the number of parameters is    #
      # given, then we find the adjusted R^2 (the one that penalises due to the number of  #
      # parameters).  Otherwise we can't correct, so we just compare the sum of the        #
      # variances.                                                                         #
      #------------------------------------------------------------------------------------#
      x.tot.ok  = x.obs.ok - mean(x.obs.ok)
      ss.tot    = sum(x.tot.ok^2)
      ss.err    = sum(x.res.ok^2)
      r.squared = 1. - df.tot * ss.err / ( df.err * ss.tot )
      if (! is.finite(r.squared)) browser()
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #     Find the estimator's fraction of variance unexplained (FVU).  Because we use   #
      # MSE instead of ss.err, this is not 1 - R2.                                         #
      #------------------------------------------------------------------------------------#
      fvue        = mse / obs.moment["variance"]
      names(fvue) = NULL
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #     Run the Shapiro-Wilk test to determine whether the residuals are normally      #
      # distributed or not.                                                                #
      #------------------------------------------------------------------------------------#
      sw.test      = sw.test(x.res.ok)
      sw.statistic = sw.test$statistic
      sw.p.value   = sw.test$p.value
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #      Run the Kolmogorov-Smirnov test to compare the distributions.                 #
      #------------------------------------------------------------------------------------#
      this.ks             = ks.test(x=x.obs.ok,y=x.mod.ok)
      ks.statistic        = this.ks$statistic
      ks.p.value          = this.ks$p.value
      names(ks.statistic) = NULL
      #------------------------------------------------------------------------------------#
   }else{
      #------------------------------------------------------------------------------------#
      #    Not enough data points.                                                         #
      #------------------------------------------------------------------------------------#
      obs.moment   = c(mean=NA,variance=NA,skewness=NA,kurtosis=NA)
      mod.moment   = c(mean=NA,variance=NA,skewness=NA,kurtosis=NA)
      res.moment   = c(mean=NA,variance=NA,skewness=NA,kurtosis=NA)
      bias         = NA
      sigma        = NA
      lsq.lnlike   = NA
      mse          = NA
      rmse         = NA
      ss.tot       = NA
      ss.err       = NA
      r.squared    = NA
      fvue         = NA
      sw.statistic = NA
      sw.p.value   = NA
      ks.statistic = NA
      ks.p.value   = NA
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#


   #---------------------------------------------------------------------------------------#
   #     Return everything to the user.                                                    #
   #---------------------------------------------------------------------------------------#
   if (out.dfr){
      ans = data.frame( n            = n.ok
                      , p            = n.ok - df.err
                      , df.tot       = df.tot
                      , df.err       = df.err
                      , obs.mean     = obs.moment[1]
                      , obs.sdev     = sqrt(obs.moment[2])
                      , obs.skew     = obs.moment[3]
                      , obs.kurt     = obs.moment[4]
                      , mod.mean     = mod.moment[1]
                      , mod.sdev     = sqrt(mod.moment[2])
                      , mod.skew     = mod.moment[3]
                      , mod.kurt     = mod.moment[4]
                      , res.mean     = res.moment[1]
                      , res.sdev     = sqrt(res.moment[2])
                      , res.skew     = res.moment[3]
                      , res.kurt     = res.moment[4]
                      , bias         = bias
                      , sigma        = sigma
                      , lsq.lnlike   = lsq.lnlike
                      , mse          = mse
                      , rmse         = rmse
                      , ss.tot       = ss.tot
                      , ss.err       = ss.err
                      , r.squared    = r.squared
                      , fvue         = fvue
                      , sw.statistic = sw.statistic
                      , sw.p.value   = sw.p.value
                      , ks.statistic = ks.statistic
                      , ks.p.value   = ks.p.value
                      )#end list
   }else{
      ans = list ( n            = n.ok 
                 , p            = n.ok - df.err
                 , df.tot       = df.tot
                 , df.err       = df.err
                 , obs.moment   = obs.moment
                 , mod.moment   = mod.moment
                 , res.moment   = res.moment
                 , bias         = bias
                 , sigma        = sigma
                 , lsq.lnlike   = lsq.lnlike
                 , mse          = mse
                 , rmse         = rmse
                 , ss.tot       = ss.tot
                 , ss.err       = ss.err
                 , r.squared    = r.squared
                 , fvue         = fvue
                 , sw.statistic = sw.statistic
                 , sw.p.value   = sw.p.value
                 , ks.statistic = ks.statistic
                 , ks.p.value   = ks.p.value
                 )#end list
   }#end if
   return(ans)
   #---------------------------------------------------------------------------------------#
}#end function
#==========================================================================================#
#==========================================================================================#
