# Set up MegaLMM priors
# runID: name for directory where "useful information will be stored"
# whichNAmap: somewhat arbitrary map to cut out missing line by environments
# If envCov is not null, that means you want to do CV0 or CV00.  It should
# contain: newEnv = vector of names of new environments
# X_Env = matrix with covariates for ALL environments (both old and new)
# X_Env_groups = vector of designations of sets of covariates
setupMegaLMMstate <- function(accNames, wideData, kinMat, envCov=NULL,
                              runID="tempMegaLMMrun", whichNAmap=6){

  # Make some new matrices if you are doing CV0 or CV00
  # That is, making predictions for new environments
  if (!is.null(envCov)){
    oldEnv <- setdiff(colnames(wideData), envCov$newEnv)
    envCov$X_Env_Test <- envCov$X_Env[envCov$newEnv,,drop=FALSE]
    envCov$X_Env_Train <- envCov$X_Env[oldEnv,,drop=FALSE]
    wideData <- wideData[, oldEnv]
  }

  run_parameters <- MegaLMM_control(
    h2_divisions = 20,
    # Each variance component is allowed to explain between 0% and 100% of the
    # total variation. How many segments should the range [0,100) be divided
    # into for each random effect?
    burn = 0,
    # number of burn in samples before saving posterior samples. I set this to
    # zero and instead run the chain in small chunks, doing the burning manually
    # as described below.
    thin = 2,
    # during sampling, we'll save every 2nd sample to the posterior database.
    K = 10 # number of factors.
  )

  # I'm unclear whether there needs to be a main environment effect.
  # Initially, I will assume not
  MegaLMM_state = setup_model_MegaLMM(
    Y = wideData,
    # The n x p trait matrix
    formula = ~ (1|germplasmName),
    # This is syntax like lme4 for mixed effect models.
    # Again, assuming the fixed effect for environment is implicit
    # a random effect for genotype (germplasmName)
    data = accNames,
    # the data.frame with information for constructing the model matrices
    relmat = list(germplasmName = kinMat),
    # A list of covariance matrices to link to the random effects in formula.
    # each grouping variable in formula can be linked to a covariance matrix.
    # If so, every level of the grouping variable must be in the rownames of K.
    # additional rows of K not present in data will still be predicted
    # (and therefore will use memory and computational time!)
    run_parameters=run_parameters,
    # This list of control parameters created above
    run_ID = runID
    # A run identifier. The function will create a folder with this name
    # and store lots of useful data inside it
  )

  Lambda_prior <- list(
    sampler = sample_Lambda_prec_ARD,
    # function that implements the ARD Lambda prior described in
    # Runcie et al 2013. See code to see requirements for this function.
    # other options are:
    # ?sample_Lambda_prec_horseshoe
    # ?sample_Lambda_prec_BayesC
    Lambda_df = 3,
    delta_1   = list(shape = 2, rate = 1),
    delta_2   = list(shape = 3, rate = 1),
    # parameters of the gamma distribution giving the expected change in
    # proportion of non-zero loadings in each consecutive factor
    delta_iterations_factor = 100
    # parameter that affects mixing of the MCMC sampler. This value is generally fine.
  )
  # More prior parameters for Lambda
  if(!is.null(envCov)){ # There are environmental covariates to predict Lambda
    Lambda_prior = c(Lambda_prior, list(
      X = envCov$X_Env_Train,
      X_group = envCov$X_Env_groups,
      fit_X = FALSE,  # Start by letting Lambda converge without X
      # but then turn it on during burnins.
      Lambda_beta_var_shape = 3,
      Lambda_beta_var_rate = 1
    ))
  }

  priors <- MegaLMM_priors(
    tot_Y_var = list(V = 0.5,   nu = 5),
    # Prior variance of trait residuals after accounting for fixed effects and factors
    # See MCMCglmm for meaning of V and nu
    tot_F_var = list(V = 18/20, nu = 20),
    # Prior variance of factor traits. This is included to improve MCMC mixing,
    # but can be turned off by setting nu very large
    h2_priors_resids_fun = function(h2s, n)  1,
    # Function that returns the prior density for any value of the h2s vector
    # (ie the vector of random effect proportional variances across all random effects.
    # 1 means constant prior.
    # n is the number of h2 divisions above (here=20)
    # 1-n*sum(h2s)/n linearly interpolates between 1 and 0,
    # giving more weight to lower values
    h2_priors_factors_fun = function(h2s,n) 1,
    # See above.
    # sum(h2s) linearly interpolates between 0 and 1,
    # giving more weight to higher values
    # Another choice is one that gives 50% weight to h2==0: ifelse(h2s == 0,n,n/(n-1))
    Lambda_prior = Lambda_prior
    # from above
  )

  MegaLMM_state <- set_priors_MegaLMM(MegaLMM_state, priors)
  # I need to get X_Env_Test in there where it can be used to calculate
  # posterior samples.  I think $priors is an environment that is used
  MegaLMM_state$priors$X_Env_Test <- envCov$X_Env_Test

  # WARNING: I don't know how to deal with this in an automated way.
  # This is arbitrary
  maps <- make_Missing_data_map(MegaLMM_state,
                                max_NA_groups = ncol(yldWide) + 1,
                                verbose=F)
  MegaLMM_state <- set_Missing_data_map(MegaLMM_state,
                                        maps$Missing_data_map_list[[whichNAmap]])

  # Pre-calculate useful matrices
  MegaLMM_state <- initialize_variables_MegaLMM(MegaLMM_state)
  MegaLMM_state <- initialize_MegaLMM(MegaLMM_state,verbose=F)

  # Set up the posterior parameters you want to keep
  MegaLMM_state$Posterior$posteriorSample_params <-
    c('Lambda','F_h2','resid_h2','tot_Eta_prec','B1')
  if (!is.null(envCov)){
    MegaLMM_state$Posterior$posteriorSample_params <-
      c(MegaLMM_state$Posterior$posteriorSample_params,
        c('U_F','F','B2_F','Lambda_beta','Lambda_beta_var'))
  }

  # `Eta_mean` is the internal parameter for the predicted phenotypic value `Y`.
  MegaLMM_state$Posterior$posteriorMean_params <- 'Eta_mean'

  # 26 Oct 2025 I'm setting this up assuming an IID kinMat so no U_cond
  # No possibility for CV1 or CV00
  MegaLMM_state$Posterior$posteriorFunctions <- list(
    U_CV2noU_R = 'U_F %*% Lambda', # U_R might be more noise than signal
    U_CV2 = 'U_F %*% Lambda + U_R', # + X1 %*% B1', # Why should X1 %*% B1 be here?
    G = 't(Lambda) %*% diag(F_h2[1,]) %*% Lambda + diag(resid_h2[1,]/tot_Eta_prec[1,])',
    R = 't(Lambda) %*% diag(1-F_h2[1,]) %*% Lambda + diag((1-resid_h2[1,])/tot_Eta_prec[1,])',
    h2 = '(colSums(F_h2[1,]*Lambda^2)+resid_h2[1,]/tot_Eta_prec[1,])/(colSums(Lambda^2)+1/tot_Eta_prec[1,])'
  )
  if (!is.null(envCov)){
    MegaLMM_state$Posterior$posteriorFunctions <-
      c(MegaLMM_state$Posterior$posteriorFunctions,
        list(
          U_CV0 = 'U_F %*% Lambda_beta %*% t(X_Env_Test)',
          Eta_CV0 = 'F %*% Lambda_beta %*% t(X_Env_Test)'
        ))
  }

  MegaLMM_state <- clear_Posterior(MegaLMM_state)

  return(MegaLMM_state)
}
