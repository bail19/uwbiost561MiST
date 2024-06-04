#' Implementation of set-based mixed effects score tests via \emph{MiST}.
#'
#' @param data A dataframe \code{[Y,X,G]} with rows represent subjects and columns are \code{Y} (outcome), \code{X} (confounders), and \code{G} (genotypes).
#' @param d Number of confounders. If \code{d} is 0, there is no confounder, and the input data should be arranged as \code{[Y G]}.
#' @param p Number of genetic variants in the set.
#' @param R Number of burden scores (R>=0). When \code{R} is 0, only the variance component test is performed and more than one variants are required in this situation.
#' @param outcome_type Either "Continuous" for quantitative trait, or "Binary" for dichotomous trait.
#' @param weight_method Specifying weights for calculating weighted burden scores. It takes "User" or "No weight". "User"(default) allows user-defined weights for each variant (e.g., functional annotations and equal weights), which are specified by the input option "user_weight". "No weight" sets all weights = 0 and tests only the variance component.
#' @param user_weight A vector or a matrix specifying weights for calculating weighted burden scores. This option only works when the weight_method is set as "User". If the weight_method is set as "User"" and no user_weight is specified, the weight is set as 1.
#' @param burden_scale A logical value (default: TRUE) specifying if standardization on burden components needs to be carried out when there are more than 1 burden score.
#' @param chisq_app Either "3M" (default) or "4M" for the moment matching (Liu's) method in the quantile approximation in the optimal linear combination of burden and variance components. "3M" matches the 3rd moment and "4M" matches the 4th moment of the target and approximate distributions.
#' @param combinations_return A logical value with TRUE (default) indicating that the tests for combining the burden and variance components are performed. The combined methods include the optimal linear combination, data-adpative weighted combination, and Fisher's combination. If it is FALSE, only the burden and variance component tests are returned.
#' @param combination_preference Either of "All" (default), or a vector containing "OptMin", "AdptWt", or "Fisher" to specify the combination method(s).
#' @param acc A numerical value indicating the precision of the Davies method for p-value calculation. Default is 5e-10.
#' @param acc_auto A logical value (default: TRUE) indicating if data adaptive precision is used in optimal linear combination. We recommend to set this as TRUE for computational efficiency.
#' @param accurate_app_threshold A numerical value specifying the threshold to determine when the Liu or Davies method is used in the quantile approximation in the optimal linear combination of burden and variance components. Default is -log10(0.05).
#' @param max_core An integer specifying the maximum number of cores that can be recruited in the parallel package. Default is 4 cores.
#'
#' @return A list containing \code{pvalue}, \code{stat}, \code{pvalue.ind.f}, \code{stat.ind.f}, \code{rho}, and \code{data.info}.
#' Specifically, \code{pvalue} are obtained from the burden and variance components tests, as well as three combination tests including optimal linear combination \emph{oMiST}, data-adaptive weighted combination \emph{aMiST}, and Fiser's combination \emph{fMiST}.
#' \code{stat} are the corresponding test statistics.
#' \code{pvalue.ind.f} and \code{stat.ind.f} are p-values and test statistics of individual burden scores if multiple weights are included.
#' \code{rho} is the optimal weight of the burden and random-effects components in \emph{oMiST} and \emph{aMiST}.
#' \code{data.info} includes the numbers of genetic variants and burden scores.
#' @export
#'
#' @importFrom stats pchisq qchisq dchisq rchisq var integrate glm lm optimize
#' @import CompQuadForm
#' @import parallel
#' @importFrom stats coef fitted residuals uniroot
#'
#' @examples
#' # Data generation
#' n = 2000
#' set.seed(1234)
#' X = rbinom(n,size=1,prob=0.5)
#' MAF = runif(10,min=0.001,max=0.01)
#' G = sapply(MAF,function(maf) rbinom(n,size=1,prob=maf))
#' eta = X*0.5 + G%*%rnorm(10,mean=0,sd=0.5)
#' Y = rbinom(n,size=1,prob=exp(eta)/(1+exp(eta)))
#' d = 1
#' p = 10
#'
#' data = data.frame(Y=Y, X=X,G=G)
#' mist = MiST(data = data,
#'          outcome_type = "Binary",
#'          d = d,
#'          p = p,
#'          R = 1,
#'          weight_method = "User")
#' mist
MiST = function(data,d = 0,p,R=1,
                outcome_type="Continuous",
                weight_method= "User", # "No weight",
                user_weight = rep(1,p),
                burden_scale = TRUE,
                chisq_app = "3M",
                combinations_return=TRUE,
                combination_preference = "All", # other options: "OptMin", "AdptWt", "Fisher"
                acc = 5e-10,
                acc_auto = TRUE, # if "TRUE": acc is determined by the magnitute of min_pvalue in OptMin
                accurate_app_threshold = -log10(0.05), # if the min_pvalue<0.05, the accurate approximation on min_quantile in OptMin combination is used instead of approximation with Liu's method.
                max_core = 4
){

  if(p==0) stop("There must be at least 1 varant in the set to perform MiST.")
  if(p==1 & weight_method=="No weight") stop("Variance component test requires p>=2.")
  if(p==1 & weight_method=="User") warning("Variance component test requires p>=2. Only burden component test is implemented and reported.")

  if(!outcome_type%in%c("Continuous","Binary")) stop("outcome_type needs to be either Continuous or Binary")
  if(weight_method=="No weight"){
    wt = rep(0,p)
    R = 0
  }
  else if(weight_method=="ScrnStat1" | weight_method=="ScrnStat2") wt = Weight_ScrnStat(data,d=0,p,method=weight_method)
  else if(weight_method=="User"){
    if(length(user_weight) %/% p == R & length(user_weight) %% p ==0){
      user_weight = as.matrix(user_weight)
      if(all(dim(user_weight)==c(p,R))) wt = user_weight
      else stop("user_weight needs to be either a vector of length p or a p*R matrix if R>1")
    }
    else stop("user_weight needs to be either a vector of length p or a p*R matrix if R>1")
  }
  else stop("weight needs to be one of the following: No weight, ScrnStat1, ScrnStat2, or User")

  if(all(wt==0) & p==1) stop("There is only 1 variant left in the set with given weight as 0. MiST can not be performed uder this situation. Please use non-zero weights for the set of size 1.")

  n = nrow(data)
  pvalue = c(pvalue.f=NA,pvalue.r=NA,pvalue.oMiST=NA,pvalue.aMiST=NA,pvalue.fMiST=NA)
  stat = c(stat.f=NA,stat.r=NA,stat.oMiST=NA,stat.aMiST=NA,stat.fMiST=NA)
  pvalue.f.ind = NA
  stat.f.ind = NA
  rho = c(rho.oMiST.f=NA,rho.oMiST.r=NA,rho.aMiST.f=NA,rho.aMiST.r = NA)

  if(d==0){
    MainCov.f = as.matrix(rep(1,n))
    burden = crossprod(t(data[,2:(2+p-1)]),wt) # produce a n*R matrix
    if(burden_scale==TRUE) S.f = scale(as.matrix(burden),scale=TRUE,center=TRUE)
    else S.f = as.matrix(burden)
    if(p>1){
      if(weight_method=="No weight") MainCov.r = MainCov.f
      else MainCov.r = as.matrix(cbind(rep(1,n),S.f))
      S.r = as.matrix(data[,2:(2+p-1)])
    }

    # Fit glm under null for fixed and random component
    Null.f = NullRegressionLargeData(Data=as.data.frame(data[,1]),n=n,p=p,type=outcome_type,null.cov=NULL)
    hfit2.f = Null.f$fit2
    if(p>1){
      if(weight_method=="No weight") Null.r = Null.f
      else Null.r = NullRegressionLargeData(Data=as.data.frame(cbind(data[,1],MainCov.r[,-1])),n=n,p=p,type=outcome_type,null.cov=c(2:(2+(R-1))),nullvar=hfit2.f)
    }
  }
  else{
    MainCov.f = as.matrix(cbind(rep(1,n),data[,2:(2+d-1)]))
    burden = crossprod(t(data[,(2+d):(2+d+p-1)]),wt) # produce a n*R matrix
    if(burden_scale==TRUE) S.f = scale(as.matrix(burden),scale=TRUE,center=FALSE)
    else S.f = as.matrix(burden)
    if(p>1){
      if(weight_method=="No weight") MainCov.r = MainCov.f
      else MainCov.r = as.matrix(cbind(rep(1,n),data[,2:(2+d-1)],S.f))
      S.r = as.matrix(data[,(2+d):(2+d+p-1)])
    }
    # Fit glm under null for fixed and random component
    Null.f = NullRegressionLargeData(Data=as.data.frame(cbind(data[,1],MainCov.f[,-1])),n=n,p=p,type=outcome_type,null.cov=2:(2+d-1))
    hfit2.f = Null.f$fit2
    if(p>1){
      if(weight_method=="No weight") Null.r = Null.f
      else Null.r = NullRegressionLargeData(Data=as.data.frame(cbind(data[,1],MainCov.r[,-1])),n=n,p=p,type=outcome_type,null.cov=2:(2+d+R-1),nullvar=hfit2.f)
    }
  }

  Q.f = 0
  pvalue.f = 1
  EigenvalueD.f = 1
  stat.f.ind = NULL
  pvalue.f.ind = NULL

  if(weight_method!="No weight"){
    # Extract output from the first null model for burden component
    MainCovScale.f = Null.f$MainCov
    hOmega.f = Null.f$Omega
    hresid.f = Null.f$resid
    hfit2.f = Null.f$fit2


    # Calculate score statistics and p-value for fixed component
    halfD.f.1 = t(sqrt(hfit2.f) * S.f) # tilde(S.f)^T
    halfD.f.2 = t(sqrt(hfit2.f) * S.f) %*% (sqrt(hfit2.f) * MainCovScale.f) # tilde(S.f)^T tilde(X)
    halfD.f.3 = solve(hOmega.f) %*% t(sqrt(hfit2.f) * MainCovScale.f) # (tilde(X)^T tilde(X))^(-1) tilde(X)
    halfD.f = halfD.f.1 - halfD.f.2 %*% halfD.f.3 # q*n
    D.f = halfD.f %*% t(halfD.f) # covariance matrix of n*hat(gamma)
    Q.f = t(hresid.f) %*% S.f %*% solve(D.f) %*% t(S.f) %*% hresid.f
    EigenvalueD.f = rep(1,times=ncol(S.f))
    pvalue.f = pchisq(Q.f,df=ncol(S.f),lower.tail=FALSE)
    pvalue["pvalue.f"] = pvalue.f
    stat["stat.f"] = Q.f

    # Calculate the score stat for each individual fixed component
    if(ncol(S.f)>1){
      Q.f.ind = NULL
      res.f.ind = sapply(1:ncol(S.f),function(k){
        S.f.ind = as.matrix(S.f[,k])
        if(d==0){
          MainCov.f.ind = as.matrix(cbind(rep(1,n),S.f))
          # Fit glm under null for individual fixed component adjusted for other burden scores
          Null.f.ind = NullRegressionLargeData(Data=as.data.frame(cbind(data[,1],MainCov.f.ind[,-1])),n=n,p=p,type=outcome_type,null.cov=c((2:(2+ncol(S.f)-1))[-k]))
          hfit2.f.ind= Null.f.ind$fit2
        }
        else{
          MainCov.f.ind = as.matrix(cbind(rep(1,n),data[,2:(2+d-1)],S.f))
          # Fit glm under null for individual fixed component adjusted for other burden scores
          Null.f.ind = NullRegressionLargeData(Data=as.data.frame(cbind(data[,1],MainCov.f.ind[,-1])),n=n,p=p,type=outcome_type,null.cov=c(2:(2+d-1),((2+d):(2+d+ncol(S.f)-1))[-k]))
          hfit2.f.ind = Null.f.ind$fit2
        }
        MainCovScale.f.ind = Null.f.ind$MainCov
        hOmega.f.ind = Null.f.ind$Omega
        hresid.f.ind = Null.f.ind$resid
        hfit2.f.ind = Null.f.ind$fit2
        halfD.f.1.ind = t(sqrt(hfit2.f.ind) * S.f.ind) # q*n
        halfD.f.2.ind = t(sqrt(hfit2.f.ind) * S.f.ind) %*% (sqrt(hfit2.f.ind) * MainCovScale.f.ind) # q*q
        halfD.f.3.ind = solve(hOmega.f.ind) %*% t(sqrt(hfit2.f.ind) * MainCovScale.f.ind) # q*n
        halfD.f.ind = halfD.f.1.ind - halfD.f.2.ind %*% halfD.f.3.ind # q*n
        D.f.ind = halfD.f.ind %*% t(halfD.f.ind) # covariance matrix of n*hat(gamma)
        Q.f.ind = t(hresid.f.ind) %*% S.f.ind %*% solve(D.f.ind) %*% t(S.f.ind) %*% hresid.f.ind
        pvalue.f.ind = pchisq(Q.f.ind,df=ncol(S.f.ind),lower.tail=FALSE)
        return(list(pvalue.f.ind=pvalue.f.ind,Q.f.ind=Q.f.ind))
      })
      pvalue.f.ind = unlist(res.f.ind["pvalue.f.ind",])
      stat.f.ind = unlist(res.f.ind["Q.f.ind",])
    }
  }

  if(p>1){
    # Extract output from the second null model with burden
    MainCovScale.r = Null.r$MainCov
    hOmega.r = Null.r$Omega
    hresid.r = Null.r$resid
    hfit2.r = Null.r$fit2
    hfit2Inv.r = 1/hfit2.r
    hfit2Inv.r[hfit2Inv.r==Inf] = 0

    # Calculate the score stat for the random components
    halfQ.r = t(hresid.r) %*% S.r
    Q.r = halfQ.r %*% t(halfQ.r)
    # Q.r = t(hresid.r) %*% S.r %*% t(S.r) %*% hresid.r # score stat for random component

    # Calculate p-value for random component
    halfD.r.1 = t(sqrt(hfit2.r) * S.r) # p*n
    halfD.r.2 = t(sqrt(hfit2.r) * S.r) %*% (sqrt(hfit2.r)*MainCovScale.r) # p*(d+R)
    halfD.r.3 = solve(hOmega.r) %*% t(sqrt(hfit2.r) * MainCovScale.r) # (d+R)*n
    halfD.r = halfD.r.1 - halfD.r.2 %*% halfD.r.3 # p*n
    D.r = halfD.r %*% t(halfD.r) # covariance matrix of n*hat(gamma)
    EigenvalueD.r = eigen(D.r)$values
    pvalue.r = CompQuadForm::davies(q=Q.r,lambda=EigenvalueD.r,acc=acc, lim=1/acc)$Qq
    if(pvalue.r<=acc) pvalue.r = pvalue.liu(Q=Q.r,weight=EigenvalueD.r,method=chisq_app)$pvalue
    pvalue["pvalue.r"] = pvalue.r
    stat["stat.r"] = Q.r

    if(weight_method!="No weight"){

      if(combinations_return & (("All" %in% combination_preference) | ("OptMin" %in% combination_preference))){
        # OptMin method - Calculate the optimal weight for linear combinations of Q.f and Q.r by minimizing the pvalues
        p_rho = function(rho,df_f,lambda_r,u_f,u_r,acc=5e-10){
          pvalue = CompQuadForm::davies(q=rho*u_f+(1-rho)*u_r,
                                        lambda=c(rho*rep(1,df_f),(1-rho)*lambda_r),acc=acc,lim=1/acc)$Qq
          # Note on 2017/2/6: previous version doesn't include acc and lim input here and it causes the returned pvalue >1 in some simulation runs. Caused error in the following calculation of quantiles based on returned pvalues. acc and lim are added to control this.
          if(pvalue<=acc) pvalue = pvalue.liu(Q=rho*u_f+(1-rho)*u_r,weight=c(rho*rep(1,df_f),(1-rho)*lambda_r),method=chisq_app)$pvalue
          return(pvalue)
        }
        rho_min_pvalue_inside01 = optimize(p_rho,interval=c(0,1),maximum=FALSE,df_f=ncol(S.f),lambda_r=EigenvalueD.r,u_f=Q.f,u_r=Q.r,acc=acc)$minimum
        min_pvalue_inside01 = optimize(p_rho,interval=c(0,1),maximum=FALSE,df_f=ncol(S.f),lambda_r=EigenvalueD.r,u_f=Q.f,u_r=Q.r,acc=acc)$objective
        ### Note: optimize function does NOT include the comparison at the endpoints of the interval. Add comparison with p_values at 0 and 1 manually below
        if(min_pvalue_inside01>pvalue.r | min_pvalue_inside01>pvalue.f){
          rho_min_pvalue = ifelse(pvalue.r<pvalue.f,0,1)
          min_pvalue = ifelse(pvalue.r<pvalue.f,pvalue.r,pvalue.f)
        }
        else{
          rho_min_pvalue = rho_min_pvalue_inside01
          min_pvalue = min_pvalue_inside01
        }

        Q.OptMin = Q.f*rho_min_pvalue + Q.r*(1-rho_min_pvalue)

        if(acc_auto==TRUE){
          acc_supp = max(min(min_pvalue,0.01)*0.1,acc)
          lim_supp = as.integer(1/acc_supp)
        }
        else{
          acc_supp = acc
          lim_supp = as.integer(1/acc)
        }

        if(-log10(min_pvalue)>accurate_app_threshold){
          # quan_grid = quan_rho_calculator(grid=seq(0,1,by=0.05),df_f=ncol(S.f),lambda_r=EigenvalueD.r,min_pvalue=min_pvalue,acc=acc_supp,lim=lim_supp,max_core=max_core)
          pvalue.OptMin = pvalue_minimizePValue_davies(u_f=Q.f,u_r=Q.r,df_f=ncol(S.f),lambda_r=EigenvalueD.r,min_pvalue=min_pvalue,acc=acc,ChisqApp=chisq_app,max_core=max_core)
        }
        else pvalue.OptMin = pvalue_minimizePValue_liu(u_f=Q.f,u_r=Q.r,df_f=ncol(S.f),lambda_r=EigenvalueD.r,min_pvalue=min_pvalue,acc=acc,ChisqApp=chisq_app)
        pvalue["pvalue.oMiST"] = pvalue.OptMin
        stat["stat.oMiST"] = Q.OptMin
        rho["rho.oMiST.f"] = rho_min_pvalue
        rho["rho.oMiST.r"] = 1-rho_min_pvalue
      }

      if(combinations_return & (("All" %in% combination_preference) | ("AdptWt" %in% combination_preference))){
        # Adaptive weighted combination
        Z.f = -2*log(pvalue.f)
        if(is.null(log(pvalue.r))) Z.r = -2*log(1e-16)
        else Z.r = -2*log(pvalue.r)
        esti.weight.f = Z.f/sqrt(Z.f^2+Z.r^2)
        esti.weight.r = Z.r/sqrt(Z.f^2+Z.r^2)
        Q.AdptWt = sqrt(Z.f^2+Z.r^2)[1,] # [1,] is added to avoid operation problems. It guarantees it's a value.
        integrand = function(y){
          pchisq(sqrt(Q.AdptWt^2-y^2),df=2,lower.tail=FALSE)*dchisq(y,df=2)
        }
        pvalue.AdptWt = pchisq(Q.AdptWt,df=2,lower.tail=FALSE)+integrate(integrand,lower=0,upper=Q.AdptWt)$value
        pvalue["pvalue.aMiST"] = pvalue.AdptWt
        stat["stat.aMiST"] = Q.AdptWt
        rho["rho.aMiST.f"] = esti.weight.f
        rho["rho.aMiST.r"] = esti.weight.r
      }

      if(combinations_return & (("All" %in% combination_preference) | ("Fisher" %in% combination_preference))){
        # Fisher's combination
        Q.Fisher = -2*log(pvalue.f)-2*log(pvalue.r)
        pvalue.Fisher = pchisq(as.numeric(Q.Fisher),df=4,lower.tail=FALSE)
        pvalue["pvalue.fMiST"] = pvalue.Fisher
        stat["stat.fMiST"] = Q.Fisher
      }
    }
  }

  return(list(stat=stat,
              pvalue=pvalue,
              stat.f.ind=stat.f.ind,
              pvalue.f.ind=pvalue.f.ind,
              rho=rho,
              data.info=c(p=p,R=R)
  ))
}
