%%Exercise 3b
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wh0=load('atlantic.txt'); %read wave height from data file
n=length(wh0);
gamb_est0=est_gumbel(wh0); %get mu and beta from est_gumbel.m
beta0=gamb_est0(1); %beta of data set
mu0=gamb_est0(2); %mu of data set
u=rand(n,1); %n samples of between 0-1
wh1=mu0-beta0*log(-log(u)); %wave height from distribution
qqplot(wh0,wh1); 

%%Exercise 3c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rng(123)
B=10000;
beta=zeros(B,1);
mu=zeros(B,1);
for i=1:B
   u=rand(n,1);
   wh=mu0-beta0*log(-log(u));
   gamb_est=est_gumbel(wh);
   beta(i)=gamb_est(1);
   mu(i)=gamb_est(2);
end
CI_beta=[quantile(beta,0.025),quantile(beta,0.975)]
CI_mu=[quantile(mu,0.025),quantile(mu,0.975)]

%%Exercise 3d - 100 year wave. Use mu and beta from 3c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
T=4200;
h100=zeros(B,1);
for i=1:B
   h100(i)=mu(i)-beta(i)*log(-log(1-1/T));
end
CI_h100=[quantile(h100,0.025),quantile(h100,0.975)] %95% conf interval for 100 year wave

%%Exercise 3e - Check 99% CI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
I_h100=[quantile(h100,0.005),quantile(h100,0.995)]

%%Exercise 3f - Non-parametric Gumbel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rng(123)
B=10000;
for i=1:B
   wh=randsample(wh0,n,true); %n sample from atlantic data with replacement
   gamb_est=est_gumbel(wh); %get mu and beta with est_gumbel.m
   beta(i)=gamb_est(1);
   mu(i)=gamb_est(2);
end
CI_beta=[quantile(beta,0.025),quantile(beta,0.975)]
CI_mu=[quantile(mu,0.025),quantile(mu,0.975)]
