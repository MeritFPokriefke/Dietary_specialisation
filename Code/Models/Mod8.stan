data {
  int<lower=0> N_B;       // number of broods
  int<lower=0> N_z;       // number of phenotype observations
  int<lower=0> N_w;       // number of fitness observations

  #int<lower=1> N_spy_z;   // number of SubplotYear levels z
  int<lower=1> N_spy_w;   // number of SubplotYear levels w
  int<lower=1> N_wnr;   // number of SubplotYear levels w
  
  int<lower=1> b_z[N_z];      // brood ID for phenotype
  int<lower=1> b_w[N_w];      // brood IDs for fitness
  #int<lower=1> spy_z[N_z];    // SubplotYear ID
  int<lower=1> spy_w[N_w];    // SubplotYear IDs for fitness
  int<lower=1> wnr[N_z];    // SubplotYear IDs for fitness  


  vector[N_z] z;      // phenotype scores
  #vector[N_z] TMB;   // tm b
  #vector[N_z] TMG;   // tm g
  #vector[N_z] Box;   // neighbour box
  #vector[N_z] Y2024; // year
 

  vector[N_w] w;   // fitness response (sumweight of a brood)
  vector[N_w] TMB_w;    // Treatment blue tit
  vector[N_w] TMG_w;    // Treatment great tit
  vector[N_w] Box_w;    // neighbour box
  vector[N_w] Y2024_w;  // Year 2024
  vector[N_w] time_w;   // time after sunrise weight was taken
  vector[N_w] size_w;   // brood size (number of nestlings)
}


#################################################################################################################


// scale y variable w 
transformed data {
  real w_mean = mean(w);
  real w_sd = sd(w);

  vector[N_w] w_s;  for (n in 1:N_w) {
    w_s[n] = (w[n] - w_mean) / w_sd;
  }
}

#################################################################################################################

parameters {
  // phenotype (z)
  real beta_0;
  #vector[2] be;
  #vector[1] be_int;
  
  vector[N_B] B_s;
  real<lower=0> sigma_B;

  #vector[N_spy_z] spy_s;
  #real<lower=0> sigma_spy;
  
  vector[N_wnr] wnr_s;
  real<lower=0> sigma_wnr;
  
  real<lower=0, upper=1> pi;   // zero-inflation probability
  real<lower=0> phi;           // beta precision


  // fitness (w)
  real beta_0_w;             // intercept
  vector[7] b;               // fixed effects
  vector[4] b_int;           // interactions

  vector[N_B] B_w_s;         // brood RE
  real<lower=0> sigma_B_w;
  
  vector[N_spy_w] spy_w_s;
  real<lower=0> sigma_spy_w;
  
  real<lower=0> sigma_e_w; // residual variance
  
  
}

#################################################################################################################

transformed parameters {
  vector[N_B]       B = B_s * sigma_B;
  #vector[N_spy_z] SPY = spy_s * sigma_spy;
  vector[N_wnr] WNR = wnr_s * sigma_wnr;

  vector[N_B]       B_w = B_w_s * sigma_B_w;
  vector[N_spy_w] SPY_w = spy_w_s * sigma_spy_w;
}

#################################################################################################################

model {
  // priors phenotype
  beta_0 ~ normal(0, 1);
  #be ~ normal(0, 1);
  #be_int ~ normal(0, 1);
  B_s ~ normal(0, 1);
  sigma_B ~ student_t(3, 0, 1); # (0,0.5) best
  #spy_s ~ normal(0, 1);
  #sigma_spy ~ normal(0, 1);
  wnr_s ~ normal(0, 1);
  sigma_wnr ~ normal(0, 1);
  pi ~ beta(2,2);
  phi ~ gamma(2,0.5);

  // priors fitness
  beta_0_w ~ normal(0,1);
  b ~ normal(0,1);
  b_int ~ normal(0,1);
  B_w_s ~ normal(0,1);
  sigma_B_w ~ normal(0,1);
  spy_w_s ~ normal(0,1);
  sigma_spy_w ~ normal(0,1);
  sigma_e_w ~ normal (0,1);


  // Likelihood phenotype
for (n in 1:N_z) {
  real eta_mu = beta_0 +
                #be[1] .* TMB[n] + 
                #be[2] .* TMG[n] + 
                #be[3] .* Box[n] + 
                #be[4] .* Y2024[n] +
                #be_int[1] .* TMB[n] * TMG[n] +
                B[b_z[n]] + 
                #SPY[spy_z[n]] +
                WNR[wnr[n]];

  real mu_i = inv_logit(eta_mu);
  real mu_i_safe = fmin(fmax(mu_i, 1e-12), 1 - 1e-12);

  real z_adj = fmin(fmax(z[n], 1e-12), 1 - 1e-12);

  if (z[n] == 0) {
    target += log1m(pi);  // zero part of hurdle
  } else {
    target += log(pi) + beta_lpdf(z_adj | mu_i_safe * phi, (1 - mu_i_safe) * phi);
  }
}

  // Likelihood fitness (w)
for (i in 1:N_w) {
  real mu_w = beta_0_w +
               b[1] .* B[b_w[i]] +
               b[2] .* TMB_w[i] +
               b[3] .* TMG_w[i] +
               b[4] .* Box_w[i] +
               b[5] .* Y2024_w[i] +
               b[6] .* time_w[i] +
               b[7] .* size_w[i] +
               b_int[1] .* TMB_w[i] .* B[b_w[i]]  +
               b_int[2] .* B[b_w[i]] .* TMG_w[i] +
               b_int[3] .* TMB_w[i] .* TMG_w[i] +
               b_int[4] .* B[b_w[i]] .* TMB_w[i] .* TMG_w[i] +
               B_w[b_w[i]] +
               SPY_w[spy_w[i]];
  w_s[i] ~ normal(mu_w, sigma_e_w);
}
}

#################################################################################################################

generated quantities {
///// Phenotype variance components

vector[N_z] mu_i_vec;
vector[N_z] var_resid_z;

real V_brood = sigma_B^2;
#real V_spy   = sigma_spy^2;
real V_wnr   = sigma_wnr^2;


// intercept
real beta_0_prop = inv_logit(beta_0);  // back-transform the fixed intercept

for (n in 1:N_z) {
  real eta_mu = beta_0 +
                #be[1] .* TMB[n] + 
                #be[2] .* TMG[n] + 
                #be[3] .* Box[n] + 
                #be[4] .* Y2024[n] +
                #be_int[1] .* TMB[n] * TMG[n] +
                B[b_z[n]] + 
                #SPY[spy_z[n]] +
                WNR[wnr[n]];

  real mu_i = inv_logit(eta_mu);
  mu_i_vec[n] = mu_i;

  // residual variance for beta hurdle
  var_resid_z[n] = pi * mu_i * (1 - mu_i) / (1 + phi) +  // beta variance
                   pi * (1 - pi) * square(mu_i);          // zero-inflation contribution
}

real mean_z = mean(mu_i_vec);
real mean_z_zi = pi * mean_z;
real V_e = mean(var_resid_z);

// total variance including random effects
real V_total_z =  V_brood + V_wnr + V_e;#V_spy +

#real VC_spy     = V_spy  / V_total_z;
real VC_brood   = V_brood / V_total_z;
real VC_wnr     = V_wnr   / V_total_z;
real VC_e       = V_e     / V_total_z;

  
// true diet calc  
  vector[N_B] true_diets;
  for (n in 1:N_B) true_diets[n] = inv_logit(B[n] + beta_0);  


///// Backtransform fitness (w)
real beta_0_w_u = beta_0_w * w_sd + w_mean; // intercept
vector[7] b_u;                              // fixed effects
for (i in 1:7) {
  b_u[i] = b[i] * w_sd;
}
vector[4] b_int_u;                         // interactions
for (i in 1:4) { 
  b_int_u[i] = b_int[i] * w_sd;
}
  
// Variance components for fitness (w)
real V_spy_w   = sigma_spy_w^2;
real V_brood_w = sigma_B_w^2; 
real V_e_w = sigma_e_w^2;


real V_total_w = V_spy_w  + V_e_w + V_brood_w;

real VC_spy_w   = V_spy_w   / V_total_w;
real VC_brood_w = V_brood_w / V_total_w;
real VC_e_w     = V_e_w     / V_total_w;


// treatment effects (back-transformed to response scale)
real estimate1 = beta_0_w_u;                              // TMB=0, TMG=0
real estimate2 = beta_0_w_u + b_u[3];                       // TMB=0, TMG=1
real estimate3 = beta_0_w_u + b_u[2];                       // TMB=1, TMG=0
real estimate4 = beta_0_w_u + b_u[2] + b_u[3] + b_int_u[3];     // TMB=1, TMG=1

// Main effects
real bluetit_effect  = (estimate3 + estimate4)/2 - (estimate1 + estimate2)/2;
real greattit_effect = (estimate2 + estimate4)/2 - (estimate1 + estimate3)/2;

// Interaction effect
//real interaction_effect = estimate4 - (estimate2 + estimate3 - estimate1);  

// calc selec gradients for ppp                           
real sel_1;
real sel_2;
real sel_3;
real sel_4;

real sel_bluetit_effect;
real sel_greattit_effect;

real mean_w = mean(w);

// estimate slopes and divide by sd of brood for z stadardisation of trait
real slope_1 = b_u[1]/sigma_B;
real slope_2 = (b_u[1] + b_int_u[2])/sigma_B;
real slope_3 = (b_u[1] + b_int_u[1])/sigma_B;
real slope_4 = (b_u[1] + b_int_u[1] + b_int_u[2] + b_int_u[4])/sigma_B;


// divide by mean fitness
sel_1 = slope_1 / mean_w;
sel_2 = slope_2 / mean_w;
sel_3 = slope_3 / mean_w;
sel_4 = slope_4 / mean_w;


// Marginal ppp effects
sel_bluetit_effect  = (sel_3 + sel_4) / 2 - (sel_1 + sel_2) / 2;

sel_greattit_effect = (sel_2 + sel_4) / 2 - (sel_1 + sel_3) / 2;
 
}
