data {
  int<lower=0> N_w;       // number of RRA bird
  int<lower=1> J;         // number of families
  
  int<lower=1> N_brood_w; // number of broodlevels w
  int<lower=0> N_wnr_w;   // number of within nest rep levels 
  int<lower=1> N_spy_w;   // number of SPY
  int<lower=1> N_sp_w;    // number of SPY
  
  int<lower=1> brood_w[N_w]; // Brood ID
  int<lower=0> wnr_w[N_w];   // wnr IDs 
  int<lower=1> spy_w[N_w];   // wnr IDs 
  int<lower=1> sp_w[N_w];    // wnr IDs 

  
  matrix[N_w, J] w;        // RRA birds, each column = family
  
  
  vector[N_w] TMB;       // Treatment blue tit
  vector[N_w] TMG;       // Treatment great tit
  vector[N_w] Size;      // size subplot
  vector[N_w] Box;       // double box
  vector[N_w] TypeAM;    // Type AM
  vector[N_w] TypeC10;   // Type C10
  vector[N_w] TypeC15;   // Type C15
  vector[N_w] Species;   // Species
  vector[N_w] Date_w;    // Date sample
  vector[N_w] Y2024;     // Year 2024

}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

parameters {

  // Regression parameters for RRA
  vector[J] beta_0w;                // intercept for RRA
  matrix[J, 10] bw;                 // beta fixed effects
  matrix[J, 1] bw_int;              // betas interactions
  vector<lower=0, upper=1>[J] pi_w; // prob non-zero 
  vector<lower=0>[J] phi_w;         // beta precision
  
  
  //Random effects

  matrix[N_brood_w, J] brood_w_s;
  vector<lower=0>[J] sigma_brood_w;

  matrix[N_wnr_w, J] wnr_w_s;
  vector<lower=0>[J] sigma_wnr_w;

  matrix[N_spy_w, J] spy_w_s;
  vector<lower=0>[J] sigma_spy_w;
  
  matrix[N_sp_w, J] sp_w_s;
  vector<lower=0>[J] sigma_sp_w;
  
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

transformed parameters {
  matrix[N_brood_w, J] Brood_w = diag_post_multiply(brood_w_s, sigma_brood_w);
  matrix[N_wnr_w, J]   WNR_w   = diag_post_multiply(wnr_w_s, sigma_wnr_w);
  matrix[N_spy_w, J]   SPY_w   = diag_post_multiply(spy_w_s, sigma_spy_w);
  matrix[N_sp_w, J]    SP_w    = diag_post_multiply(sp_w_s, sigma_sp_w);
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

model {
  // Priors

beta_0w ~ normal(0, 1);
to_vector(bw) ~ normal(0, 1);
to_vector(bw_int) ~ normal(0, 1);

to_vector(brood_w_s) ~ normal(0, 1);
sigma_brood_w ~ normal(0, 1);

to_vector(wnr_w_s) ~ normal(0, 1);
sigma_wnr_w ~ normal(0, 1);

to_vector(spy_w_s) ~ normal(0, 1);
sigma_spy_w ~ normal(0, 1);

to_vector(sp_w_s) ~ normal(0, 1);
sigma_sp_w ~ normal(0, 1);

pi_w ~ beta(2, 2);
phi_w ~ gamma(2, 0.5);
  

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Likelihood for RRA (zero-inflated Beta, hurdle-style)

// Likelihood: zero-inflated beta hurdle RRA (w) across families
for (j in 1:J) {                          // loop over families
  for (i in 1:N_w) {
    // Linear predictor for beta mean
    real eta_mu = beta_0w[j] + 
                  bw[j,1]  .* TMB[i] + 
                  bw[j,2]  .* TMG[i] +
                  bw[j,3]  .* Size[i] +
                  bw[j,4]  .* Box[i] +
                  bw[j,5]  .* TypeAM[i] +
                  bw[j,6]  .* TypeC10[i] +
                  bw[j,7]  .* TypeC15[i] +
                  bw[j,8]  .* Date_w[i] +
                  bw[j,9]  .* Y2024[i] +
                  bw[j,10]  .* Species[i] +
                  bw_int[j,1] .* TMB[i] .* TMG[i] +
                  Brood_w[brood_w[i], j] +
                  WNR_w[wnr_w[i], j] +
                  SPY_w[spy_w[i], j] +
                  SP_w[sp_w[i], j];

    real mu_i      = inv_logit(eta_mu);               // map to (0,1)
    real mu_i_safe = fmin(fmax(mu_i, 1e-12), 1 - 1e-12);
    real w_adj     = fmin(fmax(w[i, j], 1e-12), 1 - 1e-12);

    // family-specific zero-inflation probability
    real pi_i = pi_w[j];  

    if (w[i, j] == 0) {
      target += log1m(pi_i);                     // zero part
    } else {
      target += log(pi_i) +
                beta_lpdf(w_adj | mu_i_safe * phi_w[j],
                                       (1 - mu_i_safe) * phi_w[j]); // non-zero
    }
  }
}
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

generated quantities {

  // Variance components per family
  vector[J] V_brood_w;
  vector[J] V_wnr_w;
  vector[J] V_spy_w;
  vector[J] V_sp_w;
  vector[J] V_e_w;        // residual variance on response scale
  vector[J] V_total_w;    // total variance
  vector[J] mean_w;       // conditional mean (nonzero part)
  vector[J] mean_w_zi;    // marginal mean (with zero inflation)

  vector[J] VC_brood_w;
  vector[J] VC_wnr_w;
  vector[J] VC_spy_w;
  vector[J] VC_sp_w;
  vector[J] VC_e_w;

  // main effects
  vector[J] bluetit_effect;
  vector[J] greattit_effect;
  vector[J] interaction_effect;

  vector[J] estimate1; // TMB=0, TMG=0
  vector[J] estimate2; // TMB=0, TMG=1
  vector[J] estimate3; // TMB=1, TMG=0
  vector[J] estimate4; // TMB=1, TMG=1
  
  
  // type effects
  vector[J] estimateF; 
  vector[J] estimateM; 
  vector[J] estimateC10;
  vector[J] estimateC15;
  
  vector[J] estimateA;
  vector[J] estimateC;
  vector[J] diffA;
  vector[J] diffC;
  
  vector[J] diffT;
  

  // Loop over families
  for (j in 1:J) {

    // Variance components
    V_brood_w[j] = square(sigma_brood_w[j]);
    V_wnr_w[j]   = square(sigma_wnr_w[j]);
    V_spy_w[j]   = square(sigma_spy_w[j]);
    V_sp_w[j]    = square(sigma_sp_w[j]);

    // linear predictor for all observations
    vector[N_w] eta_mu;
    for (i in 1:N_w) {
      eta_mu[i] = beta_0w[j]
                  + bw[j,1]  .* TMB[i]
                  + bw[j,2]  .* TMG[i]
                  + bw[j,3]  .* Size[i]
                  + bw[j,4]  .* Box[i]
                  + bw[j,5]  .* TypeAM[i]
                  + bw[j,6]  .* TypeC10[i]
                  + bw[j,7]  .* TypeC15[i]
                  + bw[j,8]  .* Date_w[i]
                  + bw[j,9]  .* Y2024[i]
                  + bw[j,10]  .* Species[i]
                  + bw_int[j,1] .* TMB[i] .* TMG[i]
                  + Brood_w[brood_w[i], j]
                  + WNR_w[wnr_w[i], j]
                  + SPY_w[spy_w[i], j]
                  + SP_w[sp_w[i], j];
    }

    // Residual variance on response scale (zero-inflated Beta)
    real acc_var = 0;
    real acc_mu  = 0;
    for (i in 1:N_w) {
      real mu = inv_logit(eta_mu[i]);

      // Beta variance part
      real var_beta = mu * (1 - mu) / (1 + phi_w[j]);

      // Mixture variance (π = prob(nonzero))
      acc_var += pi_w[j] * var_beta
               + pi_w[j] * (1 - pi_w[j]) * square(mu);

      // Contribution to conditional mean
      acc_mu += mu;
    }
    V_e_w[j]     = acc_var / N_w;
    mean_w[j]    = acc_mu  / N_w;           // conditional mean
    mean_w_zi[j] = pi_w[j] * mean_w[j];     // marginal mean

    // Total variance and relative components
    V_total_w[j]   = V_brood_w[j] + V_wnr_w[j] + V_spy_w[j] + V_sp_w[j] + V_e_w[j];
    VC_brood_w[j]  = V_brood_w[j] / V_total_w[j];
    VC_wnr_w[j]    = V_wnr_w[j]   / V_total_w[j];
    VC_spy_w[j]    = V_spy_w[j]   / V_total_w[j];
    VC_sp_w[j]     = V_sp_w[j]    / V_total_w[j];
    VC_e_w[j]      = V_e_w[j]     / V_total_w[j];

    // treatment effects (back-transformed to response scale)
    estimate1[j] = inv_logit(beta_0w[j]);                               
    estimate2[j] = inv_logit(beta_0w[j] + bw[j,2]);                     
    estimate3[j] = inv_logit(beta_0w[j] + bw[j,1]);                     
    estimate4[j] = inv_logit(beta_0w[j] + bw[j,1] + bw[j,2] + bw_int[j,1]); 

    // main effects
    bluetit_effect[j]   = (estimate3[j] + estimate4[j]) / 2 - (estimate1[j] + estimate2[j]) / 2;
    greattit_effect[j]  = (estimate2[j] + estimate4[j]) / 2 - (estimate1[j] + estimate3[j]) / 2;

    // interaction effect
    interaction_effect[j] = estimate4[j] - (estimate2[j] + estimate3[j] - estimate1[j]);
    
    
    // differences bird types
   estimateF[j]   = inv_logit(beta_0w[j]);
   estimateM[j]   = inv_logit(beta_0w[j] + bw[j,5]);
   estimateC10[j] = inv_logit(beta_0w[j] + bw[j,6]);
   estimateC15[j] = inv_logit(beta_0w[j] + bw[j,7]);
  
   diffA[j] = estimateF[j] - estimateM[j];
   diffC[j] = estimateC10[j] - estimateC15[j];
   
   estimateA[j] = (estimateF[j] + estimateM[j]) / 2;
   estimateC[j] = (estimateC10[j] + estimateC15[j]) / 2;
  
   diffT[j] =  estimateA[j] - estimateC[j];
    
    
  }
}
