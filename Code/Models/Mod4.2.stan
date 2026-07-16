data {
  int<lower=0> N_I;       // number of individuals
  int<lower=0> N_z;       // number of phenotype observations
  int<lower=0> N_w;       // number of fitness observations
  int<lower=1> J;         // number of diet orders
  
  int<lower=1> N_spy_z;   // number of SubplotYear levels z
  int<lower=1> N_wnr_w;   // number of SubplotYear levels w
  int<lower=1> N_spy_w;   // number of SubplotYear levels w

  int<lower=1> spy_z[N_z];    // SubplotYear ID
  int<lower=1> wnr_w[N_w];    // SubplotYear IDs for fitness
  int<lower=1> spy_w[N_w];    // SubplotYear IDs for fitness
  int<lower=1> ind_z[N_z];    // individual ID for phenotype
  int<lower=1> ind_w[N_w];    // individual IDs for E in fitness

  vector[N_z] Seq2; // sequence covariate 2
  vector[N_z] Seq3; // sequence covariate 3
  vector[N_z] Seq4; // sequence covariate 4
  vector[N_z] Time; // time after sunrise
  vector[N_z] z;    // phenotype scores
  
  matrix[N_w, J] w;    // diet RRAs: rows = samples, cols = orders
  
  vector[N_w] LSC;       // chick
  vector[N_w] Date_w;    // Date sample
  vector[N_w] Y2024;     // Year 2024

}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// scale y variable z 
transformed data {
  real z_mean = mean(z);
  real z_sd = sd(z);
  
  vector[N_z] z_s;  for (n in 1:N_z) { // s is standardised
    z_s[n] = (z[n] - z_mean) / z_sd;
  }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

parameters {
  
  // phenotype
  real mu; // population mean for exploration behaviour
  vector[4] be; // betas for E
  
  vector[N_I] I_s;                         // individual random effects
  real<lower=0> sigma_I;                   // SD for individual effects
  
  vector[N_spy_z] subplot_year_effect_s;  // random effects for subplot-year
  real<lower=0> sigma_spy;                // SD for subplot-year effects
  
  real<lower=0> sigma_e;                  // residual SD for phenotype model
   
  
  
  // w
  vector[J] mu_w;                      // intercepts per order
  matrix[J, 4] b;                      // fixed effects per order (3 covariates)
  matrix[J, 1] b_int;                  // betas interactions
  
  matrix[N_wnr_w, J] wnr_w_s;          // SPY RE per order
  vector<lower=0>[J] sigma_wnr_w;      // SD per order
  
  matrix[N_I, J] I_w_s;                // individual RE per order
  vector<lower=0>[J] sigma_I_w;        // SD per order
  
  matrix[N_spy_w, J] spy_w_s;           // SPY per order
  vector<lower=0>[J] sigma_spy_w;      // SPY per order
  
  vector<lower=0, upper=1>[J] pi;      // zero-inflation prob per order
  vector<lower=0>[J] phi;              // beta precision per order
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// get unstandardised blups to get real diviations from mean
transformed parameters {
  vector[N_I] I = I_s * sigma_I;                                                  // individual random intercepts z
  vector[N_spy_z] subplot_year_effect = subplot_year_effect_s * sigma_spy;        // SPY random intercepts z
  matrix[N_wnr_w, J]           WNR_w = diag_post_multiply(wnr_w_s, sigma_wnr_w);
  matrix[N_spy_w, J]           SPY_w = diag_post_multiply(spy_w_s, sigma_spy_w);
  matrix[N_I, J]                I_w   = diag_post_multiply(I_w_s, sigma_I_w);
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

model {
  // priors E
  mu ~ normal(0, 1);
  
  be ~ normal(0, 1);
  
  I_s ~ normal(0, 1);
  sigma_I ~ normal(0, 1);
  
  subplot_year_effect_s ~ normal(0, 1);
  sigma_spy ~ normal(0, 1);
   
  sigma_e ~ normal(0, 1);
  
  // priors w
  to_vector(b) ~ normal(0, 1);                 // all betas
  to_vector(b_int) ~ normal(0, 1);
  to_vector(wnr_w_s) ~ normal(0, 1);  // all subplot-year REs
  sigma_wnr_w ~ normal(0, 1);                  // SD per order
  
  to_vector(spy_w_s) ~ normal(0, 1);  // all subplot-year REs
  sigma_spy_w ~ normal(0, 1);                  // SD per order

  to_vector(I_w_s) ~ normal(0, 1);             // all ind REs
  sigma_I_w ~ normal(0, 1);                    // SD per order

  pi ~ beta(2, 2);                             // hurdle prob per order
  phi ~ gamma(2, 0.5);                         // precision per order

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // Likelihood for phenotype model (exploration)
  for (n in 1:N_z) {
    z_s[n] ~ normal(mu + 
                    be[1] .* Seq2[n] + be[2] .* Seq3[n] + be[3] .* Seq4[n] 
                    + be[4] .* Time[n] +
                    I[ind_z[n]] + subplot_year_effect[spy_z[n]], sigma_e);
  }

// Likelihood: zero-inflated beta hurdle RRA (w)
for (j in 1:J) {
  for (i in 1:N_w) {
    // Linear predictor for order j
    real eta_mu = mu_w[j] + 
                  b[j,1]  .* I[ind_w[i]] + 
                  b[j,2]  .* LSC[i] + 
                  b[j,3]  .* Date_w[i] +
                  b[j,4]  .* Y2024[i] +
                  b_int[j,1] .* I[ind_w[i]] .* LSC[i] +
                  WNR_w[wnr_w[i], j] +
                  SPY_w[spy_w[i], j] +
                  I_w[ind_w[i], j];

real mu_i      = inv_logit(eta_mu);               // map to (0,1)
    real mu_i_safe = fmin(fmax(mu_i, 1e-12), 1 - 1e-12);
    real w_adj     = fmin(fmax(w[i, j], 1e-12), 1 - 1e-12);

    // family-specific zero-inflation probability
    real pi_i = pi[j];  

    if (w[i, j] == 0) {
      target += log1m(pi_i);                     // zero part
    } else {
      target += log(pi_i) +
                beta_lpdf(w_adj | mu_i_safe * phi[j],
                                       (1 - mu_i_safe) * phi[j]); // non-zero
    }
  }
}
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

generated quantities{
  
//E
real V_i   = sigma_I^2;
real V_spy = sigma_spy^2;
real V_e   = sigma_e^2;

// Backtransformation for E
  real mu_u = mu * z_sd + z_mean;    // back transf intercept
  vector[4] be_u;                    // Back-transform fixed effects
  for (i in 1:4) {
    be_u[i] = be[i] * z_sd;
  }  
  real V_i_u   = sigma_I^2   * z_sd^2;   // Back-transform variance components
  real V_spy_u = sigma_spy^2 * z_sd^2;
  real V_e_u   = sigma_e^2   * z_sd^2;
  
 // calc variance components
  real VC_spy = V_spy_u/(V_spy_u + V_i_u + V_e_u);
  real VC_i   = V_i_u  /(V_spy_u + V_i_u + V_e_u);
  real VC_e   = V_e_u  /(V_spy_u + V_i_u + V_e_u); 
  
// calc true exploration  
  vector[N_I] true_exploration;
  for (i in 1:N_I) {
  true_exploration[i] = I[i] * z_sd + mu_u;
  }
  
// w
  
// exploration effect back-transformed
  vector[J] b1_u;
  for (j in 1:J) b1_u[j] = b[j,1] / sqrt(V_i_u);
  
  vector[J] b_int_u;

  for (j in 1:J) b_int_u[j]  = b_int[j,1] / sqrt(V_i_u);

  

  vector[J] V_wnr_w;
  vector[J] V_spy_w;
  vector[J] V_i_w;
  vector[J] V_e_w;
  vector[J] mean_w;
  vector[J] mean_w_zi;

  vector[J] VC_wnr_w;
  vector[J] VC_spy_w;
  vector[J] VC_i_w;
  vector[J] VC_e_w;

  // Treatment effect estimates on response scale
  vector[J] estimate_LSC;

  vector[J] slope_LSC;

  for (j in 1:J) {
    // Random effect variances
    V_wnr_w[j] = sigma_wnr_w[j]^2;
    V_spy_w[j] = sigma_spy_w[j]^2;
    V_i_w[j]   = sigma_I_w[j]^2;

    // Initialize accumulators
    vector[N_w] var_resid_w;
    real var_sum = 0;
    mean_w[j] = 0;

    // Linear predictor loop
    real acc_mu = 0;
    for (i in 1:N_w) {
      real eta_mu = mu_w[j] + 
                  b[j,1]  .* I[ind_w[i]] + 
                  b[j,2]  .* LSC[i] + 
                  b[j,3]  .* Date_w[i] +
                  b[j,4]  .* Y2024[i] +
                  b_int[j,1] .* I[ind_w[i]] .* LSC[i] +
                  WNR_w[wnr_w[i], j] +
                  SPY_w[spy_w[i], j] +
                  I_w[ind_w[i], j];

      real mu_i = inv_logit(eta_mu);

      // Beta variance part
      real var_beta = mu_i * (1 - mu_i) / (1 + phi[j]);

      // Residual variance (zero-inflated Beta, same style as old model)
      var_resid_w[i] = pi[j] * var_beta
                     + pi[j] * (1 - pi[j]) * square(mu_i);

      // Contribution to conditional mean
      acc_mu += mu_i;
    }

    // Conditional & marginal means
    mean_w[j]    = acc_mu / N_w;
    mean_w_zi[j] = pi[j] * mean_w[j];

    // Average residual variance
    V_e_w[j] = mean(var_resid_w);
    

    // Variance component ratios
    real V_total = V_wnr_w[j] + V_spy_w[j] + V_i_w[j] + V_e_w[j];
    VC_wnr_w[j] = V_wnr_w[j] / V_total;
    VC_spy_w[j] = V_spy_w[j] / V_total;
    VC_i_w[j]   = V_i_w[j] / V_total;
    VC_e_w[j]   = V_e_w[j] / V_total;


    // Main effects
  estimate_LSC[j] = inv_logit(mu_w[j] + b[j,2]); // no female

  
 // Slope effects
  slope_LSC[j]  = b[j,1] + b_int[j,1];
    
  }
}
