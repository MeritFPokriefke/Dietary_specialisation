data {
  int<lower=1> J;         //total families
  int<lower=1> N_w;       // number of fitness observations
  int<lower=1> N_spy;     // number of SubplotYear levels w
  int<lower=1> N_locyear; // number of broods
  
  int<lower=1> spy_w[N_w];      // SubplotYear IDs for fitness
  int<lower=1> locyear_w[N_w];  // brood IDs for fitness
  
  
  // Response variables
  matrix[N_w, J] w;        // RRA 
  
  // Fixed effects
  vector[N_w] TypeB;
  vector[N_w] TypeG;
  vector[N_w] Y2024;
  vector[N_w] Date_w;
}

#################################################################################################################

parameters {
  
  vector[J] beta0;       // intercept RRA
  matrix[4, J] bw;      // beta fixed effects

  matrix[N_spy, J] spy_w_s;       // random effects for subplot-year
  vector<lower=0>[J] sigma_spy_w;   // SD for subplot-year effects
  
  matrix[N_locyear, J] locyear_w_s;   // random effects for location year
  vector<lower=0>[J] sigma_locyear_w;   // SD for location year
  
  vector<lower=0, upper=1>[J] pi;   // zero-inflation probability
  vector<lower=0>[J] phi;           // Beta precision
  
}

#################################################################################################################

transformed parameters {
  matrix[N_spy, J] SPY     = diag_post_multiply(spy_w_s, sigma_spy_w);
  matrix[N_locyear, J] LY  = diag_post_multiply(locyear_w_s, sigma_locyear_w);
}

#################################################################################################################


model {
  // Priors

beta0 ~ normal(0, 1);                  // vector[J]
to_vector(bw) ~ normal(0, 1);          // matrix[4, J]

to_vector(spy_w_s) ~ normal(0, 1);     // matrix[N_spy, J]
sigma_spy_w ~ normal(0, 1);            // vector[J]
to_vector(locyear_w_s) ~ normal(0, 1); // matrix[N_locyear, J]
sigma_locyear_w ~ normal(0, 1);        // vector[J]

pi ~ beta(2, 2);                        // vector[J]
phi ~ gamma(2, 0.5);                    // vector[J]
  

// Likelihood for RRA (zero-inflated Beta, hurdle-style), every zero is on zero inflated
for (i in 1:N_w) {
  for (j in 1:J) {
    real eta_mu = beta0[j] +
                  bw[1,j] .* TypeB[i] +
                  bw[2,j] .* TypeG[i] +
                  bw[3,j] .* Y2024[i] +
                  bw[4,j] .* Date_w[i] +
                  SPY[spy_w[i], j] +
                  LY[locyear_w[i], j];

    real mu = inv_logit(eta_mu);
    real mu_safe = fmin(fmax(mu, 1e-12), 1 - 1e-12);
    real w_safe = fmin(fmax(w[i, j], 1e-12), 1 - 1e-12);

    if (w[i, j] == 0) {
      target += log1m(pi[j]);
    } else {
      target += log(pi[j]) +
                beta_lpdf(w_safe | mu_safe * phi[j], (1 - mu_safe) * phi[j]);
    }
  }
}
}

#################################################################################################################


generated quantities {
  
  vector[J] V_spy;
  vector[J] V_locyear;
  vector[J] V_e;          // residual variance on response scale
  vector[J] mean_w;       // conditional mean (nonzero part)
  vector[J] mean_w_zi;    // marginal mean (with zero inflation)

  vector[J] V_total;
  vector[J] VC_spy;
  vector[J] VC_locyear;
  vector[J] VC_e;  

  for (j in 1:J) {

    // Random effect variances
    V_spy[j]     = square(sigma_spy_w[j]);
    V_locyear[j] = square(sigma_locyear_w[j]);

    // Linear predictors
    vector[N_w] eta_mu;
    for (i in 1:N_w) {
      eta_mu[i] = beta0[j]
                  + bw[1,j] .* TypeB[i]
                  + bw[2,j] .* TypeG[i]
                  + bw[3,j] .* Y2024[i]
                  + bw[4,j] .* Date_w[i]
                  + SPY[spy_w[i], j]
                  + LY[locyear_w[i], j];
    }

    // Residual variance on response scale (ZIB)
    real acc_var = 0;
    real acc_mu  = 0;
    for (i in 1:N_w) {
      real mu = inv_logit(eta_mu[i]);

      // Beta variance part
      real var_beta = mu * (1 - mu) / (1 + phi[j]);

      // Mixture variance (zero-inflated Beta)
      acc_var += (1 - pi[j]) * var_beta
               + pi[j] * (1 - pi[j]) * square(mu);

      // Contribution to conditional mean
      acc_mu += mu;
    }
    V_e[j]    = acc_var / N_w;
    mean_w[j] = acc_mu  / N_w;            // conditional mean
    mean_w_zi[j] = pi[j] * mean_w[j];     // marginal mean

    // Variance components (response scale)
    V_total[j]     = V_spy[j] + V_locyear[j] + V_e[j];
    VC_spy[j]      = V_spy[j]     / V_total[j];
    VC_locyear[j]  = V_locyear[j] / V_total[j];
    VC_e[j]        = V_e[j]       / V_total[j];
  }
}
