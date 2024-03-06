
//Predicting block choices in Minecraft collective foraging experiment
// D.Deffner, 2023

// Player-specific social learning model

//Functions block: Here we define the partial sum function for parallel computation of the likelihood.
functions {

real partial_sum(int[] y_slice, //important: uses only the slice from start to end
                   int start,
                   int end,
                   int N_feat,
                   int[] env,
                   int[] B,
                   int[] id,
                   int[] group,
                   int[] decision_number,
                   array[,,] real Feature_matrix,
                   array[,] int Possibilities,
                   int[] available,
                   matrix weights,
                   matrix v_ID ,
                   matrix v_group) {

      real logp=0;
      int counter=0;

for(i in start:end){
      counter += 1;

  //We only start modeling the second decision in each round, because the GP prediction don't apply for first choice.
  if ( decision_number[i] > 1){

  //Vector for choice probabilities
  vector[B[i]] p;

  //Get number of previous weights (asocial weights + previous social weights)
  int previous;
  int indices[7];

  previous = 3 + (group[i]-1)*4;
  indices = {1, 2, 3, previous+1, previous+2, previous+3, previous+4} ;

  //Get indicator vector: This is a vector of 0s and 1s that will "turn on" available features
  vector[7] indicators = to_vector(Possibilities[available[i],]);

  //Slice out relevant block infos

   matrix[B[i] , N_feat] features = to_matrix(Feature_matrix[i,1:B[i],]) ;

  //Compute choice probabilities using softmax function
  p = softmax( features * ((weights[ indices , env[i]] +
                          v_ID[id[i],{1,2,3,4,4,4,4}]' +
                          v_group[group[i],{1,2,3,4,4,4,4}]' ) .* indicators) );



  //Add log probability of observed block choice to target
  logp += categorical_lpmf(y_slice[counter] | p);
  }

}
   return logp;
}

}

//Data block: Define and name the size of each observed variable
data{
   int N;              //Number of observations (block choices)
   int N_id;           //Number of individuals
   int N_groups;       //Number of groups
   int N_block;        //Number of blocks
   int N_feat;         //Number of features
   int B[N];           //Number of available blocks for each choice
   int id[N];          //Unique individual identification
   int group[N];       //Group ID
   int env[N];         //Environment: random (=1) vs. smooth (=2)
   int decision_number[N]; //Decision number in round
   int choice[N];        //Chosen block
   int Possibilities[16,7]; //Matrix with different combinations of features
   int available[N];       //Which of the possibilities applies?
   real Feature_matrix[N,N_block, N_feat]; //Feature design matrix for each of N choices,
                                           //1 distance, 2 GP_predictions, 3 Successful players, 4 Unsuccessful players
}

//Parameter block: Define and name the size of each unobserved variable.
parameters{
   matrix[131,2] weights;

   // Varying effects clustered on individual
    matrix[4,N_id] z_ID;
    vector<lower=0>[4] sigma_ID;
    cholesky_factor_corr[4] Rho_ID;

    // Varying effects clustered on groups
     matrix[4,N_groups] z_group;
     vector<lower=0>[4] sigma_group;
     cholesky_factor_corr[4] Rho_group;
}

//Transformed Parameters block: Here we multiply z-scores with variances and Cholesky factors to get varying effects back to right scale
transformed parameters{
      matrix[N_id,4] v_ID;
      matrix[N_groups,4] v_group;

      v_ID = ( diag_pre_multiply( sigma_ID , Rho_ID ) * z_ID )';
      v_group = ( diag_pre_multiply( sigma_group , Rho_group ) * z_group )';
}

//Model block: Here compute the log posterior
model{
int grainsize = 1;

  //Priors
  //Weights
  to_vector(weights) ~ normal(0,3);

  //Varying effects priors
  to_vector(z_ID) ~ normal(0,1);
  sigma_ID ~ exponential(1);
  Rho_ID ~ lkj_corr_cholesky(4);

  to_vector(z_group) ~ normal(0,1);
  sigma_group ~ exponential(1);
  Rho_group ~ lkj_corr_cholesky(4);

 target += reduce_sum(partial_sum, choice , grainsize,
                     N_feat, env, B, id, group, decision_number,Feature_matrix,
                     Possibilities, available, weights, v_ID , v_group);

}// end model





//We use generated quantities block to compute log pointwise predictive densities for model comparison
generated quantities {

  vector[N] log_lik;
  for (i in 1:N) {

  //We only start modeling the second decision in each round, because the GP prediction don't apply for first choice.
  if ( decision_number[i] > 1){

  //Vector for choice probabilities
  vector[B[i]] p;

  //Get number of previous weights (asocial weights + previous social weights)
  int previous;
  int indices[7];

  previous = 3 + (group[i]-1)*4;
  indices = {1, 2, 3, previous+1, previous+2, previous+3, previous+4} ;

  //Get indicator vector: This is a vector of 0s and 1s that will "turn on" available features
  vector[7] indicators = to_vector(Possibilities[available[i],]);

  //Slice out relevant block infos

   matrix[B[i] , N_feat] features = to_matrix(Feature_matrix[i,1:B[i],]) ;

  //Compute choice probabilities using softmax function
  p = softmax( features * ((weights[ indices , env[i]] +
                          v_ID[id[i],{1,2,3,4,4,4,4}]' +
                          v_group[group[i],{1,2,3,4,4,4,4}]' ) .* indicators) );


    //Compute log likelihood of observed choice
    log_lik[i] = categorical_lpmf(choice[i] | p);
  } else{
    log_lik[i] = 0;
  }

}

}//end generated quantities