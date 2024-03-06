
//Predicting block choices in Minecraft collective foraging experiment
// D.Deffner, 2023

//Critical Social Learning (weight of individual learning depends on time since last social success)

//Functions block: Here we define the partial sum function for parallel computation of the likelihood.
functions {

real partial_sum(int[] y_slice, //important: uses only the slice from start to end
                   int start, int end,
                   int N_feat,
                   int[] env,
                   int[] B,
                   int[] id,
                   int[] group,
                   int[] decision_number,
                   int[] successful_NA,
                   int[] unsuccessful_NA,
                   array[,,] real Feature_matrix,
                   real[] last_soc_reward,
                   matrix weights,
                   vector beta_t_soc,
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

  //Slice out relevant block infos

  //All features available for choice i
  if(successful_NA[i]==0 && unsuccessful_NA[i]==0){
    vector[N_feat] time_weights = rep_vector(0,N_feat);
    time_weights[3] = beta_t_soc[env[i]] + v_ID[id[i],N_feat+1] + v_group[group[i],N_feat+1];
     matrix[B[i] , N_feat] features = to_matrix(Feature_matrix[i,1:B[i],]) ;
  //Compute choice probabilities using softmax function
  p = softmax(features * ((weights[ , env[i]] + v_ID[id[i],1:N_feat]' + v_group[group[i],1:N_feat]' ) + time_weights*last_soc_reward[i] )   );
 }

 //Only successful players visible before choice i
 if(successful_NA[i]==0 && unsuccessful_NA[i]==1){
    vector[4] time_weights = rep_vector(0,4);
    time_weights[3] = beta_t_soc[env[i]] + v_ID[id[i],N_feat+1] + v_group[group[i],N_feat+1];
    matrix[B[i] , 4] features = to_matrix(Feature_matrix[i,1:B[i],{1,2,3,4}]) ;
  //Compute choice probabilities using softmax function
  p = softmax(features * ((weights[{1,2,3,4} , env[i]] + v_ID[id[i],{1,2,3,4}]' + v_group[group[i],{1,2,3,4}]' ) + time_weights*last_soc_reward[i] ) );
 }

 //Only unsuccessful players visible before choice i
if(successful_NA[i]==1 && unsuccessful_NA[i]==0){
    vector[4] time_weights = rep_vector(0,4);
    time_weights[3] = beta_t_soc[env[i]] + v_ID[id[i],N_feat+1] + v_group[group[i],N_feat+1];
    matrix[B[i] , 4] features = to_matrix(Feature_matrix[i,1:B[i],{1,2,3,5}]) ;
  //Compute choice probabilities using softmax function
  p = softmax(features * ((weights[{1,2,3,5} , env[i]] + v_ID[id[i],{1,2,3,5}]' + v_group[group[i],{1,2,3,5}]' )+ time_weights*last_soc_reward[i] ) );
}

 //No other players visible before choice i
if(successful_NA[i]==1 && unsuccessful_NA[i]==1){
    vector[3] time_weights = rep_vector(0,3);
    time_weights[3] = beta_t_soc[env[i]] + v_ID[id[i],N_feat+1] + v_group[group[i],N_feat+1];
    matrix[B[i] , 3] features = to_matrix(Feature_matrix[i,1:B[i],{1,2,3}]) ;
  //Compute choice probabilities using softmax function
  p = softmax(features * ((weights[{1,2,3} , env[i]] + v_ID[id[i],{1,2,3}]' + v_group[group[i],{1,2,3}]' )+ time_weights*last_soc_reward[i] ) );
}

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
   int successful_NA[N];   //Whether any successful others were observed for choice
   int unsuccessful_NA[N]; //Whether any unsuccessful others were observed for choice
   real Feature_matrix[N,N_block, N_feat]; //Feature design matrix for each of N choices,
                                           //1 distance, 2 GP_predictions, 3 Successful players, 4 Unsuccessful players
   real last_soc_reward[N]; //(standardized) time since last social success; 
}

//Parameter block: Define and name the size of each unobserved variable.
parameters{
   matrix[N_feat,2] weights;
 
   //Influence of time since last personal success on social weights [1 = successful players, 2 = unsuccesful players]
   vector[2] beta_t_soc;

   // Varying effects clustered on individual
    matrix[N_feat+1,N_id] z_ID;
    vector<lower=0>[N_feat+1] sigma_ID;
    cholesky_factor_corr[N_feat+1] Rho_ID;

    // Varying effects clustered on groups
     matrix[N_feat+1,N_groups] z_group;
     vector<lower=0>[N_feat+1] sigma_group;
     cholesky_factor_corr[N_feat+1] Rho_group;
}

//Transformed Parameters block: Here we multiply z-scores with variances and Cholesky factors to get varying effects back to right scale
transformed parameters{
      matrix[N_id,N_feat+1] v_ID;
      matrix[N_groups,N_feat+1] v_group;

      v_ID = ( diag_pre_multiply( sigma_ID , Rho_ID ) * z_ID )';
      v_group = ( diag_pre_multiply( sigma_group , Rho_group ) * z_group )';
}

//Model block: Here compute the log posterior
model{
int grainsize = 1;

  //Priors
  //Weights
  to_vector(weights) ~ normal(0,3);
  beta_t_soc ~ normal(0,3);

  //Varying effects priors
  to_vector(z_ID) ~ normal(0,1);
  sigma_ID ~ exponential(1);
  Rho_ID ~ lkj_corr_cholesky(4);

  to_vector(z_group) ~ normal(0,1);
  sigma_group ~ exponential(1);
  Rho_group ~ lkj_corr_cholesky(4);

 target += reduce_sum(partial_sum, choice , grainsize,
                     N_feat, env, B, id, group, decision_number,
                     successful_NA, unsuccessful_NA,
                     Feature_matrix, last_soc_reward, weights, beta_t_soc, v_ID , v_group);

}// end model



//We use generated quantities block to compute log pointwise predictive densities for model comparison
generated quantities {

  vector[N] log_lik;
  for (i in 1:N) {

    //We only start modeling the second decision in each round, because the GP prediction don't apply for first choice.
    if ( decision_number[i] > 1){

    //Vector for choice probabilities
    vector[B[i]] p;

  //All features available for choice i
  if(successful_NA[i]==0 && unsuccessful_NA[i]==0){
    vector[N_feat] time_weights = rep_vector(0,N_feat);
    time_weights[3] = beta_t_soc[env[i]] + v_ID[id[i],N_feat+1] + v_group[group[i],N_feat+1];
     matrix[B[i] , N_feat] features = to_matrix(Feature_matrix[i,1:B[i],]) ;
  //Compute choice probabilities using softmax function
  p = softmax(features * ((weights[ , env[i]] + v_ID[id[i],1:N_feat]' + v_group[group[i],1:N_feat]' ) + time_weights*last_soc_reward[i] )   );
 }

 //Only successful players visible before choice i
 if(successful_NA[i]==0 && unsuccessful_NA[i]==1){
    vector[4] time_weights = rep_vector(0,4);
    time_weights[3] = beta_t_soc[env[i]] + v_ID[id[i],N_feat+1] + v_group[group[i],N_feat+1];
    matrix[B[i] , 4] features = to_matrix(Feature_matrix[i,1:B[i],{1,2,3,4}]) ;
  //Compute choice probabilities using softmax function
  p = softmax(features * ((weights[{1,2,3,4} , env[i]] + v_ID[id[i],{1,2,3,4}]' + v_group[group[i],{1,2,3,4}]' ) + time_weights*last_soc_reward[i] ) );
 }

 //Only unsuccessful players visible before choice i
if(successful_NA[i]==1 && unsuccessful_NA[i]==0){
    vector[4] time_weights = rep_vector(0,4);
    time_weights[3] = beta_t_soc[env[i]] + v_ID[id[i],N_feat+1] + v_group[group[i],N_feat+1];
    matrix[B[i] , 4] features = to_matrix(Feature_matrix[i,1:B[i],{1,2,3,5}]) ;
  //Compute choice probabilities using softmax function
  p = softmax(features * ((weights[{1,2,3,5} , env[i]] + v_ID[id[i],{1,2,3,5}]' + v_group[group[i],{1,2,3,5}]' )+ time_weights*last_soc_reward[i] ) );
}

 //No other players visible before choice i
if(successful_NA[i]==1 && unsuccessful_NA[i]==1){
    vector[3] time_weights = rep_vector(0,3);
    time_weights[3] = beta_t_soc[env[i]] + v_ID[id[i],N_feat+1] + v_group[group[i],N_feat+1];
    matrix[B[i] , 3] features = to_matrix(Feature_matrix[i,1:B[i],{1,2,3}]) ;
  //Compute choice probabilities using softmax function
  p = softmax(features * ((weights[{1,2,3} , env[i]] + v_ID[id[i],{1,2,3}]' + v_group[group[i],{1,2,3}]' )+ time_weights*last_soc_reward[i] ) );
}


  //Compute log likelihood of observed choice
    log_lik[i] = categorical_lpmf(choice[i] | p);
  } else{
    log_lik[i] = 0;
  }

}

}//end generated quantities


