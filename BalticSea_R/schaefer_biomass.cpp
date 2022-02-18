// Create a file to run the TMB version of sprat
#include <TMB.hpp>
#include <iostream>


template <class Type>
vector<Type> cumsum(vector<Type> x) {
  int n = x.size();
  vector<Type> ans(n);
  ans[0] = x[0];
  for (int i = 1; i < n; i++) ans[i] = x[i] + ans[i-1];
  return ans;
}


template<class Type> // TMB function to run a stock assessment on the MSE
Type objective_function<Type>::operator() ()
{

  DATA_VECTOR(Biomass); // Weight in the beginning of the year
  DATA_VECTOR(Catch);// Obersved productivity
  DATA_INTEGER(nyear); // Number of years
  DATA_SCALAR(SDB); // Biomass SD
    // Biological pa

  PARAMETER(logr); // growth rate
  PARAMETER(logK); // Carrying capacity
  PARAMETER(logSD); // Standard deviation of catch
  //PARAMETER(logSDC);
  PARAMETER(logm); // PT shape parameter
  PARAMETER_VECTOR(logBpred);
  //PARAMETER_VECTOR(logFzero);


  Type K = exp(logK);
  Type r = exp(logr);
  Type m = exp(logm);
// Uncertainty parameters
  Type SD = exp(logSD);
  //Type SDC = exp(logSDC);

  vector<Type> Bpred = exp(logBpred); // Expected biomass with error
  //vector<Type> Fzero = exp(logFzero); // Keep exploitation rate between [0;1]

  vector<Type> B(nyear+1); // Biomass
  //vector<Type> C(nyear);// Catch estimated
    //
    //
  //  B(0) = Bpred(0)-Fzero(0)*Bpred(0);
  B(0) = Bpred(0)-Catch(0);


    for(int time=0;time<(nyear);time++){

      //C(time) = Fzero(time)*Bpred(time);

      B(time+1) = Bpred(time)+((m+1)/m)*r*Bpred(time)*(1-pow(Bpred(time)/K, m))-Catch(time); // Deterministic model
      // if(B(time+1) < 0){
      //   B(time+1) = B(time)*.01;
      // }
    }

   Type ans=0.0;
 //  //
   for(int i=0;i<(nyear);i++){ // RANDOM EFFECT likelihood
     // ans += pow(PB(i)-PBobs(i), 2);
     ans += -dnorm(log(Bpred(i)),log(B(i)), SD, TRUE);
     ans += -dnorm(log(Bpred(i)),log(Biomass(i)), SDB, TRUE); // survey
     //ans += -dnorm(log(C(i)),log(Catch(i)), SDC, TRUE);
  }

//  Catch(nyear-1) = Bpred(nyear-1)*Fzero(nyear-1);

 //
 //
 //
 //  std::cout << "--- DEBUG: script start --- ans: " << ans << std::endl;


  REPORT(r)
  REPORT(K)
  REPORT(ans)
  REPORT(Bpred)
  REPORT(B)
  //REPORT(C)

  ADREPORT(r)
  ADREPORT(K)
  ADREPORT(Bpred)
  ADREPORT(B)
  //ADREPORT(C)


  return ans;
}
