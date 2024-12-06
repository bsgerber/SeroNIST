#include <Rcpp.h>
using namespace Rcpp;

double takeMean(Rcpp::NumericVector vec){
  int n = vec.size();
  double sum = 0; 
  
  for (int i = 0; i < n; i++){
    sum = sum + vec[i];
  }
  
  return sum/n;
}


// [[Rcpp::export]]
double set_IC_rcpp(
        Rcpp::NumericVector &pos, 
        Rcpp::NumericVector &pos2){
  
  int MaxResultLength = 14000;
  // initialize vectors for results
  // std::vector<float> Score(MaxResultLength);
  // std::vector<int> Index(MaxResultLength);
  
  
  
  int n = pos.size();
  Rcpp::Rcout<< "Pos size: " << n << std::endl;
  
  float sum = 0;
  float mean_arr1 = takeMean(pos);
  float mean_arr2 = takeMean(pos2);
  Rcpp::Rcout<< "mean pos: " << mean_arr1 << std::endl;
  
  for (int i = 0; i < n; i++){
    // Rcpp::Rcout<< "pos at i [" << i << "]: " << pos[i] << std::endl;
    sum = sum + (pos[i] - mean_arr1) * (pos2[i] - mean_arr2);
    // Rcpp::Rcout<< "new sum: " << sum<< std::endl;
  }
  return sum / (n - 1);
  
  // Rcpp::DataFrame results = DataFrame::create(Named("Index")=Index,
  //                                             Named("Score")=Score);
  // return results;
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
// float asm_EfficientCosineSimilarity(std::vector<float> &qx,
//                                     std::vector<float> &qy,
//                                     std::vector<float> &rx,
//                                     std::vector<float> &ry,
//                                     float denominator,
//                                     float resolution){
//   // Values that get updated with each loop
//   float numerator = 0;
//   int iterS = 0;
//   float mz_diff = 0;
//   // Create a vector to keep track of which peaks are already added
//   int rLength = rx.size();
//   int qLength = qx.size();
//   std::vector<int> rAdded(rLength);
//   std::vector<int> qAdded(qLength);
//   
//   for (int t = 0; t < rLength; t++){
//     rAdded[t] = 0; 
//   }
//   for (int t = 0; t < qLength; t++){
//     qAdded[t] = 0; 
//   }
//   
//   // Loop through entire query spectrum
//   for (int i = 0; i < qx.size() - 1; i++){
//     // Use this var to store the largest refspec y value within +/- resolution
//     float cry=0;
//     // For each query datapoint, loop through each refspec point
//     for (int j = 0; j < rx.size() - 1; j++){
//       // Calculate mz_diff
//       mz_diff = qx[i]-rx[j];
//       
//       if (abs(mz_diff) <= resolution){
//         if((cry - ry[j]) < 0 && rAdded[j] == 0){
//           cry = ry[j];
//           rAdded[j] = 1;
//           break;
//         } 
//       } 
//       
//       // if(mz_diff < resolution){
//       //   iterS = j;
//       //   break;
//       // }
//     }
//     
//     numerator = numerator + qy[i]*cry;
//   }
//   
//   float score = numerator/denominator;
//   return score;
// }
// 
// 
// 
// // // [[Rcpp::export]]
// // float asm_EfficientCosineSimilarityRevised(Rcpp::NumericVector &qx,
// //                                     Rcpp::NumericVector &qy,
// //                                     std::vector<float> &rx,
// //                                     std::vector<float> &ry,
// //                                     float denominator,
// //                                     float resolution){
// //   
// //   int i,j;
// //   int iterS = 0;
// //   float mz_diff = 0;
// //   float cry=0; // largest y value in reference spectrum within +/- resolution of q(x)
// //   float numerator = 0;
// //   
// //   for(i=0;i<qx.length();i++){
// //     cry = 0;
// //     for(j=iterS;j<rx.size();j++){
// //       mz_diff = qx[i]-rx[j];
// //       
// //       if (abs(mz_diff) <= resolution){
// //         if((cry-ry[j])<0){
// //           cry = ry[j];
// //         } 
// //       }
// //       
// //       if(mz_diff < resolution){
// //         iterS = j;
// //         break;
// //       }
// //     }
// //     
// //     numerator = numerator + qy[i]*cry;
// //   }
// //   
// //   float score = numerator/denominator;
// //   return score;
// // }
// 
// // [[Rcpp::export]]
// Rcpp::DataFrame asm_StandardSearch(
//     Rcpp::NumericVector &qx, // Query spectrum x values
//     Rcpp::NumericVector &qy, // Query spectrum y values
//     Rcpp::NumericVector &lx, // Library spectra x values
//     Rcpp::NumericVector &ly, // Library spectra y values
//     Rcpp::NumericVector &li, // Library spectra index column
//     float resolution) {
//   
//   // For each search, reset these
//   int i=0;
//   int j=0;
//   int k=0;
//   float score = 0;
  // int MaxResultLength = 14000;
//   
  // // initialize vectors for results
  // std::vector<float> Score(MaxResultLength);
  // std::vector<int> Index(MaxResultLength);
//   // For storing curent library spectrum
//   std::vector<float> RefSpecX;
//   std::vector<float> RefSpecY;
//   std::vector<float> QuerySpecX;
//   std::vector<float> QuerySpecY;
//   
//   
//   // Must be calculated for each query
//   float denominatorQ = 0;
//   
//   // For entire length of squery spectrum
//   for(i=0;i<qy.length();i++){
//     // Copy over query 
//     QuerySpecX.push_back(qx[i]);
//     QuerySpecY.push_back(qy[i]);
//     
//     // Summation over squared ab values
//     denominatorQ = denominatorQ + qy[i]*qy[i];
//   }
//   // Take square root of total
//   denominatorQ = sqrt(denominatorQ);
//   
//   int test;
//   
//   // Initialize j as the first index
//   j = li[0];
//   
  // // Iterate through entire library
  // for(i=0;i<li.length();i++){
  //   // Get current index
  //   test = li[i];
//     // If not equal to j (reached next spectrtum) or reached end of library
//     if(test!=j || i==(li.length()-1)){
//       // If the latter
//       if(i==(li.length()-1)){
//         // Store the spectrum 
//         RefSpecX.push_back(lx[i]);
//         RefSpecY.push_back(ly[i]);
//       }
//       
//       // Compute the denominator for current spectrum
//       float denominatorR = 0;
//       for(k=0;k<RefSpecY.size();k++){
//         denominatorR = denominatorR + RefSpecY[k]*RefSpecY[k];
//       }
//       denominatorR = sqrt(denominatorR);
//       
//       // Use library and query spec computatoins for new denominator
//       float denominator = 0;
//       denominator = denominatorQ*denominatorR;
//       
//       score = 0;
//       score = asm_EfficientCosineSimilarity(QuerySpecX,QuerySpecY,
//                                             RefSpecX,RefSpecY,
//                                             denominator,resolution); 
//       Score[j-1] = score;
//       Index[j-1] = j;
//       // printf("%d \t %f \n",j,score);
//       RefSpecX.clear();
//       RefSpecY.clear();
//       j++;
//       
//     }
//     
//     // equivalent to determine which lines in the data table belong to compound j
//     RefSpecX.push_back(lx[i]);
//     RefSpecY.push_back(ly[i]);
//     
//   }
//   
//   
  // Rcpp::DataFrame results = DataFrame::create(Named("Index")=Index,
  //                                             Named("Score")=Score);
  // return results;
// }
// 
// 
// // You can include R code blocks in C++ files processed with sourceCpp
// // (useful for testing and development). The R code will be automatically 
// // run after the compilation.
// //

/*** R

*/
