
#include <Rcpp.h>
using namespace Rcpp;

struct kahan_total { double total; double comp; };

void kahan_addition(const double x, kahan_total& kt){
    double y = x-kt.comp;
    double t = kt.total+y;
    kt.comp = (t-kt.total)-y;
    kt.total += y;
}

// [[Rcpp::export(dsa_get_metric_mean)]]
double dsa_get_metric_mean(const DataFrame& head_df, const DataFrame& chest_df, const DataFrame& hands_df, const DataFrame& legs_df, const int index){

    int I = head_df.nrows(); int J = chest_df.nrows(); int K = hands_df.nrows(); int L = legs_df.nrows();

    NumericVector head_metric = head_df[index]; NumericVector chest_metric = chest_df[index]; NumericVector hands_metric = hands_df[index]; NumericVector legs_metric = legs_df[index];

    double step_1; double step_2; double step_3;
    
    kahan_total metric_mean = { 0.0, 0.0 };

    for(int i = 0; i < I; ++i){

        step_1 = head_metric[i]; 

        for(int j = 0; j < J; ++j){

            step_2 = step_1+chest_metric[j];

            for(int k = 0; k < K; ++k){

                step_3 = step_2+hands_metric[k];

                for(int l = 0; l < L; ++l){

                    kahan_addition(step_3+legs_metric[l], metric_mean);

                }

            }

        }

    }

    return metric_mean.total;

}

// [[Rcpp::export(dsa_get_metric_var)]]
double dsa_get_metric_var(const DataFrame& head_df, const DataFrame& chest_df, const DataFrame& hands_df, const DataFrame& legs_df, const int index, const double metric_mean){

    int I = head_df.nrows(); int J = chest_df.nrows(); int K = hands_df.nrows(); int L = legs_df.nrows();

    NumericVector head_metric = head_df[index]; NumericVector chest_metric = chest_df[index]; NumericVector hands_metric = hands_df[index]; NumericVector legs_metric = legs_df[index];

    double step_1; double step_2; double step_3; double step_4;

    kahan_total metric_var = { 0.0, 0.0 };

    for(int i = 0; i < I; ++i){

        step_1 = head_metric[i]-metric_mean;

        for(int j = 0; j < J; ++j){

            step_2 = step_1+chest_metric[j];

            for(int k = 0; k < K; ++k){

                step_3 = step_2+hands_metric[k];

                for(int l = 0; l < L; ++l){
                    
                    step_4 = step_3+legs_metric[l];
                    kahan_addition(step_4*step_4, metric_var); 

                }

            }

        }

    }

    return metric_var.total;

}

// [[Rcpp::export(dsa_get_metrics_covar)]]
double dsa_get_metrics_covar(const DataFrame& head_df, const DataFrame& chest_df, const DataFrame& hands_df, const DataFrame& legs_df, const int index_1, const int index_2, const double metric_mean_1, const double metric_mean_2){

    int I = head_df.nrows(); int J = chest_df.nrows(); int K = hands_df.nrows(); int L = legs_df.nrows();

    NumericVector head_metric_1 = head_df[index_1]; NumericVector chest_metric_1 = chest_df[index_1]; NumericVector hands_metric_1 = hands_df[index_1]; NumericVector legs_metric_1 = legs_df[index_1];
    NumericVector head_metric_2 = head_df[index_2]; NumericVector chest_metric_2 = chest_df[index_2];  NumericVector hands_metric_2 = hands_df[index_2]; NumericVector legs_metric_2 = legs_df[index_2];

    double step_1_1; double step_2_1; double step_3_1; double step_4_1;
    double step_1_2; double step_2_2; double step_3_2; double step_4_2;

    kahan_total metrics_covar = { 0.0, 0.0 };

    for(int i = 0; i < I; ++i){
        
        step_1_1 = head_metric_1[i]-metric_mean_1; 
        step_1_2 = head_metric_2[i]-metric_mean_2; 

        for(int j = 0; j < J; ++j){

            step_2_1 = step_1_1+chest_metric_1[j];
            step_2_2 = step_1_2+chest_metric_2[j];

            for(int k = 0; k < K; ++k){

                step_3_1 = step_2_1+hands_metric_1[k];
                step_3_2 = step_2_2+hands_metric_2[k];

                for(int l = 0; l < L; ++l){
                    
                    step_4_1 = step_3_1+legs_metric_1[l];
                    step_4_2 = step_3_2+legs_metric_2[l];
                    kahan_addition(step_4_1*step_4_2, metrics_covar); 

                }

            }

        }

    }

    return metrics_covar.total;

}

