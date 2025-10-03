
#include <queue>
#include <vector>

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(all_armor_combinations)]]
void all_armor_combinations(
    const DataFrame& head_df,
    const DataFrame& chest_df,
    const DataFrame& hands_df,
    const DataFrame& legs_df,
    DataFrame& full_df
){

    CharacterVector head_ARMOR = head_df["ARMOR"];
    NumericVector head_PHYS_DEF = head_df["PHYS_DEF"];
    NumericVector head_STRIKE_DEF = head_df["STRIKE_DEF"];
    NumericVector head_SLASH_DEF = head_df["SLASH_DEF"];
    NumericVector head_THRUST_DEF = head_df["THRUST_DEF"];
    NumericVector head_MAG_DEF = head_df["MAG_DEF"];
    NumericVector head_FIRE_DEF = head_df["FIRE_DEF"];
    NumericVector head_LITNG_DEF = head_df["LITNG_DEF"];
    NumericVector head_POISE = head_df["POISE"];
    NumericVector head_BLEED_RES = head_df["BLEED_RES"];
    NumericVector head_POIS_RES = head_df["POIS_RES"];
    NumericVector head_CURSE_RES = head_df["CURSE_RES"];
    NumericVector head_DURABILITY = head_df["DURABILITY"];
    NumericVector head_WEIGHT = head_df["WEIGHT"];

    CharacterVector chest_ARMOR = chest_df["ARMOR"];
    NumericVector chest_PHYS_DEF = chest_df["PHYS_DEF"];
    NumericVector chest_STRIKE_DEF = chest_df["STRIKE_DEF"];
    NumericVector chest_SLASH_DEF = chest_df["SLASH_DEF"];
    NumericVector chest_THRUST_DEF = chest_df["THRUST_DEF"];
    NumericVector chest_MAG_DEF = chest_df["MAG_DEF"];
    NumericVector chest_FIRE_DEF = chest_df["FIRE_DEF"];
    NumericVector chest_LITNG_DEF = chest_df["LITNG_DEF"];
    NumericVector chest_POISE = chest_df["POISE"];
    NumericVector chest_BLEED_RES = chest_df["BLEED_RES"];
    NumericVector chest_POIS_RES = chest_df["POIS_RES"];
    NumericVector chest_CURSE_RES = chest_df["CURSE_RES"];
    NumericVector chest_DURABILITY = chest_df["DURABILITY"];
    NumericVector chest_WEIGHT = chest_df["WEIGHT"];

    CharacterVector hands_ARMOR = hands_df["ARMOR"];
    NumericVector hands_PHYS_DEF = hands_df["PHYS_DEF"];
    NumericVector hands_STRIKE_DEF = hands_df["STRIKE_DEF"];
    NumericVector hands_SLASH_DEF = hands_df["SLASH_DEF"];
    NumericVector hands_THRUST_DEF = hands_df["THRUST_DEF"];
    NumericVector hands_MAG_DEF = hands_df["MAG_DEF"];
    NumericVector hands_FIRE_DEF = hands_df["FIRE_DEF"];
    NumericVector hands_LITNG_DEF = hands_df["LITNG_DEF"];
    NumericVector hands_POISE = hands_df["POISE"];
    NumericVector hands_BLEED_RES = hands_df["BLEED_RES"];
    NumericVector hands_POIS_RES = hands_df["POIS_RES"];
    NumericVector hands_CURSE_RES = hands_df["CURSE_RES"];
    NumericVector hands_DURABILITY = hands_df["DURABILITY"];
    NumericVector hands_WEIGHT = hands_df["WEIGHT"];

    CharacterVector legs_ARMOR = legs_df["ARMOR"];
    NumericVector legs_PHYS_DEF = legs_df["PHYS_DEF"];
    NumericVector legs_STRIKE_DEF = legs_df["STRIKE_DEF"];
    NumericVector legs_SLASH_DEF = legs_df["SLASH_DEF"];
    NumericVector legs_THRUST_DEF = legs_df["THRUST_DEF"];
    NumericVector legs_MAG_DEF = legs_df["MAG_DEF"];
    NumericVector legs_FIRE_DEF = legs_df["FIRE_DEF"];
    NumericVector legs_LITNG_DEF = legs_df["LITNG_DEF"];
    NumericVector legs_POISE = legs_df["POISE"];
    NumericVector legs_BLEED_RES = legs_df["BLEED_RES"];
    NumericVector legs_POIS_RES = legs_df["POIS_RES"];
    NumericVector legs_CURSE_RES = legs_df["CURSE_RES"];
    NumericVector legs_DURABILITY = legs_df["DURABILITY"];
    NumericVector legs_WEIGHT = legs_df["WEIGHT"];

    CharacterVector HEAD = full_df["HEAD"];
    CharacterVector CHEST = full_df["CHEST"];
    CharacterVector HANDS = full_df["HANDS"];
    CharacterVector LEGS = full_df["LEGS"];
    NumericVector PHYS_DEF = full_df["PHYS_DEF"];
    NumericVector STRIKE_DEF = full_df["STRIKE_DEF"];
    NumericVector SLASH_DEF = full_df["SLASH_DEF"];
    NumericVector THRUST_DEF = full_df["THRUST_DEF"];
    NumericVector MAG_DEF = full_df["MAG_DEF"];
    NumericVector FIRE_DEF = full_df["FIRE_DEF"];
    NumericVector LITNG_DEF = full_df["LITNG_DEF"];
    NumericVector POISE = full_df["POISE"];
    NumericVector BLEED_RES = full_df["BLEED_RES"];
    NumericVector POIS_RES = full_df["POIS_RES"];
    NumericVector CURSE_RES = full_df["CURSE_RES"];
    NumericVector DURABILITY = full_df["DURABILITY"];
    NumericVector WEIGHT = full_df["WEIGHT"];

    int I = head_df.nrows();
    int J = chest_df.nrows();
    int K = hands_df.nrows();
    int L = legs_df.nrows();

    int index = -1;

    double curr_head_PHYS_DEF; double curr_chest_PHYS_DEF; double curr_hands_PHYS_DEF; double curr_legs_PHYS_DEF;
    double curr_head_STRIKE_DEF; double curr_chest_STRIKE_DEF; double curr_hands_STRIKE_DEF; double curr_legs_STRIKE_DEF;
    double curr_head_SLASH_DEF; double curr_chest_SLASH_DEF; double curr_hands_SLASH_DEF; double curr_legs_SLASH_DEF;
    double curr_head_THRUST_DEF; double curr_chest_THRUST_DEF; double curr_hands_THRUST_DEF; double curr_legs_THRUST_DEF;
    double curr_head_MAG_DEF; double curr_chest_MAG_DEF; double curr_hands_MAG_DEF; double curr_legs_MAG_DEF;
    double curr_head_FIRE_DEF; double curr_chest_FIRE_DEF; double curr_hands_FIRE_DEF; double curr_legs_FIRE_DEF;
    double curr_head_LITNG_DEF; double curr_chest_LITNG_DEF; double curr_hands_LITNG_DEF; double curr_legs_LITNG_DEF;
    double curr_head_POISE; double curr_chest_POISE; double curr_hands_POISE; double curr_legs_POISE;
    double curr_head_BLEED_RES; double curr_chest_BLEED_RES; double curr_hands_BLEED_RES; double curr_legs_BLEED_RES;
    double curr_head_POIS_RES; double curr_chest_POIS_RES; double curr_hands_POIS_RES; double curr_legs_POIS_RES;
    double curr_head_CURSE_RES; double curr_chest_CURSE_RES; double curr_hands_CURSE_RES; double curr_legs_CURSE_RES;
    double curr_head_DURABILITY; double curr_chest_DURABILITY; double curr_hands_DURABILITY; double curr_legs_DURABILITY;
    double curr_head_WEIGHT; double curr_chest_WEIGHT; double curr_hands_WEIGHT; double curr_legs_WEIGHT;

    for(int i = 0; i < I; ++i){

        curr_head_PHYS_DEF = head_PHYS_DEF[i];
        curr_head_STRIKE_DEF = head_STRIKE_DEF[i];
        curr_head_SLASH_DEF = head_SLASH_DEF[i];
        curr_head_THRUST_DEF = head_THRUST_DEF[i];
        curr_head_MAG_DEF = head_MAG_DEF[i];
        curr_head_FIRE_DEF = head_FIRE_DEF[i];
        curr_head_LITNG_DEF = head_LITNG_DEF[i];
        curr_head_POISE = head_POISE[i];
        curr_head_BLEED_RES = head_BLEED_RES[i];
        curr_head_POIS_RES = head_POIS_RES[i];
        curr_head_CURSE_RES = head_CURSE_RES[i];
        curr_head_DURABILITY = head_DURABILITY[i];
        curr_head_WEIGHT = head_WEIGHT[i];

        for(int j = 0; j < J; ++j){

            curr_chest_PHYS_DEF = chest_PHYS_DEF[j];
            curr_chest_STRIKE_DEF = chest_STRIKE_DEF[j];
            curr_chest_SLASH_DEF = chest_SLASH_DEF[j];
            curr_chest_THRUST_DEF = chest_THRUST_DEF[j];
            curr_chest_MAG_DEF = chest_MAG_DEF[j];
            curr_chest_FIRE_DEF = chest_FIRE_DEF[j];
            curr_chest_LITNG_DEF = chest_LITNG_DEF[j];
            curr_chest_POISE = chest_POISE[j];
            curr_chest_BLEED_RES = chest_BLEED_RES[j];
            curr_chest_POIS_RES = chest_POIS_RES[j];
            curr_chest_CURSE_RES = chest_CURSE_RES[j];
            curr_chest_DURABILITY = chest_DURABILITY[j];
            curr_chest_WEIGHT = chest_WEIGHT[j];

            for(int k = 0; k < K; ++k){

                curr_hands_PHYS_DEF = hands_PHYS_DEF[k];
                curr_hands_STRIKE_DEF = hands_STRIKE_DEF[k];
                curr_hands_SLASH_DEF = hands_SLASH_DEF[k];
                curr_hands_THRUST_DEF = hands_THRUST_DEF[k];
                curr_hands_MAG_DEF = hands_MAG_DEF[k];
                curr_hands_FIRE_DEF = hands_FIRE_DEF[k];
                curr_hands_LITNG_DEF = hands_LITNG_DEF[k];
                curr_hands_POISE = hands_POISE[k];
                curr_hands_BLEED_RES = hands_BLEED_RES[k];
                curr_hands_POIS_RES = hands_POIS_RES[k];
                curr_hands_CURSE_RES = hands_CURSE_RES[k];
                curr_hands_DURABILITY = hands_DURABILITY[k];
                curr_hands_WEIGHT = hands_WEIGHT[k];

                for(int l = 0; l < L; ++l){

                    ++index;

                    curr_legs_PHYS_DEF = legs_PHYS_DEF[l];
                    curr_legs_STRIKE_DEF = legs_STRIKE_DEF[l];
                    curr_legs_SLASH_DEF = legs_SLASH_DEF[l];
                    curr_legs_THRUST_DEF = legs_THRUST_DEF[l];
                    curr_legs_MAG_DEF = legs_MAG_DEF[l];
                    curr_legs_FIRE_DEF = legs_FIRE_DEF[l];
                    curr_legs_LITNG_DEF = legs_LITNG_DEF[l];
                    curr_legs_POISE = legs_POISE[l];
                    curr_legs_BLEED_RES = legs_BLEED_RES[l];
                    curr_legs_POIS_RES = legs_POIS_RES[l];
                    curr_legs_CURSE_RES = legs_CURSE_RES[l];
                    curr_legs_DURABILITY = legs_DURABILITY[l];
                    curr_legs_WEIGHT = legs_WEIGHT[l];
                    
                    HEAD[index] = head_ARMOR[i]; CHEST[index] = chest_ARMOR[j]; HANDS[index] = hands_ARMOR[k]; LEGS[index] = legs_ARMOR[l];
                    PHYS_DEF[index] = curr_head_PHYS_DEF+curr_chest_PHYS_DEF+curr_hands_PHYS_DEF+curr_legs_PHYS_DEF;
                    STRIKE_DEF[index] = curr_head_STRIKE_DEF+curr_chest_STRIKE_DEF+curr_hands_STRIKE_DEF+curr_legs_STRIKE_DEF;
                    SLASH_DEF[index] = curr_head_SLASH_DEF+curr_chest_SLASH_DEF+curr_hands_SLASH_DEF+curr_legs_SLASH_DEF;
                    THRUST_DEF[index] = curr_head_THRUST_DEF+curr_chest_THRUST_DEF+curr_hands_THRUST_DEF+curr_legs_THRUST_DEF;
                    MAG_DEF[index] = curr_head_MAG_DEF+curr_chest_MAG_DEF+curr_hands_MAG_DEF+curr_legs_MAG_DEF;
                    FIRE_DEF[index] = curr_head_FIRE_DEF+curr_chest_FIRE_DEF+curr_hands_FIRE_DEF+curr_legs_FIRE_DEF;
                    LITNG_DEF[index] = curr_head_LITNG_DEF+curr_chest_LITNG_DEF+curr_hands_LITNG_DEF+curr_legs_LITNG_DEF;
                    POISE[index] = curr_head_POISE+curr_chest_POISE+curr_hands_POISE+curr_legs_POISE; 
                    BLEED_RES[index] = curr_head_BLEED_RES+curr_chest_BLEED_RES+curr_hands_BLEED_RES+curr_legs_BLEED_RES;
                    POIS_RES[index] = curr_head_POIS_RES+curr_chest_POIS_RES+curr_hands_POIS_RES+curr_legs_POIS_RES;
                    CURSE_RES[index] = curr_head_CURSE_RES+curr_chest_CURSE_RES+curr_hands_CURSE_RES+curr_legs_CURSE_RES;
                    DURABILITY[index] = std::min(curr_head_DURABILITY, std::min(curr_chest_DURABILITY, std::min(curr_hands_DURABILITY, curr_legs_DURABILITY)));
                    WEIGHT[index] = curr_head_WEIGHT+curr_chest_WEIGHT+curr_hands_WEIGHT+curr_legs_WEIGHT;

                }

            }

        }

    }

    PHYS_DEF = round(PHYS_DEF, 1); STRIKE_DEF = round(STRIKE_DEF, 1); SLASH_DEF = round(SLASH_DEF, 1); THRUST_DEF = round(THRUST_DEF, 1);
    MAG_DEF = round(MAG_DEF, 1); FIRE_DEF = round(FIRE_DEF, 1); LITNG_DEF = round(LITNG_DEF, 1); POISE = round(POISE, 1);
    BLEED_RES = round(BLEED_RES, 1); POIS_RES = round(POIS_RES, 1); CURSE_RES = round(CURSE_RES, 1); 
    DURABILITY = round(DURABILITY, 1); WEIGHT = round(WEIGHT, 1); 

}


struct armor_combo { 
    double score; 
    String head; String chest; String hands; String legs;
    double physdef; double strikedef; double slashdef; double thrustdef;
    double magdef; double firedef; double litngdef;
    double bleedres; double poisres; double curseres;
    double durability; double poise; double weight; double load; 
    bool operator<(const armor_combo& comparison) const
    {
       return score > comparison.score;
    }
};

//[[Rcpp::export(optimize_armor_combinations)]]
DataFrame optimize_armor_combinations(

    const int starting_loop_size,
    const int max_output_size,

    const double base_weight,
    const double load,
    const double load_motf,
    const double load_threshold,
    const double load_threshold_motf,
    const int motf_index,
    const bool wolf,

    const NumericVector& minima,
    const NumericVector& means,
    const NumericVector& scalars,

    const DataFrame& head_df,
    const DataFrame& chest_df,
    const DataFrame& hands_df,
    const DataFrame& legs_df

){

    CharacterVector head_ARMOR = head_df["ARMOR"];
    NumericVector head_PHYS_DEF = head_df["PHYS_DEF"];
    NumericVector head_STRIKE_DEF = head_df["STRIKE_DEF"];
    NumericVector head_SLASH_DEF = head_df["SLASH_DEF"];
    NumericVector head_THRUST_DEF = head_df["THRUST_DEF"];
    NumericVector head_MAG_DEF = head_df["MAG_DEF"];
    NumericVector head_FIRE_DEF = head_df["FIRE_DEF"];
    NumericVector head_LITNG_DEF = head_df["LITNG_DEF"];
    NumericVector head_POISE = head_df["POISE"];
    NumericVector head_BLEED_RES = head_df["BLEED_RES"];
    NumericVector head_POIS_RES = head_df["POIS_RES"];
    NumericVector head_CURSE_RES = head_df["CURSE_RES"];
    NumericVector head_DURABILITY = head_df["DURABILITY"];
    NumericVector head_WEIGHT = head_df["WEIGHT"];

    CharacterVector chest_ARMOR = chest_df["ARMOR"];
    NumericVector chest_PHYS_DEF = chest_df["PHYS_DEF"];
    NumericVector chest_STRIKE_DEF = chest_df["STRIKE_DEF"];
    NumericVector chest_SLASH_DEF = chest_df["SLASH_DEF"];
    NumericVector chest_THRUST_DEF = chest_df["THRUST_DEF"];
    NumericVector chest_MAG_DEF = chest_df["MAG_DEF"];
    NumericVector chest_FIRE_DEF = chest_df["FIRE_DEF"];
    NumericVector chest_LITNG_DEF = chest_df["LITNG_DEF"];
    NumericVector chest_POISE = chest_df["POISE"];
    NumericVector chest_BLEED_RES = chest_df["BLEED_RES"];
    NumericVector chest_POIS_RES = chest_df["POIS_RES"];
    NumericVector chest_CURSE_RES = chest_df["CURSE_RES"];
    NumericVector chest_DURABILITY = chest_df["DURABILITY"];
    NumericVector chest_WEIGHT = chest_df["WEIGHT"];

    CharacterVector hands_ARMOR = hands_df["ARMOR"];
    NumericVector hands_PHYS_DEF = hands_df["PHYS_DEF"];
    NumericVector hands_STRIKE_DEF = hands_df["STRIKE_DEF"];
    NumericVector hands_SLASH_DEF = hands_df["SLASH_DEF"];
    NumericVector hands_THRUST_DEF = hands_df["THRUST_DEF"];
    NumericVector hands_MAG_DEF = hands_df["MAG_DEF"];
    NumericVector hands_FIRE_DEF = hands_df["FIRE_DEF"];
    NumericVector hands_LITNG_DEF = hands_df["LITNG_DEF"];
    NumericVector hands_POISE = hands_df["POISE"];
    NumericVector hands_BLEED_RES = hands_df["BLEED_RES"];
    NumericVector hands_POIS_RES = hands_df["POIS_RES"];
    NumericVector hands_CURSE_RES = hands_df["CURSE_RES"];
    NumericVector hands_DURABILITY = hands_df["DURABILITY"];
    NumericVector hands_WEIGHT = hands_df["WEIGHT"];

    CharacterVector legs_ARMOR = legs_df["ARMOR"];
    NumericVector legs_PHYS_DEF = legs_df["PHYS_DEF"];
    NumericVector legs_STRIKE_DEF = legs_df["STRIKE_DEF"];
    NumericVector legs_SLASH_DEF = legs_df["SLASH_DEF"];
    NumericVector legs_THRUST_DEF = legs_df["THRUST_DEF"];
    NumericVector legs_MAG_DEF = legs_df["MAG_DEF"];
    NumericVector legs_FIRE_DEF = legs_df["FIRE_DEF"];
    NumericVector legs_LITNG_DEF = legs_df["LITNG_DEF"];
    NumericVector legs_POISE = legs_df["POISE"];
    NumericVector legs_BLEED_RES = legs_df["BLEED_RES"];
    NumericVector legs_POIS_RES = legs_df["POIS_RES"];
    NumericVector legs_CURSE_RES = legs_df["CURSE_RES"];
    NumericVector legs_DURABILITY = legs_df["DURABILITY"];
    NumericVector legs_WEIGHT = legs_df["WEIGHT"];

    int I = head_df.nrows(); int J = chest_df.nrows(); int K = hands_df.nrows(); int L = legs_df.nrows();

    int curr_count = 0;
    double score;

    double curr_load;
    double curr_load_threshold;
    double eps = 1.0e-10;

    double extra_poise = 0.0;
    if(wolf){extra_poise = 40.0;};

    String curr_head; String curr_chest; String curr_hands; String curr_legs;
    double curr_PHYS_DEF; double curr_head_PHYS_DEF; double curr_chest_PHYS_DEF; double curr_hands_PHYS_DEF; double curr_legs_PHYS_DEF;
    double curr_STRIKE_DEF; double curr_head_STRIKE_DEF; double curr_chest_STRIKE_DEF; double curr_hands_STRIKE_DEF; double curr_legs_STRIKE_DEF;
    double curr_SLASH_DEF; double curr_head_SLASH_DEF; double curr_chest_SLASH_DEF; double curr_hands_SLASH_DEF; double curr_legs_SLASH_DEF;
    double curr_THRUST_DEF; double curr_head_THRUST_DEF; double curr_chest_THRUST_DEF; double curr_hands_THRUST_DEF; double curr_legs_THRUST_DEF;
    double curr_MAG_DEF; double curr_head_MAG_DEF; double curr_chest_MAG_DEF; double curr_hands_MAG_DEF; double curr_legs_MAG_DEF;
    double curr_FIRE_DEF; double curr_head_FIRE_DEF; double curr_chest_FIRE_DEF; double curr_hands_FIRE_DEF; double curr_legs_FIRE_DEF;
    double curr_LITNG_DEF; double curr_head_LITNG_DEF; double curr_chest_LITNG_DEF; double curr_hands_LITNG_DEF; double curr_legs_LITNG_DEF;
    double curr_POISE; double curr_head_POISE; double curr_chest_POISE; double curr_hands_POISE; double curr_legs_POISE;
    double curr_BLEED_RES; double curr_head_BLEED_RES; double curr_chest_BLEED_RES; double curr_hands_BLEED_RES; double curr_legs_BLEED_RES;
    double curr_POIS_RES; double curr_head_POIS_RES; double curr_chest_POIS_RES; double curr_hands_POIS_RES; double curr_legs_POIS_RES;
    double curr_CURSE_RES; double curr_head_CURSE_RES; double curr_chest_CURSE_RES; double curr_hands_CURSE_RES; double curr_legs_CURSE_RES;
    double curr_DURABILITY; double curr_head_DURABILITY; double curr_chest_DURABILITY; double curr_hands_DURABILITY; double curr_legs_DURABILITY;
    double curr_WEIGHT; double curr_head_WEIGHT; double curr_chest_WEIGHT; double curr_hands_WEIGHT; double curr_legs_WEIGHT;

    int curr_I; int curr_J; int curr_K; int curr_L; 
    bool I_capped = false; bool J_capped = false; bool K_capped = false; bool L_capped = false;
    int max_loop_size = std::max(I, std::max(J, std::max(K, L)));
    armor_combo curr_combo;
    std::priority_queue<armor_combo> armor_combos;
    bool at_max_queue_size = false;
    int loop_size_1;
    for(int loop_size = starting_loop_size; loop_size <= max_loop_size; ++loop_size){
        
        loop_size_1 = loop_size-1;

        if(I < loop_size){
            I_capped = true;
            curr_I = I;
        } else{
            curr_I = loop_size;
        }
        if(J < loop_size){
            J_capped = true;
            curr_J = J;
        } else{
            curr_J = loop_size;
        }
        if(K < loop_size){
            K_capped = true;
            curr_K = K;
        } else{
            curr_K = loop_size;
        }
        if(L < loop_size){
            L_capped = true;
            curr_L = L;
        } else{
            curr_L = loop_size;
        }

        for(int i = 0; i < curr_I; ++i){

            if(i != loop_size_1 && L_capped && K_capped && J_capped && !I_capped){
                i = loop_size_1;
            }

            if(i == motf_index){
                curr_load = load_motf;
                curr_load_threshold = load_threshold_motf;
            } else{
                curr_load = load;
                curr_load_threshold = load_threshold;
            }
            
            curr_head = head_ARMOR[i];
            curr_head_PHYS_DEF = head_PHYS_DEF[i];
            curr_head_STRIKE_DEF = head_STRIKE_DEF[i];
            curr_head_SLASH_DEF = head_SLASH_DEF[i];
            curr_head_THRUST_DEF = head_THRUST_DEF[i];
            curr_head_MAG_DEF = head_MAG_DEF[i];
            curr_head_FIRE_DEF = head_FIRE_DEF[i];
            curr_head_LITNG_DEF = head_LITNG_DEF[i];
            curr_head_POISE = head_POISE[i];
            curr_head_BLEED_RES = head_BLEED_RES[i];
            curr_head_POIS_RES = head_POIS_RES[i];
            curr_head_CURSE_RES = head_CURSE_RES[i];
            curr_head_DURABILITY = head_DURABILITY[i];
            curr_head_WEIGHT = head_WEIGHT[i];

            for(int j = 0; j < curr_J; ++j){

                if(i != loop_size_1 && j != loop_size_1 && L_capped && K_capped && !J_capped){
                    j = loop_size_1;
                }
                
                curr_chest = chest_ARMOR[j];
                curr_chest_PHYS_DEF = chest_PHYS_DEF[j];
                curr_chest_STRIKE_DEF = chest_STRIKE_DEF[j];
                curr_chest_SLASH_DEF = chest_SLASH_DEF[j];
                curr_chest_THRUST_DEF = chest_THRUST_DEF[j];
                curr_chest_MAG_DEF = chest_MAG_DEF[j];
                curr_chest_FIRE_DEF = chest_FIRE_DEF[j];
                curr_chest_LITNG_DEF = chest_LITNG_DEF[j];
                curr_chest_POISE = chest_POISE[j];
                curr_chest_BLEED_RES = chest_BLEED_RES[j];
                curr_chest_POIS_RES = chest_POIS_RES[j];
                curr_chest_CURSE_RES = chest_CURSE_RES[j];
                curr_chest_DURABILITY = chest_DURABILITY[j];
                curr_chest_WEIGHT = chest_WEIGHT[j];

                for(int k = 0; k < curr_K; ++k){

                    if(i != loop_size_1 && j != loop_size_1 && k != loop_size_1 && L_capped && !K_capped){
                        k = loop_size_1;
                    }

                    curr_hands = hands_ARMOR[k];
                    curr_hands_PHYS_DEF = hands_PHYS_DEF[k];
                    curr_hands_STRIKE_DEF = hands_STRIKE_DEF[k];
                    curr_hands_SLASH_DEF = hands_SLASH_DEF[k];
                    curr_hands_THRUST_DEF = hands_THRUST_DEF[k];
                    curr_hands_MAG_DEF = hands_MAG_DEF[k];
                    curr_hands_FIRE_DEF = hands_FIRE_DEF[k];
                    curr_hands_LITNG_DEF = hands_LITNG_DEF[k];
                    curr_hands_POISE = hands_POISE[k];
                    curr_hands_BLEED_RES = hands_BLEED_RES[k];
                    curr_hands_POIS_RES = hands_POIS_RES[k];
                    curr_hands_CURSE_RES = hands_CURSE_RES[k];
                    curr_hands_DURABILITY = hands_DURABILITY[k];
                    curr_hands_WEIGHT = hands_WEIGHT[k];

                    for(int l = 0; l < curr_L; ++l){

                        if(i != loop_size_1 && j != loop_size_1 && k != loop_size_1 && l != loop_size_1 && !L_capped){
                            l = loop_size_1;
                        }
                        
                        curr_legs = legs_ARMOR[l];
                        curr_legs_PHYS_DEF = legs_PHYS_DEF[l];
                        curr_legs_STRIKE_DEF = legs_STRIKE_DEF[l];
                        curr_legs_SLASH_DEF = legs_SLASH_DEF[l];
                        curr_legs_THRUST_DEF = legs_THRUST_DEF[l];
                        curr_legs_MAG_DEF = legs_MAG_DEF[l];
                        curr_legs_FIRE_DEF = legs_FIRE_DEF[l];
                        curr_legs_LITNG_DEF = legs_LITNG_DEF[l];
                        curr_legs_POISE = legs_POISE[l];
                        curr_legs_BLEED_RES = legs_BLEED_RES[l];
                        curr_legs_POIS_RES = legs_POIS_RES[l];
                        curr_legs_CURSE_RES = legs_CURSE_RES[l];
                        curr_legs_DURABILITY = legs_DURABILITY[l];
                        curr_legs_WEIGHT = legs_WEIGHT[l];

                        curr_PHYS_DEF = curr_head_PHYS_DEF+curr_chest_PHYS_DEF+curr_hands_PHYS_DEF+curr_legs_PHYS_DEF;
                        if(curr_PHYS_DEF < (minima[0]-eps)){
                            continue;
                        }
                        curr_STRIKE_DEF = curr_head_STRIKE_DEF+curr_chest_STRIKE_DEF+curr_hands_STRIKE_DEF+curr_legs_STRIKE_DEF;
                        if(curr_STRIKE_DEF < (minima[1]-eps)){
                            continue;
                        }
                        curr_SLASH_DEF = curr_head_SLASH_DEF+curr_chest_SLASH_DEF+curr_hands_SLASH_DEF+curr_legs_SLASH_DEF;
                        if(curr_SLASH_DEF < (minima[2]-eps)){
                            continue;
                        }
                        curr_THRUST_DEF = curr_head_THRUST_DEF+curr_chest_THRUST_DEF+curr_hands_THRUST_DEF+curr_legs_THRUST_DEF;
                        if(curr_THRUST_DEF < (minima[3]-eps)){
                            continue;
                        }
                        curr_MAG_DEF = curr_head_MAG_DEF+curr_chest_MAG_DEF+curr_hands_MAG_DEF+curr_legs_MAG_DEF;
                        if(curr_MAG_DEF < (minima[4]-eps)){
                            continue;
                        }
                        curr_FIRE_DEF = curr_head_FIRE_DEF+curr_chest_FIRE_DEF+curr_hands_FIRE_DEF+curr_legs_FIRE_DEF;
                        if(curr_FIRE_DEF < (minima[5]-eps)){
                            continue;
                        }
                        curr_LITNG_DEF = curr_head_LITNG_DEF+curr_chest_LITNG_DEF+curr_hands_LITNG_DEF+curr_legs_LITNG_DEF;
                        if(curr_LITNG_DEF < (minima[6]-eps)){
                            continue;
                        }
                        curr_POISE = curr_head_POISE+curr_chest_POISE+curr_hands_POISE+curr_legs_POISE;
                        if((curr_POISE+extra_poise) < (minima[7]-eps)){
                            continue;
                        }
                        curr_BLEED_RES = curr_head_BLEED_RES+curr_chest_BLEED_RES+curr_hands_BLEED_RES+curr_legs_BLEED_RES;
                        if(curr_BLEED_RES < (minima[8]-eps)){
                            continue;
                        }
                        curr_POIS_RES = curr_head_POIS_RES+curr_chest_POIS_RES+curr_hands_POIS_RES+curr_legs_POIS_RES;
                        if(curr_POIS_RES < (minima[9]-eps)){
                            continue;
                        }
                        curr_CURSE_RES = curr_head_CURSE_RES+curr_chest_CURSE_RES+curr_hands_CURSE_RES+curr_legs_CURSE_RES;
                        if(curr_CURSE_RES < (minima[10]-eps)){
                            continue;
                        }
                        curr_DURABILITY = std::min(curr_head_DURABILITY, std::min(curr_chest_DURABILITY, std::min(curr_hands_DURABILITY, curr_legs_DURABILITY)));
                        if(curr_DURABILITY < (minima[11]-eps)){
                            continue;
                        }
                        curr_WEIGHT = curr_head_WEIGHT+curr_chest_WEIGHT+curr_hands_WEIGHT+curr_legs_WEIGHT;
                        if(curr_WEIGHT > (-base_weight+curr_load_threshold+eps)){
                            continue;
                        }

                        score = scalars[0]*(curr_PHYS_DEF-means[0]);
                        score += scalars[1]*(curr_STRIKE_DEF-means[1]);
                        score += scalars[2]*(curr_SLASH_DEF-means[2]);
                        score += scalars[3]*(curr_THRUST_DEF-means[3]);
                        score += scalars[4]*(curr_MAG_DEF-means[4]);
                        score += scalars[5]*(curr_FIRE_DEF-means[5]);
                        score += scalars[6]*(curr_LITNG_DEF-means[6]);
                        score += scalars[7]*(curr_BLEED_RES-means[7]);
                        score += scalars[8]*(curr_POIS_RES-means[8]);
                        score += scalars[9]*(curr_CURSE_RES-means[9]);
                        
                        curr_combo.score = score;
                        curr_combo.head = curr_head; curr_combo.chest = curr_chest; curr_combo.hands = curr_hands; curr_combo.legs = curr_legs;
                        curr_combo.physdef = curr_PHYS_DEF; curr_combo.strikedef = curr_STRIKE_DEF; curr_combo.slashdef = curr_SLASH_DEF; curr_combo.thrustdef = curr_THRUST_DEF;
                        curr_combo.magdef = curr_MAG_DEF; curr_combo.firedef = curr_FIRE_DEF; curr_combo.litngdef = curr_LITNG_DEF; 
                        curr_combo.bleedres = curr_BLEED_RES; curr_combo.poisres = curr_POIS_RES; curr_combo.curseres = curr_CURSE_RES; 
                        curr_combo.durability = curr_DURABILITY; curr_combo.poise = curr_POISE; curr_combo.weight = curr_WEIGHT; curr_combo.load = curr_load;
                        
                        if(at_max_queue_size){
                            armor_combos.push(curr_combo);
                            armor_combos.pop();
                        } else{
                            armor_combos.push(curr_combo);
                        }
                        ++curr_count;

                        if(curr_count >= max_output_size){
                            at_max_queue_size = true;
                        }

                    }

                }

            }

        }

        if(at_max_queue_size){
            break;
        }

    }

    int out_size = armor_combos.size();
    NumericVector SCORE(out_size);
    CharacterVector HEAD(out_size); CharacterVector CHEST(out_size); CharacterVector HANDS(out_size); CharacterVector LEGS(out_size);
    NumericVector PHYS_DEF(out_size); NumericVector STRIKE_DEF(out_size); NumericVector SLASH_DEF(out_size); NumericVector THRUST_DEF(out_size);
    NumericVector MAG_DEF(out_size); NumericVector FIRE_DEF(out_size); NumericVector LITNG_DEF(out_size);
    NumericVector BLEED_RES(out_size); NumericVector POIS_RES(out_size); NumericVector CURSE_RES(out_size);
    NumericVector DURABILITY(out_size); NumericVector ARMOR_POISE(out_size); NumericVector TOTAL_POISE(out_size);
    NumericVector ARMOR_WEIGHT(out_size); NumericVector TOTAL_WEIGHT(out_size); NumericVector EQUIP_LOAD(out_size); NumericVector PCT_LOAD(out_size);

    DataFrame out = DataFrame::create( 
        Named("SCORE") = SCORE , 
        _["HEAD"] = HEAD , _["CHEST"] = CHEST , _["HANDS"] = HANDS , _["LEGS"] = LEGS ,
        _["PHYS_DEF"] = PHYS_DEF , _["STRIKE_DEF"] = STRIKE_DEF , _["SLASH_DEF"] = SLASH_DEF , _["THRUST_DEF"] = THRUST_DEF ,
        _["MAG_DEF"] = MAG_DEF , _["FIRE_DEF"] = FIRE_DEF , _["LITNG_DEF"] = LITNG_DEF , 
        _["BLEED_RES"] = BLEED_RES , _["POIS_RES"] = POIS_RES , _["CURSE_RES"] = CURSE_RES , 
        _["DURABILITY"] = DURABILITY , _["ARMOR_POISE"] = ARMOR_POISE , _["TOTAL_POISE"] = TOTAL_POISE , 
        _["ARMOR_WEIGHT"] = ARMOR_WEIGHT , _["TOTAL_WEIGHT"] = TOTAL_WEIGHT , _["EQUIP_LOAD"] = EQUIP_LOAD , _["PCT_LOAD"] = PCT_LOAD 
    );

    if(out_size == 0){
        return out;
    }

    for(int n = (out_size-1); n > -1; --n){
        curr_combo = armor_combos.top();
        SCORE[n] = R::pnorm(curr_combo.score, 0.0, 1.0, true, false);
        HEAD[n] = curr_combo.head; CHEST[n] = curr_combo.chest; HANDS[n] = curr_combo.hands; LEGS[n] = curr_combo.legs;
        PHYS_DEF[n] = curr_combo.physdef; STRIKE_DEF[n] = curr_combo.strikedef; SLASH_DEF[n] = curr_combo.slashdef; THRUST_DEF[n] = curr_combo.thrustdef;
        MAG_DEF[n] = curr_combo.magdef; FIRE_DEF[n] = curr_combo.firedef; LITNG_DEF[n] = curr_combo.litngdef; 
        BLEED_RES[n] = curr_combo.bleedres; POIS_RES[n] = curr_combo.poisres; CURSE_RES[n] = curr_combo.curseres; 
        DURABILITY[n] = curr_combo.durability; ARMOR_POISE[n] = curr_combo.poise; TOTAL_POISE[n] = curr_combo.poise+extra_poise;
        ARMOR_WEIGHT[n] = curr_combo.weight; TOTAL_WEIGHT[n] = curr_combo.weight+base_weight; EQUIP_LOAD[n] = curr_combo.load; PCT_LOAD[n] = (curr_combo.weight+base_weight)/curr_combo.load;
        armor_combos.pop();
    }
    
    PHYS_DEF = round(PHYS_DEF, 1); STRIKE_DEF = round(STRIKE_DEF, 1); SLASH_DEF = round(SLASH_DEF, 1); THRUST_DEF = round(THRUST_DEF, 1);
    MAG_DEF = round(MAG_DEF, 1); FIRE_DEF = round(FIRE_DEF, 1); LITNG_DEF = round(LITNG_DEF, 1); 
    BLEED_RES = round(BLEED_RES, 1); POIS_RES = round(POIS_RES, 1); CURSE_RES = round(CURSE_RES, 1); 
    DURABILITY = round(DURABILITY, 1); ARMOR_POISE = round(ARMOR_POISE, 1); TOTAL_POISE = round(TOTAL_POISE, 1);
    ARMOR_WEIGHT = round(ARMOR_WEIGHT, 1); TOTAL_WEIGHT = round(TOTAL_WEIGHT, 1); EQUIP_LOAD = round(EQUIP_LOAD, 1);

    return out;

}
