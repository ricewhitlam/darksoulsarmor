
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

}

