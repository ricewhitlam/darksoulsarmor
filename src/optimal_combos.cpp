
#include <queue>
#include <vector>

#include <Rcpp.h>
using namespace Rcpp;

// Only the score and the four per-slot row indices are cached per candidate: every other
// metric can be recomputed from the indices, and only needs to be for the (at most
// max_output_size) combos that survive to the output loop, rather than for every candidate
// that touches the heap.
struct armor_combo {
    double score;
    int32_t h; int32_t c; int32_t g; int32_t l;
    bool operator<(const armor_combo& comparison) const
    {
        return score > comparison.score;
    }
};

// One armor slot's columns (head/chest/hands/legs are all shaped the same way), as raw
// pointers for use in the hot loop below. The wrapping NumericVector/CharacterVector members
// own/protect the underlying SEXPs; .begin() on a numeric column is already a plain double*,
// so indexing through the raw pointer is identical in behavior to indexing the vector directly.
struct Slot {
    NumericVector SCORE_vec, PHYS_DEF_vec, STRIKE_DEF_vec, SLASH_DEF_vec, THRUST_DEF_vec,
                  MAG_DEF_vec, FIRE_DEF_vec, LITNG_DEF_vec, POISE_vec, BLEED_RES_vec,
                  POIS_RES_vec, CURSE_RES_vec, DURABILITY_vec, WEIGHT_vec;
    CharacterVector ARMOR;
    const double *SCORE, *PHYS_DEF, *STRIKE_DEF, *SLASH_DEF, *THRUST_DEF,
                 *MAG_DEF, *FIRE_DEF, *LITNG_DEF, *POISE, *BLEED_RES,
                 *POIS_RES, *CURSE_RES, *DURABILITY, *WEIGHT;
    int n;
};

Slot make_slot(const DataFrame& df){
    Slot s;
    s.SCORE_vec = df["SCORE"]; s.SCORE = s.SCORE_vec.begin();
    s.ARMOR = df["ARMOR"];
    s.PHYS_DEF_vec = df["PHYS_DEF"]; s.PHYS_DEF = s.PHYS_DEF_vec.begin();
    s.STRIKE_DEF_vec = df["STRIKE_DEF"]; s.STRIKE_DEF = s.STRIKE_DEF_vec.begin();
    s.SLASH_DEF_vec = df["SLASH_DEF"]; s.SLASH_DEF = s.SLASH_DEF_vec.begin();
    s.THRUST_DEF_vec = df["THRUST_DEF"]; s.THRUST_DEF = s.THRUST_DEF_vec.begin();
    s.MAG_DEF_vec = df["MAG_DEF"]; s.MAG_DEF = s.MAG_DEF_vec.begin();
    s.FIRE_DEF_vec = df["FIRE_DEF"]; s.FIRE_DEF = s.FIRE_DEF_vec.begin();
    s.LITNG_DEF_vec = df["LITNG_DEF"]; s.LITNG_DEF = s.LITNG_DEF_vec.begin();
    s.POISE_vec = df["POISE"]; s.POISE = s.POISE_vec.begin();
    s.BLEED_RES_vec = df["BLEED_RES"]; s.BLEED_RES = s.BLEED_RES_vec.begin();
    s.POIS_RES_vec = df["POIS_RES"]; s.POIS_RES = s.POIS_RES_vec.begin();
    s.CURSE_RES_vec = df["CURSE_RES"]; s.CURSE_RES = s.CURSE_RES_vec.begin();
    s.DURABILITY_vec = df["DURABILITY"]; s.DURABILITY = s.DURABILITY_vec.begin();
    s.WEIGHT_vec = df["WEIGHT"]; s.WEIGHT = s.WEIGHT_vec.begin();
    s.n = df.nrows();
    return s;
}

//[[Rcpp::export(optimal_armor_combinations)]]
DataFrame optimal_armor_combinations(

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

    const DataFrame& head_df,
    const DataFrame& chest_df,
    const DataFrame& hands_df,
    const DataFrame& legs_df

){

    // ,
    // const double lm_beta,
    // const double lm_alpha,
    // const double lm_resid_se_inv

    Slot head = make_slot(head_df);
    Slot chest = make_slot(chest_df);
    Slot hands = make_slot(hands_df);
    Slot legs = make_slot(legs_df);

    // All four tables arrive sorted descending by SCORE (see setorder() in get.optimal.armor.combos),
    // so element 0 of each is the best score achievable from that slot alone. Once the output heap is
    // full, curr_head_SCORE/curr_chest_SCORE/curr_hands_SCORE plus these bounds give the best possible
    // score reachable from the remaining, not-yet-fixed slots at each nesting level. If that best case
    // cannot beat the heap's current worst kept score, neither can this candidate or any later one in
    // the same (descending-sorted) loop, since the loop only ever advances forward - so it is safe to
    // break out of that level entirely rather than merely skip the current candidate.
    double best_chest_SCORE = chest.SCORE[0];
    double best_hands_SCORE = hands.SCORE[0];
    double best_legs_SCORE = legs.SCORE[0];

    int I = head.n; int J = chest.n; int K = hands.n; int L = legs.n;

    int curr_count = 0;

    double curr_load_threshold;
    double eps = 1.0e-10;

    double extra_poise = 0.0;
    if(wolf){extra_poise = 40.0;};

    double curr_head_SCORE; double curr_chest_SCORE; double curr_hands_SCORE; double curr_legs_SCORE;
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
    // Reserve capacity for the heap's backing storage up front, so it never has to reallocate
    // and copy its contents as it fills to max_output_size. priority_queue exposes no reserve()
    // of its own, but its constructor can take ownership of an already-reserved container.
    std::vector<armor_combo> armor_combos_storage;
    armor_combos_storage.reserve(max_output_size);
    std::priority_queue<armor_combo> armor_combos(std::less<armor_combo>(), std::move(armor_combos_storage));
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
                curr_load_threshold = load_threshold_motf;
            } else{
                curr_load_threshold = load_threshold;
            }

            curr_head_SCORE = head.SCORE[i];

            if(at_max_queue_size && (curr_head_SCORE+best_chest_SCORE+best_hands_SCORE+best_legs_SCORE) <= armor_combos.top().score){
                break;
            }

            curr_head_PHYS_DEF = head.PHYS_DEF[i];
            curr_head_STRIKE_DEF = head.STRIKE_DEF[i];
            curr_head_SLASH_DEF = head.SLASH_DEF[i];
            curr_head_THRUST_DEF = head.THRUST_DEF[i];
            curr_head_MAG_DEF = head.MAG_DEF[i];
            curr_head_FIRE_DEF = head.FIRE_DEF[i];
            curr_head_LITNG_DEF = head.LITNG_DEF[i];
            curr_head_POISE = head.POISE[i];
            curr_head_BLEED_RES = head.BLEED_RES[i];
            curr_head_POIS_RES = head.POIS_RES[i];
            curr_head_CURSE_RES = head.CURSE_RES[i];
            curr_head_DURABILITY = head.DURABILITY[i];
            curr_head_WEIGHT = head.WEIGHT[i];

            for(int j = 0; j < curr_J; ++j){

                if(i != loop_size_1 && j != loop_size_1 && L_capped && K_capped && !J_capped){
                    j = loop_size_1;
                }

                curr_chest_SCORE = chest.SCORE[j];

                if(at_max_queue_size && (curr_head_SCORE+curr_chest_SCORE+best_hands_SCORE+best_legs_SCORE) <= armor_combos.top().score){
                    break;
                }

                curr_chest_PHYS_DEF = chest.PHYS_DEF[j];
                curr_chest_STRIKE_DEF = chest.STRIKE_DEF[j];
                curr_chest_SLASH_DEF = chest.SLASH_DEF[j];
                curr_chest_THRUST_DEF = chest.THRUST_DEF[j];
                curr_chest_MAG_DEF = chest.MAG_DEF[j];
                curr_chest_FIRE_DEF = chest.FIRE_DEF[j];
                curr_chest_LITNG_DEF = chest.LITNG_DEF[j];
                curr_chest_POISE = chest.POISE[j];
                curr_chest_BLEED_RES = chest.BLEED_RES[j];
                curr_chest_POIS_RES = chest.POIS_RES[j];
                curr_chest_CURSE_RES = chest.CURSE_RES[j];
                curr_chest_DURABILITY = chest.DURABILITY[j];
                curr_chest_WEIGHT = chest.WEIGHT[j];

                for(int k = 0; k < curr_K; ++k){

                    if(i != loop_size_1 && j != loop_size_1 && k != loop_size_1 && L_capped && !K_capped){
                        k = loop_size_1;
                    }

                    curr_hands_SCORE = hands.SCORE[k];

                    if(at_max_queue_size && (curr_head_SCORE+curr_chest_SCORE+curr_hands_SCORE+best_legs_SCORE) <= armor_combos.top().score){
                        break;
                    }

                    curr_hands_PHYS_DEF = hands.PHYS_DEF[k];
                    curr_hands_STRIKE_DEF = hands.STRIKE_DEF[k];
                    curr_hands_SLASH_DEF = hands.SLASH_DEF[k];
                    curr_hands_THRUST_DEF = hands.THRUST_DEF[k];
                    curr_hands_MAG_DEF = hands.MAG_DEF[k];
                    curr_hands_FIRE_DEF = hands.FIRE_DEF[k];
                    curr_hands_LITNG_DEF = hands.LITNG_DEF[k];
                    curr_hands_POISE = hands.POISE[k];
                    curr_hands_BLEED_RES = hands.BLEED_RES[k];
                    curr_hands_POIS_RES = hands.POIS_RES[k];
                    curr_hands_CURSE_RES = hands.CURSE_RES[k];
                    curr_hands_DURABILITY = hands.DURABILITY[k];
                    curr_hands_WEIGHT = hands.WEIGHT[k];

                    for(int l = 0; l < curr_L; ++l){

                        if(i != loop_size_1 && j != loop_size_1 && k != loop_size_1 && l != loop_size_1 && !L_capped){
                            l = loop_size_1;
                        }

                        curr_legs_SCORE = legs.SCORE[l];

                        if(at_max_queue_size && (curr_head_SCORE+curr_chest_SCORE+curr_hands_SCORE+curr_legs_SCORE) <= armor_combos.top().score){
                            break;
                        }

                        curr_legs_WEIGHT = legs.WEIGHT[l];
                        curr_WEIGHT = curr_head_WEIGHT+curr_chest_WEIGHT+curr_hands_WEIGHT+curr_legs_WEIGHT;
                        if(curr_WEIGHT > (-base_weight+curr_load_threshold+eps)){
                            continue;
                        }
                        curr_legs_POISE = legs.POISE[l];
                        curr_POISE = curr_head_POISE+curr_chest_POISE+curr_hands_POISE+curr_legs_POISE;
                        if((curr_POISE+extra_poise) < (minima[7]-eps)){
                            continue;
                        }
                        curr_legs_PHYS_DEF = legs.PHYS_DEF[l];
                        curr_PHYS_DEF = curr_head_PHYS_DEF+curr_chest_PHYS_DEF+curr_hands_PHYS_DEF+curr_legs_PHYS_DEF;
                        if(curr_PHYS_DEF < (minima[0]-eps)){
                            continue;
                        }
                        curr_legs_STRIKE_DEF = legs.STRIKE_DEF[l];
                        curr_STRIKE_DEF = curr_head_STRIKE_DEF+curr_chest_STRIKE_DEF+curr_hands_STRIKE_DEF+curr_legs_STRIKE_DEF;
                        if(curr_STRIKE_DEF < (minima[1]-eps)){
                            continue;
                        }
                        curr_legs_SLASH_DEF = legs.SLASH_DEF[l];
                        curr_SLASH_DEF = curr_head_SLASH_DEF+curr_chest_SLASH_DEF+curr_hands_SLASH_DEF+curr_legs_SLASH_DEF;
                        if(curr_SLASH_DEF < (minima[2]-eps)){
                            continue;
                        }
                        curr_legs_THRUST_DEF = legs.THRUST_DEF[l];
                        curr_THRUST_DEF = curr_head_THRUST_DEF+curr_chest_THRUST_DEF+curr_hands_THRUST_DEF+curr_legs_THRUST_DEF;
                        if(curr_THRUST_DEF < (minima[3]-eps)){
                            continue;
                        }
                        curr_legs_MAG_DEF = legs.MAG_DEF[l];
                        curr_MAG_DEF = curr_head_MAG_DEF+curr_chest_MAG_DEF+curr_hands_MAG_DEF+curr_legs_MAG_DEF;
                        if(curr_MAG_DEF < (minima[4]-eps)){
                            continue;
                        }
                        curr_legs_FIRE_DEF = legs.FIRE_DEF[l];
                        curr_FIRE_DEF = curr_head_FIRE_DEF+curr_chest_FIRE_DEF+curr_hands_FIRE_DEF+curr_legs_FIRE_DEF;
                        if(curr_FIRE_DEF < (minima[5]-eps)){
                            continue;
                        }
                        curr_legs_LITNG_DEF = legs.LITNG_DEF[l];
                        curr_LITNG_DEF = curr_head_LITNG_DEF+curr_chest_LITNG_DEF+curr_hands_LITNG_DEF+curr_legs_LITNG_DEF;
                        if(curr_LITNG_DEF < (minima[6]-eps)){
                            continue;
                        }
                        curr_legs_BLEED_RES = legs.BLEED_RES[l];
                        curr_BLEED_RES = curr_head_BLEED_RES+curr_chest_BLEED_RES+curr_hands_BLEED_RES+curr_legs_BLEED_RES;
                        if(curr_BLEED_RES < (minima[8]-eps)){
                            continue;
                        }
                        curr_legs_POIS_RES = legs.POIS_RES[l];
                        curr_POIS_RES = curr_head_POIS_RES+curr_chest_POIS_RES+curr_hands_POIS_RES+curr_legs_POIS_RES;
                        if(curr_POIS_RES < (minima[9]-eps)){
                            continue;
                        }
                        curr_legs_CURSE_RES = legs.CURSE_RES[l];
                        curr_CURSE_RES = curr_head_CURSE_RES+curr_chest_CURSE_RES+curr_hands_CURSE_RES+curr_legs_CURSE_RES;
                        if(curr_CURSE_RES < (minima[10]-eps)){
                            continue;
                        }
                        curr_legs_DURABILITY = legs.DURABILITY[l];
                        curr_DURABILITY = std::min(curr_head_DURABILITY, std::min(curr_chest_DURABILITY, std::min(curr_hands_DURABILITY, curr_legs_DURABILITY)));
                        if(curr_DURABILITY < (minima[11]-eps)){
                            continue;
                        }

                        curr_combo.score = curr_head_SCORE+curr_chest_SCORE+curr_hands_SCORE+curr_legs_SCORE;
                        curr_combo.h = i; curr_combo.c = j; curr_combo.g = k; curr_combo.l = l;

                        if(at_max_queue_size){
                            if(curr_combo < armor_combos.top()){
                                armor_combos.push(curr_combo);
                                armor_combos.pop();
                            }
                        } else{
                            armor_combos.push(curr_combo);
                            ++curr_count;
                            if(curr_count >= max_output_size){
                                at_max_queue_size = true;
                            }
                        }

                    }

                }

            }

        }

    }

    int out_size = armor_combos.size();
    NumericVector SCORE_RAW(out_size); NumericVector SCORE_PCT(out_size);
    // NumericVector SCORE_RESID_RAW(out_size); NumericVector SCORE_RESID_PCT(out_size);
    CharacterVector HEAD(out_size); CharacterVector CHEST(out_size); CharacterVector HANDS(out_size); CharacterVector LEGS(out_size);
    NumericVector PHYS_DEF(out_size); NumericVector STRIKE_DEF(out_size); NumericVector SLASH_DEF(out_size); NumericVector THRUST_DEF(out_size);
    NumericVector MAG_DEF(out_size); NumericVector FIRE_DEF(out_size); NumericVector LITNG_DEF(out_size);
    NumericVector BLEED_RES(out_size); NumericVector POIS_RES(out_size); NumericVector CURSE_RES(out_size);
    NumericVector DURABILITY(out_size); NumericVector ARMOR_POISE(out_size); NumericVector TOTAL_POISE(out_size); NumericVector POISE_TIMER(out_size);
    NumericVector ARMOR_WEIGHT(out_size); NumericVector TOTAL_WEIGHT(out_size); NumericVector EQUIP_LOAD(out_size); NumericVector PCT_LOAD(out_size);

    DataFrame out = DataFrame::create(
        Named("SCORE_RAW") = SCORE_RAW , _["SCORE_PCT"] = SCORE_PCT ,
        // _["SCORE_RESID_RAW"] = SCORE_RESID_RAW ,  _["SCORE_RESID_PCT"] = SCORE_RESID_PCT ,
        _["HEAD"] = HEAD , _["CHEST"] = CHEST , _["HANDS"] = HANDS , _["LEGS"] = LEGS ,
        _["PHYS_DEF"] = PHYS_DEF , _["STRIKE_DEF"] = STRIKE_DEF , _["SLASH_DEF"] = SLASH_DEF , _["THRUST_DEF"] = THRUST_DEF ,
        _["MAG_DEF"] = MAG_DEF , _["FIRE_DEF"] = FIRE_DEF , _["LITNG_DEF"] = LITNG_DEF ,
        _["BLEED_RES"] = BLEED_RES , _["POIS_RES"] = POIS_RES , _["CURSE_RES"] = CURSE_RES ,
        _["DURABILITY"] = DURABILITY , _["ARMOR_POISE"] = ARMOR_POISE , _["TOTAL_POISE"] = TOTAL_POISE , _["POISE_TIMER"] = POISE_TIMER ,
        _["ARMOR_WEIGHT"] = ARMOR_WEIGHT , _["TOTAL_WEIGHT"] = TOTAL_WEIGHT , _["EQUIP_LOAD"] = EQUIP_LOAD , _["PCT_LOAD"] = PCT_LOAD
    );

    if(out_size == 0){
        return out;
    }

    double timer_0 = 5.0; double timer_1 = timer_0*0.9; double timer_2 = timer_1*0.9; double timer_3 = timer_2*0.9; double timer_4 = timer_3*0.9;

    int out_h; int out_c; int out_g; int out_l;
    double out_head_POISE; double out_chest_POISE; double out_hands_POISE; double out_legs_POISE; double out_POISE;
    double out_WEIGHT; double out_load; int out_poise_count;

    for(int n = (out_size-1); n > -1; --n){
        curr_combo = armor_combos.top();
        out_h = curr_combo.h; out_c = curr_combo.c; out_g = curr_combo.g; out_l = curr_combo.l;

        SCORE_RAW[n] = curr_combo.score; SCORE_PCT[n] = R::pnorm(curr_combo.score, 0.0, 1.0, true, false);
        // SCORE_RESID_RAW[n] = lm_resid_se_inv*((lm_beta*out_WEIGHT+lm_alpha)-curr_combo.score); SCORE_RESID_PCT[n] = R::pnorm(SCORE_RESID_RAW[n], 0.0, 1.0, true, false);
        HEAD[n] = head.ARMOR[out_h]; CHEST[n] = chest.ARMOR[out_c]; HANDS[n] = hands.ARMOR[out_g]; LEGS[n] = legs.ARMOR[out_l];
        PHYS_DEF[n] = head.PHYS_DEF[out_h]+chest.PHYS_DEF[out_c]+hands.PHYS_DEF[out_g]+legs.PHYS_DEF[out_l];
        STRIKE_DEF[n] = head.STRIKE_DEF[out_h]+chest.STRIKE_DEF[out_c]+hands.STRIKE_DEF[out_g]+legs.STRIKE_DEF[out_l];
        SLASH_DEF[n] = head.SLASH_DEF[out_h]+chest.SLASH_DEF[out_c]+hands.SLASH_DEF[out_g]+legs.SLASH_DEF[out_l];
        THRUST_DEF[n] = head.THRUST_DEF[out_h]+chest.THRUST_DEF[out_c]+hands.THRUST_DEF[out_g]+legs.THRUST_DEF[out_l];
        MAG_DEF[n] = head.MAG_DEF[out_h]+chest.MAG_DEF[out_c]+hands.MAG_DEF[out_g]+legs.MAG_DEF[out_l];
        FIRE_DEF[n] = head.FIRE_DEF[out_h]+chest.FIRE_DEF[out_c]+hands.FIRE_DEF[out_g]+legs.FIRE_DEF[out_l];
        LITNG_DEF[n] = head.LITNG_DEF[out_h]+chest.LITNG_DEF[out_c]+hands.LITNG_DEF[out_g]+legs.LITNG_DEF[out_l];
        BLEED_RES[n] = head.BLEED_RES[out_h]+chest.BLEED_RES[out_c]+hands.BLEED_RES[out_g]+legs.BLEED_RES[out_l];
        POIS_RES[n] = head.POIS_RES[out_h]+chest.POIS_RES[out_c]+hands.POIS_RES[out_g]+legs.POIS_RES[out_l];
        CURSE_RES[n] = head.CURSE_RES[out_h]+chest.CURSE_RES[out_c]+hands.CURSE_RES[out_g]+legs.CURSE_RES[out_l];
        DURABILITY[n] = std::min(head.DURABILITY[out_h], std::min(chest.DURABILITY[out_c], std::min(hands.DURABILITY[out_g], legs.DURABILITY[out_l])));

        out_head_POISE = head.POISE[out_h]; out_chest_POISE = chest.POISE[out_c]; out_hands_POISE = hands.POISE[out_g]; out_legs_POISE = legs.POISE[out_l];
        out_POISE = out_head_POISE+out_chest_POISE+out_hands_POISE+out_legs_POISE;
        ARMOR_POISE[n] = out_POISE; TOTAL_POISE[n] = out_POISE+extra_poise;
        out_poise_count = (out_head_POISE > 1.0e-10)+(out_chest_POISE > 1.0e-10)+(out_hands_POISE > 1.0e-10)+(out_legs_POISE > 1.0e-10);
        switch(out_poise_count){
            case 0:
                POISE_TIMER[n] = timer_0;
                break;
            case 1:
                POISE_TIMER[n] = timer_1;
                break;
            case 2:
                POISE_TIMER[n] = timer_2;
                break;
            case 3:
                POISE_TIMER[n] = timer_3;
                break;
            case 4:
                POISE_TIMER[n] = timer_4;
                break;
        }

        out_WEIGHT = head.WEIGHT[out_h]+chest.WEIGHT[out_c]+hands.WEIGHT[out_g]+legs.WEIGHT[out_l];
        out_load = (out_h == motf_index) ? load_motf : load;
        ARMOR_WEIGHT[n] = out_WEIGHT; TOTAL_WEIGHT[n] = out_WEIGHT+base_weight; EQUIP_LOAD[n] = out_load; PCT_LOAD[n] = (out_WEIGHT+base_weight)/out_load;
        armor_combos.pop();
    }

    return out;

}
