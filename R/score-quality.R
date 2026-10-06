

## Builds SCORE_QUALITY: for each score, its exact rarity among every armor combination at one
## upgrade level - "Top 1 in N" when at most half of all combinations score at least as well,
## "Bottom 1 in N" otherwise - where N is the total number of combinations divided by how many
## score at least as well (Top) or at most as well (Bottom). Ties count toward that number, so the
## single best combination reads "Top 1 in <every combination at this level>".
##
## Takes each slot's per-piece scores for every piece at the level, unfiltered - the same values
## get.optimal.armor.combos sums into SCORE_RAW. A combination's score is just the sum of its four
## pieces' scores, so the count can be made without visiting the ~11.9 million combinations: every
## head+chest+legs sum is computed once and sorted (~221 thousand values), and then for each of the
## hands scores (~54 values) a binary search (findInterval) counts how many of those sums complete
## a combination at or past each target score. Hands is the slot looped over because it has the
## fewest pieces: the loop does one search per hands piece per target score, while the sorted sums
## are built only once, so a small loop side keeps the per-score cost down (measured ~50x faster
## than pairing head+chest against hands+legs on 100,000 results, for the same exact counts). Materializing all ~11.9
## million sums instead would make each search cheaper still, but holds ~95 MB.
##
## SCORE_RAW adds the four slot scores in a different order than the sums here, so a combination's
## own total can differ from its SCORE_RAW in the last floating-point bits. The 1e-9 tolerance
## makes every combination count itself (and its exact ties) - scores of genuinely different
## combinations are many orders of magnitude further apart than that.
score.quality <- function(score.raw, head.scores, chest.scores, hands.scores, legs.scores){

    other.sums <- sort(as.vector(outer(outer(head.scores, chest.scores, `+`), legs.scores, `+`)))
    total <- as.numeric(length(hands.scores))*length(other.sums)

    ## Count once per distinct score - tied results are common (cosmetic armor variants)
    targets <- unique(score.raw)
    at.least <- numeric(length(targets))
    at.most <- numeric(length(targets))
    for(hands.score in hands.scores){
        at.least <- at.least+(length(other.sums)-findInterval(targets-hands.score-1e-9, other.sums, left.open = TRUE))
        at.most <- at.most+findInterval(targets-hands.score+1e-9, other.sums)
    }

    top <- at.least <= total/2
    quality <-
        ifelse(
            top,
            paste0("Top 1 in ", format(round(total/at.least), big.mark = ",", scientific = FALSE, trim = TRUE)),
            paste0("Bottom 1 in ", format(round(total/at.most), big.mark = ",", scientific = FALSE, trim = TRUE))
        )

    return(quality[match(score.raw, targets)])

}
