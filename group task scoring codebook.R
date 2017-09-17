#### Task Scoring Codebook ####

# suffixes
.g -> score for the whole group (i.e., based on the answer sheet the group
                                 turned in)

#### Problem Solving ####
## "problem solving scoring.R" computes scores for groups and individuals
## total score out of 28; 2 questions (#20 and 27) were dropped because of
## errors (no answer for 20, 2 possible answers for 27)


# scored variables
PSscore: sums the total number of correct answers
PS.totans: total number of questions that were answered
PS.percor: % of given answers that were correct (score/totans)
PSinfluence: measure of p's influence on the group's answers; 
        # of questions for which the group gave the same answer as the 
        participant (note that 2+ group members could both get credit if 
                     they have the same answer)
PS.perinf: % of the group's answered questions that == the p's answers
PSinfluencecor: # of influenced answers (p = g) that were correct
PS.perinfcor: % of the *influenced* answers that were correct (influencecor/influence)
PS.perinfcor_tot: % of *all* answers that were correct (influencecor/totans.g)
infwrong: # of influenced answers that were incorrect
PS.perinfwrong: % of influenced answers that were incorrect
PS.perinfwrong_tot: % of all answers that were incorrect

#### LOM scoring ####
LOM.pergrp_correl: correlation between person's responses and group's responses
LOM.perscorrect_correl: correlation between person's responses and correct responses
LOM.grpcorrect_correl: correlations bt group's response and correct response

exact: number of responses that were exactly correct
LOM.totans: total number of responses given
per.exact: percentage of given respones that were correct

LOM.influence: number of group's reponses that were the same as p's responses
LOM.perc.inf: % of total # of groups given answers that were same as p
LOM.influence_cor: # of correct influences
LOM.influence_cor_per: % of total influences that were correct
LOM.influence_c_p_tot: % of groups total answers that were correct influences
LOM.influence_wrong: # of incorrect influences
LOM.influence_wro_per: % of influences that were wrong
LOM.influence_w_p_tot: % of gorups total answers that were incorrect influences


#### lgd ####
LGD.prize: 1:5; award amounts for 1st - 5th place
LGD.winner: who was the winner (numeric) 1= alex; 2 = barb; 3=cate, 4 = don, 5=erica
LGD.nplace: who were the 2nd:5th place (numeric)
LGD.lastplace: who came in last (numeric)

LGD.winner_name: name of the winner
LGD.nplace_name: name of person who came in nth place
LGD.lastplace_name: name of last place

lgd.win: 1 if the person's  nominee won (otherwise 0)
lgd.lose: 1 if the person's nominee came in last place in their group
lgd.place: place (1-5) the person finished; 5 is always last place - 
        if person finished 4th in a 4-person group, coded as 5 (no 4th place in
                                                                that group);
        if person finished 4th in a 5-person group, coded as 4
