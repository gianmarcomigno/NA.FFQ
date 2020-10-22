##### Create FULL dataset to work on
read_dta("ffq_nutrient.snmc.covariates.allcohort.10july2020.dta") %>%
 # select variable of interest
 select(lopnr, age, sex, education, syssstat, bmi, health, pre_cancer, pre_cvd, charlsonA, charlsonB, walk91, walk93, walk95, walk97, smoke, alc, civil,
        lightmilk, mediummilk, standmilk, lightyoug, yougurt, juice, lightbeer, tea, coffee, honey, cottcheese, cheese, lightcheese, crispbread, whitebread, 
        ryebread, buttbread, oatmeal, gruel, muesli, pasta, pankakes, pizza, rice, bran, meat, pork, beef, sausage, pudding, liver, liverpaste, meat_sand,
        chicken, herring, salmon, cod, caviar, shellfish, egg, potato_b, potato_f, french, carrot, lettuce, cabbage, cauli, broccoli, tomato, paprika,
        spinach, peas, onion, garlic, vegsoup, orange, ap_pear, banana, berries, otherfruit, marmalade, coffebrea, biscuits, pastries, chocolate,
        sweets, icecream, chips, nuts, dressing, mayo, cream, ketchup, gravy, medbeer, strongbeer, whitewine, redwine, desswine, liquor) %>%
 # rename columns (syssstat=employment status in 1997)
 setNames(c("id","age","sex","education", "occup_status", "bmi", "health", "pre_cancer", "pre_cvd", "charlsonA", "charlsonB", "walk91", "walk93", "walk95", 
            "walk97","smoke","alcol","civil_status",
            "milk_lowfat", "milk_medfat", "milk_highfat","yoghurt_lowfat", "yoghurt_highfat", "juice", "beer_lowalco", "tea", "coffee", "sugar", 
            "cheese_cottage", "cheese_regular", "cheese_lowfat", "bread_crisp", "bread_white", "bread_grain", "bread_sweet", "oatmeal", "porridge",
            "cerial", "spagetti", "pancake", "pizza", "rice", "wheatbran", "groundmeat", "pork", "beef", "sausage", "blacksausage", "liver",
            "liverpaste", "sandwichemeat", "chicken", "herring", "salmon", "cod", "caviar", "seafood", "egg", "potatoboiled", "potatofried",
            "french_fries", "carrot", "lettuce", "cabbage", "cauliflower", "broccoli", "tomato", "pepper", "spinach", "pea", "onion", "garlic",
            "peasoup", "orange", "apple", "banana", "berry", "fruit", "jam", "fruitsoup", "cracker", "cake", "chocolate", "candy", "icecream",
            "chips", "nut", "dressing", "mayonnaise", "cream", "ketchup", "sauce", "beer", "beer_strong", "wine_white", "wine_red", "wine_strong", "spirit")) %>%
  filter(age>=18) %>%     # keep only if age>= 18
  # add columns for sum of NA and # of walks
  mutate(na_sum = apply(.[, 19:98], MARGIN =1, function(x) sum(is.na(x))),
         walk_attended = apply(.[, 12:15], MARGIN =1, function(x) length(which(!is.na(x)))),
         age_bin = cut(age, breaks= {quantile(age, probs=seq(0,1, by=0.25), na.rm=TRUE) %>% round}, include.lowest=TRUE),
         # https://www.nhlbi.nih.gov/health/educational/lose_wt/BMI/bmicalc.htm
         bmi_bin = cut(bmi, breaks = c(-Inf,18.5,24.9,29.9,+Inf), include.lowest=TRUE),
         charlsonA = replace_na(.$charlsonA,0),
         charlsonB = replace_na(.$charlsonB,0)) %>%
  # save as a new file
  write.csv(., "ffq_snmc_subset.csv", row.names = F)
