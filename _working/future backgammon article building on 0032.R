#----------------------prov winning with different PR-------------------
xg <- xg_orig %>%
   mutate(pr_me = -Eq_per_Decision * 500,
          pr_opp = -Opp_Eq_per_move * 500,
          Result = factor(Result)) %>%
   filter(!is.na(Result))

ggplot(xg, aes(y = pr_me, x = Result) )+
   geom_boxplot() +
   coord_flip() 


# xg_doubled <- xg %>%
#    mutate(tmp = pr_me,
#           pr_me = pr_opp,
#           pr_opp = tmp, 
#           Result = factor(1 - as.numeric(as.character(Result)))) %>%
#    select(-tmp) %>%
#    rbind(xg) 
# 
#=============prediction over a grid===========
mod1 <- glm(Result ~  pr_me + pr_opp, family = "binomial", data = xg_doubled)
mod2 <- glm(Result ~  Match_Length + pr_me + pr_opp, family = "binomial", data = xg_doubled)
mod3 <- glm(Result ~  Match_Length + pr_me * pr_opp, family = "binomial", data = xg)
mod4 <- glm(Result ~  Match_Length * pr_me * pr_opp, family = "binomial", data = xg)
mod5 <- glm(Result ~  Match_Length * (pr_me + pr_opp), family = "binomial", data = xg)
# on straight comparison of AIC we'd choose the simplest model:
AIC(mod1, mod2, mod3, mod4, mod5)

# but lets check with cross-validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

set.seed(123)
mod1.cv <- train(Result ~ pr_me + pr_opp, method = "glm", family = "binomial",
                 data = xg, trControl = ctrl)
mod2.cv <- train(Result ~ Match_Length + pr_me + pr_opp, method = "glm", family = "binomial",
                 data = xg, trControl = ctrl)
mod3.cv <- train(Result ~ Match_Length + pr_me * pr_opp, method = "glm", family = "binomial",
                 data = xg, trControl = ctrl)
mod4.cv <- train(Result ~ Match_Length * pr_me * pr_opp, method = "glm", family = "binomial",
                 data = xg, trControl = ctrl)
mod5.cv <- train(Result ~ Match_Length * (pr_me + pr_opp), method = "glm", family = "binomial",
                 data = xg, trControl = ctrl)
# mod3.cv actually seems the most accurate; but only just.  Would be good to have more data!

# Note that inference (confidence intervals etc) will be biased to 'significant'
# but ok for prediction


mls <- c(1, 3, 5, 7, 9, 11)

pred_grid <- expand.grid(
   Match_Length = mls,
   pr_me = 0:300 / 10,
   pr_opp = 0:300 / 10
)

preds <- predict(mod3, newdata = pred_grid, type = "response")

pred_results <- cbind(pred_grid, preds)


p1 <- pred_results %>%
   mutate(Match_Length = paste0("Match length = ", Match_Length),
          Match_Length = factor(Match_Length, levels = paste0("Match length = ", mls))) %>%
   ggplot(aes(x = pr_me, y = pr_opp, z = preds,
              colour = ..level..)) +
   stat_contour() +
   geom_abline(slope = 1, intercept = 0, colour = "grey80") + # theoretically should be 50-50 along this line
   facet_wrap(~Match_Length) +
   labs(x = "How well I play (PR; lower is better)",
        y = "How well opponent plays (PR; lower is better)",
        title = "My chance of winning against opponent playing at different skill levels") +
   coord_equal()

svg("../img/0032-p1.svg", 8, 6)
direct.label(p1, "top.points")
dev.off()

p2 <- pred_results %>%
   filter(pr_opp %in% c(0, 5, 10, 20)) %>%
   mutate(pr_opp = paste0("Opponent PR = ", pr_opp)) %>%
   ggplot(aes(x = pr_me, y = Match_Length, z = preds,
              colour = ..level..)) +
   stat_contour() +
   facet_wrap(~pr_opp) +
   labs(x = "How well I play (PR; lower is better)",
        y = "Match length",
        title = "My chance of winning against opponent playing at different skill levels")

svg("../img/0032-p2.svg", 8, 6)
direct.label(p2, "top.points")
dev.off()
