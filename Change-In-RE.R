calc_play_score <- function(success, 
                         failure,
                         nothing,
                         prob = 0.6) {
  a <- prob * success
  b <- (1-prob) * failure
  c <- -1*nothing
  return(a+b+c)
}

# 0 Out
calc_play_score(1.119, 0.165, 0.860, 
             prob = 0.88)
# -0.03

# 1 Out
calc_play_score(1.12, 0.16, 0.86, 
             prob = 0.75)
#0.02

