require(stackr)
so_index <- function(uid, site = "StackOverflow"){
  answers <- stackr::stack_users(uid, "answers", site = site),
                                 sort = "votes", pagesize = 100, num_pages = 2)
  
  sum(answers$score >= seq_along(answers$score))
  
}


# David Robinson
so_index(712603)

# Peter ellis
so_index(1181097)
uid <- 1181097; site = "Stats"
so_index(7972, site = "stats")
