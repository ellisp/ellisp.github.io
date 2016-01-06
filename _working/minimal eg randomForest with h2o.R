library(h2o)

h2o.init(ip = "localhost", 
         port = 54321, 
         startH2O = TRUE, 
         max_mem_size = '1g', 
         nthreads = -1)



mix_eg <- data.frame(X1 = sample(letters, 1000, replace = TRUE), 
                     X2 = sample(LETTERS, 1000, replace = TRUE), 
                     X3 = rnorm(1000),
                     Y = sample(c("cat", "dog", "tiger"), 1000, replace = TRUE))
train_h2o <- as.h2o(mix_eg)

mod <- h2o.randomForest(x = c("X1", "X2", "X3"), y = "Y",
                        training_frame = train_h2o)

mod
summary(mod)

h2o.shutdown(prompt = FALSE)
