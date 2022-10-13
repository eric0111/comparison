#install.packages("matrixStats")
library(matrixStats)

# REPORT 
avg_results = function(timestamp){
  filenames = list.files(pattern="*output_csv")
  
  data <- data.frame(matrix(ncol = 11, nrow = length(filenames)))
  colnames(data) <- c("SEED","algo","N_FACTOR", "p", "easy", "elapsed_time_secs", "tot_clusters", "cluster_found","ARI" ,"FMI","AMI")
  
  for (i in 1:length(filenames)){
    temp = read.table(filenames[i], header = FALSE)
    print(as.vector(temp$V1))
    data[i,] = as.vector(temp$V1)
  }
  print("filenames")
  print(filenames)
  
  print("data")
  print(data)
  
  combinations = unique(data[,c('algo','N_FACTOR', 'p','easy')])
  print("combinations")
  print(combinations)
  
  report <- data.frame(matrix(ncol = 13, nrow = nrow(combinations)))
  
  colnames(report) <- c("n_iter",'algo',"N_FACTOR", "p", "easy", "elapsed_time_secs_mean","elapsed_time_secs_sd", "mean_ARI","sd_ARI","mean_FMI", "sd_FMI","mean_AMI", "sd_AMI")
  
  i= 1 
  while(i <= nrow(combinations)){
    indexes = which((data$algo == combinations$algo[i]) & (data$N_FACTOR == combinations$N_FACTOR[i]) & (data$p == combinations$p[i]) & (data$easy == combinations$easy[i]))
    print("indexes")
    print(indexes)
    
    report[i,1] = nrow(data[indexes,])
    report[i,2:5] = combinations[i,1:4]
    report[i,6] = mean(as.numeric(data[indexes,6]))
    report[i,7] = sd(as.numeric(data[indexes,6]))
    report[i,8] = mean(as.numeric(data[indexes,9]))
    report[i,9] = sd(as.numeric(data[indexes, 9]))
    report[i,10] = mean(as.numeric(data[indexes,10]))
    report[i,11] = sd(as.numeric(data[indexes, 10]))
    report[i,12] = mean(as.numeric(data[indexes,11]))
    report[i,13] = sd(as.numeric(data[indexes, 11]))
    
    
    i=i+1
  }
  
  print("report")
  print(report)
  
  #setwd('C:/Users/user/Desktop/VariationalBRAND-cpp/tests/varbrand')
  report_filename = paste('report','_',timestamp,'.csv',sep = "")
  write.csv(report,report_filename, row.names = FALSE)
 
}


## Test ##
# setwd("/home/eb/Desktop/comparison/results/test_avg")
# 
# i = 0
# while(i<2){
#   seed = i
#   set.seed(seed)
#   n_factor_list = c(0.5) # c(0.5, 1, 2.5, 5, 10)
#   p_list= c(2) #c(2, 3, 5, 7, 10)
#   easy_list = c(0)# c(0, 1)
# 
#   for(n_factor in n_factor_list){
#     for(p in p_list){
#       for(easy in easy_list){
#         generated_data = generate_data(n_factor, p, easy)
#         save_generated_data(seed, n_factor, p, easy, generated_data)
# 
#         output_brand = pico_brand(generated_data$X, generated_data$Y, generated_data$p ,generated_data$cl_train_label_noise, generated_data$G, generated_data$cl_tot)
#         save_output("BRAND", seed, n_factor, p, easy, output_brand)
# 
#         output_varbrand = pico_varbrand(generated_data$X, generated_data$Y, generated_data$p ,generated_data$cl_train_label_noise, generated_data$G, generated_data$cl_tot)
#         save_output("VARBRAND", seed, n_factor, p, easy, output_varbrand)
# 
#       }
#     }
#   }
#   i=i+1
# }

#avg_results("00:00-test")




