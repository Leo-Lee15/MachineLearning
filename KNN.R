### 用R实现KNN算法 ---------------------------------

## Step 1：定义距离函数
distance <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("x and y must be numeric!")
  } 
  dd <- sqrt(sum((x - y)^2))
  return(dd)
}

## Step 2: 构造KNN函数
# 这里.df表示特征组成的数据框
# .y表示分类标签向量
# .k表示最近邻k
# .newpoint表示新的点，这里只能是数值向量
leo_knn_mon <- function(.df, .y, .k, .new_point) {
  
  if (!is.numeric(.new_point)) {
    stop(".new_point must be numeric")
  } 

   # 对特征数据进行标准化
    scale_mat <- scale(.df)
    
    # 新点到原始点的距离
    x_new_dist <- apply(scale_mat, 1, 
                        function(x) sqrt(sum((x - .new_point)^2)))
    
    # 构造距离的数据框
    dist_df <- data.frame(
      x_new_dist,
      label = .y
    )
    
    # 分类
    library(magrittr)
    dist_df %>%
      dplyr::arrange(x_new_dist) %>%
      dplyr::slice(1:.k) %>%
      dplyr::group_by(label) %>%
      dplyr::summarise(num = n()) %>%
      dplyr::arrange(num) %>%
      dplyr::slice(1) %>%
      dplyr::select(label) -> results
    
    # 对输出结果进行格式调整
    result_df <- data.frame(as.data.frame(t(.new_point)), label = results$label)
    names(result_df)[1:length(.df)] <- names(.df)
    # return(results)
    return(result_df)
}

## 例子
set.seed(1234)
x_knn <- data.frame(
  x1 = c(rnorm(100, 2, 1), rnorm(100)),
  x2 = c(rpois(100, 6), rpois(100, 10)),
  x3 = rep(c("yes", "no"), each = 100)
)
x_knn
leo_knn_mon(x_knn[, -3], x_knn[, 3], 20, .new_point = c(0.88, 20))
  

## 上面的leo_knn_mon函数一个只能对一个观测值分类
# leo_knn_mul可以同时对新的观测值分类
leo_knn_mul <- function(.df, .y, .k, .new_point_df) {
  n <- nrow(.new_point_df)
  res <- vector(mode = "list", length = n)
  for (i in 1:n) {
    res[[i]] <- leo_knn_mon(.df, .y, .k, unlist(.new_point_df[i, ]))
  }
  
  res_final <- do.call(rbind, res)
  return(res_final)
}

# 例子

leo_knn_mul(x_knn[, -3], x_knn[, 3], 20, 
            .new_point_df = data.frame(x1 = rpois(10, 5),
                                       x2 = rt(10, 3)))
leo_knn_mon(x_knn[, -3], x_knn[, 3], 30, .new_point_df[1, ])


