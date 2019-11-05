#' 查询所有
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_multiA_current_all()
ques_multiA_current_all <- function(brand ='JBLH'){
  conn <- conn_nsim();
  sql <-sql_paste("  select FQuestion,answ_count,FQuestionID,FBrand  from vw_qalist_multiA_current
  where FBrand = '",brand,"'
  order by answ_count desc")
  res <-sql_select(conn,sql)
  return(res)
}

#' 查询记录大于部分记录
#'
#' @param brand 品牌
#' @param count 记录
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_multiA_current_moreThan();
ques_multiA_current_moreThan <- function(brand ='JBLH',count=5){
  conn <- conn_nsim();
  sql <-sql_paste("  select FQuestion,answ_count,FQuestionID,FBrand  from vw_qalist_multiA_current
  where FBrand = '",brand,"' and answ_count > ",count,"
  order by answ_count desc")
  res <-sql_select(conn,sql)
  return(res)
}

#' 检查问题的数量
#'
#' @param brand 品牌
#' @param count 数量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_multiA_current_lessThan();
ques_multiA_current_lessThan <- function(brand ='JBLH',count=5){
  conn <- conn_nsim();
  sql <-sql_paste("  select FQuestion,answ_count,FQuestionID,FBrand  from vw_qalist_multiA_current
  where FBrand = '",brand,"' and answ_count <= ",count,"
  order by answ_count desc")
  res <-sql_select(conn,sql)
  return(res)
}




#' 获取一个问题标准答案的数据
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_standard_top9_list()
ques_standard_top9_list <- function(brand='JBLH'){
  conn <-conn_nsim();
  sql <-sql_paste("select   FQuestionId,FQuestion,FAnswerId,FAnswer  from vw_ques_standard_current
where FQuestionId in
(select  top 9 FQuestionId from vw_ques_standard_current_FQuestionId_New
where FBrand ='",brand,"') and Fbrand ='",brand,"'")
  res <- sql_select(conn,sql);
  res <-split(res,as.character(res$FQuestionId))
  return(res);
}
#查看设置标准答的处理进度
#' 查看问题的处理进度
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_standard_shedule();
ques_standard_shedule <- function(brand='JBLH'){
  conn <-conn_nsim();
  sql <-sql_paste("select FType,FCount from vw_ques_standard_schedule
where FBrand ='",brand,"'")
  res <- sql_select(conn,sql);
  #names(res) <-c('类型','数量')
  FF <- res[res$FType == '已处理','FCount'];
  FU <- res[res$FType == '待处理','FCount'];
  FP <-round(FF/(FF+FU)*100,2);
  res2 <-data.frame(FF,FU,FP,stringsAsFactors = F);
  names(res2) <-c('已处理','待处理','整体进度%')
  return(res2);
}

#' 增加辅助无效功能
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_invalidOptions();
ques_invalidOptions <- function(data){

  FQuestionId <- rep(unique(data$FQuestionId),2);
  FQuestion <-rep(unique(data$FQuestion) ,2);
  FAnswerId <- c(-1L,-2L);
  FAnswer <- c('无效问题','无效答案');
  data_invalid <- data.frame(FQuestionId,FQuestion,FAnswerId,FAnswer,stringsAsFactors = F);
  res <-rbind(data,data_invalid);
  return(res)
}


#' 问题添加辅助处理项
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_addOptions();
ques_addOptions <- function(data){
  res <- lapply(data, ques_invalidOptions);
  return(res)
}
