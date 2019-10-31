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
