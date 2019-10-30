#' 问题分类查询
#'
#' @param brand  品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_categoried_query_current();
ques_categoried_query_current <- function(brand ='JBLH'){
  conn <- conn_nsim();
  sql <- sql_paste("select  c.FQuestion, a.FCategory  from ques_category_version a
inner join nsim_version b
on a.FVersionTxt = b.FVersionTxt
and a.FBrand = b.FBrand
inner join  item_question_version c
on a.FQuestionId = c.FQuestionId
and a.FBrand = c.FBrand
inner join nsim_version d
on c.FVersionTxt = d.FVersionTxt
and c.FBrand = d.FBrand
where a.FBrand='",brand,"' and b.FCurrentVersion =1 and b.FType='nscs'
and a.FCategory <>'未分类'
and d.FCurrentVersion =1 and d.FType='nscl'
order by a.FCategory
")
  res <- sql_select(conn,sql);
  return(res);
}


#' 未分类问题清单
#'
#' @param brand  品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_uncategoried_query_current();
ques_uncategoried_query_current <- function(brand ='JBLH'){
  conn <- conn_nsim();
  sql <- sql_paste("select  c.FQuestion, a.FCategory  from ques_category_version a
inner join nsim_version b
on a.FVersionTxt = b.FVersionTxt
and a.FBrand = b.FBrand
inner join  item_question_version c
on a.FQuestionId = c.FQuestionId
and a.FBrand = c.FBrand
inner join nsim_version d
on c.FVersionTxt = d.FVersionTxt
and c.FBrand = d.FBrand
where a.FBrand='",brand,"' and b.FCurrentVersion =1 and b.FType='nscs'
and a.FCategory = '未分类'
and d.FCurrentVersion =1 and d.FType='nscl'
order by a.FCategory
")
  res <- sql_select(conn,sql);
  return(res);
}

