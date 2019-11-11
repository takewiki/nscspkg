#' 从人工指定的地方获取相应的标准问答
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' getStandardAsnwer_Manually();
getStandardAsnwer_Manually <- function(brand ='JBLH'){
  conn <- conn_nsim();
  sql <- sql_paste("select b.FQuestion,b.FAnswer from  ques_standard_answer a
inner join (select FQuestionId,FQuestion,FAnswerId,FAnswer  from  qalist_ques_combined_by_answer
where FVersionTxt in
(select FVersionTxt from nsim_version where FCurrentVersion =1
and FType='nscs')
) b
on a.FQuestionId = b.FQuestionId and a.FAnswerId= b.FAnswerId
where a.FAnswerId >0
");
  res <- sql_select(conn,sql);
  return(res)
}



#' GET
#'
#' @param input  INPUT
#' @param dict DICT
#'
#' @return RETURN
#' @export
#'
#' @examples
#' getAnswerByQuestion_dict();
getAnswerByQuestion_dict <- function(input,dict){
  res <- dict[dict$FQuestion ==input,'FAnswer']

  return(res[1])
}
