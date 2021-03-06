# 1.查找标准问题辅助函数--------
#' 查询标准问题
#'
#' @param data  数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_find_standardQues()
nsim_find_standardQues <- function(data){
  ncount <- nrow(data);
  data$FStandard <- rep(0L,ncount);
  data[1,'FStandard'] <-1L;
  return(data);
}


#' find the answer
#'
#' @param data data
#' @param dict dict
#'
#' @return return
#' @export
#'
#' @examples
#' nsim_find_standardAnsw()
nsim_find_standardAnsw <- function(data,dict)
{
   ncount <- nrow(data);
   input <- unique(data$FQues_std);
   data_manual <- getAnswerByQuestion_dict(input,dict);
   if( is.na(data_manual)){
      data$FStandard <- rep(0L,ncount);
      data[1,'FStandard'] <-1L;

   }else{
      data<- data[1,]
      data[1,'FAnsw_std'] <- data_manual
      data[1,'FStandard'] <-1L



   }
   return(data)

}


#' 处理名称
#'
#' @param data 数据
#'
#' @return 返回值'
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' nsim_aux_combine_ques();
nsim_aux_combine_ques <- function(data){
  FQuestion <-vect_as_dbl_equal(data$FQuestion)
  FAnswer <- data$FAnswer[1]
  FAnswerId <-data$FAnswerId[1]
  res <- data.frame(FQuestion,FAnswer,FAnswerId,stringsAsFactors = F)
  return(res)

}

#2.根据答案聚合问题，进行版本化update------
#' 提供标准问题,进行版本化存储
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_getStandarOne();
#'
#'
ques_combine_ByAnsw <- function(brand='JBLH',version='V1'){
  data <- nsim_qalist_fullTxt_current(brand = brand);
  ques_count <-nsim_item_ques_count_currentVersion(brand);
  res <- merge(data,ques_count);
  res <- res[order(res$FAnswerId,-res$ques_count),]
  #针对数据进行分类处理；
  FAnswerId_str <- as.character(res$FAnswerId);
  res2 <- split(res,FAnswerId_str);
  res2 <- lapply(res2, nsim_find_standardQues);
  res2 <-do.call('rbind',res2);
  # var_version <- nsim_version_getCurrentVersion(brand,'nsdict');
  res2$FVersionTxt <- rep(version,nrow(res2));
  rownames(res2) <-NULL
  #return(res2)
  nsim_save(res2,'qalist_ques_combined_by_answer');

}

#2.1提取标准问题update-----
#' 查询
#'
#' @param brand 品牌
#' @param var_version 提示版本处理
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_combine_ByAnsw_query_standard();
ques_combine_ByAnsw_query_standard <- function(brand='JBLH',var_version='V1') {
  conn <- conn_nsim();
  #var_version <-nsim_version_getCurrentVersion(brand,'nsdict');
  sql <- paste("select FQuestion,FAnswer,FAnswerId from qalist_ques_combined_by_answer
where fbrand ='",brand,"' and FStandard =1 and FVersionTxt='",var_version,"'",sep="");
  res <- sql_select(conn,sql);
  return(res);

}

#2.2生产类似问题 update-----
#'处理问题合并，生成类似问题
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_combine_ByAnsw_query_like();
ques_combine_ByAnsw_query_like <-function(brand='JBLH',var_version='V1') {
  conn <- conn_nsim();
  #var_version <-nsim_version_getCurrentVersion(brand,'nsdict');
  sql <- paste("select FQuestion,FAnswer,FAnswerId from qalist_ques_combined_by_answer
where fbrand ='",brand,"' and FStandard = 0 and FVersionTxt='",var_version,"'",sep="");
  res <- sql_select(conn,sql);
  FAnswerId_str <- as.character(res$FAnswerId)
  res2 <- split(res,FAnswerId_str);
  res2 <- lapply(res2,nsim_aux_combine_ques)
  res2 <- do.call('rbind',res2);
  return(res2);

}





#' 处理数据，分步处理
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_nscs_version_step1();
nsim_nscs_version_step1 <- function(brand='JBLH'){
   #获取版本号
   var_version <- tsda::nsim_version_getNextVersion(brand,'nscs');
   print('1')
   #1.更新问题分类
   ques_category_byDictSegment_underline(brand,var_version);
   print('2')
   #2.更新答案分类
   answ_category_by_ques_category_via_qalist(brand,var_version);
   print('3')
   #3.根据答案聚合问题
   ques_combine_ByAnsw(brand,var_version);
   print('4')
   # 获取标题问题
   data_standard <-ques_combine_ByAnsw_query_standard(brand,var_version);
   print('5')
   names(data_standard) <-c('FQues_std','FAnsw_std','FId');
   print('6')

   #获取相应问题
   data_like <-ques_combine_ByAnsw_query_like(brand,var_version);
   names(data_like) <-c('FQues_like','FAnsw_like','FId');
   data_detail <-left_join(data_standard,data_like,by='FId');
   data_detail$FQues_like <- tsdo::na_values(data_detail$FQues_like,data_detail$FQues_std);
   print('7')
   #获取答案分类
   data_category <-answ_category_query_version(brand,var_version);
   names(data_category) <-c('FId','FCategory');
   res<- merge(data_detail,data_category)
   print('8')
   #针对标题问题进行排序，然后取答案的第一个
   res <- res[order(res$FQues_std),];
   #然后处理答答案第一个
   print('9')
   #col_names <-names(res);
   print('9.1')
   #插入答案功能，如果存在人工指定的答案，则取人工，另则取第一个；
   return(res)
}

#针对问题与答案进行处理
#' 问题聚类的函数
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @import nsdictpkg
#' @import nsclpkg
#' @import nsblpkg
#' @import dplyr
#' @export
#'
#' @examples
#' nsim_nscs_version();
nsim_nscs_version <- function(brand='JBLH'){
   #获取版本号
   var_version <- tsda::nsim_version_getNextVersion(brand,'nscs');

   res <-nsim_nscs_version_step1(brand)
   col_names <- names(res);
   print('step1-9')
   res <-split(res,res$FQues_std);
   print('9.2')
   dict <- getStandardAsnwer_Manually(brand);
   print('9.3')
   #完善查看标准答案的功能--
   res <- lapply(res, nsim_find_standardAnsw,dict=dict);
   print('9.4')
   View(res);
   print(str(res));
   res <- do.call('rbind',res);
   res <- res[res$FStandard ==1L,col_names]
   print('10')
   row_count <- nrow(res);
   A <- paste(brand,tsdo::left(as.character(Sys.Date()),7),res$FCategory,sep="/");
   B <- res$FQues_std;
   C <- res$FQues_like;
   D <- res$FAnsw_std;
   E <- rep("1",row_count);
   F <-rep("",row_count);
   G <-rep("",row_count);
   H <- rep("1",row_count);
   I <-rep("",row_count);
   FBrand <-rep(brand,row_count)
    FVersionTxt<-rep(var_version,row_count)
   res_formatted <- data.frame(A,B,C,D,E,F,G,H,I,FBrand,FVersionTxt,stringsAsFactors = FALSE);
   print('11')
   #return(res_formatted);
   tsda::nsim_save(res_formatted,'nscs_version')
   print('12')
   #更新版本信息
   tsda::nsim_version_setCurrentVersion(brand,'nscs',var_version);
   print('13')

}


#' 获取最新版本的聚类数据
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_nscs_current();
nsim_nscs_current <- function(brand ='JBLH'){
   conn <- conn_nsim()
   var_version <- tsda::nsim_version_getCurrentVersion(brand,'nscs');
   sql <- paste("select  A,B,C,D,E,F,G,H,I from nscs_version
where FVersionTxt ='",var_version,"' and FBrand='",brand,"'",sep="")
   res <-sql_select(conn,sql);
   return(res);
}

