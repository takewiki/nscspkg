nsim_nscs_version_step1();


brand ='JBLH'

#获取版本号
var_version <- tsda::nsim_version_getNextVersion(brand,'nscs');
print('1')
#1.更新问题分类
ques_category_byDictSegment_underline(brand,var_version);

#检查问题；


ques_category_byDictSegment_underline









print('2')
#2.更新答案分类

answ_category_by_ques_category_via_qalist(brand,var_version);

#查询计划；
answ_category_by_ques_category_via_qalist



# var_version <- nsim_version_getCurrentVersion(brand,'nsdict');
qalist <-nsim_qalist_fullTxt_current(brand);
qalist_id <-unique(qalist[,c('FQuestionId','FAnswerId')]);
ques_category <-ques_category_query_current(brand);
res <- merge(ques_category,qalist_id);
res <-res[,c('FAnswerId','FCategory')];
res <- unique(res);
res <- res[order(res$FAnswerId),]
rownames(res)<-NULL
res$FBrand <- rep(brand,nrow(res));
res$FVersionTxt <-rep(var_version,nrow(res));
col_names <- names(res);
#处理一个答案多个分类的问题
res <- split(res,res$FAnswerId);
res <- lapply(res,nsim_find_top_Answer);
res <- do.call('rbind',res);
res <- res[res$FChecked == 1, ];
res <- res[,col_names];
#return(res)
nsim_save(res,'answ_category_version')





print('3')
#3.根据答案聚合问题
ques_combine_ByAnsw(brand,var_version);
print('4')
