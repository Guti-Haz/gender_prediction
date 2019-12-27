pacman::p_load(randomNames,stringr,pbapply,data.table,magrittr)
# generate data
cleanName=function(s){
  str_replace_all(s,"[:punct:]"," ")%>%
    str_replace_all("\\s{2,}"," ")%>%
    str_trim%>%
    str_to_upper
}
n=1e4
nameDT=list(data.table(name=randomNames(n,0,1),gender=0),
            data.table(name=randomNames(n,1,1),gender=1))%>%
  rbindlist%>%
  .[,name:=cleanName(name)]%>%
  unique
# feature engineering
f1_count=pblapply(nameDT$name,function(str){
    sapply(LETTERS,function(patt){
      str_count(str,patt)
    })    
  })
f2_ordering=pblapply(nameDT$name,function(str){
  i_space=str_locate_all(str,"\\s")[[1]][,1]%>%c(.,nchar(str)+1)
  res=sapply(LETTERS,function(x){
    i_reg=str_locate_all(str,x)[[1]][,1]
    sapply(i_reg,function(y){
      sapply(i_space,function(x){y<x})%>%
        which%>%min})%>%
      unique%>%
      as.character%>%
      paste0(.,collapse="")
  })
})
