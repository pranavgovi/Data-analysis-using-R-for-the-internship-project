
install.packages("RODBC")

library(RODBC)
db_conn <- odbcConnect("abc", rows_at_time = 1)

if(db_conn == -1) {
  quit("no", 1)
}
sql <- "
SET NOCOUNT ON;
select * from dbo.clg "
    s <- sqlQuery(db_conn, sql, stringsAsFactors = FALSE)
    odbcClose(db_conn)
View(s)
df=data.frame(s)
print(df)
ncol(df)
nrow(df) #to find df length
set.seed(1)


x <- vector("list",100)

for(col in df["college"]){
  for (col_name in col){
    if(col_name %in% x){
      
    }
    else{
      out <- df[df$college %in% c(col_name),]
      # $ is used to select a column named college in the data frame
      len=length(x)
      x[[len+1]] <- col_name
      new_df=data.frame(out)
      print(new_df)
      
      sample <- sample(c(TRUE, FALSE), nrow(new_df), replace=TRUE, prob=c(0.8,0.2))
      train  <- df[sample, ]
      test   <- df[!sample, ]
    
    
      model=lm(placed ~apps+ accept, data=train,na.action=na.exclude)
      #print(model)
      model_summary <- summary(model)
      
      intercept_value <- model_summary$coefficients[1,1]
      
      print(intercept_value)
      
      model=lm(books ~phd+Alumni+apps, data=train,na.action=na.exclude)
      #print(model)
      model_summary <- summary(model)
      
      aa <- model_summary$coefficients[1,1]
      
      print(aa)
      li <- vector("list")
      li <- c(li,'2024',col_name,'yes',1000,500,intercept_value,1000,500,10,aa)
  
      
      
      dff=data.frame(li)
      db_conn <- odbcConnect("abc", rows_at_time = 1)
      
      sql_statement <- paste("INSERT INTO dbo.clg (YEAR,college,private,apps,accept,placed,Alumni,expend,phd,books) VALUES ",dff,sep="")
  
      
      sqlQuery(db_conn,sql_statement)
      odbcClose(db_conn)
      
      
    }
  }
}







dep<-c("placed~","expend~") # list of unique dependent variables with ~ 
indep1<-c("apps","Alumni")  # list of first unique independent variables 
indep2<-c("books","accept") # list of second unique independent variables 
myvar<-cbind(dep,indep1,indep2) # matrix of variables
myvar

k=c()

for (i in 1:dim(myvar)[1]){
  print(paste("This is", i, "regression", "with dependent var",gsub("~","",myvar[i,1])))
  k[[i]]<-lm(as.formula(paste(myvar[i,1],paste(myvar[i,2:3],collapse="+"))),df)
  print(k[[i]])
}


db_conn <- odbcConnect("abc", rows_at_time = 1)
sql <- "
SET NOCOUNT ON;
            insert into dbo.clg(YEAR,college,private,apps,accept,placed,Alumni,expend,phd,books)VALUES('2024','Crescent','yes',1000,200,100,100,10000,12,23) 
a <- sqlQuery(db_conn, sql, stringsAsFactors = FALSE)

odbcClose(db_conn)
