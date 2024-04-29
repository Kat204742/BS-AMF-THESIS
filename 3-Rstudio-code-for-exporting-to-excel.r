install.packages("writexl")
library("writexl")

xlx_store_path <- "C:\\Users\\acer\\Desktop\\Second Semester Thesis Files\\twotailedbetahatandteststat.xlsx"


 
df1 <- as.data.frame(betahatmatrix)
df2 <- as.data.frame(teststatmatrix)
 
colnames(df1) <- column.labelspar
colnames(df2) <- column.labelspar

write_xlsx(list("betahatmatrix" = df1,
                "teststatmatrix" = df2),
                xlx_store_path)

