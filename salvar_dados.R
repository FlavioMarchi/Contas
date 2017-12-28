salvar_dados <- function(dados, nomearquivo){
  nomearquivo <- str_c(nomearquivo, ".xlsx")
  caminho <- file.path(getwd(),"Saida",nomearquivo)
  wb <- loadWorkbook(caminho, create = TRUE)
  
  #creating sheets within an Excel workbook
  createSheet(wb, name = "dados")
  
  #writing into sheets within an Excel workbook : 
  #writing ChickWeight data frame into chickSheet
  writeWorksheet(wb, dados, sheet = "dados", startRow = 1, startCol = 1)
  
  #saving a workbook to an Excel file :
  #saves a workbook to the corresponding Excel file and writes the file to disk.
  saveWorkbook(wb)
  
}