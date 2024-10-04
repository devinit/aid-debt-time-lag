lapply(c("data.table", "openxlsx"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("../")

wb = createWorkbook()

exclusions = c(
  "pn_metadata.csv"
)

csvs = list.files(path="output", pattern="*.csv")
for(csv in csvs){
  if(!csv %in% exclusions){
    dat = fread(paste("output", csv, sep="/"))
    basename = substr(csv, 1, nchar(csv)-4)
    addWorksheet(wb, basename)
    writeData(wb, basename, dat)
    image_filename = paste0("output/",basename,".png")
    if(
      file.exists(image_filename)
    ){
      insertImage(
        wb,
        basename,
        image_filename,
        width = 8,
        height = 5,
        startRow = 1,
        startCol = 5,
        units = "in"
      )
    }
  }
}

saveWorkbook(wb, file="output/aid_debt.xlsx", overwrite = T)
