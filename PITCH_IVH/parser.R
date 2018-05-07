parser = function(){
  x = readLines("CT_ICH_Segmentation.Rnw")
  row = grep("begin\\{document\\}", x)
  x = x[-seq(1, row)]
  row = grep("end\\{document\\}", x)
  x = x[-seq(row, length(x))]  
  
  row = grep("end\\{frontmatter\\}", x)
  x = x[-seq(1, row)]  
  
  row = gsub("\\printbibliography", "\\printbibliography[title={References}]", x)
  
  
#   row = grep("(begin|end)\\{abstract\\}", x)
#   x = x[-seq(row[1], row[2])]
#   row = grep("(begin|end)\\{keyword\\}", x)
#   x = x[-seq(row[1], row[2])]

  writeLines(x, "ich_chapter.Rnw")  
}
parser()
