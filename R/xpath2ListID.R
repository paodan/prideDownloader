
#############################
xpath2ListID = function(xmlList,
                        xpath = "/html/body/div[2]/div/section/div/div[3]/div[2]/"){
  # ProjectAccessionID = "PXD006401"
  # url = paste0("https://www.ebi.ac.uk/pride/archive/projects/", ProjectAccessionID)
  # html = getURL(url)
  # doc = htmlParse(html, asText = TRUE)
  # xmlList = xmlToList(doc),

  l$body[[5]]$div$section$div[[6]][[6]][[2]][[2]]$p$a$text
  pathName = strsplit2(xpath, "/")[-(1:2)]

  # the number of each tag
  id = regexpr("\\d", pathName, perl=TRUE)
  id[id == -1] = 1
  tagID = regmatches(pathName, id)
  tagID[tagID == ""] = "1"
  tagID = as.numeric(tagID)
  tagID

  # the tag type
  tags = strsplit2(pathName, split = "\\[")[,1]

  # xpath to list ID
  tmp = xmlList
  n = NULL
  for(mi in seq_along(tags)){
    tmpName = names(tmp)
    tagMatch = which(tmpName == tags[mi])
    if (length(tagMatch) == 1){
      # print(tmpName[[tagMatch]])
      tmp = tmp[[tagMatch]]
      n = c(n, 1)
    } else {
      # print(tmpName[[tagMatch[tagID[mi]]]])
      tmp = tmp[[tagMatch[tagID[mi]]]]
      n = c(n, tagMatch[tagID[mi]])
    }
  }

  return(list(listID = n, res = tmp))
}
