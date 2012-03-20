library("XML")
library("tm")

doc<-xmlTreeParse("http://publications.cetis.ac.uk/feed") 
src<-xpathApply(xmlRoot(doc), "//category")
tags<- NULL

for (i in 1:length(src)) {

     tags<- rbind(tags,data.frame(tag=tag<-xmlSApply(src[[i]], xmlValue)) )
      
}

type <- subset(tags, tag == "Briefing Paper" || tag == "White Paper" || tag == "Other Publication" || tag == "Journal Paper"  || tag == "Report")
cats <- subset(tags, tag != "Briefing Paper" & tag != "White Paper" & tag != "Other Publication" & tag != "Journal Paper"  & tag != "Report")
levels(cats$tag)
df$tag = factor(df$tag)
pie(table(cats$tag))

#table(tags$tag)
#pie(table(tags$tag) )

#(tags[  tags$tag %in% c("White Paper","Report","Other Publication","Journal Paper"))
    