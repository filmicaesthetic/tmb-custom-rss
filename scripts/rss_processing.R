## Create more customised Etsy RSS feed

pacman::p_load(dplyr, tidyr, tidyRSS, rvest)

feed <- tidyfeed("https://www.etsy.com/shop/themushroombabes/rss") |>
  mutate(feed_id = row_number())

random_image_selection <- function(url) {
  
  main_img <- url |>
    read_html() |>
    html_elements("img") |>
    html_attr("src")
  
  other_imgs <- url |>
    read_html() |>
    html_elements("img") |>
    html_attr("data-src")
  
  if(which(cumsum(is.na(other_imgs))==2) > 5) {
    
    all_imgs <- c(main_img[1], other_imgs[2:(min(which(cumsum(is.na(other_imgs))==2))-4)])
    
    chosen_img <- sample(all_imgs, 1)
    
  } else {
    
    chosen_img <- main_img[1]
    
  }
  
  return(chosen_img)
  
  Sys.sleep(0.5)
  
}

new_rss <- feed |>
  rowwise() |>
  #mutate(selected_img = random_image_selection(item_link)) |>
  mutate(tweet = gsub(" - ", " 🍄 ", gsub(" - Eco-Friendly Recycled Paper", " 🌱 Recycled Paper", gsub(" by TheMushroomBabes| - The Mushroom Babes| - Unframed", "", item_title)))) |>
  mutate(tweet = paste0(tweet, " | 👉 ",item_link)) |>
  mutate(img = random_image_selection(item_link)) |>
  mutate(main_description = paste0('<p class="image"><img src="',img, '" border="0" width="570" height="429" /></p><p class="description">',tweet,'</p>'))

new_rss_sel <- new_rss |>
  mutate(main_description = gsub("&#39;", "'", main_description)) |>
  mutate(main_description = gsub("&quot;", '"', main_description)) |>
  select(item_pub_date, item_title, item_link, img, tweet, description = main_description)

write.csv(new_rss_sel, "outputs/rss.csv", row.names = FALSE)

# write.rss function from old version of animation package
`write.rss` <- function(file = "feed.xml", entry, 
                        xmlver = "1.0", rssver = "2.0", title = "The Mushroom Babes Item Feed", link = "https://www.themushroombabes.co.uk", 
                        description = "Customised Etsy item feed for The Mushroom Babes", language = "en-us", 
                        copyright = "", pubDate = Sys.time(), 
                        lastBuildDate = Sys.time(), docs = "https://www.github.com/filmicaesthetic", 
                        generator = "Function write.rss() in R package animation", 
                        managingEditor = "@filmicaesthetic", 
                        webMaster = "@filmicaesthetic", 
                        maxitem = 10, ...) {
  x = read.csv(entry, stringsAsFactors = FALSE, colClasses = "character")
  if (nrow(x) > maxitem) 
    x = x[(nrow(x) - maxitem + 1):nrow(x), ]
  x = x[nrow(x):1, ] 
  lcl = Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  pubDate = format(pubDate, "%a, %d %b %Y %H:%M:%S GMT")
  lastBuildDate = format(lastBuildDate, "%a, %d %b %Y %H:%M:%S GMT")
  cat("<?xml version", "=\"", xmlver, "\"?>\n", "<rss version=\"", 
      rssver, "\">\n", "\t", "<channel>\n", "\t\t", "<title>", 
      title, "</title>\n", "\t\t", "<link>", link, "</link>\n", 
      "\t\t", "<description>", description, "</description>\n", 
      "\t\t", "<language>", language, "</language>\n", "\t\t", 
      "<pubDate>", pubDate, "</pubDate>\n", "\t\t", "<lastBuildDate>", 
      lastBuildDate, "</lastBuildDate>\n", "\t\t", "<docs>", 
      docs, "</docs>\n", "\t\t", "<generator>", generator, 
      "</generator>\n", "\t\t", "<managingEditor>", managingEditor, 
      "</managingEditor>\n", "\t\t", "<webMaster>", webMaster, 
      "</webMaster>\n", file = file, sep = "")
  extra = list(...)
  if (length(extra)) {
    tag1 = paste("\t\t<", names(extra), ">", sep = "")
    tag2 = paste("</", names(extra), ">", sep = "")
    cat(paste(tag1, extra, tag2, sep = "", collapse = "\n"), 
        "\n", file = file, append = TRUE)
  }
  x[, "description"] = paste("<![CDATA[", x[, "description"], 
                             "]]>", sep = "")
  tag1 = paste("<", colnames(x), ">", sep = "")
  tag2 = paste("</", colnames(x), ">", sep = "")
  cat(paste("\t\t<item>", apply(x, 1, function(xx) paste("\t\t\t", 
                                                         paste(tag1, xx, tag2, sep = "", collapse = "\n\t\t\t"), 
                                                         sep = "")), "\t\t</item>", sep = "\n", collapse = "\n"), 
      file = file, append = TRUE)
  cat("\n\t", "</channel>", file = file, append = TRUE)
  cat("\n</rss>", file = file, append = TRUE)
  Sys.setlocale("LC_TIME", lcl)
  cat("RSS feed created at:", file, "\n")
}

# write rss feed
write.rss(file = "outputs/feed.xml", entry = "outputs/rss.csv")
