library(tidyverse)
library(xml2)

#Input
bao_version <- "22-18-2025"
input_path <- str_glue("...BAO\\{bao_version}__BAO-master\\vocabulary\\")
output_file <- str_glue("...BAO\\{bao_version}__BAO-vocabulary.csv")
file_vec <- list.files(path = input_path)
name_vec <- file_vec |> str_replace_all(".owl", "")

#Read the files
data <- as.list(rep(NA, length(file_vec)))
for (i in seq(1:length(file_vec))) {
	#Parse
	vocab <- read_xml(str_glue("{input_path}{file_vec[i]}")) |> xml_find_all(".//owl:Class")
	#Gather the important
	ids <- c(rep(NA, length(vocab)))
	labels <- c(rep(NA, length(vocab)))
	descriptions <- c(rep(NA, length(vocab)))
	subclasses <- c(rep(NA, length(vocab)))
	source <- c(rep(NA, length(vocab)))
	if (length(vocab) > 0) {
		for (k in seq(1:length(vocab))) {
			tryCatch(
						{ ids[k] <- vocab[k] |> xml_attr("about") |> str_replace("http://www.bioassayontology.org/bao#", "")
						}, error = function(e) {
										ids[k] <- NA_character_
						}, warning = function(w) {
										ids[k] <- NA_character_
						}
					)
			tryCatch(
						{ labels[k] <- vocab[k] |> xml_find_first(".//rdfs:label") |> xml_contents() |> xml_text()
						}, error = function(e) {
										labels[k] <- NA_character_
						}, warning = function(w) {
										labels[k] <- NA_character_
						}
					)
			tryCatch(
						{ labels[k] <- vocab[k] |> xml_find_first(".//rdfs:label") |> xml_contents() |> xml_text()
						}, error = function(e) {
										labels[k] <- NA_character_
						}, warning = function(w) {
										labels[k] <- NA_character_
						}
					)
			tryCatch(
						{ descriptions[k] <- vocab[k] |> xml_find_first(".//obo:IAO_0000115") |> xml_contents() |> xml_text()
						}, error = function(e) {
										descriptions[k] <- NA_character_
						}, warning = function(w) {
										descriptions[k] <- NA_character_
						}
					)
			tryCatch( 
						{ subclasses[k] <- vocab[k] |> xml_find_all(".//rdfs:subClassOf") |> xml_attr("resource") |> str_replace("http://www.bioassayontology.org/bao#", "") |> str_c(collapse = ", ")
						}, error = function(e) {
										subclasses[k] <- NA_character_
						}, warning = function(w) {
										subclasses[k] <- NA_character_
						}
					)
			source[k] <- name_vec[i]
		}
		data[i] <- list(tibble(id = ids, term = labels, definition = descriptions, source = source, subclass_of = subclasses))
	} else {
		source <- name_vec[i]
		data[i] <- list(tibble(id = NA_character_, term = NA_character_, definition = NA_character_, source = source, subclass_of = NA_character_))
	}
}

#Bind to the single tibble
vocabulary <- bind_rows(data) |> filter(str_detect(id, "^BAO"))

#Save the results
vocabulary_export <- vocabulary |> mutate_all(~replace_na(., ""))
write_delim(vocabulary_export, output_file, delim = ";")