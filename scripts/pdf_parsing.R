library(pdftools)
library(tesseract)
library(rJava) # also requires the JRE to be installed locally
library(tabulapdf)
library(ellmer)
library(tidyverse)

# Gemma4 is an open multimodal (text & images) LLM released by Google DeepMind
# We could run it locally if we had a good enough PC, we're running it in the cloud because we don't/
# Gemma can extract from PDFs and images, but not from images embedded in PDFs...
# So, we ideally want to parse a PDF into images and text (?) before sending it?

pdf_txt <- pdf_text("data-raw/norheim_1987.pdf")
pdf_tables <- tabulapdf::extract_tables("data-raw/norheim_1987.pdf")


pdftools::pdf_ocr_data("data-raw/norheim_1987.pdf")

# ollama models
ollama_api <- function() {
  "cacc4270abc945a7bf9cfa22680a2b57.nhfy1U8bcILCTv_13HiH9-Oo"
}
models_ollama(base_url = "https://ollama.com/api/chat")

models_ollama(base_url = "https://ollama.com", credentials = NULL)

chat <- chat_openai_compatible(
  model = "gpt-oss:120b", # Or another available Ollama cloud model
  base_url = "https://ollama.com",
  credentials = ollama_api
)


#  HTTP 405 Method Not Allowed.

# this works. anopther model?
client <- chat_ollama(
  model = "gemma3:12b",
  base_url = "https://ollama.com",
  credentials = ollama_api
)

client$chat(
  "Roughly how many pages of text do the includes represnt",
  pdf_txt,
  paste0(pdf_tables)
)

schema_str <- get_schema_display()

# Ollama’s Cloud currently does not support structured outputs.
# Fuck.
# I'm sure we could ask the model to approximate it, but I have no confidence that it'd be any good. Let's call that plan b.
response <- client$chat(
  create_extraction_prompt(),
  paste0(
    "Respond ONLY with a valid JSON object matching this schema exactly.",
    " Return nothing but the JSON — no explanation, no markdown fences.\n\n",
    schema_str
  ),
  pdf_txt,
  paste0(pdf_tables)
)

# Recursively convert an ellmer Type S7 object to a plain list matching JSON Schema structure
type_to_list <- function(type) {
  if (S7::S7_inherits(type, ellmer::TypeObject)) {
    schema <- list(type = "object")
    if (!is.null(type@description)) {
      schema$description <- type@description
    }
    if (length(type@properties) > 0) {
      schema$properties <- lapply(type@properties, type_to_list)
    }
    return(schema)
  }
  if (S7::S7_inherits(type, ellmer::TypeArray)) {
    schema <- list(type = "array", items = type_to_list(type@items))
    if (!is.null(type@description)) {
      schema$description <- type@description
    }
    return(schema)
  }
  if (S7::S7_inherits(type, ellmer::TypeEnum)) {
    schema <- list(type = "string", enum = type@values)
    if (!is.null(type@description)) {
      schema$description <- type@description
    }
    return(schema)
  }
  if (S7::S7_inherits(type, ellmer::TypeBasic)) {
    schema <- list(type = type@type)
    if (!is.null(type@description)) {
      schema$description <- type@description
    }
    return(schema)
  }
  list(type = "string")
}

schema_to_json <- function(type, pretty = TRUE) {
  jsonlite::toJSON(
    type_to_list(type),
    pretty = pretty,
    auto_unbox = TRUE,
    null = "null"
  )
}

schema_json <- schema_to_json(create_extraction_schema())

response <- client$chat(
  create_extraction_prompt(),
  paste0(
    "Respond ONLY with a valid JSON object — no explanation, no markdown fences.\n",
    "The JSON must conform to this JSON Schema:\n\n",
    schema_json
  ),
  pdf_txt,
  paste0(pdf_tables),
  echo = "none"
)

# We can expect the chatbot to merrily disregard our instruction to exclude markdown fences#| label:
response <- response |> stringr::str_remove_all(pattern = "^(```json)|(```)$")

result <- jsonlite::fromJSON(response, simplifyVector = FALSE)

# easiest at this point to reuse the Shiny-specific populate infrastructure with a dummy data object
# I guess?
# Actually, no, because they're looking for all sorts of inputs I can't be bothered to mock.
# So probably the best option is to...
# But do we have transformative logic that kicks in based on the inputs? I think we do.
# Damn.
# Ok, right. For single-row data tables we just do everything with inputs then build the tibble, but for sites, etc. we go straight to the tibble and send that to the handsontalbe

session <- list("userData" = initialise_userData())

create_sites_from_llm(
  session = session,
  llm_sites_data = result$sites |> as_tibble(),
  llm_campaign_data = results$campaign |> as_tibble()
)
