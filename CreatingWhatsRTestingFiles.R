# Load your package
library(WhatsR)

# Create combinations of parameters
params <- expand.grid(
  n_messages = c(250, 1000),
  n_chatters = c(2, 3),
  media_excluded = c(TRUE, FALSE),
  language = c("english", "german"),
  time_format = c("24h", "ampm"),
  os = c("android", "ios"),
  stringsAsFactors = FALSE
)

# Optionally set a seed for reproducibility
#set.seed(42)

# Output directory
output_dir <- file.path(getwd(), "UploadData")
if (!dir.exists(output_dir)) dir.create(output_dir)

# Loop through combinations and generate chats
for (i in seq_len(nrow(params))) {
  p <- params[i, ]

  # Construct file name
  fname <- paste0(
    "chat_n", p$n_messages,
    "_c", p$n_chatters,
    "_", ifelse(p$media_excluded, "media_excluded", "media_included"),
    "_", p$language,
    "_", p$time_format,
    "_", p$os
  )

  # Run the function and save to file
  create_chatlog(
    n_messages = p$n_messages,
    n_chatters = p$n_chatters,
    media_excluded = p$media_excluded,
    language = p$language,
    time_format = p$time_format,
    os = p$os,
    path = output_dir,
    chatname = fname
  )

  message("Saved: ", fname)
}


test_chats <- list()
error_files <- character()

files <- list.files("UploadData", full.names = TRUE)

for (i in seq_along(files)) {
  result <- try(parse_chat(files[i]), silent = TRUE)

  if (inherits(result, "try-error")) {
    error_files <- c(error_files, files[i])
    message("❌ Failed to parse: i", files[i])
  } else {
    test_chats[[i]] <- result
    message("✅ Successfully parsed: i ", files[i])
  }
}

saveRDS(test_chats, "parsed_test_chats.rds", ver = 2)

