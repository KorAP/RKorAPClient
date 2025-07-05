# Helper function to find README.md file in current or parent directories
find_readme_path <- function() {
  readme_paths <- c("Readme.md", "../Readme.md", "../../Readme.md")
  for (path in readme_paths) {
    if (file.exists(path)) {
      return(path)
    }
  }
  return(NULL)
}

# Helper function to read README content
read_readme_content <- function() {
  readme_path <- find_readme_path()
  if (is.null(readme_path)) {
    return(NULL)
  }
  readme_content <- readLines(readme_path)
  paste(readme_content, collapse = "\n")
}

# Helper function to call OpenAI API
call_openai_api <- function(prompt, max_tokens = 500, temperature = 0.1) {
  library(httr2)
  library(jsonlite)

  tryCatch({
    response <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(list(
        model = "gpt-4.1-mini",
        messages = list(
          list(role = "user", content = prompt)
        ),
        max_tokens = max_tokens,
        temperature = temperature
      )) |>
      req_retry(max_tries = 3) |>
      req_perform()

    # Parse the response
    result <- response |> resp_body_json()
    result$choices[[1]]$message$content
  }, error = function(e) {
    if (grepl("429", as.character(e))) {
      skip("OpenAI API rate limit exceeded - please try again later or check your API key/credits")
    } else if (grepl("401", as.character(e))) {
      skip("OpenAI API authentication failed - please check your OPENAI_API_KEY")
    } else {
      stop(paste("OpenAI API error:", as.character(e)))
    }
  })
}

# Helper function to create README-guided prompt
create_readme_prompt <- function(task_description, specific_task) {
  readme_text <- read_readme_content()
  if (is.null(readme_text)) {
    stop("README.md not found")
  }

  paste0(
    "You are an expert R programmer. Based on the following README documentation for the RKorAPClient package, ",
    task_description, "\n\n",
    "README Documentation:\n",
    readme_text,
    "\n\nTask: ", specific_task,
    "\n\nProvide only the R code without explanations."
  )
}

# Helper function to extract R code from markdown code blocks
extract_r_code <- function(response_text) {
  # Remove markdown code blocks if present
  code <- gsub("```[rR]?\\n?", "", response_text)
  code <- gsub("```\\n?$", "", code)
  # Remove leading/trailing whitespace
  trimws(code)
}

test_that("GPT-4.1 mini can solve frequency query task with README guidance", {
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")), "OPENAI_API_KEY not set")
  skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

  # Create the prompt with README context and task
  prompt <- create_readme_prompt(
    "write R code to perform a frequency query for the word 'Deutschland' across multiple years (2010-2015). The code should use the RKorAPClient package and return a data frame with year and frequency columns.",
    "Write R code to query frequency of 'Deutschland' from 2010-2015 using RKorAPClient."
  )

  # Call OpenAI API
  generated_response <- call_openai_api(prompt, max_tokens = 500)
  generated_code <- extract_r_code(generated_response)

  # Basic checks on the generated code
  expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
  expect_true(grepl("frequencyQuery", generated_code), "Generated code should include frequencyQuery")
  expect_true(grepl("Deutschland", generated_code), "Generated code should include the search term 'Deutschland'")
  expect_true(grepl("201[0-5]", generated_code), "Generated code should include years 2010-2015")

  # Check that the generated code contains essential RKorAPClient patterns
  expect_true(grepl("\\|>", generated_code) || grepl("%>%", generated_code),
              "Generated code should use pipe operators")

  # Optional: Try to parse the generated code to check for syntax errors
  parsed_successfully <- tryCatch({
    parse(text = generated_code)
    TRUE
  }, error = function(e) {
    FALSE
  })

  expect_true(parsed_successfully, "Generated code should be syntactically valid R code")

  # Print the generated code for manual inspection
  cat("Generated code:\n", generated_code, "\n")
})

test_that("GPT-4.1 mini can solve collocation analysis task with README guidance", {
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")), "OPENAI_API_KEY not set")
  skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

  # Create the prompt for collocation analysis
  prompt <- create_readme_prompt(
    "write R code to perform a collocation analysis for the word 'setzen' (looking for light verb constructions). The code should use the RKorAPClient package's collocationAnalysis function.",
    "Write R code to perform collocation analysis for 'setzen' using RKorAPClient."
  )

  # Call OpenAI API
  generated_response <- call_openai_api(prompt, max_tokens = 500)
  generated_code <- extract_r_code(generated_response)

  # Basic checks on the generated code
  expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
  expect_true(grepl("collocationAnalysis", generated_code), "Generated code should include collocationAnalysis")
  expect_true(grepl("setzen", generated_code), "Generated code should include the search term 'setzen'")
  expect_true(grepl("auth", generated_code), "Generated code should include auth() for collocation analysis")

  # Check for collocation analysis parameters
  expect_true(grepl("leftContextSize|rightContextSize", generated_code),
              "Generated code should include context size parameters")

  # Print the generated code for manual inspection
  cat("Generated collocation analysis code:\n", generated_code, "\n")
})

test_that("GPT-4.1 mini can solve corpus query task with README guidance", {
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")), "OPENAI_API_KEY not set")
  skip_if_not(!is.null(find_readme_path()), "Readme.md not found in current or parent directories")

  # Create the prompt for corpus query
  prompt <- create_readme_prompt(
    "write R code to perform a simple corpus query for 'Hello world' and fetch all results. The code should use the RKorAPClient package.",
    "Write R code to query 'Hello world' and fetch all results using RKorAPClient."
  )

  # Call OpenAI API
  generated_response <- call_openai_api(prompt, max_tokens = 300)
  generated_code <- extract_r_code(generated_response)

  # Basic checks on the generated code
  expect_true(grepl("KorAPConnection", generated_code), "Generated code should include KorAPConnection")
  expect_true(grepl("corpusQuery", generated_code), "Generated code should include corpusQuery")
  expect_true(grepl("Hello world", generated_code), "Generated code should include the search term 'Hello world'")
  expect_true(grepl("fetchAll", generated_code), "Generated code should include fetchAll")

  # Check that the generated code follows the README example pattern
  expect_true(grepl("\\|>", generated_code) || grepl("%>%", generated_code),
              "Generated code should use pipe operators")

  # Print the generated code for manual inspection
  cat("Generated corpus query code:\n", generated_code, "\n")
})
