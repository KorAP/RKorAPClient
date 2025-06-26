library("testthat")
library("RKorAPClient")

# Set up keyring for testing to avoid interactive password prompts
if (requireNamespace("keyring", quietly = TRUE)) {
  # Use environment backend for testing to avoid password prompts
  if (!nzchar(Sys.getenv("KEYRING_BACKEND"))) {
    Sys.setenv("KEYRING_BACKEND" = "env")
  }
  # Set empty password for file backend if needed
  if (!nzchar(Sys.getenv("KEYRING_PASS"))) {
    Sys.setenv("KEYRING_PASS" = "")
  }
}

test_check("RKorAPClient")
