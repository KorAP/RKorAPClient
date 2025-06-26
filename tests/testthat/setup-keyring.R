# Setup keyring for testing
if (requireNamespace("keyring", quietly = TRUE)) {
  # Set environment variables to use a non-interactive keyring backend
  # This prevents password prompts during testing
  if (!nzchar(Sys.getenv("KEYRING_BACKEND"))) {
    Sys.setenv("KEYRING_BACKEND" = "env")
  }
  
  # Alternatively, if env backend doesn't work, try file backend with empty password
  if (!nzchar(Sys.getenv("KEYRING_PASS"))) {
    Sys.setenv("KEYRING_PASS" = "")
  }
}
