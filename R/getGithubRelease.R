#' Get GitHub Version
#'
#' A short function to obtain the github version from the repository. This is
#' a generalisable function that can be used for any version tags on a repo.
#'
#' @param repo_owner GitHub Username
#' @param repo_name GitHub Repository Name
#' @return Version tag string
#' @export
#' @author Dionne Argyropoulos
#'
#' @examples
#'
#' getGithubRelease(
#'  repo_owner = "dionnecargy",
#'  repo_name = "SeroTrackR"
#' )
#'
getGithubRelease <- function(repo_owner, repo_name) {

  # Check if httr is installed
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for getGitHubRelease(). Please install it.", call. = FALSE)
  }
  # Check if jsonlite is installed
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for getGitHubRelease(). Please install it.", call. = FALSE)
  }

  url <- paste0("https://api.github.com/repos/", repo_owner, "/", repo_name, "/releases/latest")
  response <- httr::GET(url)

  if (httr::status_code(response) == 200) {
    release_info <- jsonlite::fromJSON(httr::content(response, "text"))
    return(release_info$tag_name)  # Extracts the tag name (release version)
  } else {
    return(NULL)
  }
}
