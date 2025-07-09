#' Get GitHub Version
#'
#' A short function to obtain the github version from the repository. This is
#' a generalisable function that can be used for any version tags on a repo.
#'
#' @param repo_owner GitHub Username
#' @param repo_name GitHub Repository Name
#' @return Version tag string
#' @export
#' @importFrom httr GET
#' @author Dionne Argyropoulos
getGithubRelease <- function(repo_owner, repo_name) {
  url <- paste0("https://api.github.com/repos/", repo_owner, "/", repo_name, "/releases/latest")
  response <- httr::GET(url)

  if (status_code(response) == 200) {
    release_info <- fromJSON(content(response, "text"))
    return(release_info$tag_name)  # Extracts the tag name (release version)
  } else {
    return(NULL)
  }
}
