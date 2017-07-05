#' load partials from nonmem
#' @export
load_nonmem_partials <- function() {
  partials <- dir(system.file("templates/nonmem/partials", package = "blueprint"), full.names = T)
  partial_templates <- purrr::set_names(purrr::map(partials, ~ paste0(paste0(readr::read_lines(.x), collapse = ""), "\n")),
                                        tools::file_path_sans_ext(basename(partials)))
  return(partial_templates)
}

#' load nonmem templates
#' @export
load_nonmem_templates <- function() {
  template_files <- dir(system.file("templates/nonmem", package = "blueprint"), full.names = T) %>% purrr::discard(is_dir)
  templates <- purrr::set_names(purrr::map(template_files, readr::read_file),
                                        tools::file_path_sans_ext(basename(template_files)))
  return(templates)
}
