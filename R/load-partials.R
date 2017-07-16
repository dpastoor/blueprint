#' load partials from a given type
#' @param type type - nonmem or mrgsolve
#' @export
load_partials <- function(type) {
  partials <- dir(system.file(sprintf("templates/%s/partials", type),
                              package = "blueprint"),
                  full.names = T,
                  recursive = T) %>% purrr::discard(is_dir)
  partial_templates <- purrr::set_names(purrr::map(partials, ~ paste0(collapse(readr::read_lines(.x)), "\n")),
                                        tools::file_path_sans_ext(basename(partials)))
  return(partial_templates)
}

#' load  templates
#' @param type type - nonmem or mrgsolve
#' @export
load_templates <- function(type) {
  template_files <- dir(system.file(sprintf("templates/%s", type), package = "blueprint"), full.names = T) %>% purrr::discard(is_dir)
  templates <- purrr::set_names(purrr::map(template_files, readr::read_file),
                                        tools::file_path_sans_ext(basename(template_files)))
  return(templates)
}
