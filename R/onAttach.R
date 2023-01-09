.onAttach <- function (libname, pkgname){
  packageStartupMessage(paste0(
    'See the following open access paper for details on support.BWS:\n',
    '  Aizaki and Fogarty (2023) https://doi.org/10.1016/j.jocm.2022.100394\n',
    'To cite support.BWS in publications use this paper. For details, type:\n',
    ' citation("support.BWS")'))
}

