user_data_dir <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    appdata <- Sys.getenv("APPDATA")
    if(appdata == "") {
      stop("APPDATA environment variable not set")
    }
    return(file.path(appdata, "jupyter"))
  }
  if (Sys.info()["sysname"] == "Darwin") {
    home <- Sys.getenv("HOME")
    if(home == "") {
      stop("HOME environment variable not set")
    }
    return(file.path(home, "Library", "Jupyter"))
  }
  home <- Sys.getenv("XDG_DATA_HOME")
  if(home == "") {
    home <- Sys.getenv("HOME")
  }
  if(home == "") {
    stop("HOME environment variable not set")
  }
  return(file.path(home, ".local", "share", "jupyter"))
}

guess_sys_prefix <- function() {
  dirs <- strsplit(Sys.getenv("PATH"), ":")[[1]]
  is_windows <- Sys.info()["sysname"] == "Windows"
  if(is_windows) {
    pathext <- strsplit(Sys.getenv("PATHEXT"), ";")
  } else {
    pathext <- c("")
  }
  for (dir in dirs) {
    for (ext in pathext) {
      bin <- file.path(dir, paste0("python", ext))
      if (file.exists(bin)) {
        if(is_windows) {
          return(normalizePath(dirname(bin)))
        } else {
          return(normalizePath(dirname(dirname(bin))))
        }
      }
    }
  }
}

system_data_dirs <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    programdata <- Sys.getenv("PROGRAMDATA")
    if(programdata == "") {
      stop("PROGRAMDATA environment variable not set")
    }
    return(c(file.path(programdata, "jupyter")))
  }
  return(c("/usr/local/share/jupyter", "/usr/share/jupyter"))
}

find_data_dir <- function() {
  sys_prefix <- guess_sys_prefix()
  if (!is.null(sys_prefix)) {
    return(file.path(sys_prefix, "share", "jupyter"))
  }
  user_dir <- user_data_dir()
  if(dir.exists(user_dir)) {
    return(user_dir)
  }
  return(system_data_dirs()[0])
}

library(httr2)
fetch_package_info <- function(name) {
  req <- request(paste0("https://pypi.org/pypi/", name, "/json"))
  resp <- req_perform(req) |> resp_body_json()
  return(resp)
}

fetch_wheel <- function(info, version = NA) {
  if(!is.na(version)) {
    releases <- info[["releases"]][[version]]
  } else {
    releases <- info[["urls"]]
  }
  if (length(releases) < 1) {
    stop("No entries found for version ${version}");
  }
  wheel <- NA
  for(release in releases) {
    if(release[["packagetype"]] == "bdist_wheel") {
      wheel <- release
    }
  }
  if (all(is.na(wheel))) {
    stop("No wheel found for version ${version}");
  }

  req <- request(wheel[["url"]])
  resp <- req_perform(req) |> resp_body_raw()

  tmp <- tempfile()
  writeBin(resp, con = tmp)

  result <- list(
    version = version,
    wheel = tmp
  )
  if(is.na(version)) {
    result[["version"]] <- info[["info"]][["version"]]
  }

  return(result)
}

library(stringr)

extract_data_files <- function(tmp_zipped_wheel, out_dir) {
  zip_list <- unzip(tmp_zipped_wheel, list = TRUE)$Name
  data_prefix <- "^.*\\.data\\/data\\/share\\/jupyter\\/"

  print(zip_list)
  matches <- zip_list[str_detect(zip_list, data_prefix)]
  print(matches)
  for(match in matches) {
     out_path <- file.path(out_dir, gsub(data_prefix, "", match))
     unzip(tmp_zipped_wheel, files = match, exdir = dirname(out_path), junkpaths = TRUE)
  }
}

# Reference: https://github.com/manzt/anywidget/blob/ed4176ac71da232b676124a5236367ce09703896/packages/deno/src/install.ts#L99
install_anywidget <- function() {
  out_dir <- find_data_dir()

  info <- fetch_package_info("anywidget")
  wheel_info <- fetch_wheel(info)
  extract_data_files(wheel_info[["wheel"]], out_dir)
  version <- wheel_info[["version"]]
  print("Installed anywidget ${version} in ${out_dir}")
}

install_jupyterlab_widgets <- function() {
  out_dir <- find_data_dir()

  info <- fetch_package_info("jupyterlab_widgets")
  wheel_info <- fetch_wheel(info)
  extract_data_files(wheel_info[["wheel"]], out_dir)
  version <- wheel_info[["version"]]
  print("Installed jupyterlab_widgets ${version} in ${out_dir}")
}

install <- function() {
  install_anywidget()
  install_jupyterlab_widgets()
}
