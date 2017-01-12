# https://www.gleif.org/en/lei-data/gleif-concatenated-file/download-the-concatenated-file#
leis <-
  function() {
url <-
  "https://www.gleif.org/lei-files/20161129/GLEIF/20161129-GLEIF-concatenated-file.zip"

tmp <-
  tempfile()

url %>%
  curl::curl_download(url = ., tmp)

con <-
  unzip(tmp)

con %>%
  xml2::read_xml()

con %>%
  unlink()
  }