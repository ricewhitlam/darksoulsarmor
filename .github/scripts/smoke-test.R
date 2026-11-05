## A smoke test of the app as exported by shinylive, run before the Pages workflow publishes it:
## in headless Chrome, the app must start, refresh the Results table, draw the Trade-offs chart
## and offer its download as an Excel workbook. Takes the exported site's directory (served here)
## or a URL; exits with an error, naming the stage, if any of that fails.
##
## Usage: Rscript .github/scripts/smoke-test.R <directory or URL>

target <- commandArgs(trailingOnly = TRUE)[1]
if(is.na(target)){
    stop("Give the exported site's directory or a URL")
}
if(grepl("^https?://", target)){
    url <- target
} else{
    server <- httpuv::startServer("127.0.0.1", 8080, list(staticPaths = list("/" = httpuv::staticPath(target, indexhtml = TRUE))))
    url <- "http://127.0.0.1:8080/"
}

fail <- function(stage){
    message("Smoke test failed: ", stage)
    quit(status = 1)
}

b <- chromote::ChromoteSession$new(width = 1400, height = 1000)
## The app runs inside the page's frame; evaluate code there
js <- function(code){
    b$Runtime$evaluate(paste0("(() => { const f = document.querySelector('iframe'); const w = f ? f.contentWindow : window; const d = w.document; ", code, " })()"))$result$value
}
wait.for <- function(code, seconds){
    start <- Sys.time()
    repeat{
        if(isTRUE(tryCatch(js(code), error = function(e) FALSE))) return(TRUE)
        if(difftime(Sys.time(), start, units = "secs") > seconds) return(FALSE)
        later::run_now(0.5)
    }
}

invisible(b$Page$navigate(url))
if(!wait.for("return !!(w.Shiny && w.Shiny.shinyapp && w.Shiny.shinyapp.isConnected() && d.querySelector('#go'));", 180)){
    fail("the app didn't start")
}
message("The app started")
later::run_now(2)

invisible(js("w.Shiny.setInputValue('go', 1, {priority: 'event'}); return true;"))
if(!wait.for("return d.querySelectorAll('#table tbody tr').length > 1 && !d.querySelector('#table td.dataTables_empty') && !d.querySelector('.shinybusy-modal');", 180)){
    fail("Refresh didn't fill the Results table")
}
message("Refresh filled the Results table")

invisible(js("[...d.querySelectorAll('a')].find(a => a.textContent.trim() == 'Trade-offs').click(); return true;"))
if(!wait.for("return !!d.querySelector('#tradeoffs-plot .main-svg') && !d.querySelector('.shinybusy-modal');", 180)){
    fail("the Trade-offs chart didn't draw")
}
message("The Trade-offs chart drew")

## The download, fetched from inside the app's frame as the browser would
download <- b$Runtime$evaluate(
    "(async () => { const f = document.querySelector('iframe'); const w = f ? f.contentWindow : window; const a = w.document.querySelector('#download');
      const r = await w.fetch(a.href); const buf = await r.arrayBuffer();
      return JSON.stringify({status: r.status, type: r.headers.get('content-type'), bytes: buf.byteLength}); })()",
    awaitPromise = TRUE, timeout_ = 180
)$result$value
download <- jsonlite::fromJSON(download)
if(download$status != 200 || !grepl("spreadsheetml", download$type) || download$bytes < 10000){
    fail(sprintf("the download wasn't an Excel workbook (status %s, type %s, %s bytes)", download$status, download$type, download$bytes))
}
message(sprintf("The download is an Excel workbook (%s bytes)", download$bytes))

invisible(b$close())
message("Smoke test passed")
