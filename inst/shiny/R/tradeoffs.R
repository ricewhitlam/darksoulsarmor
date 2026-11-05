
## The Trade-offs tab, as a Shiny module: its choices (Maximize, Jumps, Flats), the chart and the
## table under it, for the settings of the last refresh.

## From darksoulsarmor: the stats (R/data.R), for a minimum's place in the search's minima, and the
## game's equip-load rule (R/equip-load.R), which gives each point its movement type
MOVEMENT.SHARES <- darksoulsarmor:::MOVEMENT.SHARES
weapons.weight <- darksoulsarmor:::weapons.weight
equip.load <- darksoulsarmor:::equip.load
movement.line <- darksoulsarmor:::movement.line
total.weight <- darksoulsarmor:::total.weight
movement.type <- darksoulsarmor:::movement.type
minima.index.of <- function(stat){ darksoulsarmor:::METRICS[metric == stat, minima.index] }

## The curve: the most of one stat any armor set can reach at each armor weight (see
## get.armor.tradeoffs), for the last refresh's settings, over every armor weight from 0 to
## the heaviest possible armor. The curve is computed in one segment per movement class - up to the
## Light line as Light movement, from there to the Mid line as Mid, and so on, then Poop past the
## Fat line - so every set in a segment moves at least as lightly as that segment's type, by the
## game's own check (which credits the Mask of the Father's equip load bonus).
tradeoff.metric.labels <- c(
    SCORE = "Score", POISE = "Poise",
    PHYS_DEF = "Physical Defense", STRIKE_DEF = "Strike Defense", SLASH_DEF = "Slash Defense", THRUST_DEF = "Thrust Defense",
    MAG_DEF = "Magic Defense", FIRE_DEF = "Fire Defense", LITNG_DEF = "Lightning Defense",
    BLEED_RES = "Bleed Resistance", POIS_RES = "Poison Resistance", CURSE_RES = "Curse Resistance"
)

## The color jumps are drawn in on the Trade-offs chart: a dark blue, set apart from the curve's
## lighter one by depth and weight
JUMP.COLOR <- "rgb(8,48,107)"
CURVE.COLOR <- "rgb(31,119,180)"

## A color for each efficiency ratio (a region's slope as a multiple of the curve's average): grey
## at the average, deepening to green at twice it or more and to purple at flat (0) - the ends of
## ColorBrewer's purple-green scale, which stay distinguishable with the common forms of color
## blindness. Measured on the ratio's log, so half and double the average are equally far from
## grey; no ratio (a flat curve) is grey. A tint below 1 mixes the color with white: the chart's
## bands and hover boxes use 0.35, light enough to read black text on.
efficiency.color <- function(ratio, tint = 1){
    t <- ifelse(is.na(ratio), 0, pmax(-1, pmin(1, log2(ratio))))
    s <- abs(t)
    red <- 255+(189+(ifelse(t > 0, 27, 118)-189)*s-255)*tint
    green <- 255+(189+(ifelse(t > 0, 120, 42)-189)*s-255)*tint
    blue <- 255+(189+(ifelse(t > 0, 55, 131)-189)*s-255)*tint
    sprintf("rgb(%d,%d,%d)", round(red), round(green), round(blue))
}

## The movement type each set (by its pieces' names) gets with the given weapons and equip loads,
## by the game's check, and whether only the Mask of the Father's bonus keeps it that light
movement.class <- function(head, chest, hands, legs, carried, load, load.father.mask){
    weight.of <- function(table, pieces){ table$WEIGHT[match(pieces, table$ARMOR)] }
    total <- total.weight(carried, weight.of(head.data.unupgraded, head), weight.of(chest.data.unupgraded, chest), weight.of(hands.data.unupgraded, hands), weight.of(legs.data.unupgraded, legs))
    mask <- !is.na(head) & head == "Mask of the Father"
    with.bonus <- movement.type(total, ifelse(mask, load.father.mask, load))
    list(class = with.bonus, by.mask.bonus = mask & with.bonus != movement.type(total, load))
}

## What a curve depends on: a refresh's settings except the table size and the movement type (the
## curve covers every movement class), and the Maximize choice
tradeoff.key <- function(snapshot, metric){
    list(settings = snapshot[setdiff(names(snapshot), c("max.table.size", "movement"))], metric = metric)
}

## The table under the chart: limit, best value (named for the chosen stat), movement class, the
## set's scores, then its pieces - with the score itself as the value when it's the chosen stat
## (its quality beside it), rather than shown twice. Each set's real armor weight is in the
## chart's hover.
tradeoff.table <- function(result){
    if(result$metric == "SCORE"){
        return(result$data[, .(ARMOR_WEIGHT_LIMIT, SCORE, SCORE_QUALITY, MOVEMENT, HEAD, CHEST, HANDS, LEGS)])
    }
    table <- result$data[, .(ARMOR_WEIGHT_LIMIT, BEST_VALUE, MOVEMENT, SCORE, SCORE_QUALITY, HEAD, CHEST, HANDS, LEGS)]
    data.table::setnames(table, "BEST_VALUE", result$metric)
    return(table)
}

## The tab's contents
tradeoffs.tab <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                4,
                shiny::selectInput(
                    inputId = ns("metric"),
                    label = "Maximize",
                    choices = c(
                        "Score" = "SCORE", "Poise" = "POISE",
                        "Physical Defense" = "PHYS_DEF", "Strike Defense" = "STRIKE_DEF", "Slash Defense" = "SLASH_DEF", "Thrust Defense" = "THRUST_DEF",
                        "Magic Defense" = "MAG_DEF", "Fire Defense" = "FIRE_DEF", "Lightning Defense" = "LITNG_DEF",
                        "Bleed Resistance" = "BLEED_RES", "Poison Resistance" = "POIS_RES", "Curse Resistance" = "CURSE_RES"
                    )
                )
            ),
            shiny::column(
                2,
                shiny::selectInput(inputId = ns("jumps"), label = "Jumps", choices = c("Few" = "few", "Some" = "some", "Many" = "many"), selected = "some")
            ),
            shiny::column(
                2,
                shiny::selectInput(inputId = ns("flats"), label = "Flats", choices = c("Few" = "few", "Some" = "some", "Many" = "many"), selected = "some")
            )
        ),
        shiny::helpText(
            "For every armor weight, the most of the chosen stat any armor set can reach, using the settings of the last refresh.",
            "Vertical lines mark where your movement type changes (the selected movement type's line is solid). Shaded bands are stretches of steady",
            "gain: green ones buy more of the stat per unit of armor weight than the curve's average, purple ones less. Hatched areas are flats, where",
            "more weight buys nothing. Dark blue rises are jumps, single steps that gain far more than the weight around them, labelled with their gain.",
            "Diamonds mark where one stretch gives way to the next. Jumps and Flats set the sensitivity at which these features are identified. Hover",
            "over a point for its set, its movement type and the stretch it's in; click a point or a table row for its links."
        ),
        plotly::plotlyOutput(outputId = ns("plot")),
        DT::dataTableOutput(outputId = ns("table"))
    )
}

## The tab's computation and display. last.args: the last refresh's arguments (NULL before the
## first); shown: whether the tab is open; show.links: shows one armor set's wiki links;
## show.error: shows a message as the app's error ("" to clear it). Returns the curve (data), its
## description (efficiency), how many times it's been computed (computations), and the Trade-offs
## table for a download (download.table).
tradeoffs.tab.server <- function(id, last.args, shown, show.links, show.error){
    shiny::moduleServer(id, function(input, output, session){

        tradeoffdata <- shiny::reactiveVal(NULL)

        ## How many times the curve has been computed - lets the tests tell a recompute from a reuse
        tradeoff.computations <- shiny::reactiveVal(0)

        ## The curve of `metric`, every 0.1 armor weight (the precision weights are shown at), for a
        ## refresh's settings (snapshot), as tradeoffdata() holds it - for the tab and for the download
        compute.tradeoffs <- function(snapshot, metric){

            ## The last refresh's (validated) settings, less the Results-only table size
            settings <- snapshot[setdiff(names(snapshot), "max.table.size")]
            ## A minimum on the charted stat itself would only cut the curve off below it, so it's
            ## ignored here and drawn as a reference line instead; every other minimum still applies
            stat.minimum <- 0
            if(metric != "SCORE"){
                stat.minimum <- settings$minima[minima.index.of(metric)]
                settings$minima[minima.index.of(metric)] <- 0
            }
            carried <- weapons.weight(settings$weapons)
            load <- equip.load(settings$endurance.level, settings$havel.ring, settings$favor.ring, father.mask = FALSE)
            load.father.mask <- equip.load(settings$endurance.level, settings$havel.ring, settings$favor.ring, father.mask = TRUE)
            heaviest.armor <- max(head.data.unupgraded$WEIGHT)+max(chest.data.unupgraded$WEIGHT)+max(hands.data.unupgraded$WEIGHT)+max(legs.data.unupgraded$WEIGHT)
            ## Armor weight at which each movement class ends (Light/Mid, Mid/Fat, Fat/Poop): its line less
            ## what the weapons weigh. The chart draws these; the segments end on the 0.1 grid armor
            ## weights are listed on, and the game's own check decides the sets right at a line.
            lines <- sapply(names(MOVEMENT.SHARES), function(movement){ movement.line(load, movement)-carried })
            step <- 0.1

            ## One segment per movement class, each from the previous line (exclusive) to its own
            ## (inclusive), clipped to [0, heaviest armor]; past the Fat line, no load limit
            segment.movements <- c(names(MOVEMENT.SHARES), "Poop")
            segment.ends <- c(floor(10*lines+1e-6)/10, Inf)
            segments <- list()
            covered.to <- -Inf
            for(s in seq_along(segment.movements)){
                lower <- max(0, covered.to)
                upper <- min(segment.ends[s], heaviest.armor)
                if(upper >= lower){
                    segment.settings <- settings
                    segment.settings$movement <- segment.movements[s]
                    curve <- do.call(get.armor.tradeoffs, c(list(metric = metric, weight.step = step, min.armor.weight = lower, max.armor.weight = upper), segment.settings))$data
                    ## A limit on the previous line belongs to the faster class
                    curve <- curve[ARMOR_WEIGHT_LIMIT > covered.to+1e-9]
                    if(nrow(curve) > 0){
                        segments[[length(segments)+1]] <- curve[, MOVEMENT_LIMIT := segment.movements[s]]
                    }
                }
                covered.to <- max(covered.to, upper)
            }
            curve <- data.table::rbindlist(segments)

            classes <- movement.class(curve$HEAD, curve$CHEST, curve$HANDS, curve$LEGS, carried, load, load.father.mask)
            curve[, MOVEMENT := ifelse(is.na(ARMOR_WEIGHT), NA_character_, ifelse(classes$by.mask.bonus, paste(classes$class, "(Mask of the Father bonus)"), classes$class))]

            tradeoff.computations(tradeoff.computations()+1)
            list(
                metric = metric, data = curve, lines = lines, selected.movement = settings$movement,
                carried = carried, equip.load = load, stat.minimum = stat.minimum,
                key = tradeoff.key(snapshot, metric)
            )
        }

        ## Where extra armor weight pays off on the chart's curve (get.tradeoff.efficiency): its jumps,
        ## lines, flats and boundary points, with as many jumps and flats as the tab's choices ask for
        ## ("some" if a client sends anything else) - when it has two points to join. Recomputed when only
        ## those choices change, without searching again.
        tradeoff.efficiency <- shiny::reactive({
            result <- tradeoffdata()
            shiny::req(result)
            level <- function(value){ if(length(value) == 1 && value %in% c("few", "some", "many")) value else "some" }
            if(sum(!is.na(result$data$BEST_VALUE)) < 2){
                return(NULL)
            }
            get.tradeoff.efficiency(result$data, jumps = level(input$jumps), flats = level(input$flats))
        })

        ## The chart shows the settings of the last successful refresh (last.args()), exactly like
        ## the Results table - never unsaved or newer sidebar settings - so both tabs always describe
        ## the same character. It's computed while its tab is open, whenever something it depends on has
        ## changed: a refresh with different settings, or another Maximize choice. Neither the
        ## table size nor the movement type changes the curve (it covers every movement class), so a refresh that
        ## changes only those keeps it and just moves the emphasized movement line.
        shiny::observeEvent(list(shown(), last.args(), input$metric), {
            shiny::req(shown(), last.args(), input$metric)
            snapshot <- last.args()
            current <- tradeoffdata()
            if(!is.null(current) && identical(current$key, tradeoff.key(snapshot, input$metric))){
                if(!identical(current$selected.movement, snapshot$movement)){
                    current$selected.movement <- snapshot$movement
                    tradeoffdata(current)
                }
                return(invisible(NULL))
            }

            ## Same error/warning handling as "Refresh Armor Data" above
            tradeoff.warnings <- character(0)

            tryCatch(

            withCallingHandlers({

                shinybusy::show_modal_spinner()

                tradeoffdata(compute.tradeoffs(snapshot, input$metric))
                show.error("")

                for(message in unique(tradeoff.warnings)){
                    shiny::showNotification(message, type = "warning")
                }

            },

            warning = function(w){
                tradeoff.warnings <<- c(tradeoff.warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            }),

            error = function(e) {
                show.error(conditionMessage(e))
            },

            finally = {
                shinybusy::remove_modal_spinner()
            }

            )

        })

        output$plot <- plotly::renderPlotly({
            result <- tradeoffdata()
            shiny::req(result)
            d <- data.table::copy(result$data)
            metric.label <- tradeoff.metric.labels[[result$metric]]
            ## The charted stat at its display precision: the score to 3 decimals, poise whole, a
            ## defense or resistance to 1 decimal
            value.digits <- if(result$metric == "SCORE") 3 else if(result$metric == "POISE") 0 else 1
            ## (None for points with no set: those only ever come before a curve's first set, and are
            ## trimmed off the chart below)
            d$hover <-
                ifelse(
                    is.na(d$BEST_VALUE),
                    NA_character_,
                    sprintf(
                        "Armor weight up to %.1f<br>%s: %s<br>%s<br>%s<br>%s<br>%s<br>Weighs %.1f - Movement: %s<br>Score %.3f (%s)",
                        d$ARMOR_WEIGHT_LIMIT, metric.label, formatC(round(d$BEST_VALUE, value.digits), format = "f", digits = value.digits),
                        ## The Mask of the Father abbreviated, to keep the hover box narrow
                        d$HEAD, d$CHEST, d$HANDS, d$LEGS, d$ARMOR_WEIGHT, sub("Mask of the Father bonus", "MotF bonus", d$MOVEMENT, fixed = TRUE), d$SCORE, d$SCORE_QUALITY
                    )
                )
            d$point <- seq_len(nrow(d))

            shapes <- list()
            annotations <- list()
            ## The value axis: the curve, and the poise breakpoints and minimum drawn across it, with a
            ## margin - set rather than left to plotly, so that the flats' hatching can fill it top to bottom
            across <- c(if(result$metric == "POISE") c(21, 31, 46, 61), if(result$stat.minimum > 0) result$stat.minimum)
            value.range <- range(c(d$BEST_VALUE[!is.na(d$BEST_VALUE)], across))
            margin <- if(diff(value.range) > 0) 0.06*diff(value.range) else 1
            value.range <- value.range+c(-1, 1)*margin

            ## Where extra weight pays off (get.tradeoff.efficiency): each line shaded behind the chart,
            ## colored by how much it buys per unit of weight against the curve's average, outlined in white
            ## (the chart's background) so neighbouring lines stay apart, translucent so the grid shows
            ## through; each flat hatched in grey; each jump's rise drawn over the curve and labelled with
            ## its gain; each boundary point marked on the curve. Each point's hover text names the piece
            ## it's in, and its box takes that piece's color (a line's tint, a flat's grey, white otherwise);
            ## a boundary point also gives the rates either side.
            pieces <- if(is.null(tradeoff.efficiency())) NULL else tradeoff.efficiency()$data
            d$hover.color <- "white"
            if(!is.null(pieces)){
                lines.of <- pieces[TYPE == "line"]
                colors <- efficiency.color(lines.of$RATIO_TO_AVERAGE)
                for(i in seq_len(nrow(lines.of))){
                    shapes[[length(shapes)+1]] <- list(type = "rect", x0 = lines.of$FROM[i], x1 = lines.of$TO[i], y0 = 0, y1 = 1, yref = "paper", fillcolor = colors[i], opacity = 0.35, line = list(width = 2, color = "white"), layer = "below")
                }
                ## The pieces a point can belong to: jumps, lines, flats, and boundary points that are a
                ## single step between two flats (others share their step with the line beside them)
                between.flats <- pieces$TYPE == "boundary" & c(FALSE, head(pieces$TYPE, -1) == "flat") & c(tail(pieces$TYPE, -1) == "flat", FALSE)
                owners <- pieces[pieces$TYPE != "boundary" | between.flats]
                first.weight <- d$ARMOR_WEIGHT_LIMIT[which(!is.na(d$BEST_VALUE))[1]]
                rate <- function(v) if(v == 0) "flat" else sprintf("%.2fx average", v)
                for(j in which(!is.na(d$BEST_VALUE))){
                    w <- d$ARMOR_WEIGHT_LIMIT[j]
                    i <- which((owners$FROM < w-1e-9 | abs(w-first.weight) < 1e-9) & owners$TO >= w-1e-9)[1]
                    if(is.na(i)){
                        next
                    }
                    o <- owners[i]
                    ## Its first point: the next weight after the point it's measured from
                    from <- if(abs(o$FROM-first.weight) < 1e-9 && i == 1) o$FROM else min(d$ARMOR_WEIGHT_LIMIT[d$ARMOR_WEIGHT_LIMIT > o$FROM+1e-9])
                    span <- if(abs(from-o$TO) < 1e-9) sprintf("%.1f", o$TO) else sprintf("%.1f-%.1f", from, o$TO)
                    d$hover[j] <- paste0(d$hover[j], "<br>", switch(o$TYPE,
                        line = sprintf("Line %s: %.2fx average (%+.*f %s per unit weight)", span, o$RATIO_TO_AVERAGE, value.digits+1, o$GAIN/(o$TO-o$FROM), metric.label),
                        flat = sprintf("Flat %s: no gain", span),
                        jump = sprintf("Jump at %.1f: %+.*f %s for %.1f weight", o$TO, value.digits, o$GAIN, metric.label, o$TO-o$FROM),
                        boundary = sprintf("Step at %.1f between two flats: %+.*f %s", o$TO, value.digits, o$GAIN, metric.label)
                    ))
                    d$hover.color[j] <- switch(o$TYPE, line = efficiency.color(o$RATIO_TO_AVERAGE, tint = 0.35), flat = "rgb(232,232,232)", "white")
                    at <- which(pieces$TYPE == "boundary" & abs(pieces$TO-w) < 1e-9 & !between.flats)
                    if(length(at) > 0){
                        d$hover[j] <- paste0(d$hover[j], sprintf("<br>Boundary: %s, then %s",rate(pieces$RATIO_BEFORE[at[1]]), rate(pieces$RATIO_AFTER[at[1]])))
                    }
                }
                ## Jump labels, to the left of each rise, halfway up it
                jumps.of <- pieces[TYPE == "jump"]
                for(i in seq_len(nrow(jumps.of))){
                    annotations[[length(annotations)+1]] <- list(
                        x = jumps.of$TO[i], y = (jumps.of$START_VALUE[i]+jumps.of$END_VALUE[i])/2, text = sprintf("%+.*f", value.digits, jumps.of$GAIN[i]),
                        showarrow = FALSE, xanchor = "right", xshift = -4, font = list(color = JUMP.COLOR), bgcolor = "rgba(255,255,255,0.8)"
                    )
                }
            }
            ## Movement breakpoints that fall within the chart, the selected movement type's emphasized
            line.labels <- c(Light = "Light | Mid", Mid = "Mid | Fat", Fat = "Fat | Poop")
            for(movement in names(result$lines)){
                x <- result$lines[[movement]]
                if(x >= 0 && x <= max(d$ARMOR_WEIGHT_LIMIT)){
                    selected <- movement == result$selected.movement
                    shapes[[length(shapes)+1]] <- list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper", line = list(dash = if(selected) "solid" else "dash", width = if(selected) 2 else 1, color = if(selected) "black" else "gray"))
                    annotations[[length(annotations)+1]] <- list(x = x, y = 1, yref = "paper", text = if(selected) paste0("<b>", line.labels[[movement]], "</b>") else line.labels[[movement]], showarrow = FALSE, xanchor = "center", yanchor = "bottom")
                }
            }
            ## Poise breakpoints players aim for (see the User Guide)
            if(result$metric == "POISE"){
                for(breakpoint in c(21, 31, 46, 61)){
                    shapes[[length(shapes)+1]] <- list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = breakpoint, y1 = breakpoint, line = list(dash = "dot", color = "firebrick"))
                    annotations[[length(annotations)+1]] <- list(x = 0, xref = "paper", y = breakpoint, text = paste("Poise", breakpoint), showarrow = FALSE, xanchor = "left", yanchor = "bottom")
                }
            }
            ## The user's minimum on the charted stat (ignored by the curve itself)
            if(result$stat.minimum > 0){
                shapes[[length(shapes)+1]] <- list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = result$stat.minimum, y1 = result$stat.minimum, line = list(dash = "dash", color = "steelblue"))
                annotations[[length(annotations)+1]] <- list(x = 1, xref = "paper", y = result$stat.minimum, text = paste("Your minimum:", format(result$stat.minimum)), showarrow = FALSE, xanchor = "right", yanchor = "bottom")
            }
            ## The curve from its first point with a set to its last: plotly drops points with no set at
            ## either end from the line and its hover text, but not from the hover colors, which would then
            ## be out of step. (Points with no set in between stay, as a gap, so the colors keep step.)
            shown <- which(!is.na(d$BEST_VALUE))
            if(length(shown) > 0){
                d <- d[min(shown):max(shown)]
            }
            ## The flats first (behind the curve), then the curve, then the jumps' rises and the boundary
            ## points over it - all but the curve left out of hover, so that each weight has one box
            p <- plotly::plot_ly(source = "tradeoffs")
            if(!is.null(pieces)){
                flats.of <- pieces[TYPE == "flat"]
                for(i in seq_len(nrow(flats.of))){
                    p <- plotly::add_trace(
                        p, x = c(flats.of$FROM[i], flats.of$FROM[i], flats.of$TO[i], flats.of$TO[i], flats.of$FROM[i]),
                        y = value.range[c(1, 2, 2, 1, 1)], name = "flat", type = "scatter", mode = "lines", fill = "toself",
                        fillcolor = "rgba(150,150,150,0.12)", fillpattern = list(shape = "/", fgcolor = "rgb(150,150,150)", size = 8, solidity = 0.15),
                        line = list(width = 0), hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
                    )
                }
            }
            p <- plotly::add_trace(
                p, data = d, x = ~ARMOR_WEIGHT_LIMIT, y = ~BEST_VALUE, customdata = ~point, text = ~hover, hoverinfo = "text",
                hoverlabel = list(bgcolor = d$hover.color, font = list(color = "black")), name = "curve",
                ## plotly's usual first color, set because the flats' traces come before it
                type = "scatter", mode = "lines+markers", line = list(shape = "hv", color = CURVE.COLOR), marker = list(color = CURVE.COLOR),
                showlegend = FALSE, inherit = FALSE
            )
            if(!is.null(pieces) && any(pieces$TYPE == "jump")){
                jumps.of <- pieces[TYPE == "jump"]
                ## Each rise as the curve draws it (along, then up), the jumps apart
                rise.x <- unlist(lapply(seq_len(nrow(jumps.of)), function(i) c(jumps.of$FROM[i], jumps.of$TO[i], jumps.of$TO[i], NA)))
                rise.y <- unlist(lapply(seq_len(nrow(jumps.of)), function(i) c(jumps.of$START_VALUE[i], jumps.of$START_VALUE[i], jumps.of$END_VALUE[i], NA)))
                p <- plotly::add_trace(
                    p, x = rise.x, y = rise.y, name = "jumps", type = "scatter", mode = "lines",
                    line = list(color = JUMP.COLOR, width = 5), connectgaps = FALSE, hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
                )
            }
            if(!is.null(pieces) && any(pieces$TYPE == "boundary")){
                bounds.of <- pieces[TYPE == "boundary"]
                p <- plotly::add_trace(
                    p, x = bounds.of$TO, y = bounds.of$END_VALUE, name = "boundaries", type = "scatter", mode = "markers",
                    marker = list(symbol = "diamond-open", size = 11, color = "rgb(40,40,40)", line = list(width = 2)),
                    hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
                )
            }
            p <-
                plotly::layout(
                    p,
                    xaxis = list(title = "Armor weight limit"), yaxis = list(title = paste("Best", metric.label), range = value.range),
                    shapes = shapes, annotations = annotations,
                    ## Hover (and click) by weight alone, so the pointer needn't be on the marker
                    hovermode = "x"
                )
            plotly::event_register(p, "plotly_click")
        })

        ## Clicking a point: its armor set's links. Triggered by plotly's own click input rather than by
        ## event_data(), which (via a deferred check) warns whenever it's called before the chart has
        ## been drawn and registered its click event - here it only runs after an actual click. plotly
        ## sends its clicks to the app's own inputs, not the module's, under the chart's source.
        shiny::observeEvent(session$rootScope()$input[["plotly_click-tradeoffs"]], {
            click <- plotly::event_data("plotly_click", source = "tradeoffs")
            point <- tradeoffdata()$data[click$customdata[1]]
            if(nrow(point) == 1 && !is.na(point$HEAD)){
                show.links(point)
            }
        })

        output$table <- DT::renderDataTable({
            result <- tradeoffdata()
            shiny::req(result)
            value.digits <- if(result$metric == "POISE") 0 else 1
            table <-
                DT::datatable(
                    tradeoff.table(result),
                    selection = "single",
                    options = list(scrollX = TRUE, paging = FALSE, scrollY = "400px", scrollCollapse = TRUE)
                ) |>
                DT::formatCurrency("ARMOR_WEIGHT_LIMIT", currency = "", interval = 3, mark = ",", digits = 1) |>
                DT::formatCurrency("SCORE", currency = "", interval = 3, mark = ",", digits = 3)
            if(result$metric != "SCORE"){
                table <- DT::formatCurrency(table, result$metric, currency = "", interval = 3, mark = ",", digits = value.digits)
            }
            table
        })

        shiny::observeEvent(input$table_rows_selected, {
            point <- tradeoffdata()$data[input$table_rows_selected]
            if(!is.na(point$HEAD)){
                show.links(point)
            }
        })

        ## The Trade-offs table for a download of a refresh (snapshot, its settings), for the current
        ## Maximize choice: the curve the tab holds if it's for those, computed now (and kept)
        ## otherwise
        download.table <- function(snapshot){
            result <- tradeoffdata()
            if(is.null(result) || !identical(result$key, tradeoff.key(snapshot, input$metric))){
                result <- compute.tradeoffs(snapshot, input$metric)
                tradeoffdata(result)
            }
            tradeoff.table(result)
        }

        list(data = tradeoffdata, efficiency = tradeoff.efficiency, computations = tradeoff.computations, download.table = download.table)

    })
}
