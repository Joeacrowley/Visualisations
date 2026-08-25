# house_colours.R
#
# A five-family qualitative palette with a five-step tint ramp per family,
# plus a standalone grey - similar in shape to a typical workplace
# brand-colour template (one base hue per family, tinted from saturated to
# pale), but with independently chosen hues so it stays free of any
# employer's brand identity. Bases alternate deep/vivid in character: blue
# and purple sit deep and dark, pink, mint and gold sit bright and punchy.

# make_tint_ramp(): generate n tints of a single base colour, from the base
# colour itself through to a pale tint of the SAME hue - deliberately capped
# short of pure white, so even the palest swatch still reads as a faint
# version of the colour rather than washing out to white.
#
# Inputs:
#   base_colour    - a single hex colour, the most saturated end of the ramp
#   n              - number of tints to generate (default 5)
#   max_lightening - how far toward white to travel, 0-1 (default 0.85 - the
#                    palest tint still carries 15% of the base colour)
# Output:
#   a character vector of n hex colours, base_colour first, palest last
make_tint_ramp <- function(base_colour, n = 5, max_lightening = 0.85) {
  ramp <- colorRampPalette(c(base_colour, "white"))(101)
  idx  <- round(seq(1, 1 + max_lightening * 100, length.out = n))
  ramp[idx]
}

# house_colours: six colour families plus a standalone grey family, each a
# five-step tint ramp built from one base hue - generated rather than
# hand-typed, so adding or re-tuning a family only means changing one hex
# value, not five.
#
# blue and purple are hand-corrected rather than fully generated:
# interpolating straight through RGB toward white desaturates them fast
# (blue's base 70% saturation collapses to ~37% by the second swatch,
# purple's 65% to ~40%) because both have two low, closely-matched RGB
# channels that rush toward white together. pink/gold don't have this
# problem - each has one channel already near white, which keeps their
# saturation close to the base throughout - so they're left on the
# generator.
#
# blue and purple both ended up on the same shape after a fair amount of
# trial and error: swatch 1 is a deep, saturated base (deeper than any other
# family's), and swatches 2-5 hold hue and saturation essentially flat
# (blue ~247/~74, purple ~290/~64) while lightness alone carries the rest
# of the ramp. Earlier attempts that dipped saturation right after the base
# (cushioning the big first lightness jump, the way mint/pink/green's rule
# works) either read as washed-out/grey through the middle swatches, or, if
# saturation was held high enough to avoid that, read as a distinct, vivid
# accent colour sitting next to the dark base rather than a lighter version
# of it. Keeping saturation flat and fairly high across all four lighter
# swatches - closer in spirit to how gold works than to a classic dip -
# avoided both problems: a bold, consistently-saturated group of four
# lighter tints sitting below one deliberately much darker anchor.
#
# mint, pink and green are hand-corrected on a different rule: swatch 1 is
# the original, untouched base colour, not deepened. Saturation only moves
# on the 5->4->3 leg (decreasing together with lightness, mirroring the
# generator's natural taper); it then holds flat across 3->2->1, so those
# three swatches share one saturation and are told apart by lightness
# alone. That lightness spacing is front-loaded - widest gap between 1 and
# 2, narrowing toward the pale end - so the flat-saturation trio still
# reads as clearly distinct steps. This rule suits bases that sit in a
# mid-range lightness band (L42-55) - it doesn't suit blue/purple's much
# darker bases, where the resulting first jump is too big to carry at flat
# saturation. gold is left on the plain generator, as its base saturation
# already sits near the ceiling with no room for this rule to do anything
# visible.
#
# green (hue 120) fills the largest empty stretch of the wheel, between
# gold (48) and mint (originally 160, since nudged to 170 - see note below).
# it shares blue/purple's desaturation problem
# rather than mint/pink's easy case - its base has two low, closely-matched
# RGB channels (R and B) that rush toward white together - so unlike mint
# and pink, its saturation doesn't hold perfectly flat at 4 and 5; it tapers
# down some there too. Checked against mint by CIE76 delta-E before adding:
# every swatch pair across the two families comes in above 9, comfortably
# distinct even at the pale end where both collapse toward the same
# near-white ceiling.
#
# the palest (5th) swatch of each of the six colour families - not grey -
# is hand-nudged +8 points of saturation over what the generator/ramp would
# otherwise give, hue and lightness both held fixed. at ~93% lightness the
# maximum possible chroma is small regardless of saturation, so this reads
# as a gentle lift rather than a dramatic one - most visible on blue and
# purple, which had the most headroom to begin with.
house_colours <- list(
  pink   = c("#F20D80", "#F65FAB", "#FA9DCC", "#FDC5E1", "#FFD9EC"),
  # mint's hue (160) sits close to where human brightness perception peaks
  # (the photopic luminosity function tops out in the yellow-green region),
  # so mint read as louder than its siblings in categorical charts even at
  # matched S/L. A hue shift was tried first (both toward chartreuse, which
  # made it worse, and toward cyan, which muted it but didn't read well in a
  # bar chart) and rejected in favour of a direct fix: saturation on the flat
  # 1-3 trio nudged down (-8 initially, settled on -12), tiers 4-5 left alone
  # at first, then also pulled back (-25 points) once they read as too
  # bright against the calmer 1-3 trio - both moves working against the same
  # ceiling from opposite directions, since at L89/L96 there's very little
  # chroma room regardless of saturation, so neither nudge is dramatic close
  # up. mint at -8/tiers 4-5 untouched, kept for reference:
  # mint  = c("#3ADFA8", "#7BEAC5", "#ACF1DA", "#CBFBEB", "#EBFFF8"),
  # mint at full saturation throughout, kept for reference:
  # mint  = c("#30E8AB", "#75F0C7", "#A8F5DB", "#CBFBEB", "#EBFFF8"),
  mint   = c("#3EDAA6", "#7EE7C4", "#AEEFD9", "#D2F4E9", "#EEFDF7"),
  # blue's earlier attempts, kept for reference (see comment above):
  # blue  = c("#231782", "#4F3DDD", "#9A8FEB", "#CEC9F1", "#E7E5F6"),  # early flat-saturation try
  # blue  = c("#231782", "#4C3DB8", "#887DD8", "#C0BBE7", "#E6E5F6"),  # purple-style dip pattern
  # blue  = c("#231782", "#3D2BCA", "#8174E2", "#BEB8EA", "#E6E5F6"),  # dip pattern, raised mid saturation
  blue   = c("#231782", "#6D5CE8", "#A399F0", "#DAD6F9", "#ECEAFC"),
  # gold was originally a straight orange (hue 38) - rotated hue +10 degrees
  # (hue 48) so it reads as gold rather than orange, keeping the same
  # saturation/lightness and the same generator/boost recipe throughout.
  # original orange, kept for reference:
  # gold  = c(make_tint_ramp("#F59B00")[1:4], "#FFF0D8"),
  gold   = c(make_tint_ramp("#F5C400")[1:4], "#FFF7D7"),
  # purple's earlier dip-pattern attempt, kept for reference:
  # purple = c("#8F22A0", "#B863C5", "#D79FE0", "#E5C8EA", "#F3E5F5"),
  purple = c("#8F22AC", "#BE31D3", "#D67EE3", "#EECBF3", "#F6E5F9"),
  green  = c("#15C115", "#4FEB4F", "#93F393", "#C3F6C3", "#DAF7DA"),
  grey   = make_tint_ramp("#616161")
)

# gradient_pink(): low/high pair for a single-hue continuous scale
# (scale_fill_gradient()) - palest pink at the low end, vivid pink at the
# high end. A function rather than a fixed object, so it re-reads
# house_colours live on every call rather than freezing pink's tiers at
# source time - used by both scale_fill_house_gradient() and the n>5
# branch of scale_fill_house_ordinal(), so it's kept as a shared helper
# rather than duplicated inline in each.
#
# Output:
#   a list with low/high hex colours
gradient_pink <- function() {
  list(low = house_colours$pink[5], high = house_colours$pink[1])
}

# alternative_palette(): swap-in alternatives to house_colours, each an
# earlier or externally-sourced starting point rather than the finished,
# hand-tuned families above. Two options:
#
#   "desaturate" - an earlier, more saturated six-base-colour set that
#     predates the current palette's hand corrections, each base run
#     through the plain make_tint_ramp() generator rather than the
#     hand-tuned rules above. Will visibly reintroduce blue and purple's
#     desaturation problem (see comment near the top of this file) - it's
#     an earlier stage of the design, not a finished alternative.
#
#   "tailwind" - the closest hue-matched families from Tailwind CSS's
#     default palette (tailwindcss.com/docs/colors), a well-established,
#     fully-documented 22-family/11-shade design system, mapped onto house
#     names: mint -> Tailwind cyan (H215, further from mint's H160 than
#     teal or emerald were, but the most legibly distinct "mint" of the
#     three tried), gold -> Tailwind orange (H48, near-exact match), purple
#     -> Tailwind fuchsia (H322, more magenta-leaning than house purple's
#     H287-292, tried after violet), pink -> Tailwind rose (H16, warmer/
#     more red-leaning than house pink's H330, tried after Tailwind's own
#     "pink"), gold -> Tailwind amber (H70, warmer/more yellow-leaning than
#     house gold's H48, tried after Tailwind's own "orange"), green ->
#     Tailwind green itself (H150). Five shades sampled directly from each
#     family's real ramp (Tailwind's own values - not run through
#     make_tint_ramp()), running from Tailwind's 500 (its own vivid "brand"
#     shade) down to 100, not 900 - 900 is a near-black darkening shade in
#     every Tailwind family, which read as uniformly too dark for a tier-1
#     base against house's own punchy (non-blue/purple) starting points.
#
# Returns the alternate palette rather than assigning it directly, so
# swapping it in is a visible, explicit step at the call site:
#   house_colours <- alternative_palette("tailwind")
# rather than a hidden side effect of calling the function.
#
# Inputs:
#   option - "desaturate" or "tailwind" (default "desaturate")
# Output:
#   a house_colours-shaped named list (six families + grey)
alternative_palette <- function(option = "desaturate") {

  if (option == "desaturate") {
    bases <- list(
      blue = "#6D5CE8", pink = "#F25C64", mint = "#009EAB",
      green = "#00AB52", purple = "#BE31D3", gold = "#FF8000"
    )
    return(c(lapply(bases, make_tint_ramp), list(grey = make_tint_ramp("#616161"))))
  }

  if (option == "tailwind") {
    return(list(
      blue   = c("#2B7FFF", "#51A2FF", "#8EC5FF", "#BEDBFF", "#DBEAFE"),
      pink   = c("#FF2056", "#FF637E", "#FFA1AD", "#FFCCD3", "#FFE4E6"),
      mint   = c("#00B8DB", "#00D3F2", "#53EAFD", "#A2F4FD", "#CEFAFE"),
      gold   = c("#FE9A00", "#FFB900", "#FFD230", "#FEE685", "#FEF3C6"),
      purple = c("#E12AFB", "#ED6AFF", "#F4A8FF", "#F6CFFF", "#FAE8FF"),
      green  = c("#00C950", "#05DF72", "#7BF1A8", "#B9F8CF", "#DCFCE7"),
      grey   = make_tint_ramp("#616161")
    ))
  }

  stop("'", option, "' isn't a valid option. Valid options: desaturate, tailwind")
}

# give_sequential_colours(): pick n colours from one family's ramp, spaced
# for maximum perceptual distinction at that count. For n <= 5, pulls
# specific tiers from within the requested family (the two extremes for
# n=2, an even spread for n=3/4, the full ramp for n=5). For n > 5, one
# family can't stretch any further, so it falls back to whole family
# ramps - the requested family's full 5, then the remaining families in
# house_colours' defined order (skipping the one already used and skipping
# grey), taking however many are needed from the last one in raw order.
#
# Inputs:
#   family - name of a family in house_colours (e.g. "blue")
#   n      - number of categories to colour, 1-30
# Output:
#   a character vector of n hex colours
give_sequential_colours <- function(family, n) {

  families <- setdiff(names(house_colours), "grey")

  if (!family %in% families) {
    stop("'", family, "' isn't a valid family. Valid families: ", paste(families, collapse = ", "))
  }
  if (n > length(families) * 5) {
    stop("n (", n, ") exceeds the maximum of ", length(families) * 5, " available colours.")
  }

  if (n <= 5) {
    idx <- switch(
      as.character(n),
      "1" = 1,
      "2" = c(1, 5),
      "3" = c(1, 3, 5),
      "4" = c(1, 2, 4, 5),
      "5" = c(1, 2, 3, 4, 5)
    )
    return(house_colours[[family]][idx])
  }

  # n > 5: whole ramps, requested family first, then the rest in order
  ordered_families <- c(family, setdiff(families, family))
  all_colours <- unlist(house_colours[ordered_families], use.names = FALSE)
  all_colours[seq_len(n)]
}

# .categorical_tier_colours(): internal helper - applies the tier-banding
# scheme (one colour per family per tier, tiers picked to maximise hue
# distinction first) to a specific set of families and returns the first n
# colours. Shared by give_categorical_colours() to service both a
# prioritised subset of families and, if n asks for more than that subset
# alone can give, the remaining families used to top it up.
.categorical_tier_colours <- function(n, primary, families) {
  k <- length(families)
  idx <- if (n <= k)        primary
         else if (n <= 2*k) c(2, 4)
         else if (n <= 3*k) c(1, 3, 5)
         else if (n <= 4*k) c(1, 2, 4, 5)
         else               c(1, 2, 3, 4, 5)

  colours <- unlist(lapply(idx, function(tier) {
    vapply(families, function(f) house_colours[[f]][tier], character(1))
  }), use.names = FALSE)

  colours[seq_len(n)]
}

# give_categorical_colours(): a set of colours for an unordered categorical
# variable, biased toward maximum hue distinction within whichever families
# are prioritised. By default draws on all six families equally. If
# `families` names a subset (e.g. c("blue", "gold")), that subset fills
# first - tier-banded exactly as the default case, just scoped to those
# families - and only once it's exhausted (n exceeds 5 colours per selected
# family) does the rest of the palette get pulled in to make up the
# difference. The hard ceiling stays at 30 (5 tiers x 6 families, grey
# excluded) regardless of how `families` is set - asking for more than your
# selection alone can give doesn't error, it just spills into the rest.
#
# Inputs:
#   n        - number of categories to colour (default 12)
#   primary  - which tier (1-5) the single-tier case draws from (default 1)
#   families - which house_colours families to prioritise (default all six,
#              excluding grey)
# Output:
#   a character vector of n hex colours
give_categorical_colours <- function(n = 12, primary = 1,
                                      families = setdiff(names(house_colours), "grey")) {

  all_families <- setdiff(names(house_colours), "grey")

  if (n > length(all_families) * 5) {
    stop("n (", n, ") exceeds the maximum of ", length(all_families) * 5, " available colours.")
  }

  n_selected <- min(n, length(families) * 5)
  colours <- .categorical_tier_colours(n_selected, primary, families)

  if (n > n_selected) {
    remaining <- setdiff(all_families, families)
    colours <- c(colours, .categorical_tier_colours(n - n_selected, primary, remaining))
  }

  colours
}

# scale_fill_house_categorical() / scale_colour_house_categorical(): a
# scale_fill_manual()/scale_colour_manual() equivalent for an unordered
# categorical variable, backed by give_categorical_colours().
#
# Inputs:
#   n        - number of categories to colour (default 12)
#   primary  - passed straight through to give_categorical_colours() (default 1)
#   families - passed straight through to give_categorical_colours()
#              (default all six, excluding grey)
# Output:
#   a scale_fill_manual()/scale_colour_manual() object
scale_fill_house_categorical <- function(n = 12, primary = 1,
                                          families = setdiff(names(house_colours), "grey")) {
  ggplot2::scale_fill_manual(values = give_categorical_colours(n, primary, families))
}

scale_colour_house_categorical <- function(n = 12, primary = 1,
                                            families = setdiff(names(house_colours), "grey")) {
  ggplot2::scale_colour_manual(values = give_categorical_colours(n, primary, families))
}

# scale_fill_house_sequential() / scale_colour_house_sequential(): a
# scale_fill_manual()/scale_colour_manual() equivalent for a single-family
# ramp, backed by give_sequential_colours().
#
# Inputs:
#   family - name of a family in house_colours (e.g. "blue")
#   n      - number of categories to colour
# Output:
#   a scale_fill_manual()/scale_colour_manual() object
scale_fill_house_sequential <- function(family, n) {
  ggplot2::scale_fill_manual(values = give_sequential_colours(family, n))
}

scale_colour_house_sequential <- function(family, n) {
  ggplot2::scale_colour_manual(values = give_sequential_colours(family, n))
}

# scale_fill_house_ordinal(): a scale_fill_ordinal() equivalent for an
# ordered factor. For n<=5 uses give_sequential_colours("pink", n) (the
# same discrete tiers as the sequential case); for n>5 samples n equally
# spaced points along the continuous gradient_pink low-high ramp, the same
# way ggplot2's own scale_fill_ordinal() discretely samples the continuous
# viridis colourmap rather than switching to a true continuous scale.
#
# Inputs:
#   n - number of ordered levels to colour
# Output:
#   a scale_fill_manual() object
scale_fill_house_ordinal <- function(n) {
  values <- if (n <= 5) {
    give_sequential_colours("pink", n)
  } else {
    gp <- gradient_pink()
    colorRampPalette(c(gp$low, gp$high))(n)
  }
  ggplot2::scale_fill_manual(values = values)
}

# scale_fill_house_gradient(): a scale_fill_gradient() equivalent for
# continuous data, backed by gradient_pink().
#
# Output:
#   a scale_fill_gradient() object
scale_fill_house_gradient <- function() {
  gp <- gradient_pink()
  ggplot2::scale_fill_gradient(low = gp$low, high = gp$high)
}

# scale_fill_house_diverging(): a scale_fill_gradient2() equivalent for
# continuous data with a meaningful midpoint - vivid pink and vivid mint at
# the two extremes, house_colours$grey's palest tier as the neutral centre.
# Reads house_colours live at call time (only used here, so kept inline
# rather than as a separate shared object).
#
# Inputs:
#   midpoint - the data value the neutral colour should sit at (default 0)
# Output:
#   a scale_fill_gradient2() object
scale_fill_house_diverging <- function(midpoint = 0) {
  ggplot2::scale_fill_gradient2(
    low = house_colours$pink[1], mid = house_colours$grey[5],
    high = house_colours$mint[1], midpoint = midpoint
  )
}
