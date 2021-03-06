#' Create a choropleth
#' 
#' Creates a choropleth from a specified data.frame and level of detail.
#'
#' @param df A data.frame with a column named "region" and a column named "value".  If lod is "state" 
#' then region must contain state names (e.g. "California" or "CA").  If lod is "county" then region must  
#' contain county FIPS codes.  if lod is "zip" then region must contain 5 digit ZIP codes.
#' @param lod A string indicating the level of detail of the map.  Must be "state", "county" or "zip".
#' @param num_buckets The number of equally sized buckets to places the values in.  A value of 1 
#' will use a continuous scale, and a value in [2, 9] will use that many buckets.  For
#' example, 2 will show values above or below the median, and 9 will show the maximum
#' resolution.  Defaults to 9.
#' @param title A title for the map.  Defaults to "".
#' @param scaleName An optional label for the legend.  Defaults to "".
#' @param showLabels For state choropleths, whether or not to show state abbreviations on the map.
#' Defaults to T. 
#' @param states A vector of states to render.  Defaults to state.abb.
#' @param renderAsInsets If true, Alaska and Hawaii will be rendered as insets on the map.  If false, all 50 states will be rendered
#' on the same longitude and latitude map to scale. This variable is only checked when the "states" variable is equal to all 50 states.
#' @return A choropleth.
#' 
#' @keywords choropleth
#' 
#' @importFrom ggplot2 ggplot aes geom_polygon scale_fill_brewer ggtitle theme theme_grey element_blank geom_text
#' @importFrom ggplot2 scale_fill_continuous map_data scale_colour_brewer
#' @importFrom plyr arrange rename
#' @importFrom scales comma
#' @importFrom Hmisc cut2
#' 
#' @seealso \code{\link{choroplethr_acs}} which generates choropleths from Census tables.
#' 
#' @export
#' @examples
#' data(choroplethr)
#'
#' # 2012 US Presidential results
#' choroplethr(df_president, lod="state", title="2012 US Presidential Results")
#'
#' # 2012 state population estimates - continuous vs. discrete scale
#' choroplethr(df_pop_state, lod="state", num_buckets=1, title="2012 State Population Estimates")
#' choroplethr(df_pop_state, lod="state", num_buckets=9, title="2012 State Population Estimates")
#' 
#' # West coast counties in 2012 which were above or below the median population
#' choroplethr(df_pop_county, lod="county", num_buckets=2, 
#' title="2012 County Population Estimates", states=c("CA", "OR", "WA"))
#' 
#' # Zip Code Tabulated Area (ZCTA) population estimates.  
#' choroplethr(df_pop_zip, "zip", title="2012 Population of each ZIP Code Tabulated Area (ZCTA)")
choroplethr = function(df, 
                       lod, 
                       num_buckets = 9, 
                       title = "", 
                       scaleName = "",
                       showLabels = TRUE,
                       states = state.abb,
                       renderAsInsets = TRUE)
{
  stopifnot(c("region", "value") %in% colnames(df))
  stopifnot(lod %in% c("state", "county", "zip"))
  stopifnot(num_buckets > 0 && num_buckets < 10)
  
  # states shouldn't be duplicated, and must be entered as valid postal codes
  stopifnot(states %in% state.abb)
  stopifnot(!any(duplicated(states)))
  
  df = df[, c("region", "value")] # prevent naming collision from merges later on
  
  if (lod == "state") {
    state_choropleth_auto(df, num_buckets, title, showLabels, scaleName, states, renderAsInsets);
  } else if (lod == "county") {
    county_choropleth_auto(df, num_buckets, title, scaleName, states)
  } else if (lod == "zip") {
    zip_choropleth_auto(df, num_buckets, title, scaleName, states)
  }
}