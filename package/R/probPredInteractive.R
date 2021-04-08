#' probPredInteractive function
#'
#' Loads the probabilistic model to an interactive R-Shiny interface. By J Hunter & Team.
#' @keywords none
#' @export
#' @examples
#' probPredInteractive()          ## An example of how to use the function. All user inputs are accessed from within the Shiny interface.

probPredInteractive=function() {

#   JScode <-
#
#     "$(function() {
#
#     setTimeout(function(){
#
#     var vals = [0];
#
#     var powStart = 7;
#
#     var powStop = 0;
#
#     for (i = powStart; i >= powStop; i--) {
#
#     var val = Math.pow(10, -i);
#
#     val = parseFloat(val.toFixed(8));
#
#     vals.push(val);
#
#     }
#
#     $('#offset').data('ionRangeSlider').update({'values':vals})
#
# }, 5)})"

  appDir = system.file("shiny",package="ProbPred")

  shiny::runApp(appDir=appDir)
}
