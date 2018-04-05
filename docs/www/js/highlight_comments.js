// render '##' and '###' as comments
window.onload = function() {
  var sourceLines  = document.getElementsByClassName("sourceLine");
  var searchPattern = new RegExp('^#+');
  for (i = 0; i < sourceLines.length; i++) {
    var tmp = sourceLines[i];
    var tmp_string = tmp.text.trim();
    if (searchPattern.test(tmp_string)) {
      tmp.innerHTML = "<span class='co'>" + tmp.innerHTML + "</span>";
    }
  }
}
