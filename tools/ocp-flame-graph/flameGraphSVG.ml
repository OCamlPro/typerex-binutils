
let title_height = 40.
let caption_height = 20.
let xmargin = 10.
let box_height = 16.

type t = {
  width : float;
  height : float;
  buffer : Buffer.t;
  title : string;
}

let realwidth t = t.width +. 2. *. xmargin
let realheight t = t.height +. title_height +. caption_height

let header t =
  let realwidth = realwidth t in
  let realheight = realheight t in
  Printf.bprintf t.buffer "<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg version=\"1.1\" width=\"%.0f\" height=\"%.0f\" onload=\"init(evt)\" viewBox=\"0 0 %.0f %.0f\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">
<!-- Flame graph stack visualization. See https://github.com/brendangregg/FlameGraph for latest version, and http://www.brendangregg.com/flamegraphs.html for examples. -->
<defs >
	<linearGradient id=\"background\" y1=\"0\" y2=\"1\" x1=\"0\" x2=\"0\" >
		<stop stop-color=\"#eeeeee\" offset=\"5%%\" />
		<stop stop-color=\"#eeeeb0\" offset=\"95%%\" />
	</linearGradient>
</defs>
<style type=\"text/css\">
	.func_g:hover { stroke:black; stroke-width:0.5; cursor:pointer; }
         </style>\n"
    realwidth realheight realwidth realheight
;;

let javascript t =
  Printf.bprintf t.buffer "%s"
    (List.assoc "flameGraphSVG.js" FlameGraphSVGFiles.files)

let screen t =
  let realwidth = realwidth t in
  let realheight = realheight t in
  let details_y = realheight -. caption_height +. 10.5 in
  Printf.bprintf t.buffer
    "<rect x=\"0.0\" y=\"0\" width=\"%.0f\" height=\"%.0f\" fill=\"url(#background)\"  />
<text text-anchor=\"middle\" x=\"600.00\" y=\"24\" font-size=\"17\" font-family=\"Verdana\" fill=\"rgb(0,0,0)\"  >%s</text>
<text text-anchor=\"\" x=\"10.00\" y=\"%.0f\" font-size=\"12\" font-family=\"Verdana\" fill=\"rgb(0,0,0)\" id=\"details\" > </text>
<text text-anchor=\"\" x=\"10.00\" y=\"24\" font-size=\"12\" font-family=\"Verdana\" fill=\"rgb(0,0,0)\" id=\"unzoom\" onclick=\"unzoom()\" style=\"opacity:0.0;cursor:pointer\" >Reset Zoom</text>
<text text-anchor=\"\" x=\"1090.00\" y=\"24\" font-size=\"12\" font-family=\"Verdana\" fill=\"rgb(0,0,0)\" id=\"search\" onmouseover=\"searchover()\" onmouseout=\"searchout()\" onclick=\"search_prompt()\" style=\"opacity:0.1;cursor:pointer\" >Search</text>
<text text-anchor=\"\" x=\"1090.00\" y=\"2081\" font-size=\"12\" font-family=\"Verdana\" fill=\"rgb(0,0,0)\" id=\"matched\" > </text>\n"
    realwidth realheight
    t.title
    details_y
;;

let rectangle t ~title ~caption ~x ~y ~width ~red ~green ~blue =
  let y = title_height +. t.height -. y -. box_height in
  Printf.bprintf t.buffer "<g class=\"func_g\" onmouseover=\"s(this)\" onmouseout=\"c()\" onclick=\"zoom(this)\">\n";
  Printf.bprintf t.buffer "<title>%s</title><rect x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"15.0\" fill=\"rgb(%d,%d,%d)\" rx=\"2\" ry=\"2\" />\n" caption (xmargin +. x) y width red green blue;
  Printf.bprintf t.buffer "<text text-anchor=\"\" x=\"%.2f\" y=\"%.2f\" font-size=\"12\" font-family=\"Verdana\" fill=\"rgb(0,0,0)\"  >%s</text>\n"
    (xmargin +. x +. 3.) (y +. 10.5) title;
  Printf.bprintf t.buffer "</g>\n";
  ()

let trailer t = Printf.bprintf t.buffer "</svg>\n"

let to_string t =
  trailer t;
  Buffer.contents t.buffer

let create ~title ~width ~height =
  let buffer = Buffer.create 10_000 in
  let t =
  {
    title; width; height; buffer;
  }
  in
  header t;
  javascript t;
  screen t;
  t
