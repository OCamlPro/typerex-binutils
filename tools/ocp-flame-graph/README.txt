ocp-flame-graph
===============

Generates a simple Flame Graph, with builtin commands to interpret directly
  Linux Perf files.

Inspired from flamegraph.pl, by Brendan Gregg.

ocp-flame-graph [OPTIONS] [FILES] : FILES should be in "folded" format, as
  expected by flamegraph.pl

Usage with Perf:
----------------

* You can ask ocp-flame-graph to run your program:

ocp-flame-graph [OPTIONS] --perf COMMAND ARGS

It will generate an SVG file called "perf-COMMAND-PID.svg" that you can view
in most browsers.

* If you have already generated the "perf.data" file, usually with
"perf -F 99 -g -- COMMAND ARGS", you can use ocp-flame-graph to
generate the Flame Graph from the output of "perf script" :

ocp-flame-graph [OPTIONS] --perf-script

By default, the Flame Graph is generated on the standard output.

Other Options:
--------------

  -o OUTPUT.svg
  --output OUTPUT.svg   Output SVG to this file (-- is stdout)
  --max-depth MAX_DEPTH max depth of flame graph (default 30)
  --js FILE.js          Javascript to include in generated SVG file
  -F Freq               Frequency for perf (current 99)
