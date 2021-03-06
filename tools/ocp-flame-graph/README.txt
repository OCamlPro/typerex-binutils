ocp-flame-graph
===============

Generates a simple Flame Graph, with builtin commands to interpret directly
  Linux Perf files.

Inspired from flamegraph.pl, by Brendan Gregg.

ocp-flame-graph [OPTIONS] [FILES] : FILES should be in "folded" format, as
  expected by flamegraph.pl

OCaml configuration:
--------------------

For ocp-flame-graph output to be meaningful, you need a distribution
of OCaml compiled with --with-frame-pointers, and your application and
libraries to be compiled and linked with -g.

Usage with Perf:
----------------

* You can ask ocp-flame-graph to run your program:

```
ocp-flame-graph [OPTIONS] --perf COMMAND ARGS
```

It will generate an SVG file called "perf-COMMAND-PID.svg" that you can view
in most browsers.

* If you have already generated the "perf.data" file, usually with
"perf -F 99 -g -- COMMAND ARGS", you can use ocp-flame-graph to
generate the Flame Graph from the output of "perf script" :

```
ocp-flame-graph [OPTIONS] --perf-script
```

By default, the Flame Graph is generated on the standard output.

Other Options:
--------------

```
  -o OUTPUT.svg
  --output OUTPUT.svg   Output SVG to this file (-- is stdout)
  --max-depth MAX_DEPTH max depth of flame graph (default 30)
  --js FILE.js          Javascript to include in generated SVG file
  -F Freq               Frequency for perf (current 99)
  --interpolate Fun     Try to interpolate partial stacks within Fun
  --output-folded FILE  Output also in folded format
  --recursive           Merge recursive function calls
```

Discussions:
------------
You should avoid values such as 100 for frequency, to avoid clock steps.
If your flame graph shows "partial stacks", i.e. the unwinder didn't unwind
all the stack, resulting in a missing bottom of stack, you can use
the --interpolate option to try to recover the correct bottom of stack.
Typically:

```
ocp-flame-graph -o perf.svg --max-depth 40 --interpolate __libc_start_main --perf-script
```

Notice that the argument of --interpolate should be the function at
the bottom of the stack.

The folded format is the format used by flamegraph.pl, and generated by
many "collapse*" tools available for many profilers.
