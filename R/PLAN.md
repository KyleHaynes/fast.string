KYLE, give attention to this...

One thing I'd flag but didn't change
.onAttach() attaches wrappers for grepl, grep, sub, gsub, trimws, substr, nchar, chartr at search path position 2 in interactive sessions. Every console-level nchar() then goes through a ...-forwarding closure instead of the primitive — a real cost inside sapply, and exists/get/environment on those names see your wrapper. It's deliberate and opt-out-able, so I left it, but it's worth knowing it's there when something at the console behaves oddly.

Status
R CMD check: Status: OK, no errors/warnings/notes. Build is warning-free. test-rlink-inspect.R 360 passing, test-linkage-model.R 3,319 passing. Full suite in one process passes under the check. Final per-file run is still going — I'll report it when it lands.