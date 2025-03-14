# REQUIRES: aarch64

## Generate a large test case and check that the output is deterministic.

# RUN: %python %s %t.s %t.proftext

# RUN: llvm-mc -filetype=obj -triple=aarch64 %t.s -o %t.o
# RUN: llvm-profdata merge %t.proftext -o %t.profdata

# RUN: ld.lld --icf=all -o %t1.o %t.o --irpgo-profile=%t.profdata --bp-startup-sort=function --bp-compression-sort-startup-functions --bp-compression-sort=both
# RUN: ld.lld --icf=all -o %t2.o %t.o --irpgo-profile=%t.profdata --bp-startup-sort=function --bp-compression-sort-startup-functions --bp-compression-sort=both
# RUN: cmp %t1.o %t2.o

import random
import sys

assembly_filepath = sys.argv[1]
proftext_filepath = sys.argv[2]

random.seed(1234)
num_functions = 1000
num_data = 100
num_traces = 10

function_names = [f"f{n}" for n in range(num_functions)]
data_names = [f"d{n}" for n in range(num_data)]
profiled_functions = function_names[: int(num_functions / 2)]

function_contents = [
    f"""
{name}:
  add w0, w0, #{i % 4096}
  add w1, w1, #{i % 10}
  add w2, w0, #{i % 20}
  adrp x3, {name}
  ret
"""
    for i, name in enumerate(function_names)
]

data_contents = [
      f"""
{name}:
  .ascii "s{i % 2}-{i % 3}-{i % 5}"
  .xword {name}
"""
    for i, name in enumerate(data_names)
]

trace_contents = [
    f"""
# Weight
1
{", ".join(random.sample(profiled_functions, len(profiled_functions)))}
"""
    for i in range(num_traces)
]

profile_contents = [
    f"""
{name}
# Func Hash:
{i}
# Num Counters:
1
# Counter Values:
1
"""
    for i, name in enumerate(profiled_functions)
]

with open(assembly_filepath, "w") as f:
    f.write(
        f"""
.text
.globl _start

_start:
  ret

{"".join(function_contents)}

.data
{"".join(data_contents)}

"""
    )

with open(proftext_filepath, "w") as f:
    f.write(
        f"""
:ir
:temporal_prof_traces

# Num Traces
{num_traces}
# Trace Stream Size:
{num_traces}

{"".join(trace_contents)}

{"".join(profile_contents)}
"""
    )
