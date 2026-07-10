#!/usr/bin/env python3
"""Health check used by Shiny before generating TFM reports."""
from __future__ import annotations
import importlib.util
import sys
print("PYTHON_OK=" + sys.executable)
print("PYTHON_VERSION=" + sys.version.split()[0])
missing = []
for name in ("pandas", "numpy"):
    if importlib.util.find_spec(name) is None:
        missing.append(name)
if missing:
    print("PYTHON_MISSING=" + ",".join(missing))
    sys.exit(42)
print("PYTHON_DEPS_OK=1")
