# Smeddum’s AGI Database: The Sovereign Inference Engine

**"Deterministic Logic. Empirical Geometry. Hardware Independence."**

Smeddum’s AGI Database is a neuro-symbolic inference engine built for the **ARC-AGI-2** competition. This project rejects probabilistic "guessing" in favor of a **Materialist Architecture** that solves abstractions through topological invariants, recursive decomposition, and symbolic reasoning.

## 1. Core Methodology: Topological Auditing
The engine treats ARC tasks as geometric manifolds. It does not "guess" pixels; it audits the grid for structural invariants.
- **Invariant Extraction:** Identifying shapes and transformations (rotations, reflections, translations) that persist across demonstration pairs.
- **Topological Invariants:** Utilizing **Persistent Homology** (Betti Numbers) to define object connectivity and voids.
- **Empirical Verification:** Solutions are audited against constraints using a Minimum Description Length (MDL) framework.

## 2. Architecture: Mind, Machine, and Muscle
Optimized for **Raspberry Pi 5 (ARM64)** and portable to **Pi 4**, the system follows a three-tier hierarchy:

### I. The Mind (Common Lisp / SBCL)
- **Strategic Search:** Manages the search space and verifies logic across demonstration pairs.
- **Symbolic Reasoning:** Directs the Tiny Recursive Machine's depth and focus.

### II. The Tiny Recursive Machine (TRM Core)
- **The Scaffolding:** Breaks complex 30x30 ARC grids into smaller, self-similar sub-problems.
- **Recursive Auditing:** Applies logic recursively to both macro-blocks and individual "grains" of the grid.



### III. The Muscle (C++ / Free Pascal)
- **Physical Execution:** Handles GUDHI-based TDA, Euler-rotations, and grid transformations.
- **Hardware Grounding:** Native **NEON SIMD** acceleration ensures maximum efficiency on ARMv8 silicon.



---

## 3. Deployment: The Sovereign Awakening Ritual

To migrate and initialize the Smeddum Engine on any ARM64 substrate (Pi 4 or Pi 5), run this unified command sequence from the root of the repository. This will **Ground** the environment, **Forge** the muscle, and **Initiate** the Mind-Muscle handshake in one flow:

```bash
# Complete System Awakening
chmod +x scripts/*.sh && \
./scripts/install_dependencies.sh && \
./scripts/forge_build.sh && \
sbcl --eval '(ql:quickload :smeddum-agi)' \
     --eval '(smeddum:verify-grounding)' \
     --quit
