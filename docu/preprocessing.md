Implementation Brief: Multi-Resolution Time-Series Preprocessing and Plotting
1. Purpose
Implement multi-resolution preprocessing and interactive plotting
inside the existing Shiny application. This is an implementation
task, not a request to create a separate library or redesign the
application from scratch.
The feature must make large time-series datasets responsive while
preserving scientifically meaningful information. At wide time ranges,
show a compact summary of the measurements; as the user zooms or pans,
progressively reveal more detail until individual raw observations are
shown where practical.
The system should be designed so the preprocessing and plotting code can
later be extracted into a reusable library, but extraction is out of
scope for this implementation.
Core behavior
- Read and work with data files that may be present in data/ and
  data_input/.
- data_input/ may contain arbitrary CSV or RDS files; do not assume
  one fixed schema or a fixed set of filenames.
- Inspect the existing application, its data conventions, and the
  plotting functions available in the local folder / Git submodule
  before implementing anything.
- Choose the plotting backend based on the existing code rather than
  introducing a new one by default.
- Select the displayed resolution based on the current viewport
  (visible time range) and plot width.
- Refresh the selected representation immediately after zooming or
  panning.
- Preserve missing-data gaps; do not connect lines across significant
  periods without observations.
- Use multitasking / parallel work where it provides a measurable
  benefit, while keeping the application stable under concurrent Shiny
  sessions.
- Benchmark performance against realistic data sizes, including
  millions of observations per dataset and larger where feasible.
2. Mandatory discovery phase
Before changing code, inspect the repository and report a concise
findings summary.
2.1 Inspect the application
Identify:
- The Shiny entry point(s), UI/server structure, and reactive data
  flow.
- Existing plotting code and the current plotting backend.
- Existing zoom, pan, brush, date-range, and plot-resize behavior.
- Current package dependencies and the project's supported R version.
- Existing caching, asynchronous processing, background jobs, and
  parallel-processing conventions.
- Current tests, test framework, and CI commands.
- How errors and user-facing notifications are handled.
- Whether datasets are loaded once per app process, once per session,
  or on demand.
Do not replace working application patterns without a clear reason.
2.2 Inspect existing data and plotting assets
Inspect data/, data_input/, and relevant local folders / Git
submodules. Determine what example data and reusable plotting functions
already exist. Check their documentation, callers, assumptions, and
tests.
The existing data description may be useful, but must not be treated as
a guarantee that all future input files share the same schema. Support
CSV and RDS inputs where feasible, and detect/report unsupported or
ambiguous formats clearly.
Record representative examples of:
- Timestamp column names and formats.
- Measurement/value columns.
- Variable, sensor, station, depth, or other grouping columns.
- Units and metadata.
- Time zone conventions.
- Missing values, duplicate timestamps, irregular sampling, and data
  gaps.
- File sizes, row counts, and approximate memory footprint.
Do not silently guess when a file cannot be interpreted unambiguously.
Provide an understandable error or ask the user to identify the relevant
columns.
2.3 Produce a short discovery report before implementation
Summarize:
1. Existing app architecture and plotting backend.
2. Available plotting helpers in the local folder / Git submodule.
3. Input schemas and likely edge cases.
4. Candidate storage approaches and benchmark plan.
5. Candidate multi-resolution algorithms and their trade-offs.
6. Proposed functions and integration points.
7. Decisions that require user approval.
Do not proceed to a final storage design or final
resolution-generation strategy until the alternatives have been compared
and the user has been asked to choose. The agent may build small
disposable benchmarks or prototypes to make that recommendation
evidence-based, but must not commit the application to one of those
designs before approval.
3. Design constraints and decision gates
Two architecture decisions are intentionally open. The implementing
agent must investigate, compare, recommend, and ask the user before
making the final choice.
3.1 Storage and caching decision --- approval required
Benchmark and compare suitable options, which may include:
- In-memory summaries and session/process-level caches.
- Persistent precomputed summaries on disk.
- A columnar format such as Parquet, if compatible with the current
  environment and dependencies.
- An existing database or storage mechanism already used by the app.
- A hybrid design with persistent summaries plus a bounded in-memory
  cache.
Compare at least:
- First-load and repeat-load latency.
- Preprocessing time.
- Memory use during preprocessing and plotting.
- Query latency for common viewport ranges.
- Disk space and write amplification.
- Concurrent-session behavior and cache contention.
- Invalidation when source files change.
- Incremental updates when new data arrives.
- Operational complexity, dependencies, and portability on the current
  Shiny Server.
Present measured results where practical. State the data sizes,
hardware/runtime environment, and benchmark method. Do not claim
benchmark results that were not measured.
Then ask the user to approve the recommended storage design before
integrating it as the final architecture. If the user has not answered,
stop at the decision gate rather than silently choosing.
3.2 Resolution-generation decision --- approval required
Compare and recommend an appropriate approach for the actual data and
app, including at least:
Option A --- Fixed time bins: precompute summaries for intervals
such as seconds/minutes/hours/days, chosen to fit the sampling cadence
and use case.
Option B --- Hierarchical multi-resolution pyramid: construct levels
by combining adjacent lower-level summaries, where the stored statistics
can be combined correctly.
Option C --- Hybrid: use time-aligned bins with hierarchical
aggregation or another method where it is demonstrably beneficial.
Evaluate:
- Accuracy of statistics at each level.
- Ability to preserve spikes and extrema.
- Behavior with irregular sampling and gaps.
- Alignment of bins across levels and viewport boundaries.
- Query efficiency for arbitrary time ranges.
- Storage cost and preprocessing cost.
- Suitability for multiple variables and different sampling
  frequencies.
- Handling of timezone and daylight-saving transitions.
- Ease of extending the scheme to new measurements or data types.
Ask the user to approve the recommended strategy before committing to
the final implementation. The recommendation must explain why it fits
the inspected datasets, not merely name a preferred algorithm.
4. Functional responsibilities
Keep responsibilities separated even if the first implementation remains
in one or a few R files. Follow the repository's naming and organization
conventions where possible. The names below are illustrative, not
mandatory public APIs.
4.1 Input discovery and loading
Suggested responsibilities: discover_data_files(),
read_timeseries_file()
- Discover supported CSV and RDS files in the configured input
  locations without hard-coding a single filename.
- Reuse existing loaders if they already meet these requirements.
- Read files defensively and return informative errors for corrupt,
  empty, unsupported, or ambiguous inputs.
- Avoid loading every large file into memory merely to populate a
  file-selection control.
- For CSV, use an efficient reader already installed or approved for
  the project; do not add dependencies without checking the current
  environment.
- For RDS, validate the loaded object type and required fields.
- Preserve source-file identity and enough metadata to trace a plotted
  observation back to its source.
- Do not modify or overwrite raw source files.
- Make encoding, decimal conventions, date parsing, and timezone
  handling explicit where needed.
4.2 Schema inspection and normalization
Suggested responsibility: normalize_timeseries()
Normalize supported input variants into an internal representation
suitable for plotting and aggregation. Do not require all source files
to use identical column names.
The normalized representation should identify, as applicable:
- Timestamp.
- Numeric measurement value.
- Variable / series identity.
- Optional grouping dimensions (for example sensor, station, depth, or
  channel).
- Unit and relevant metadata.
- Source identity.
Requirements:
- Detect timestamp and value candidates from data and metadata; ask
  the user to map columns if ambiguity remains.
- Parse timestamps consistently and preserve the intended timezone.
- Sort data by timestamp within each series.
- Handle NA, NaN, infinite values, duplicate timestamps, and
  non-numeric values deliberately.
- Do not silently aggregate duplicate timestamps during normalization;
  make that policy explicit and consistent with existing app
  semantics.
- Preserve irregular sampling rather than fabricating observations.
- Keep series separate: never aggregate values from different sensors,
  variables, units, or groups into the same bin unless the user
  explicitly requests that operation.
- Record diagnostics such as row count, valid count, time range,
  sampling interval estimates, duplicates, and missingness.
- Keep normalization deterministic and testable.
4.3 Data-quality and gap detection
Suggested responsibility: detect_timeseries_gaps()
- Estimate the expected sampling cadence robustly, or use known
  sampling metadata when available.
- Detect long gaps using a documented rule, configurable threshold, or
  existing domain-specific rule.
- Distinguish genuine missing periods from normal jitter in
  irregularly sampled data.
- Keep the gap-detection policy separate from aggregation so it can be
  tested and changed.
- Preserve gap boundaries in every derived resolution.
- Never draw a continuous line across a detected significant gap.
- If sampling cadence cannot be estimated reliably, use a conservative
  fallback and expose the limitation rather than inventing a cadence.
A bin with no observations must not be represented as a zero-valued
observation. Track observation count and coverage separately from the
measured value.
4.4 Preprocessing and summary generation
Suggested responsibilities: build_resolution_summaries(),
aggregate_timeseries_bin()
Once the resolution-generation strategy has been approved:
- Generate summaries per independent series and per approved time bin
  or hierarchy level.
- Store the time extent or bin boundaries, observation count, and
  statistics needed by the display.
- At minimum, evaluate mean and minimum/maximum. Consider median,
  quantiles, standard deviation, and data-coverage measures if the
  data and performance budget justify them.
- Preserve timestamps associated with minima and maxima where
  practical and scientifically useful.
- Ensure aggregation does not mix series or units.
- Ensure summary-combination operations are mathematically valid. For
  example, a global mean cannot generally be calculated as the
  unweighted mean of lower-level means when their observation counts
  differ. Store sufficient statistics (such as sum and count) when
  hierarchical merging is used.
- Keep raw observations available for the finest zoom level; summaries
  must not replace or destroy raw data.
- Avoid applying a smoothing filter by default. Aggregation and
  smoothing have different meanings. If smoothing is required by
  existing plot semantics, make it a separate, explicit operation and
  document its effects.
- Make the preprocessing deterministic and idempotent for an unchanged
  input and configuration.
- Allow the process to report progress and cancellation where the
  app's architecture supports it.
- Write outputs atomically so a failed job cannot leave a partially
  written summary that appears valid.
- Record source identity, preprocessing version, parameters, and
  timestamps needed to determine whether a summary is stale.
4.5 Resolution selection for the viewport
Suggested responsibility: select_plot_resolution()
Inputs should include, as applicable:
- Visible start and end timestamps.
- Effective plot width in pixels.
- Series identity and relevant grouping.
- Available precomputed levels.
- A configurable target number of rendered representations per
  horizontal pixel.
Behavior:
1. Validate the viewport and plot width.
2. Estimate the temporal span represented by each pixel.
3. Select the coarsest level that retains useful detail while keeping
   the rendered point count within a measured budget. A starting target
   such as roughly 1--4 representations per pixel may be benchmarked,
   not hard-coded as a universal truth.
4. Use a finer level as the viewport narrows.
5. Use raw points only when their count is reasonable for the viewport
   and rendering backend.
6. Include sufficient data immediately outside the viewport when
   required by line rendering, but do not allow this to create
   misleading continuity across gaps.
7. Handle ranges that are not aligned to bin boundaries, empty ranges,
   and viewports outside the available data.
8. Return the chosen level and a reason/diagnostic that can be logged
   or inspected during development.
Resolution selection should be predictable and avoid excessive switching
between adjacent levels during small pan/zoom changes. If hysteresis or
debounce is needed, justify it against the requirement for prompt
refresh.
4.6 Viewport query and data retrieval
Suggested responsibility: query_timeseries_viewport()
- Retrieve only the selected series, time range, and level required
  for the current view.
- Avoid rescanning or transferring the full raw dataset for each
  zoom/pan event.
- Return data in a consistent shape understood by the plotting layer.
- Preserve chronological ordering, gap markers, and source metadata
  needed for tooltips or diagnostics.
- Ensure statistics at partial edge bins are not presented as if they
  represented only the visible portion when they actually summarize a
  wider bin. Choose a documented edge-bin policy; use raw data or a
  finer level for edges when needed.
- Return a well-defined empty result for a valid range containing no
  observations.
- Keep query logic independent of the rendering backend.
4.7 Plot configuration and rendering
Suggested responsibilities: infer_plot_config(),
render_timeseries_plot()
- Inspect the existing plotting helpers in the local folder / Git
  submodule and reuse them where appropriate.
- Preserve the current plotting backend and established app
  conventions unless the discovery report identifies a concrete reason
  to change them.
- Infer sensible defaults from the selected data and its metadata:
  variable name, unit, time range, groups, labels, and applicable plot
  type.
- Do not assume every dataset is a single numeric time series. Follow
  the existing app's supported plot types and group semantics.
- Use the overview representation to display the central summary and
  measurement range. Prefer an envelope or another existing convention
  if it communicates the range more clearly than dashed boundary
  lines.
- At intermediate zoom, progressively expose finer summaries; at close
  zoom, display raw observations.
- Preserve gaps at all levels.
- Avoid connecting summaries from different bins in ways that imply
  unobserved continuity.
- Keep axis labels, units, legends, colors, and plot-specific
  conventions consistent with existing plots.
- Do not hard-code a configuration for one example dataset when the
  app is intended to handle different input files.
- Keep rendering separate from data loading, preprocessing, and
  resolution selection.
4.8 Shiny reactivity and zoom/pan updates
Integrate with the interaction mechanism already present in the
application.
- Detect changes to the visible range after zooming or panning.
- Determine the effective plot width if the backend exposes it;
  otherwise use a documented fallback and update it when the plot is
  resized.
- Select the appropriate resolution and refresh the plot promptly.
- Avoid reactive loops in which a server update triggers another
  viewport update indefinitely.
- Avoid rerunning normalization or preprocessing on every zoom event.
- Ignore stale asynchronous responses if a newer viewport request has
  superseded them.
- Keep the current viewport and selected series stable when updating
  the plot.
- Handle invalid or transient viewport events safely.
- Provide a loading indicator or equivalent feedback if a query takes
  long enough to be noticeable.
- Ensure errors in one plot/session do not break other sessions.
5. Multitasking, parallelism, and concurrency
The requirement to use multitasking must be implemented deliberately,
not by parallelizing every operation indiscriminately.
5.1 Identify safe parallel work
Evaluate opportunities such as:
- Preprocessing independent files or independent series in parallel.
- Computing independent summary levels in parallel if the approved
  algorithm permits it.
- Running expensive preprocessing in a background task rather than
  blocking the Shiny session.
- Serving independent viewport requests concurrently where the storage
  and cache design safely support it.
Do not parallelize operations that depend on the results of earlier
stages without respecting their dependencies.
5.2 Choose the mechanism based on the existing environment
Inspect the R version, Shiny Server configuration, installed packages,
OS, process model, and existing async conventions before selecting a
mechanism. Candidate approaches may include background R processes or
existing supported async/parallel facilities. Do not add a framework
solely because it is popular.
The agent must document the selected mechanism and explain:
- Why it is appropriate for the current Shiny Server deployment.
- How work is scheduled and bounded.
- How results and errors return to the session.
- How cancellation and stale results are handled.
- How resource limits prevent multiple users from exhausting CPU or
  RAM.
- How the design behaves on platforms where a chosen parallel backend
  is unavailable.
5.3 Concurrency safeguards
- Bound worker count and queued work; do not spawn an unbounded
  process per request.
- Avoid duplicating a huge in-memory dataset in every worker if that
  would exceed the memory budget.
- Avoid concurrent writes to the same cache or summary output without
  locking or atomic replacement.
- Deduplicate identical preprocessing jobs when possible.
- Make cache access safe under concurrent sessions.
- Avoid holding global locks during expensive calculations.
- Ensure a session closing does not corrupt shared work needed by
  other sessions.
- Define what happens when a user requests a new viewport before the
  previous request completes.
- Measure whether parallelism improves end-to-end latency; include
  overhead and memory use in the benchmark.
6. Cache invalidation and source changes
Regardless of the approved storage strategy:
- Detect changes to source files using a robust identity check
  appropriate to the chosen design (for example path plus size and
  modification time, or a content hash where justified).
- Include preprocessing version and relevant configuration in the
  cache identity.
- Invalidate or rebuild derived data when the source or relevant
  configuration changes.
- Do not treat a partially written source file as a completed dataset.
- Make cache cleanup and storage growth policies explicit.
- Avoid deleting user-supplied input files.
- Report stale, corrupt, or unavailable derived data and recover
  safely.
- Document whether new observations can be appended incrementally or
  require rebuilding summaries.
7. Performance and benchmarking
Benchmark before and after implementation. Do not optimize only for one
small sample dataset.
7.1 Test data sizes
Use representative real data where possible, and generate controlled
synthetic data for repeatable tests. Include at least:
- A small dataset to validate correctness.
- A dataset with millions of observations.
- A larger dataset that helps identify scaling limits, if the
  available environment permits it.
- Regular and irregular sampling.
- Missing periods and spikes.
- Multiple independent series.
- Duplicate timestamps and missing values.
- Data whose timestamps span daylight-saving transitions, if local
  civil time is used.
Record the actual row count, file size, number of series, sampling
characteristics, machine/runtime, and test method.
7.2 Measure
Measure at least:
- File discovery and load time.
- Normalization time.
- Preprocessing time and peak memory.
- Derived-storage size.
- Cold and warm viewport-query latency.
- Time from zoom/pan event to updated plot.
- Number of points/representations returned to the plot.
- Performance with multiple simultaneous sessions or jobs.
- Cache hit rate and invalidation cost, if caching is implemented.
- Cancellation/stale-result behavior for overlapping requests.
Use realistic viewport cases: full-range overview, medium range, narrow
range, empty range, and rapid consecutive zoom/pan events.
7.3 Performance acceptance criteria
Set numerical targets after measuring the current application and
representative data; do not invent a universal target without evidence.
The implementation report must state the targets, results, known
bottlenecks, and any remaining limitations.
At a minimum:
- A zoom/pan request must not reprocess the entire raw dataset.
- The rendered point count must be bounded by a configurable budget
  related to plot width, except where raw-data display is
  intentionally selected and feasible.
- The plot must remain responsive on the largest benchmark dataset the
  environment can support.
- Memory and worker usage must be bounded under concurrent sessions.
- Parallelism must show a measurable benefit for at least one targeted
  workload or be justified as necessary for non-blocking interaction.
8. Correctness and scientific requirements
The system must not make the plot look more precise than the stored
representation supports.
- A summary value represents the observations defined by its bin, not
  a continuous measurement over the entire bin.
- Preserve extrema and their times where feasible; do not rely on the
  mean alone to reveal spikes.
- Document whether minimum/maximum, quantiles, and other statistics
  are computed over valid observations only.
- Do not treat missing values as zero.
- Do not merge different series, units, sensors, or groups.
- Do not bridge significant data gaps.
- Do not silently discard outliers. Any filtering must be an explicit
  existing domain rule or user-visible option.
- Make bin boundaries and timezone rules deterministic.
- For irregular sampling, state whether summaries are
  observation-weighted or time-weighted. Default to
  observation-weighted statistics unless existing domain semantics or
  user approval require a different method.
- Do not imply that a range envelope is a confidence interval; label
  it as min/max or the relevant statistic.
- Ensure hierarchy merging preserves correct counts and sufficient
  statistics.
- Use the same source observations and configuration to produce
  repeatable results.
9. Error handling and user experience
Handle at least:
- Missing input directories.
- No supported files found.
- Unsupported or corrupt files.
- Ambiguous timestamp/value columns.
- Empty datasets.
- No valid timestamps or numeric values.
- Invalid time zones or unparseable timestamps.
- Non-positive plot width or invalid viewport range.
- No observations in the selected range.
- Preprocessing failure or cancellation.
- Storage full, unwritable output directory, or cache corruption.
- Worker failure or unavailable parallel backend.
- Source file changing while it is being read.
- A slow request being superseded by a newer viewport request.
Errors should be actionable and visible to the user where relevant,
while detailed diagnostics go to logs. Do not expose raw stack traces as
the normal user-facing message.
10. Testing requirements
Use the repository's existing testing framework where possible. Add
unit, integration, and performance tests.
Unit tests
Test:
- CSV and RDS loading.
- Schema inference and explicit column mapping.
- Timestamp parsing and timezone handling.
- Sorting and independent-series grouping.
- Missing values, duplicates, and irregular sampling.
- Gap detection and gap preservation across levels.
- Aggregation statistics and counts.
- Correct combination of summaries with unequal observation counts.
- Preservation of extrema timestamps where supported.
- Resolution selection at several viewport spans and widths.
- Partial edge-bin handling.
- Cache identity and invalidation.
- Error handling for malformed inputs.
Integration tests
Test:
- Existing plot types still work with the new data path.
- Zooming and panning trigger resolution changes.
- A narrower viewport selects equal or finer detail, as appropriate.
- Returning to a wider viewport selects an efficient coarser
  representation.
- The selected series, axis configuration, and current viewport remain
  stable.
- Overlapping asynchronous requests cannot overwrite a newer result
  with stale data.
- Multiple sessions can request data concurrently without cache
  corruption.
- Existing plotting functions are reused where appropriate.
Regression tests
- Compare summary statistics against direct calculations on small
  known datasets.
- Confirm no observations cross series boundaries during aggregation.
- Confirm missing intervals remain visually disconnected.
- Confirm spikes are not erased from the stored min/max
  representation.
- Confirm preprocessing does not modify raw files.
- Confirm the current app still starts and existing workflows continue
  to work.
11. Suggested implementation sequence
The agent should follow this sequence, with the two approval gates
respected.
1. Inspect and report: map the current application, datasets,
   plotting helpers, dependencies, tests, and deployment constraints.
2. Characterize data: inspect representative CSV/RDS files and
   document schemas, time behavior, gaps, and scale.
3. Prototype and benchmark alternatives: compare storage/caching
   and resolution-generation options using representative data.
4. Ask for decisions: present the recommended storage design and
   resolution strategy, trade-offs, and measurements. Do not finalize
   either decision without user approval.
5. Define internal interfaces: agree on function boundaries and
   data structures that fit the existing app.
6. Implement normalization and correctness primitives: loaders,
   schema handling, timestamp rules, series separation, and gap
   detection.
7. Implement the approved preprocessing design: create summaries
   while preserving raw data and recording provenance.
8. Implement viewport queries and resolution selection: keep these
   independent of rendering.
9. Integrate with existing plotting functions and Shiny
   interactions: use the existing backend and update on zoom/pan.
10. Add bounded multitasking: implement safe background/parallel
    work based on measured need and deployment compatibility.
11. Test and benchmark: run unit, integration, concurrency, and
    performance tests.
12. Report results: document changed files, function
    responsibilities, chosen algorithms, benchmark methodology/results,
    dependencies, configuration, known limitations, and how to operate
    or troubleshoot the feature.
Do not skip directly to plotting before the data and resolution
semantics are established.
12. Expected implementation deliverables
The completed work should include:
- Working integration in the existing Shiny app.
- Reusable, clearly separated functions for loading/normalization,
  preprocessing, resolution selection, viewport retrieval, and
  rendering.
- Reuse or documented reasons for not reusing the existing plotting
  helpers.
- Tests for correctness, integration, and key edge cases.
- Benchmarks and a short performance report.
- Documentation of configuration, storage/cache behavior, gap policy,
  aggregation semantics, concurrency limits, and recovery behavior.
- A list of changed files and any added dependencies.
- Explicit confirmation that the user approved the final storage and
  resolution-generation choices before they were committed.
13. Non-goals and cautions
- Do not build a standalone R package as part of this task.
- Do not replace the existing plotting backend merely to support this
  feature.
- Do not assume all input files share one schema.
- Do not choose a persistent storage technology or final resolution
  algorithm without first benchmarking alternatives and asking the
  user.
- Do not smooth raw measurements by default or discard raw
  observations.
- Do not parallelize without resource limits and concurrency tests.
- Do not hard-code performance claims or acceptance thresholds before
  measuring the existing deployment.
- Do not refactor unrelated parts of the application unless required
  for the feature.
14. Definition of done
The feature is complete when:
- Large time series can be explored without sending the full dataset
  to the renderer for every viewport change.
- Resolution is selected automatically from viewport range and plot
  width.
- Zooming and panning promptly update the visible detail.
- Overview plots communicate the central tendency and observed range
  without hiding important extremes.
- Raw measurements are available at sufficiently close zoom.
- Significant gaps remain visible and are not bridged by lines.
- Multiple variables/series remain separate and correctly labelled.
- The system handles CSV and RDS inputs supported by the app's schema
  rules.
- Multitasking is implemented safely and its benefit or necessity is
  documented.
- Tests pass and benchmark results are recorded.
- The implementation uses the existing plotting conventions where
  appropriate.
- The final storage and resolution-generation strategies were
  presented to and approved by the user before being finalized.
