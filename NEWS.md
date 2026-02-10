# stepinWearable 2.0.1

- Cadence step progression has been incremented from 100 steps/min up to 120 steps/min.
- Month 4 start has been corrected to t = 5, ensuring the first cadence goal is issued at this point (t = 5).
- Intervention messages have been updated for clarity and consistency.
- Added a Pull Request template to standardize contributions and ensure code quality.
- Implemented GitHub Branch Protection rules and `CODEOWNERS` to formalize the review process.
- Added Issue templates for bug reports and feature requests.

# stepinWearable 2.0.0

- **Assessment module** developed:
    - Tools to preprocess, check, and build sleep logs.
    - Integration of sleep-related data streams into the package ecosystem.
    - Ensures participant IDs are properly handled and validated during sleep-log processing.
- Required valid-day threshold (Fitbit) changed to 480 minutes (8h)
- KPI calculations updated, now includes n_days_alldays.
- Minor fixes:
    - Removed a try block that previously masked write-log failures.
    - Ensures missing participant IDs cause explicit errors instead of silent failures.
    - Corrected index overwrite issues when t index = 0.
    - Cleaned up step-processing and nonwear logic.
    - Improved reliability of file handling and log generation
    - Updated documentation of functions.

# stepinWearable 1.0.0

- First complete, stable implementation of the **intervention module**:
    - Rounded daily step targets to 10s for step counts and 5-min for cadence.
    - Implemented consecutive success logic with corresponding message pipeline.
    - Improved participant summary state generation.
    - Fitbit: Refined definition of valid vs. nonvalid days, including nonwear detection based on heart rate.
- Final refinements to the user-facing application prior to deployment.
- Updated vignette to include support for the nonwear detection algorithm.
- Internal polishing and clean-up for release.

### Notes
- This version corresponds to the initial deployed version of the package used in the field phase of the Step-IN project.

# stepinWearable 0.1.0

Initial release of the `stepinWearable` package.

### Highlights
- Added **Intervention module** with full functionality for:
  - Processing and visualizing Fitbit step data.
  - Managing participant intervention states (targets, achievements, fails).
  - Generating personalized step challenges based on configurable rules.
- Added initial structure for the **Assessment module** (under development).
- Implemented documentation, test coverage, and continuous integration setup.
- Licensed under **GPL-3.0**.

### Notes
- This version marks the first stable, functional iteration of the package.
- Future releases will expand the Assessment module and add new visualization and analysis tools.
