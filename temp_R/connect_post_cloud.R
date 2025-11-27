# check our currently registered accounts
rsconnect::accounts()

# oops, let's add https://connect.posit.cloud/sawelch-niva
rsconnect::connectCloudUser() # opens the website with a one-time auth code
# check it went through properly
rsconnect::accounts()

# does it work now?
rsconnect::deployDoc("visualisation_quarto.qmd")
# No! Due to an issue with renv?

# ✔ Deploying "visualisation_quarto" using "server: connect.posit.cloud / username: sawelch-niva"
# ℹ Creating content on server...
# ✔ Created content with id "019ac4e4-3172-68a9-dbe9-fa7457d88570"
# ℹ Bundling 3 files: visualisation_quarto.qmd, _targets.R, and data/clean/literature_data.parquet
# ℹ Capturing R dependencies
# The following required packages are not installed:
# - geometries  [required by geojsonsf]
# - jsonify     [required by geojsonsf]
# - rapidjsonr  [required by geojsonsf]
# - sfheaders   [required by geojsonsf]
# Consider reinstalling these packages before snapshotting the lockfile.

# try again
rsconnect::deployDoc("visualisation_quarto.qmd")
# ah, stopeData
# fixed by installing from github
pak::pak("sawelch-NIVA/STOPeData")

# then we learned we should be uploading the HTML version instead
rsconnect::deployDoc("visualisation_quarto.html")
# worked, although we still got a warning
# ── Deployment complete ──────────────────────────────────────────────────────────────────────────────────────────────
# ✔ Successfully deployed to <https://connect.posit.cloud/sawelch-niva/content/019ac4eb-aaf0-0410-6fa5-51b9c472ff77>
# Warning message:
# In utils::tar(bundlePath, files = NULL, compression = "gzip", tar = tarImplementation) :
#   storing paths of more than 100 bytes is not portable:
#   ‘./visualisation_quarto_files/libs/quarto-html/quarto-syntax-highlighting-587c61ba64f3a5504c4d52d930310e48.css’
# >
