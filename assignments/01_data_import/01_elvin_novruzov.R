# Assignment 1: Data Import
# Instructor: Filip Strnad
# Student: Elvin Novruzov
# Date: March 9, 2025

# 1️⃣ Define URLs and File Paths
zip_url <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"
zip_file <- "dataset.zip"
data_dir <- "data"

# 2️⃣ Download ZIP File (If Not Exists)
if (!file.exists(zip_file)) {
  cat("📥 Downloading ZIP file...\n")
  download.file(zip_url, zip_file, mode = "wb", timeout = 600)
} else {
  cat("✅ ZIP file already exists. Skipping download...\n")
}

# 3️⃣ Unzip the File
cat("📂 Extracting ZIP file...\n")
unzip(zip_file, exdir = data_dir)

# 4️⃣ Filter Files (Only Model Output Files)
cat("🔎 Filtering files...\n")
files <- list.files(data_dir, full.names = TRUE)  # Get all extracted files
model_files <- files[grepl("output|model", files, ignore.case = TRUE)]  # Select only model output files

# 5️⃣ Read and Merge Data
cat("📊 Reading and merging data...\n")
data_list <- lapply(model_files, read.csv)  # Read each file
final_data <- do.call(rbind, data_list)  # Merge all into one dataset

# 6️⃣ Display the Final Data
cat("✅ Process Completed! First 6 rows:\n")
print(head(final_data))

# 7️⃣ (Optional) Save the Merged Dataset
write.csv(final_data, "final_dataset.csv", row.names = FALSE)
cat("💾 Merged dataset saved as 'final_dataset.csv'!\n")
