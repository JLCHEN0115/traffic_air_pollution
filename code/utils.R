# ==============================================================================
# PEMS data unpacking
# ==============================================================================
library(R.utils)

# the PEMS data are downloaded as compressed
# The following code extract the data and delete the compressed files

 source_dir <- here::here("data-raw", "census_VC")
 
 destination_dir <- source_dir
 
 if (!dir.exists(destination_dir)) {
   dir.create(destination_dir, recursive = TRUE)
   print(paste("Created directory:", destination_path))
 }
 
 # This finds all files in your folder that end with ".zip"
 gz_files <- list.files(path = source_dir, 
                         pattern = "\\.gz$", 
                         full.names = TRUE)
 
 
 if (length(gz_files) == 0) {
   stop(paste("No .gz files found in:", source_dir, 
              "\nCheck your 'my_folder_path' and file extensions."))
 } else {
   print(paste("Found", length(gz_files), "gz files to extract."))
 }
 
 for (file_path in gz_files) {
   
   # Print a message to show progress
   print(paste("Decompressing:", basename(file_path)))
   
   # Decompress the file.
   # The 'destname' is the original file without the .gz
   # 'remove = TRUE' automatically deletes the original .gz file 
   # after it is successfully decompressed.
   gunzip(file_path, remove = TRUE)
   
   print(paste("Decompressed and removed:", basename(file_path)))
 }
 
 # This finds all files in your folder that end with ".txt"
 txt_files <- list.files(path = destination_dir, 
                        pattern = "\\.txt$", 
                        full.names = TRUE)
 
 print("---")
 print("Decompression complete!")
 print(paste0(length(txt_files), " out of ", length(gz_files), " files decompressed."))
 print("Old gz files are deleted.")
