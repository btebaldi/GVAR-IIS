# Prompt the user for the destination directory
# This will ask the user to input the name of the destination directory
$destinationDirectory = Read-Host -Prompt "Please enter the destination directory"

# Combine the base path ".\mat_files\Result_Matrix" with the user-provided directory name
# This creates the full path for the destination directory
$destinationDirectory = Join-Path -Path ".\mat_files\Result_Matrix" -ChildPath $destinationDirectory 

# Check if the destination directory exists
# If it doesn't exist, create it
if (-not (Test-Path -Path $destinationDirectory)) {
    Write-Output "Destination directory does not exist. Creating it..."
    # Create the directory and suppress output using Out-Null
    New-Item -Path $destinationDirectory -ItemType Directory -Force | Out-Null
}

# Initialize an empty array to store the list of files that need to be moved
$filesToMove = @()

# Define the list of file paths to check and potentially move
# These are the files that the script will look for in the current directory structure
$fileNames = @(
    ".\mat_files\Result_Matrix\mGy_inv_X_mC.mat", 
    ".\mat_files\Result_Matrix\mGy_inv_X_mGyL1.mat", 
    ".\mat_files\Result_Matrix\mGy_inv_X_mGyL2.mat", 
    ".\mat_files\Result_Matrix\mGy_inv_X_mGyL3.mat", 
    ".\mat_files\Result_Matrix\mGy_inv_X_mGyL4.mat", 
    ".\mat_files\Result_Matrix\mGy_inv_X_mGyL5.mat", 
    ".\mat_files\Result_Matrix\mGy_inv_X_mGyL6.mat", 
    ".\mat_files\Result_Matrix\mGy_inv_X_mGyL7.mat", 
    ".\mat_files\Result_Matrix\mGy_inv_X_mGyL8.mat", 
    ".\mat_files\Result_Matrix\mGy_inv_X_mL.mat", 
    ".\mat_files\Result_Matrix\mGy_inv.mat", 
    ".\mat_files\Result_Matrix\mGy.mat",
    ".\02_Check Weak Exo.out",
    ".\Gvar_Passo1_v4.out",
    ".\Gvar_Passo2-Macrovariaveis.out",
    ".\Gvar_Passo3_v3.out",
    ".\Gvar_Passo4_v3.out"
)

# Iterate through each file in the list
foreach ($fileName in $fileNames) {
    # Check if the file exists at the specified path
    if (Test-Path -Path $fileName) {
        # If the file exists, add it to the list of files to move
        $filesToMove += $fileName
    }
    else {
        # If the file does not exist, output a message indicating it was not found
        Write-Output "File not found: $fileName"
    }
}

# Check if there are any files to move
if ($filesToMove.Count -eq 0) {
    # If no files were found, output a message and exit
    Write-Output "No files were found."
}
else {
    # If there are files to move, iterate through each file
    foreach ($file in $filesToMove) {
        # Construct the destination path for the file
        # This combines the destination directory with the file name
        $destinationPath = Join-Path -Path $destinationDirectory -ChildPath (Split-Path -Path $file -Leaf)
        try {
            # Attempt to move the file to the destination path
            Move-Item -Path $file -Destination $destinationPath -Force
            # Output a success message
            Write-Output "Moved: $file to $destinationPath"
        }
        catch {
            # If an error occurs during the move operation, output an error message
            Write-Output "Error moving $file : $_"
        }
    }
}

# Output a message indicating the operation is complete
Write-Output "Operation completed."
