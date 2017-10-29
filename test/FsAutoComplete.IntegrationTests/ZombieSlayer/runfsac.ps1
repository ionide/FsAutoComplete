param (
  [string] $FsacExePath,
  [switch] $UseDotnet = $false  
)

Write-Host "Host process: $pid"
Write-Host "$FsacExePath --hostPID=$pid --verbose"

if ($UseDotnet) {

& "dotnet" @($FsacExePath, "--verbose", "--hostPID=$pid")

} else {

& "$FsacExePath" @("--verbose", "--hostPID=$pid")

}
