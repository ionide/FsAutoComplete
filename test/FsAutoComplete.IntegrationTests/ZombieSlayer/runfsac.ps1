param (
  [string] $FsacExePath
)

Write-Host "Host process: $pid"
Write-Host "$FsacExePath --hostPID=$pid --verbose"
& "$FsacExePath" @("--verbose", "--hostPID=$pid")
