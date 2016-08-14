# Install F# 4.0 on Windows Server for Appveyor CI 
 $webclient = New-Object Net.WebClient
 $url = 'http://download.microsoft.com/download/9/1/2/9122D406-F1E3-4880-A66D-D6C65E8B1545/FSharp_Bundle.exe'
 $webclient.DownloadFile($url, "$pwd\FSharp_Bundle.exe")
 .\FSharp_Bundle.exe /install /quiet
