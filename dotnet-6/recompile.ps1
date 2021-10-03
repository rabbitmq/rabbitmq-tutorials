$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 'Latest' -ErrorAction 'Stop' -Verbose

Get-Childitem -Recurse '*.csproj' | ForEach-Object {
    # & dotnet build $_.FullName
    & dotnet build $_
}
