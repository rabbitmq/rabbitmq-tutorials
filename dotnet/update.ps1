$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 'Latest' -ErrorAction 'Stop' -Verbose

Get-Childitem -Recurse '*.csproj' | ForEach-Object {
    & dotnet add $_ package --version '6.0.0-rc.3' RabbitMQ.Client
}
