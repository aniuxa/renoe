<#
.SYNOPSIS
Elimina residuos de escritorio recreados dentro del repositorio.

.DESCRIPTION
Busca únicamente archivos llamados desktop.ini o .DS_Store, verifica que su
ruta resuelta permanezca dentro del repositorio y los elimina. Admite -WhatIf.

.EXAMPLE
powershell -ExecutionPolicy Bypass -File tools/limpiar_residuos.ps1 -WhatIf

.EXAMPLE
powershell -ExecutionPolicy Bypass -File tools/limpiar_residuos.ps1
#>
[CmdletBinding(SupportsShouldProcess = $true)]
param(
    [Parameter()]
    [string]$RutaRepositorio
)

if ([string]::IsNullOrWhiteSpace($RutaRepositorio)) {
    $directorioScript = Split-Path -Parent $MyInvocation.MyCommand.Path
    $RutaRepositorio = Split-Path -Parent $directorioScript
}

$raiz = (Resolve-Path -LiteralPath $RutaRepositorio -ErrorAction Stop).Path
$prefijo = $raiz.TrimEnd([IO.Path]::DirectorySeparatorChar) +
    [IO.Path]::DirectorySeparatorChar

$residuos = @(
    Get-ChildItem -LiteralPath $raiz -Recurse -Force -File |
        Where-Object { $_.Name -in @('desktop.ini', '.DS_Store') }
)

$eliminados = 0
foreach ($archivo in $residuos) {
    $ruta = [IO.Path]::GetFullPath($archivo.FullName)
    if (-not $ruta.StartsWith($prefijo, [StringComparison]::OrdinalIgnoreCase)) {
        throw "La ruta queda fuera del repositorio: $ruta"
    }

    if ($PSCmdlet.ShouldProcess($ruta, 'Eliminar residuo del sistema')) {
        Remove-Item -LiteralPath $ruta -Force
        $eliminados++
    }
}

[pscustomobject]@{
    repositorio = $raiz
    encontrados = $residuos.Count
    eliminados = $eliminados
}
