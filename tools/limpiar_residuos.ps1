<#
.SYNOPSIS
Elimina residuos de escritorio recreados dentro del repositorio.

.DESCRIPTION
Busca únicamente archivos llamados desktop.ini o .DS_Store, verifica que su
ruta resuelta permanezca dentro del repositorio y los elimina. Con -SoloGit
restringe la limpieza al directorio interno .git. Admite -WhatIf.

.EXAMPLE
powershell -ExecutionPolicy Bypass -File tools/limpiar_residuos.ps1 -WhatIf

.EXAMPLE
powershell -ExecutionPolicy Bypass -File tools/limpiar_residuos.ps1
#>
[CmdletBinding(SupportsShouldProcess = $true)]
param(
    [Parameter()]
    [string]$RutaRepositorio,

    [Parameter()]
    [switch]$SoloGit
)

if ([string]::IsNullOrWhiteSpace($RutaRepositorio)) {
    $directorioScript = Split-Path -Parent $MyInvocation.MyCommand.Path
    $RutaRepositorio = Split-Path -Parent $directorioScript
}

$raiz = (Resolve-Path -LiteralPath $RutaRepositorio -ErrorAction Stop).Path
$prefijo = $raiz.TrimEnd([IO.Path]::DirectorySeparatorChar) +
    [IO.Path]::DirectorySeparatorChar
$raizBusqueda = if ($SoloGit) {
    Join-Path $raiz '.git'
} else {
    $raiz
}
if (-not (Test-Path -LiteralPath $raizBusqueda -PathType Container)) {
    throw "No existe el directorio de búsqueda: $raizBusqueda"
}
$raizBusqueda = (Resolve-Path -LiteralPath $raizBusqueda -ErrorAction Stop).Path
$prefijoBusqueda = $raizBusqueda.TrimEnd([IO.Path]::DirectorySeparatorChar) +
    [IO.Path]::DirectorySeparatorChar

$residuos = @(
    Get-ChildItem -LiteralPath $raizBusqueda -Recurse -Force -File |
        Where-Object { $_.Name -in @('desktop.ini', '.DS_Store') }
)

$eliminados = 0
foreach ($archivo in $residuos) {
    $ruta = [IO.Path]::GetFullPath($archivo.FullName)
    if (-not $ruta.StartsWith($prefijo, [StringComparison]::OrdinalIgnoreCase)) {
        throw "La ruta queda fuera del repositorio: $ruta"
    }
    if (-not $ruta.StartsWith($prefijoBusqueda, [StringComparison]::OrdinalIgnoreCase)) {
        throw "La ruta queda fuera del alcance de búsqueda: $ruta"
    }

    if ($PSCmdlet.ShouldProcess($ruta, 'Eliminar residuo del sistema')) {
        Remove-Item -LiteralPath $ruta -Force
        $eliminados++
    }
}

[pscustomobject]@{
    repositorio = $raiz
    alcance = $raizBusqueda
    encontrados = $residuos.Count
    eliminados = $eliminados
}
