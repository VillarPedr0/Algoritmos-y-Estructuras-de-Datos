# Algoritmos y Estructuras de Datos
 ![Logo](https://www.famaf.unc.edu.ar/documents/3253/Logo-FAMAF_UNC-color-2.jpg)

# Prácticos Haskell

Guia de ejercicios resueltos de Haskell, provenientes de la materia Algoritmos y Estructuras de Datos de la carrera Ciencias de la Computación cursada en FAMAF.


## Ejecución de forma local

Clonar el repositorio

```bash
  git clone https://github.com/VillarPedr0/Algoritmos-y-Estructuras-de-Datos
```

Ir a la carpeta raíz

```bash
  cd src
```

Instalación de dependencias en Windows

```bash
  Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
```

Llamar a GHCI

```bash
  ghci
```

Correr el programa

```bash
  :l pro1.hs
```


## Ejemplos de Uso

```haskell
-- En consola ejecutar: (ejemplo)
primIguales [4,4,3,4,4,4,4,4,1]

-- Esto ejecutará la siguiente función
primIguales :: (Eq a) => [a] -> [a] 
primIguales [] = []
primIguales (x:xs) | (x == head xs) = x : primIguales xs 
                   | otherwise = x : []

```


## Autor

- [@VillarPedr0](https://github.com/VillarPedr0)
