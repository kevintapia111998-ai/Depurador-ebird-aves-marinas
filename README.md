# Depurador interactivo de datos eBird: aves marinas de Ecuador

Herramienta en R (app Shiny + funciones reutilizables) para **depurar y validar registros de aves marinas** del *eBird Basic Dataset* (EBD) en las provincias costeras de Ecuador y Galápagos. Incluye un flujo de revisión manual por voluntarios con historial de registros eliminados.

**Autor:** Kevin Tapia-Rodriguez, Instituto Público de Investigación de Acuicultura y Pesca (IPIAP), Ecuador

Este repositorio también se usa como entrega de las actividades del curso *Optimización de la gestión de ecosistemas marinos y recursos pesqueros en Latinoamérica mediante ciencia abierta* (OceanHackWeek en español, edición 2026).

---

## Estructura del repositorio

```
depurador-ebird-aves-marinas/
├── app.R                          # Interfaz Shiny (Fase 1 y Fase 2)
├── R/
│   └── funciones_depuracion.R     # Lógica de depuración y configuración
├── notebooks/
│   └── 01_exploracion_ebird.ipynb # Actividad 1: lectura y exploración de datos
├── data/
│   ├── muestra/
│   │   └── ebd_muestra_esmeraldas.csv   # Muestra anonimizada (102 registros)
│   └── (aquí van tus archivos EBD; no se suben a GitHub)
├── LICENSE
└── README.md
```

## Datos

### La base completa no se incluye

Los datos de eBird **no se redistribuyen en este repositorio** porque los [Términos de uso de eBird](https://www.birds.cornell.edu/home/ebird-api-terms-of-use/) no lo permiten. Para usar la herramienta con datos completos:

1. Crea una cuenta en [eBird](https://ebird.org).
2. Solicita el acceso al EBD en **[ebird.org/data/download](https://ebird.org/data/download)**, indicando el propósito de uso. La aprobación puede tardar algunos días.
3. Descarga el EBD filtrado por región (p. ej. `Ecuador > Esmeraldas`) y rango de fechas.
4. Descomprime el archivo `ebd_*.txt` dentro de la carpeta `data/`. El `.gitignore` evita que se suba por error.

### Muestra incluida

`data/muestra/ebd_muestra_esmeraldas.csv` es un extracto pequeño con fines **demostrativos**: 102 registros de 6 listas completas de Esmeraldas (2021–2026), tomados de la versión `relAug-2026` del EBD. Incluye las cinco especies focales y distintos protocolos de observación.

**Privacidad de los observadores:** en la muestra se **vació el contenido** de las columnas `OBSERVER ID`, `OBSERVER ORCID ID`, `CHECKLIST COMMENTS` y `SPECIES COMMENTS`. Por eso, con la muestra no se activa la alerta de "Duda en comentarios".

### Cita de los datos

> eBird Basic Dataset. Version: EBD_relAug-2026. Cornell Lab of Ornithology, Ithaca, New York. Aug 2026.

## Cómo funciona

### Fase 1: Depuración inicial

A partir de uno o varios archivos EBD de una provincia:

1. **Valida la estructura:** columnas requeridas y coincidencia entre la provincia elegida y el archivo.
2. **Elimina duplicados** por `GLOBAL UNIQUE IDENTIFIER`.
3. **Aplica filtros de esfuerzo** a listas no estructuradas: excluye los protocolos *Incidental*, *Historical* y *Banding*, y exige duración ≤ 360 min, distancia ≤ 10 km, ≤ 10 observadores y velocidad ≤ 100 km/h.
4. **Marca alertas para revisión manual** en `REVISION_MANUAL`:
   - coordenadas 0,0, puntos que caen en Galápagos o fuera de la provincia;
   - conteos "X", códigos de especie exótica y dudas en los comentarios;
   - listas incompletas y distancia 0 en recorridos o pelágicos.
5. **Unifica** las subespecies y filas repetidas por especie y lista (si algún conteo es "X", el total queda como "X").
6. Genera:
   - **Producto 1:** base completa para revisión;
   - **Producto 2:** solo las especies focales;
   - **Bitácora:** resumen de registros.

   Ambos productos incluyen las columnas vacías `REVISION` y `COMENTARIO_E` para los voluntarios.

**Especies focales:** *Sula variegata*, *Sula nebouxii*, *Leucocarbo bougainvilliorum*, *Larosterna inca*, *Ardenna grisea*.

### Fase 2: Validación post-revisión

Los voluntarios llenan `REVISION` con **R** (revisar), **E** (eliminar) o **V** (verificado). La app:

- separa los registros **E** y los acumula con el historial de eliminados anterior, si se sube;
- cambia **R → V** y genera la **base verificada final**.

## Uso

### Requisitos

R ≥ 4.1 y los paquetes:

```r
install.packages(c("shiny", "tidyverse"))
```

### Abrir la app

Desde la carpeta del repositorio:

```r
shiny::runApp()
```

### Usar las funciones sin la app

```r
source("R/funciones_depuracion.R")

ebd <- leer_ebd("data/ebd_EC-E_202001_202608_smp_relAug-2026.txt")
res <- depurar_ebd(ebd, provincia = "Esmeraldas")

res$bitacora
write_excel_csv(res$producto1, "producto1.csv", na = "")
```

Para probarlas con la muestra:

```r
muestra <- read_csv("data/muestra/ebd_muestra_esmeraldas.csv",
                    col_types = cols(.default = "c"))
depurar_ebd(muestra, provincia = "Esmeraldas")$bitacora
```

### Personalizar

Los límites de las provincias, las especies focales y los umbrales de esfuerzo están en `config_depuracion()`, dentro de `R/funciones_depuracion.R`.

## Licencia

El **código** se distribuye bajo licencia MIT (ver [LICENSE](LICENSE)). Los **datos de eBird** no están cubiertos por esta licencia y se rigen por los términos de uso del Cornell Lab of Ornithology.
