# Comparador NLP de Planes de Gobierno (Perú)

Pipeline reproducible en R para extraer propuestas, clasificarlas y generar un sitio Quarto con análisis comparativo.

## Estructura

- `data/plans_txt/`
- `outputs/`
- `scripts/`
- `site/`

## Requisitos

- R >= 4.2
- Quarto CLI instalado
- Internet para instalar paquetes CRAN en la primera ejecución

## Ejecución reproducible

```bash
Rscript scripts/run_all.R
```

Ese comando:

1. Instala/valida dependencias CRAN.
2. Intenta copiar `.txt` desde la ruta fuente absoluta al directorio `data/plans_txt/`.
3. Procesa solo planes completos (excluye `*resumen.txt`).
4. Genera datasets en `outputs/`.
5. Ejecuta validación de extracción + clasificación supervisada.
6. Ejecuta diagnóstico avanzado: duplicados, contradicciones internas y viabilidad fiscal.
7. Ejecuta cobertura poblacional/territorial + red de similitud por ejes.
8. Ejecuta benchmark externo + brechas + tablero KPI ex-post.
9. Ejecuta bloque BART para explicar score de implementabilidad.
10. Renderiza el sitio Quarto en `site/_site/`.
11. Actualiza `renv.lock`.

## Outputs principales

- `outputs/propuestas.parquet`
- `outputs/propuestas.csv`
- `outputs/party_axis_scores.parquet`
- `outputs/topics_stm.parquet`
- `outputs/embeddings.parquet`
- `outputs/umap_coords.parquet`
- `outputs/similarity_party.parquet`
- `outputs/verification_summary.csv`
- `outputs/duplicates_interparty.parquet`
- `outputs/duplicate_clusters.parquet`
- `outputs/contradictions_party.parquet`
- `outputs/fiscal_viability.parquet`
- `outputs/fiscal_viability_party.parquet`
- `outputs/diagnostics_block_summary.csv`
- `outputs/coverage_population.parquet`
- `outputs/coverage_territory.parquet`
- `outputs/coverage_party_summary.parquet`
- `outputs/coverage_blindspots_party.parquet`
- `outputs/axis_similarity_edges.parquet`
- `outputs/axis_similarity_nodes.parquet`
- `outputs/axis_similarity_summary.parquet`
- `outputs/coverage_similarity_summary.csv`
- `outputs/benchmark_indicator_reference.parquet`
- `outputs/benchmark_gap_proposal.parquet`
- `outputs/benchmark_gap_party_axis.parquet`
- `outputs/benchmark_gap_indicator_party.parquet`
- `outputs/kpi_tracker_proposal.parquet`
- `outputs/kpi_dashboard_party.parquet`
- `outputs/kpi_dashboard_axis.parquet`
- `outputs/kpi_tracking_template_2026_2031.csv`
- `outputs/benchmark_kpi_summary.csv`
- `outputs/bart_metrics.csv`
- `outputs/bart_predictions.parquet`
- `outputs/bart_variable_importance.parquet`
- `outputs/bart_partial_dependence.parquet`
- `outputs/bart_party_diagnostics.parquet`
- `outputs/bart_summary.csv`

## Outputs de validación y supervisado (v1)

- `outputs/annotation/annotation_sample_v1.csv`
- `outputs/annotation/annotation_gold_v1.csv`
- `outputs/annotation/annotation_instructions.md`
- `outputs/extraction_metrics_v1.csv`
- `outputs/extraction_confusion_v1.csv`
- `outputs/axis_model_metrics_v1.csv`
- `outputs/axis_class_metrics_v1.csv`
- `outputs/instrument_model_metrics_v1.csv`
- `outputs/instrument_class_metrics_v1.csv`
- `outputs/propuestas_supervised.parquet`
- `outputs/propuestas_supervised.csv`

Notas:

- Si `annotation_gold_v1.csv` aún no tiene etiquetas manuales, `extraction_metrics_v1.csv` se calcula en modo `seed_proxy`.
- Cuando se complete la anotación manual (`is_proposal_gold`, `axis_gold`, `instrument_gold`), al reejecutar `Rscript scripts/run_all.R` se recalculan métricas con `metric_source = manual_gold`.

## Supuestos aplicados en esta corrida

- La ruta absoluta indicada (`.../Planes`) no contenía archivos `.txt` al momento de ejecutar.
- Se trabajó con los `.txt` ya disponibles en `data/plans_txt/`.
- Se excluyeron todos los archivos terminados en `resumen.txt`.
- `OPENAI_API_KEY` no estuvo disponible, por lo que se usó fallback `tf-idf + SVD` para embeddings.

## Verificación rápida

Después de ejecutar:

- Revisar `outputs/verification_summary.csv`
- Ver sitio en `site/_site/index.html`

## Publicación en GitHub Pages

Este proyecto está preparado para publicar el sitio estático desde `docs/` en la rama `main`.

1. Actualiza la web publicada:

```bash
./scripts/update_docs.sh
```

2. Haz commit y push de `docs/` + código fuente.
3. En GitHub: `Settings` -> `Pages` -> `Build and deployment`:
   `Source = Deploy from a branch`, `Branch = main`, `Folder = /docs`.
4. Espera 1-3 minutos y abre la URL de Pages del repositorio.
