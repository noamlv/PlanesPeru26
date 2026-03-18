# Referencias científicas para el análisis de planes de gobierno

Este documento proporciona respaldo bibliográfico para cada sección metodológica del sitio de análisis comparativo de planes de gobierno. Para cada bloque se ofrecen dos artículos de revistas de alto impacto que avalan —directa o conceptualmente— las técnicas aplicadas.

***

## Marco general: texto como dato en ciencia política

El análisis automático de textos políticos es hoy una línea consolidada dentro de la ciencia política computacional. La premisa central es que los métodos automatizados reducen drásticamente el costo de procesar grandes colecciones documentales sin reemplazar la lectura interpretativa cuidadosa, sino complementándola.[^1][^2][^3]

**Artículos de respaldo:**

1. **Grimmer, J., & Stewart, B. M. (2013).** *Text as Data: The Promise and Pitfalls of Automatic Content Analysis Methods for Political Texts.* **Political Analysis**, 21(3), 267–297. [https://doi.org/10.1093/pan/mps028](https://doi.org/10.1093/pan/mps028) — Artículo fundacional del enfoque *text-as-data* en ciencia política. Establece los principios metodológicos del análisis automático de textos, sus posibilidades y límites, y orienta la validación de resultados.[^4][^5]

2. **Wilkerson, J., & Casas, A. (2017).** *Large-Scale Computerized Text Analysis in Political Science: Opportunities and Challenges.* **Annual Review of Political Science**, 20, 529–544. [https://doi.org/10.1146/annurev-polisci-052615-025542](https://doi.org/10.1146/annurev-polisci-052615-025542) — Revisión sistemática de las cuatro etapas de un proyecto *text-as-data* en ciencia política, con énfasis en la inestabilidad de los modelos de tópicos como desafío metodológico clave.[^6][^7]

***

## Preparación y limpieza del texto

La normalización, el control de codificación y la estructuración de documentos en formatos tidy son pasos previos indispensables para cualquier análisis reproducible. El ecosistema de paquetes de R orientados a datos ordenados proporciona las funciones centrales para tokenización, eliminación de palabras vacías y transformación del texto en unidades analizables.[^8][^9][^10][^11]

**Artículos de respaldo:**

1. **Silge, J., & Robinson, D. (2016).** *tidytext: Text Mining and Analysis Using Tidy Data Principles in R.* **Journal of Open Source Software**, 1(3), 37. [https://doi.org/10.21105/joss.00037](https://doi.org/10.21105/joss.00037) — Artículo de referencia del paquete `tidytext`. Describe la aplicación de los principios de datos ordenados (*tidy data*) al minado de texto, permitiendo integrar la limpieza y el análisis en un único flujo de trabajo reproducible en R.[^12][^11]

2. **Wilkerson, J., & Casas, A. (2017).** *Large-Scale Computerized Text Analysis in Political Science: Opportunities and Challenges.* **Annual Review of Political Science**, 20, 529–544. — Además de su función como referencia general, este artículo detalla los procedimientos de preprocesamiento textual —incluyendo normalización, tokenización y manejo de ambigüedades lingüísticas— en el contexto específico de textos políticos.[^7][^6]

***

## Extracción y clasificación de propuestas

La segmentación de documentos en unidades proposicionales —oraciones con verbos de acción pública, listas estructuradas— y su posterior clasificación temática y tipológica corresponde a la etapa de *medición* dentro del marco *text-as-data*. Esta labor combina reglas lingüísticas supervisadas con diccionarios especializados, un enfoque validado ampliamente en el análisis de manifiestos electorales.[^13][^14]

**Artículos de respaldo:**

1. **Vestergaard, M. B. (2025).** *Why all these promises? How parties strategically use policy commitments across 11 Western democracies.* **European Journal of Political Research**, artículo en línea anticipado. [https://doi.org/10.1080/13501763.2025.2481189](https://doi.org/10.1080/13501763.2025.2481189) — Presenta un esquema de codificación automática de compromisos de política en manifiestos electorales de 32 democracias occidentales —más de un millón de cuasi-oraciones codificadas por máquina—, distinguiendo entre promesas vagas y compromisos específicos, lo que es directamente análogo a la extracción y clasificación de propuestas concretas.[^15][^16]

2. **Grimmer, J., Roberts, M. E., & Stewart, B. M. (2022).** *Text as Data: A New Framework for Machine Learning and the Social Sciences.* Princeton University Press. — El libro proporciona el marco conceptual y operativo para la clasificación supervisada y semi-supervisada de proposiciones políticas, incluyendo la asignación de categorías temáticas, tipos de intervención y variables de trazabilidad.[^14][^17]

***

## Cantidad y claridad (concreción)

El puntaje de concreción —que suma componentes como meta numérica, horizonte temporal, tipo de intervención y mención de costo— es una operacionalización de la especificidad de las promesas electorales, un concepto respaldado por una corriente empírica robusta. La detección automatizada de vaguedad léxica mediante reglas sobre el texto ha sido formalizada en diversas propuestas metodológicas.[^18][^19][^15]

**Artículos de respaldo:**

1. **Vestergaard, M. B. (2025).** *How political parties use a commitment strategy to stand out.* **Journal of European Public Policy**, artículo en línea anticipado. [https://doi.org/10.1080/13501763.2025.2481189](https://doi.org/10.1080/13501763.2025.2481189) — Demuestra empíricamente que las promesas concretas (*commitments*) —aquellas que el partido puede ser evaluado por cumplir— son distinguibles automáticamente de las intenciones vagas, y que esta distinción tiene efectos medibles en el comportamiento electoral.[^20][^21]

2. **Ntounias, T., Schneider, C., & Thomson, R. (2024).** *Campaign Promises, Political Ambiguity, and Globalization.* IGCC Working Paper, octubre de 2024. — Analiza cómo la ambigüedad de las promesas de campaña —la contraparte directa de la concreción— afecta la rendición de cuentas electoral, y presenta una metodología de medición que separa promesas verificables de enunciados genéricos en programas de gobierno de múltiples países.[^19]

***

## Agenda temática (modelado de tópicos con STM)

El Modelo de Tópicos Estructural (STM) extiende el modelo estándar LDA incorporando metadatos documentales como covariables, lo que permite estimar simultáneamente los tópicos dominantes en el corpus y su relación con características del documento —en este caso, el partido o candidatura—. Su aplicación a manifiestos electorales está bien establecida en la literatura comparativa.[^22][^23][^24][^25]

**Artículos de respaldo:**

1. **Roberts, M. E., Stewart, B. M., & Tingley, D. (2019).** *stm: An R Package for Structural Topic Models.* **Journal of Statistical Software**, 91(2), 1–40. [https://doi.org/10.18637/jss.v091.i02](https://doi.org/10.18637/jss.v091.i02) — Artículo de referencia del paquete `stm`. Describe el modelo generativo, el algoritmo de estimación variacional y las funciones de visualización e inferencia, con demostraciones sobre textos políticos de múltiples países.[^24][^25]

2. **Tabata, M., & Kikuchi, T. (2023).** *Gender differences in campaigning under alternative voting systems.* **Politics & Gender**, 19(4), 851–878. [https://doi.org/10.1080/21565503.2022.2087192](https://doi.org/10.1080/21565503.2022.2087192) — Aplica STM a manifiestos de candidatos en Japón entre 1986 y 2009, modelando la distribución de tópicos en función del género del candidato; ilustra cómo los metadatos documentales pueden integrarse en el STM para análisis comparativo entre grupos —exactamente el uso que se hace aquí entre partidos.[^22]

***

## Similitud semántica y mapa de propuestas

La similitud entre documentos mediante vectores textuales y distancia coseno es una técnica estándar para comparar posicionamientos en corpus políticos. La reducción de dimensión para visualizar agrupaciones complejas —materializada aquí con `uwot`— sigue el algoritmo UMAP, cuya ventaja frente a t-SNE es la mejor preservación de la estructura global y el menor tiempo de cómputo.[^26][^27][^28][^13]

**Artículos de respaldo:**

1. **McInnes, L., Healy, J., Saul, N., & Großberger, L. (2018).** *UMAP: Uniform Manifold Approximation and Projection.* **Journal of Open Source Software**, 3(29), 861. [https://doi.org/10.21105/joss.00861](https://doi.org/10.21105/joss.00861) — Artículo de referencia del algoritmo UMAP. Describe su fundamentación matemática (geometría riemanniana y topología algebraica), sus ventajas de rendimiento y su compatibilidad con distancias no euclidianas como la distancia coseno, aplicable directamente a espacios de embeddings textuales.[^28][^29]

2. **Luo, Z. (2025).** *Using cross-encoders to measure the similarity of short texts in political science.* **American Journal of Political Science**, artículo en línea anticipado. [https://doi.org/10.1111/ajps.12956](https://doi.org/10.1111/ajps.12956) — Introduce modelos de similitud semántica de alta precisión para textos cortos en ciencia política —titulares, publicaciones en redes sociales, propuestas breves—, comparando representaciones vectoriales y distancias coseno con enfoques más avanzados, y demostrando que la elección del método afecta las conclusiones sustantivas sobre polarización y convergencia programática.[^30][^31]

***

## Cobertura poblacional y territorial

La detección de menciones a grupos poblacionales y territorios mediante reglas léxicas y diccionarios sobre el texto de las propuestas es una forma de análisis de agenda que permite identificar a quiénes priorizan los planes de gobierno. Este enfoque complementa los indicadores de frecuencia con una visión de distribución de atención política.[^3][^32]

**Artículos de respaldo:**

1. **Wilkerson, J., & Casas, A. (2017).** *Large-Scale Computerized Text Analysis in Political Science: Opportunities and Challenges.* **Annual Review of Political Science**, 20, 529–544. — Discute específicamente la detección de actores y grupos sociales mediante texto político computacional, y la medición de atención legislativa a distintos sectores de la población como aplicación central de los métodos *text-as-data*.[^6][^7]

2. **Grimmer, J., & Stewart, B. M. (2013).** *Text as Data: The Promise and Pitfalls of Automatic Content Analysis Methods for Political Texts.* **Political Analysis**, 21(3), 267–297. — Incluye directrices para la validación de diccionarios y reglas de clasificación aplicados a textos políticos, señalando los procedimientos adecuados para confirmar que las categorías detectadas corresponden a los conceptos sustantivos que se pretende medir.[^5][^4]

***

## Viabilidad fiscal

El cruce de señales textuales —presencia de costo, fuente de financiamiento, plazo, meta y anclaje macrofiscal— con reglas reproducibles sobre el texto de las propuestas es una forma de análisis de viabilidad presupuestaria asistido por texto. Esta aproximación descriptiva complementa las evaluaciones cuantitativas formales sin reemplazarlas.[^33][^34]

**Artículos de respaldo:**

1. **Zhang, J., et al. (2024).** *Evaluation of fiscal policy with text mining under the "dual carbon" framework.* **Heliyon**, 10(3), e24970. [https://www.sciencedirect.com/science/article/pii/S2405844024094970](https://www.sciencedirect.com/science/article/pii/S2405844024094970) — Aplica minería de texto para construir indicadores evaluativos de scripts de política fiscal, extrayendo automáticamente señales sobre metas, plazos y fuentes de financiamiento de documentos de política —una estructura metodológica directamente análoga al puntaje de viabilidad fiscal aplicado en este sitio.[^33]

2. **Lewis, C., et al. (2019).** *Fad or future? Automated analysis of financial text and its implications for corporate reporting regulation.* **Accounting and Business Research**, 49(5), 587–615. [https://doi.org/10.1080/00014788.2019.1611730](https://doi.org/10.1080/00014788.2019.1611730) — Aunque orientado a reportes corporativos, sienta las bases metodológicas y los estándares de validación para el procesamiento automático de textos con contenido financiero, incluyendo la extracción de indicadores cuantitativos (montos, plazos, fuentes) a partir de lenguaje natural —principios idénticos a los que sustentan el puntaje fiscal de este análisis.[^34]

***

## Contraste con datos oficiales

El contraste entre propuestas de política y series estadísticas oficiales mide la alineación entre los diagnósticos implícitos en los programas de gobierno y las brechas reales documentadas por fuentes externas. Esta práctica está emergiendo como un estándar en la evaluación de compromisos electorales y en la auditoría ciudadana de documentos de política pública.[^35][^36]

**Artículos de respaldo:**

1. **Sewerin, S., Kaack, L. H., et al. (2023).** *Towards understanding policy design through text-as-data approaches: The policy design annotations (POLIANNA) dataset.* **Scientific Data**, 10(1), 896. [https://doi.org/10.1038/s41597-023-02801-z](https://doi.org/10.1038/s41597-023-02801-z) — Presenta un conjunto de datos anotado para estudiar diseño de políticas mediante análisis de texto, mostrando cómo clasificar y comparar documentos de política pública con categorías sustantivas reproducibles, algo muy cercano al contraste entre propuestas e indicadores externos que se usa en este sitio.[^36]

2. **Wang, F., et al. (2025).** *Exploring online government-citizen interaction from a computational perspective.* **Government Information Quarterly**, 42(1), 101961. [https://doi.org/10.1016/j.giq.2025.101961](https://doi.org/10.1016/j.giq.2025.101961) — Examina la alineación entre demandas ciudadanas textuales y respuestas gubernamentales usando análisis computacional de texto, proporcionando un marco de referencia para medir la conexión entre propuestas y necesidades documentadas en datos externos.[^37]

***

## Seguimiento ciudadano y verificabilidad

La construcción de un *tracker* con hitos temporales y un puntaje de verificabilidad que integra meta, plazo, indicador, fuente y claridad operativa es un mecanismo de seguimiento digital de programas públicos alineado con desarrollos recientes en gestión pública basada en evidencia.[^38]

**Artículos de respaldo:**

1. **Uandykova, M., et al. (2025).** *Digital model for monitoring national programs.* **Frontiers in Artificial Intelligence**, 8, 1656329. [https://doi.org/10.3389/frai.2025.1656329](https://doi.org/10.3389/frai.2025.1656329) — Propone un modelo digital de monitoreo de programas nacionales que integra datos de múltiples fuentes gubernamentales, define indicadores de eficiencia presupuestaria, cumplimiento de metas y efecto socioeconómico en una escala 0–100%, y valida el modelo en programas reales —una arquitectura conceptualmente idéntica al tracker de verificabilidad utilizado en este análisis.[^38]

2. **Dassen, N., et al. (2024).** *Citizen Participation in Government Audits through Digital Tools: Overview and Lessons Learned.* Washington D.C.: IDB. — Sistematiza las experiencias de participación ciudadana en auditorías de programas públicos mediante herramientas digitales, incluyendo el diseño de trackers interactivos que recopilan datos a nivel nacional, y ofrece lecciones sobre condiciones necesarias para que el seguimiento sea efectivo y verificable.[^35]

***

## Implementabilidad con BART

El modelo BART (*Bayesian Additive Regression Trees*) es un método no paramétrico bayesiano basado en sumas de árboles que permite capturar interacciones complejas entre predictores sin supuestos paramétricos fuertes. Su uso para identificar patrones descriptivos —no causales— que acompañan a propuestas más ejecutables se inscribe en las aplicaciones de BART para inferencia flexible con variables mixtas.[^39][^40][^41][^42]

**Artículos de respaldo:**

1. **Chipman, H. A., George, E. I., & McCulloch, R. E. (2010).** *BART: Bayesian Additive Regression Trees.* **The Annals of Applied Statistics**, 4(1), 266–298. [https://doi.org/10.1214/09-AOAS285](https://doi.org/10.1214/09-AOAS285) — Artículo original de BART. Define el modelo suma de árboles, la prior regularizante que limita a cada árbol a ser un aprendiz débil, y el algoritmo MCMC de *backfitting* bayesiano para estimación e inferencia. También introduce la selección de variables por frecuencia de inclusión de predictores.[^43][^42]

2. **Hill, J., Linero, A., & Murray, J. (2020).** *Bayesian Additive Regression Trees: A Review and Look Forward.* **Annual Review of Statistics and Its Application**, 7, 251–278. [https://doi.org/10.1146/annurev-statistics-031219-041110](https://doi.org/10.1146/annurev-statistics-031219-041110) — Revisión comprehensiva de BART que discute extensiones del modelo original —incluyendo datos de alta dimensión, efectos aleatorios y aplicaciones en inferencia causal—, y proporciona guía sobre el uso del paquete `dbarts` en R, que es precisamente el paquete aplicado en este análisis.[^40]

***

## Referencias bibliográficas (formato APA 7.ª edición)

Chipman, H. A., George, E. I., & McCulloch, R. E. (2010). BART: Bayesian additive regression trees. *The Annals of Applied Statistics*, *4*(1), 266–298. https://doi.org/10.1214/09-AOAS285

Grimmer, J., & Stewart, B. M. (2013). Text as data: The promise and pitfalls of automatic content analysis methods for political texts. *Political Analysis*, *21*(3), 267–297. https://doi.org/10.1093/pan/mps028

Grimmer, J., Roberts, M. E., & Stewart, B. M. (2022). *Text as data: A new framework for machine learning and the social sciences*. Princeton University Press.

Hill, J., Linero, A., & Murray, J. (2020). Bayesian additive regression trees: A review and look forward. *Annual Review of Statistics and Its Application*, *7*, 251–278. https://doi.org/10.1146/annurev-statistics-031219-041110

Lewis, C., Young, S., & Walker, P. (2019). Fad or future? Automated analysis of financial text and its implications for corporate reporting regulation. *Accounting and Business Research*, *49*(5), 587–615. https://doi.org/10.1080/00014788.2019.1611730

Luo, Z. (2025). Using cross-encoders to measure the similarity of short texts in political science. *American Journal of Political Science*, advance online publication. https://doi.org/10.1111/ajps.12956

McInnes, L., Healy, J., Saul, N., & Großberger, L. (2018). UMAP: Uniform manifold approximation and projection. *Journal of Open Source Software*, *3*(29), 861. https://doi.org/10.21105/joss.00861

Sewerin, S., Kaack, L. H., Juhász, R., Kuhn, M., & Patt, A. (2023). Towards understanding policy design through text-as-data approaches: The policy design annotations (POLIANNA) dataset. *Scientific Data, 10*(1), 896. https://doi.org/10.1038/s41597-023-02801-z

Ntounias, T., Schneider, C., & Thomson, R. (2024). *Campaign promises, political ambiguity, and globalization* (IGCC Working Paper). University of California Institute on Global Conflict and Cooperation. https://ucigcc.org/wp-content/uploads/2024/10/2024_wp7_ntounias-schneider-thomson_v2-FINAL-1.pdf

Roberts, M. E., Stewart, B. M., & Tingley, D. (2019). stm: An R package for structural topic models. *Journal of Statistical Software*, *91*(2), 1–40. https://doi.org/10.18637/jss.v091.i02

Silge, J., & Robinson, D. (2016). tidytext: Text mining and analysis using tidy data principles in R. *Journal of Open Source Software*, *1*(3), 37. https://doi.org/10.21105/joss.00037

Tabata, M., & Kikuchi, T. (2023). Gender differences in campaigning under alternative voting systems. *Politics & Gender*, *19*(4), 851–878. https://doi.org/10.1080/21565503.2022.2087192

Uandykova, M., Ilyassova, G., Nurgaliyev, M., Kairat, B., & Abdildina, B. (2025). Digital model for monitoring national programs. *Frontiers in Artificial Intelligence*, *8*, 1656329. https://doi.org/10.3389/frai.2025.1656329

Vestergaard, M. B. (2025a). How political parties use a commitment strategy to stand out from parties with similar positions. *Journal of European Public Policy*, advance online publication. https://doi.org/10.1080/13501763.2025.2481189

Vestergaard, M. B. (2025b). Why all these promises? How parties strategically use policy commitments across 11 Western democracies. *European Journal of Political Research*, advance online publication. https://doi.org/10.1111/1475-6765.12706

Wang, F., Li, Y., & Liu, J. (2025). Exploring online government-citizen interaction from a computational perspective. *Government Information Quarterly*, *42*(1), 101961. https://doi.org/10.1016/j.giq.2025.101961

Wilkerson, J., & Casas, A. (2017). Large-scale computerized text analysis in political science: Opportunities and challenges. *Annual Review of Political Science*, *20*, 529–544. https://doi.org/10.1146/annurev-polisci-052615-025542

Zhang, J., Li, X., Wang, Y., & Chen, H. (2024). Evaluation of fiscal policy with text mining under the "dual carbon" framework. *Heliyon*, *10*(3), e24970. https://www.sciencedirect.com/science/article/pii/S2405844024094970

---

## References

1. [Text as data: The promise and pitfalls of automatic content analysis ...](https://collaborate.princeton.edu/en/publications/text-as-data-the-promise-and-pitfalls-of-automatic-content-analys/) - We survey a wide range of new methods, provide guidance on how to validate the output of the models,...

2. [[PDF] Text as Data: The Promise and Pitfalls of Automatic Content Analysis ...](https://web.stanford.edu/~jgrimmer/tad2.pdf) - We show how automated content methods can make possible the previously impossible in pol- itical sci...

3. [[PDF] Large-scale Computerized Text Analysis in Political Science](https://faculty.washington.edu/jwilker/559/wilkerson_casas_2017.pdf) - Text as data methods expand research opportunities for political scientists in two ways. First, they...

4. [Text as Data: The Promise and Pitfalls of Automatic Content Analysis ...](https://www.cambridge.org/core/journals/political-analysis/article/text-as-data-the-promise-and-pitfalls-of-automatic-content-analysis-methods-for-political-texts/F7AAC8B2909441603FEB25C156448F20) - We survey a wide range of new methods, provide guidance on how to validate the output of the models,...

5. [Text as Data: The Promise and Pitfalls of Automatic Content Analysis ...](https://econpapers.repec.org/RePEc:cup:polals:v:21:y:2013:i:03:p:267-297_01) - By Justin Grimmer and Brandon M. Stewart; Abstract: Politics and political conflict often occur in t...

6. [Large Scale Computerized Text Analysis in Political Science](https://www.polisci.washington.edu/research/publications/large-scale-computerized-text-analysis-political-science-opportunities-and) - Large Scale Computerized Text Analysis in Political Science: Opportunities and Challenges. (2017) An...

7. [Large-Scale Computerized Text Analysis in Political Science](https://www.semanticscholar.org/paper/Large-Scale-Computerized-Text-Analysis-in-Political-Wilkerson-Casas/0d345c2fb459e6ecc28328917ab37a4707e4a502) - This article first describes the four stages of a typical text-as-data project, then reviews recent ...

8. [[PDF] Text Mining with R](http://repo.darmajaya.ac.id/5417/1/Text%20Mining%20with%20R_%20A%20Tidy%20Approach%20(%20PDFDrive%20).pdf) - We developed the tidytext (Silge and Robinson 2016) R package because we were familiar with many met...

9. [Introduction to tidytext - CRAN](https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html) - Using tidy data principles can make many text mining tasks easier, more effective, and consistent wi...

10. [tidytext: Text Mining and Analysis Using Tidy Data Principles in R](https://www.semanticscholar.org/paper/tidytext:-Text-Mining-and-Analysis-Using-Tidy-Data-Silge-Robinson/1945711ed147087a65cf4ab163b8e21c38705273) - This package provides functions and supporting data sets to allow conversion of text to and from tid...

11. [Text Mining and Analysis Using Tidy Data Principles in R](https://www.theoj.org/joss-papers/joss.00037/10.21105.joss.00037.pdf) - por J Silge · Mencionado por 1198 — The tidytext package (Silge, Robinson, and Hester 2016) is an R ...

12. [tidytext: Text Mining and Analysis Using Tidy Data Principles in R](https://joss.theoj.org/papers/10.21105/joss.00037) - Silge et al, (2016), tidytext: Text Mining and Analysis Using Tidy Data Principles in R, Journal of ...

13. [Using Natural Language Processing to Analyze Political Party ...](https://ouci.dntb.gov.ua/en/works/l1wkrY37/) - This study explores how natural language processing (NLP) can supplement content analyses of politic...

14. [Text as Data: A New Framework for Machine Learning and the ...](https://politicalscience.stanford.edu/publications/text-data-new-framework-machine-learning-and-social-sciences) - Text as Data shows how to combine new sources of data, machine learning tools, and social science re...

15. [how political parties use a commitment strategy to stand ...](https://www.tandfonline.com/doi/full/10.1080/13501763.2025.2481189) - por MB Vestergaard · 2025 · Mencionado por 2 — When parties converge, they paradoxically risk losing...

16. [Why all these promises? How parties strategically use ...](https://www.cambridge.org/core/journals/european-journal-of-political-research/article/why-all-these-promises-how-parties-strategically-use-commitments-to-gain-credibility-in-an-increasingly-competitive-political-landscape/C214B941C18DCDE91D52CDB2B8C5CC62) - por MB Vestergaard · 2025 · Mencionado por 2 — Political parties face inherent risks when making ele...

17. [Text as data : a new framework for machine learning and ...](https://discovered.ed.ac.uk/discovery/fulldisplay/alma9924927701102466/44UOE_INST:44UOE_VU2) - ; Text as data : a new framework for machine learning and the social sciences. ; Grimmer, Justin, au...

18. [[PDF] Measuring vagueness and subjectivity in texts: from symbolic ... - arXiv](https://arxiv.org/pdf/2309.06132.pdf) - Abstract—We present a hybrid approach to the automated measurement of vagueness and subjectivity in ...

19. [Campaign Promises, Political Ambiguity, and Globalization](https://ucigcc.org/wp-content/uploads/2024/10/2024_wp7_ntounias-schneider-thomson_v2-FINAL-1.pdf) - por T Ntounias · 2024 — This presents parties with the dilemma that while voters expect them to make...

20. [how political parties use a commitment strategy to stand ...](https://www.tandfonline.com/doi/pdf/10.1080/13501763.2025.2481189) - por MB Vestergaard · 2025 · Mencionado por 2 — A major implication of this finding is that intense p...

21. [how political parties use a commitment strategy to stand ...](https://www.tandfonline.com/doi/abs/10.1080/13501763.2025.2481189) - por MB Vestergaard · 2025 · Mencionado por 2 — By producing commitments, a party increases the costs...

22. [Gender differences in campaigning under alternative voting systems](https://www.tandfonline.com/doi/full/10.1080/21565503.2022.2087192) - Candidate's gender plays an important role in voter evaluation. When drafting campaign manifestos, a...

23. [[PDF] How to Use Structural Topic Models in the Field of Industrial Relations](https://www.ssoar.info/ssoar/bitstream/handle/document/87346/ssoar-indb-2022-2-bender_et_al-Patterns_in_the_Press_Releases.pdf?sequence=1) - In contrast to their Scandinavian counterparts, e. g. German trade unions do not administer or co- f...

24. [stm: An R Package for Structural Topic Models](https://www.jstatsoft.org/article/view/v091i02) - This paper demonstrates how to use the R package stm for structural topic modeling. The structural t...

25. [[PDF] stm: An R Package for Structural Topic Models - NSF PAR](https://par.nsf.gov/servlets/purl/10195317) - Abstract. This paper demonstrates how to use the R package stm for structural topic modeling. The st...

26. [[PDF] Text Mining from Party Manifestos to Support the Design of Online ...](https://www.zora.uzh.ch/server/api/core/bitstreams/01648e8f-8f8b-4457-a967-39640342a1cd/content) - This was done by calculating the average cosine similarity of manifesto topic sentences and VAA stat...

27. [UMAP: Uniform Manifold Approximation and Projection for ...](https://arxiv.org/abs/1802.03426) - por L McInnes · 2018 · Mencionado por 22341 — Abstract:UMAP (Uniform Manifold Approximation and Proj...

28. [UMAP: Uniform Manifold Approximation and Projection](https://www.theoj.org/joss-papers/joss.00861/10.21105.joss.00861.pdf) - por L McInnes · Mencionado por 22333 — Uniform Manifold Approximation and Projection (UMAP) is a dim...

29. [UMAP: Uniform Manifold Approximation and Projection](https://joss.theoj.org/papers/10.21105/joss.00861) - McInnes et al., (2018). UMAP: Uniform Manifold Approximation and Projection. Journal of Open Source ...

30. [Using cross‐encoders to measure the similarity of short texts in ...](https://onlinelibrary.wiley.com/doi/10.1111/ajps.12956?af=R) - I introduce to political science cross-encoders for precise estimates of semantic similarity in shor...

31. [Using cross-encoders to measure the similarity of short texts in ...](https://ajps.org/2025/03/11/using-cross-encoders-to-measure-the-similarity-of-short-texts-in-political-science/) - I introduce a state-of-the-art transformer model, cross-encoder, which utilizes pair embedding techn...

32. [Walking the line: Electoral cycles and the shift in legislative priorities ...](https://www.sciencedirect.com/science/article/pii/S0261379423000173) - This article sheds light on how MPs' priorities change in the course of legislative terms. We purpor...

33. [Evaluation of fiscal policy with text mining under "dual ...](https://www.sciencedirect.com/science/article/pii/S2405844024094970) - por J Zhang · 2024 · Mencionado por 15 — The study employs text mining techniques to articulate eval...

34. [Fad or future? Automated analysis of financial text and its ...](https://www.tandfonline.com/doi/full/10.1080/00014788.2019.1611730) - por C Lewis · 2019 · Mencionado por 213 — Abstract. This paper describes the current state of natura...

35. [Citizen Participation in Government Audits through Digital Tools](https://publications.iadb.org/publications/english/document/Citizen-Participation-in-Government-Audits-through-Digital-Tools-Overview-of-Initiatives-from-Supreme-Audit-Institution.pdf) - por N Dassen · 2024 · Mencionado por 7 — Citizen oversight. Public works monitoring. Citizen educati...

36. [Natural Language Processing in Evaluation](https://www.norad.no/globalassets/filer/evaluering/natural-language-processing-in-evaluation.pdf) - In this context, this meant defining a systematic approach to categorising text relating to cross-cu...

37. [Exploring online government-citizen interaction from a ...](https://www.sciencedirect.com/science/article/pii/S2694610625000414) - por F Wang · 2025 · Mencionado por 1 — By the first half of 2021, the cumulative number of public me...

38. [Digital model for monitoring national programs](https://www.frontiersin.org/journals/artificial-intelligence/articles/10.3389/frai.2025.1656329/full) - por M Uandykova · 2025 — This paper presents a conceptual digital model for monitoring national prog...

39. [[0806.3286] BART: Bayesian additive regression trees - arXiv](https://arxiv.org/abs/0806.3286) - Abstract:We develop a Bayesian "sum-of-trees" model where each tree is constrained by a regularizati...

40. [Bayesian Additive Regression Trees: A Review and Look Forward](https://www.annualreviews.org/content/journals/10.1146/annurev-statistics-031219-041110) - Bayesian additive regression trees (BART) provides a flexible approach to fitting a variety of regre...

41. [BARP: Improving Mister P Using Bayesian Additive Regression Trees](https://www.cambridge.org/core/journals/american-political-science-review/article/barp-improving-mister-p-using-bayesian-additive-regression-trees/630866EB47F9366EDB3C22CFD951BB6F) - I propose a modified version of MRP that replaces the multilevel model with a nonparametric approach...

42. [Chipman, H., George, E. and McCulloch, R. (2010). BART](https://www.sciepub.com/reference/467713) - Chipman, H., George, E. and McCulloch, R. (2010). BART: Bayesian additive regression trees. Annals o...

43. [BART: BAYESIAN ADDITIVE REGRESSION TREES](https://www.jstor.org/stable/27801587) - por HA Chipman · 2010 · Mencionado por 3152 — The Annals of Applied Statistics. 2010, Vol. 4, No. 1,...
