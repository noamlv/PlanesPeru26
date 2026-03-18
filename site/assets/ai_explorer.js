(function () {
  const root = document.querySelector('.ai-explorer');
  if (!root) return;

  const queryEl = document.getElementById('ai-query');
  const partyEl = document.getElementById('ai-party');
  const axisEl = document.getElementById('ai-axis');
  const searchBtn = document.getElementById('ai-search-btn');
  const clearBtn = document.getElementById('ai-clear-btn');
  const metaEl = document.getElementById('ai-meta');
  const resultsEl = document.getElementById('ai-results');
  const chips = root.querySelectorAll('.ai-prompt-chip');
  if (!queryEl || !partyEl || !axisEl || !searchBtn || !clearBtn || !metaEl || !resultsEl) return;

  let rows = [];

  const normalize = (s) => (s || '')
    .toLowerCase()
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[^a-z0-9\s]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();

  const escapeHtml = (s) => String(s || '')
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;');

  const truncate = (s, n) => {
    const text = String(s || '').trim();
    return text.length > n ? text.slice(0, n - 1).trim() + '…' : text;
  };

  const populateFilters = () => {
    const parties = [...new Set(rows.map((r) => r.party).filter(Boolean))].sort((a, b) => a.localeCompare(b, 'es'));
    const axes = [...new Set(rows.map((r) => r.axis).filter(Boolean))].sort((a, b) => a.localeCompare(b, 'es'));

    partyEl.innerHTML = '<option value="">Todos</option>' + parties.map((p) => `<option value="${escapeHtml(p)}">${escapeHtml(p)}</option>`).join('');
    axisEl.innerHTML = '<option value="">Todos</option>' + axes.map((a) => `<option value="${escapeHtml(a)}">${escapeHtml(a)}</option>`).join('');
  };

  const scoreRow = (row, query) => {
    const queryNorm = normalize(query);
    const tokens = queryNorm.split(' ').filter((x) => x.length >= 2);
    const corpus = row._corpus || normalize([row.party, row.axis, row.proposal_text, row.source_snippet, row.instrument_type].join(' '));

    if (!queryNorm) {
      return (Number(row.concreteness_score || 0) / 20) + (row.has_quant_target ? 0.5 : 0) + (row.has_time_horizon ? 0.5 : 0);
    }

    let score = 0;
    if (corpus.includes(queryNorm)) score += 8;
    for (const token of tokens) {
      if (corpus.includes(token)) score += 2.2;
    }
    score += Number(row.concreteness_score || 0) / 30;
    if (row.has_quant_target) score += 0.8;
    if (row.has_time_horizon) score += 0.8;
    return score;
  };

  const renderResults = (items, query) => {
    const queryNorm = normalize(query);
    if (items.length === 0) {
      metaEl.textContent = 'No encontré propuestas con ese criterio.';
      resultsEl.innerHTML = '<div class="ai-empty">Prueba otra palabra, otro partido o un tema más específico.</div>';
      return;
    }

    metaEl.textContent = `${items.length} resultado(s) relevantes.`;
    resultsEl.innerHTML = items.map((r) => {
      const quant = r.has_quant_target ? 'Meta clara' : 'Sin meta clara';
      const horizon = r.has_time_horizon ? 'Con plazo' : 'Sin plazo';
      return `
        <article class="ai-result-card">
          <div class="ai-result-head">
            <div class="ai-result-party">${escapeHtml(r.party)}</div>
            <div class="ai-result-tags">
              <span class="ai-result-chip">${escapeHtml(r.axis)}</span>
              <span class="ai-result-chip">${escapeHtml(r.instrument_type || 'Sin tipo explícito')}</span>
              <span class="ai-result-chip">Claridad ${Number(r.concreteness_score || 0).toFixed(1)}</span>
              <span class="ai-result-chip">${quant}</span>
              <span class="ai-result-chip">${horizon}</span>
            </div>
          </div>
          <p class="ai-result-text">${escapeHtml(truncate(r.proposal_text, 360))}</p>
          <details class="ai-result-details">
            <summary>Ver snippet y trazabilidad</summary>
            <p class="ai-result-snippet">${escapeHtml(truncate(r.source_snippet, 420))}</p>
            <p class="ai-result-meta">doc_id: ${escapeHtml(r.doc_id)} · proposal_id: ${escapeHtml(r.proposal_id)}</p>
          </details>
        </article>
      `;
    }).join('');
  };

  const runSearch = () => {
    const party = partyEl.value;
    const axis = axisEl.value;
    const query = queryEl.value || '';
    const queryNorm = normalize(query);

    let filtered = rows.filter((row) => (!party || row.party === party) && (!axis || row.axis === axis));
    filtered = filtered
      .map((row) => ({ ...row, _score: scoreRow(row, queryNorm) }))
      .filter((row) => !queryNorm || row._score > 0)
      .sort((a, b) => b._score - a._score || (b.concreteness_score || 0) - (a.concreteness_score || 0))
      .slice(0, 8);

    renderResults(filtered, queryNorm);
  };

  searchBtn.addEventListener('click', runSearch);
  clearBtn.addEventListener('click', () => {
    queryEl.value = '';
    partyEl.value = '';
    axisEl.value = '';
    metaEl.textContent = 'Escribe una pregunta o elige un tema para empezar.';
    resultsEl.innerHTML = '';
  });

  chips.forEach((chip) => {
    chip.addEventListener('click', () => {
      queryEl.value = chip.dataset.prompt || '';
      runSearch();
    });
  });

  fetch('assets/ai_explorer_data.json')
    .then((r) => r.json())
    .then((data) => {
      rows = Array.isArray(data) ? data : [];
      rows = rows.map((row) => ({ ...row, _corpus: normalize([row.party, row.axis, row.proposal_text, row.source_snippet, row.instrument_type].join(' ')) }));
      populateFilters();
      metaEl.textContent = 'Escribe una pregunta o elige un tema para empezar.';
    })
    .catch(() => {
      metaEl.textContent = 'No pude cargar la base de exploración en esta página.';
      resultsEl.innerHTML = '<div class="ai-empty">Vuelve a cargar la página o revisa que el sitio se haya renderizado correctamente.</div>';
    });
})();
