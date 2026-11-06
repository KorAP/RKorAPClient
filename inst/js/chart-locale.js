function () {
  var chart = this;
  var spec = chart.options.chart.korapLocale;
  if (!spec) return;
  var preferences = typeof navigator === 'undefined' ? [] :
    (navigator.languages && navigator.languages.length ? navigator.languages : [navigator.language || 'en']);
  var language = spec.language;
  if (language === 'auto') {
    language = 'en';
    for (var i = 0; i < preferences.length; i++) {
      var candidate = String(preferences[i]).toLowerCase().split(/[-_]/)[0];
      if (candidate === 'de' || candidate === 'en') { language = candidate; break; }
    }
  }
  var de = language === 'de';
  var locale = de ? 'de-DE' : 'en-US';
  var labels = de ? {
    relative: 'Relative Häufigkeit', absolute: 'Absolute Häufigkeit', corpus: 'Korpusgröße (Tokens)',
    alternatives: 'Gesamthäufigkeit der Varianten', interval: 'Konfidenzintervall',
    tokenShare: 'Anteil an den Korpustokens', subcorpus: 'Teilkorpusgröße (Tokens)',
    share: 'Anteil an der Gesamthäufigkeit', estimatedShare: 'Geschätzter Anteil an der Gesamthäufigkeit',
    estimatedAbsolute: 'Geschätzte absolute Häufigkeit', total: 'Gesamthäufigkeit',
    estimatedTotal: 'Geschätzte Gesamthäufigkeit', sample: 'Absolute Häufigkeit in der Stichprobe',
    estimatedRelative: 'Geschätzte relative Häufigkeit', probability: 'Auswahlwahrscheinlichkeit der Seiten',
    undefined: 'nicht definiert'
  } : {
    relative: 'Relative frequency', absolute: 'Absolute frequency', corpus: 'Corpus size (tokens)',
    alternatives: 'Total frequency of alternatives', interval: 'Confidence interval',
    tokenShare: 'Share of corpus tokens', subcorpus: 'Subcorpus size (tokens)',
    share: 'Share of total frequency', estimatedShare: 'Estimated share of total frequency',
    estimatedAbsolute: 'Estimated absolute frequency', total: 'Total frequency',
    estimatedTotal: 'Estimated total frequency', sample: 'Absolute frequency in sample',
    estimatedRelative: 'Estimated relative frequency', probability: 'Page inclusion probability',
    undefined: 'undefined'
  };
  function escape(value) {
    return String(value).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
  }
  var significant = new Intl.NumberFormat(locale, { maximumSignificantDigits: 4 });
  var integer = new Intl.NumberFormat(locale, { maximumFractionDigits: 0 });
  var decimal = new Intl.NumberFormat(locale, { minimumFractionDigits: 1, maximumFractionDigits: 1 });
  function number(value, count) {
    return typeof value !== 'number' || !isFinite(value) ? labels.undefined :
      (count ? integer : significant).format(value);
  }
  function percent(value) { return number(value) + (de ? ' %' : '%'); }
  function row(label, value) { return label + ': ' + value; }
  function tooltip(point, series) {
    var rows = [], type = point.korapTooltipType;
    if (type === 'frequency') {
      if (point.korapTooltipName) rows.push('<b>' + escape(point.korapTooltipName) + '</b>');
      var unit = point.korapAlternatives ? '%' : point.korapTooltipUnit;
      if (String(unit).toLowerCase() === 'ipm') unit = 'IPM';
      rows.push(row(labels.relative, '<b>' + number(point.y) + '</b> ' + escape(unit)));
      rows.push(row(labels.absolute, number(point.count, true)));
      rows.push(row(point.korapAlternatives ? labels.alternatives : labels.corpus, number(point.total, true)));
      if (point.korapShowCI !== false)
        rows.push(row(labels.interval, number(point.confLow) + '–' + number(point.confHigh) + ' ' + escape(unit)));
    } else if (type === 'corpus') {
      rows.push(row(labels.tokenShare, '<b>' + percent(point.y) + '</b>'));
      rows.push(row(labels.subcorpus, number(point.tokens, true)));
      rows.push(row(labels.corpus, number(point.tokenTotal, true)));
    } else if (type === 'dispersion') {
      var estimated = point.korapEstimated;
      rows.push('<b>' + escape(series.name) + '</b>');
      rows.push(row(estimated ? labels.estimatedShare : labels.share, '<b>' + percent(point.y) + '</b>'));
      rows.push(row(estimated ? labels.estimatedAbsolute : labels.absolute, number(point.frequency, !estimated)));
      rows.push(row(estimated ? labels.estimatedTotal : labels.total, number(point.totalInParts, !estimated)));
      if (estimated) rows.push(row(labels.sample, number(point.observed, true)));
      rows.push(row(labels.subcorpus, number(point.tokens, true)));
      rows.push(row(estimated ? labels.estimatedRelative : labels.relative, number(point.perMillion) + ' IPM'));
      if (estimated) rows.push(row(labels.probability, percent(point.korapProbability * 100)));
    } else return null;
    if (point.korapCombinedParts) rows.push(de ?
      integer.format(point.korapCombinedParts) + (point.korapCombinedParts === 1 ? ' Teilkorpus zusammengefasst.' : ' Teilkorpora zusammengefasst.') :
      integer.format(point.korapCombinedParts) + (point.korapCombinedParts === 1 ? ' subcorpus combined.' : ' subcorpora combined.'));
    return rows.join('<br/>');
  }
  var remainingName = spec.remaining ? (de ?
    (spec.remaining === 1 ? 'Übriges Teilkorpus' : 'Übrige ' + spec.remaining + ' Teilkorpora') :
    'Remaining ' + spec.remaining + (spec.remaining === 1 ? ' subcorpus' : ' subcorpora')) : null;
  if (remainingName && chart.xAxis[0]) {
    var categories = chart.xAxis[0].categories.slice();
    if (categories.slice(0, -1).indexOf(remainingName) !== -1) remainingName += de ? ' (zusammengefasst)' : ' (combined)';
    categories[categories.length - 1] = remainingName;
    chart.xAxis[0].setCategories(categories, false);
  }
  chart.series.forEach(function (series) {
    if (spec.kind === 'dispersion' && series.index === 0 &&
        (series.name === 'Corpus tokens' || series.name === 'Korpustokens')) {
      var referenceName = de ? 'Korpustokens' : 'Corpus tokens';
      if (chart.series.some(function (s) { return s.index !== 0 && s.name === referenceName; }))
        referenceName += de ? ' (Referenz)' : ' (reference)';
      series.update({ name: referenceName }, false);
    }
    if (series.options.dataLabels && series.options.dataLabels.format === '{point.y:.1f}%' &&
        !series.options.dataLabels.formatter) {
      series.update({ dataLabels: { format: null, formatter: function () {
        return this.y === null ? '' : decimal.format(this.y) + (de ? ' %' : '%');
      } } }, false);
    }
    (series.data || []).forEach(function (point) {
      if (!point) return;
      if (point.korapCombinedParts && remainingName) {
        point.name = point.options.name = remainingName;
        point.tooltipLabel = point.options.tooltipLabel = escape(remainingName);
      }
      var text = tooltip(point, series);
      if (text !== null) { point.tooltipText = text; point.options.tooltipText = text; }
    });
  });
  var changes = {};
  if (spec.kind === 'dispersion' && chart.options.title.text === spec.title) {
    var dimensions = de ? { source: 'Quelle', corpusTitle: 'Korpustitel', pubPlaceKey: 'Publikationsland',
      region: 'Region', year: 'Publikationsjahr', pubDate: 'Publikationsjahr', creationDate: 'Entstehungsjahr' } :
      { source: 'source', corpusTitle: 'corpus title', pubPlaceKey: 'publication country',
        region: 'region', year: 'publication year', pubDate: 'publication year', creationDate: 'creation year' };
    changes.title = { text: escape((de ? 'Verteilung nach ' : 'Distribution by ') + (dimensions[spec.partition] || spec.partition)) };
  }
  var valueAxis = chart.yAxis[0];
  if (valueAxis) {
    var axisChanges = {};
    if (spec.kind === 'dispersion' && valueAxis.options.title.text === spec.defaultY)
      axisChanges.title = { text: de ? 'Anteil im analysierten Korpus (%)' : spec.defaultY };
    else if (String(valueAxis.options.title.text).toLowerCase() === 'ipm') axisChanges.title = { text: 'IPM' };
    var axisLabels = valueAxis.options.labels || {};
    var percentage = /^\{value\}\s*%$/.test(axisLabels.format || '');
    if (!axisLabels.formatter && (!axisLabels.format || percentage)) {
      axisChanges.labels = { format: null, formatter: function () { return percentage ? percent(this.value) : number(this.value); } };
    }
    valueAxis.update(axisChanges, false);
  }
  if ((chart.options.caption.text || '') === (spec.caption || '')) {
    var notes = [];
    if (spec.kind === 'dispersion') {
      if (spec.remaining) notes.push(de ? spec.shown + ' von ' + spec.parts +
        ' Teilkorpora einzeln dargestellt; die Dispersionsmaße berücksichtigen alle Teilkorpora.' :
        spec.shown + ' of ' + spec.parts + ' subcorpora shown individually; dispersion measures use all subcorpora.');
      if (spec.estimated) notes.push(de ? 'Seitenstichprobe: Häufungen innerhalb der Seiten können Schätzungen verzerren.' :
        'Page-sample estimates can be biased by clustering.');
      if (spec.unequal) notes.push(de ? 'Die Varianten haben unterschiedliche Stichprobenanteile.' : 'Variants have unequal page coverage.');
      if (spec.excluded) notes.push(de ? 'Material außerhalb der analysierten Teilkorpora bleibt unberücksichtigt.' :
        'Material outside the analysed subcorpora is excluded.');
      if (spec.zeroExact.length) notes.push(de ? 'Keine Belege für ' + spec.zeroExact.join(', ') + '; Häufigkeitsanteile nicht definiert.' :
        'No occurrences for ' + spec.zeroExact.join(', ') + '; frequency shares are undefined.');
      if (spec.zeroSample.length) notes.push(de ? 'Keine Belege in der Stichprobe für ' + spec.zeroSample.join(', ') + '; Häufigkeitsanteile nicht definiert.' :
        'No sample occurrences for ' + spec.zeroSample.join(', ') + '; frequency shares are undefined.');
    } else {
      if (spec.gaps) notes.push(de ? 'Nicht definierte Häufigkeiten bleiben ausgespart.' : 'Undefined frequencies are shown as gaps.');
      if (spec.log) notes.push(spec.showCI === false ?
        (de ? 'Logarithmische Skala: Nichtpositive Häufigkeiten bleiben ausgespart.' : 'Log scale: non-positive frequencies are omitted.') :
        (de ? 'Logarithmische Skala: Nichtpositive Häufigkeiten und Intervalle mit Untergrenze null bleiben ausgespart.' :
          'Log scale: non-positive frequencies and intervals touching zero are omitted.'));
    }
    changes.caption = { text: notes.length ? escape(notes.join(' ')) : null };
  }
  chart.korapLanguage = language;
  chart.update(changes, false);
  chart.redraw(false);
}
