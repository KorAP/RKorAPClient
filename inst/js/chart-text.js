function (spec) {
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
    undefined: 'nicht definiert', value: 'Wert', values: ['Wert', 'Werte', 'der Werte'], texts: ['Text', 'Texte', 'der Texte'],
    upTo: 'bis', under: 'unter', hits: 'Treffer', of: 'in', link: 'Klicken: Treffer in KorAP ansehen'
  } : {
    relative: 'Relative frequency', absolute: 'Absolute frequency', corpus: 'Corpus size (tokens)',
    alternatives: 'Total frequency of alternatives', interval: 'Confidence interval',
    tokenShare: 'Share of corpus tokens', subcorpus: 'Subcorpus size (tokens)',
    share: 'Share of total frequency', estimatedShare: 'Estimated share of total frequency',
    estimatedAbsolute: 'Estimated absolute frequency', total: 'Total frequency',
    estimatedTotal: 'Estimated total frequency', sample: 'Absolute frequency in sample',
    estimatedRelative: 'Estimated relative frequency', probability: 'Page inclusion probability',
    undefined: 'undefined', value: 'Value', values: ['value', 'values', 'of values'], texts: ['text', 'texts', 'of texts'],
    upTo: 'up to', under: 'below', hits: 'Hits', of: 'in', link: 'Click to see the hits in KorAP'
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
  // Lines of label and right-aligned value; no table, as documents style those.
  function grid(lines) {
    return '<div style="display:grid;grid-template-columns:auto auto;column-gap:18px;font-variant-numeric:tabular-nums">' +
      lines.map(function (line) { return '<span>' + line[0] + '</span><span style="text-align:right">' + line[1] + '</span>'; }).join('') + '</div>';
  }
  function muted(content) { return '<span style="opacity:.7">' + content + '</span>'; }
  // Same number of decimals for all values of a box, about three digits for its largest
  function boxNumber(point) {
    var largest = Math.max.apply(null, point.stats.map(Math.abs)) || Math.max(Math.abs(point.min), Math.abs(point.max));
    var digits = point.integer || !largest ? 0 : Math.min(20, Math.max(0, 2 - Math.floor(Math.log10(largest))));
    return new Intl.NumberFormat(locale, { minimumFractionDigits: digits, maximumFractionDigits: digits });
  }
  // Never 0 or 100 % for some, but not all values
  function part(count, total) {
    var share = 100 * count / total, digits = 0;
    while (digits < 4 && (+share.toFixed(digits) === 0 || +share.toFixed(digits) === 100)) digits++;
    return new Intl.NumberFormat(locale, { maximumFractionDigits: digits }).format(share) + (de ? ' %' : '%');
  }
  function boxTooltip(point, name, linked) {
    var unit = point.korapTooltipUnit ? escape(point.korapTooltipUnit) : '';
    if (unit.toLowerCase() === 'ipm') unit = 'IPM';
    var title = name ? '<b>' + escape(name) + '</b>' : '';
    if (point.korapTooltipType === 'outlier') {
      var place = [point.korapTooltipCategory, point.korapTooltipGroup].filter(Boolean).map(escape).join(' · ');
      var lines = [[unit === 'IPM' ? labels.relative : labels.value, '<b>' + number(point.y) + '</b>' + (unit ? ' ' + unit : '')]];
      if (typeof point.total === 'number')
        lines.push([labels.hits, number(point.count, true) + ' ' + labels.of + ' ' + number(point.total, true) + (de ? ' Tokens' : ' tokens')]);
      return (title ? title + '<br/>' : '') + (place ? muted(place) : '') + grid(lines) + (linked ? muted(labels.link) : '');
    }
    var noun = point.korapPerText ? labels.texts : labels.values, format = boxNumber(point);
    function line(label, value, bold) {
      value = format.format(value);
      return bold ? ['<b>' + label + '</b>', '<b>' + value + '</b>'] : [label, value];
    }
    function quantile(share, preposition) { return share + ' ' + noun[2] + ' ' + preposition; }
    var rows = unit ? [['', muted(unit)]] : [];
    rows.push(line('Maximum', point.max));
    if (point.above) rows.push(line(quantile(part(point.count - point.above, point.count), labels.upTo), point.stats[4]));
    rows.push(line(quantile(de ? '75 %' : '75%', labels.upTo), point.stats[3]), line('Median', point.stats[2], true),
      line(quantile(de ? '25 %' : '25%', labels.upTo), point.stats[1]));
    if (point.below) rows.push(line(quantile(part(point.below, point.count), labels.under), point.stats[0]));
    rows.push(line('Minimum', point.min));
    var about = [point.korapTooltipGroup ? escape(point.korapTooltipGroup) : '',
      number(point.count, true) + ' ' + noun[point.count === 1 ? 0 : 1]].filter(Boolean).join(' · ');
    return (title ? title + ' ' + muted('· ' + about) : muted(about)) + grid(rows);
  }
  // Plain, tab-separated and unrounded, to be copied into a spreadsheet
  function yearTable(series) {
    var first = series[0].data[0].korap, unit = first.korapAlternatives ? '%' : first.korapTooltipUnit;
    var head = [de ? 'Jahr' : 'Year', de ? 'Kurve' : 'Curve', labels.relative + (unit ? ' (' + unit + ')' : ''), labels.absolute,
      first.korapAlternatives ? labels.alternatives : labels.corpus];
    if (first.korapShowCI) head.push(labels.interval + (de ? ' (von)' : ' (from)'), labels.interval + (de ? ' (bis)' : ' (to)'));
    var rows = [head];
    series.forEach(function (s) {
      s.data.forEach(function (d) {
        var p = d.korap, cells = [p.year, s.name, p.y, p.count, p.total];
        if (first.korapShowCI) cells.push(p.confLow, p.confHigh);
        rows.push(cells.map(function (v) { return v === null || v === undefined ? '' : String(v).replace(/\s+/g, ' '); }));
      });
    });
    return rows.map(function (r) { return r.join('\t'); }).join('\n');
  }
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
  function caption() {
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
    } else if (spec.kind === 'box') {
      notes.push(de ? 'Kästen: Quartile und Median; Whisker: bis zum 1,5-fachen Interquartilsabstand.' :
        'Boxes: quartiles and median; whiskers: up to 1.5 times the interquartile range.');
      if (spec.hidden) notes.push(de ? integer.format(spec.hidden) + (spec.hidden === 1 ? ' Wert' : ' Werte') + ' außerhalb nicht dargestellt.' :
        integer.format(spec.hidden) + (spec.hidden === 1 ? ' value' : ' values') + ' beyond not shown.');
    } else {
      if (spec.gaps) notes.push(de ? 'Nicht definierte Häufigkeiten bleiben ausgespart.' : 'Undefined frequencies are shown as gaps.');
      if (spec.log) notes.push(spec.showCI === false ?
        (de ? 'Logarithmische Skala: Nichtpositive Häufigkeiten bleiben ausgespart.' : 'Log scale: non-positive frequencies are omitted.') :
        (de ? 'Logarithmische Skala: Nichtpositive Häufigkeiten und Intervalle mit Untergrenze null bleiben ausgespart.' :
          'Log scale: non-positive frequencies and intervals touching zero are omitted.'));
    }
    return notes.join(' ');
  }
  function dispersionTitle() {
    var dimensions = de ? { source: 'Quelle', corpusTitle: 'Korpustitel', pubPlaceKey: 'Publikationsland',
      region: 'Region', year: 'Publikationsjahr', pubDate: 'Publikationsjahr', creationDate: 'Entstehungsjahr' } :
      { source: 'source', corpusTitle: 'corpus title', pubPlaceKey: 'publication country',
        region: 'region', year: 'publication year', pubDate: 'publication year', creationDate: 'creation year' };
    return (de ? 'Verteilung nach ' : 'Distribution by ') + (dimensions[spec.partition] || spec.partition);
  }
  return { language: language, de: de, escape: escape, number: number, percent: percent,
    share: function (value) { return decimal.format(value) + (de ? ' %' : '%'); },
    tooltip: tooltip, yearTable: yearTable, boxTooltip: boxTooltip, remainingName: remainingName, caption: caption, dispersionTitle: dispersionTitle,
    corpusTokens: de ? 'Korpustokens' : 'Corpus tokens', reference: de ? ' (Referenz)' : ' (reference)',
    combined: de ? ' (zusammengefasst)' : ' (combined)',
    shareAxis: de ? 'Anteil im analysierten Korpus (%)' : spec.defaultY };
}
