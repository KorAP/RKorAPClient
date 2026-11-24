function () {
  var chart = this;
  var spec = chart.options.chart.korapLocale;
  if (!spec) return;
  var text = korapChartText(spec);
  var de = text.de, language = text.language, remainingName = text.remainingName;
  var escape = text.escape, number = text.number, percent = text.percent, tooltip = text.tooltip;
  if (remainingName && chart.xAxis[0]) {
    var categories = chart.xAxis[0].categories.slice();
    if (categories.slice(0, -1).indexOf(remainingName) !== -1) remainingName += text.combined;
    categories[categories.length - 1] = remainingName;
    chart.xAxis[0].setCategories(categories, false);
  }
  chart.series.forEach(function (series) {
    if (spec.kind === 'dispersion' && series.index === 0 &&
        (series.name === 'Corpus tokens' || series.name === 'Korpustokens')) {
      var referenceName = text.corpusTokens;
      if (chart.series.some(function (s) { return s.index !== 0 && s.name === referenceName; }))
        referenceName += text.reference;
      series.update({ name: referenceName }, false);
    }
    if (series.options.dataLabels && series.options.dataLabels.format === '{point.y:.1f}%' &&
        !series.options.dataLabels.formatter) {
      series.update({ dataLabels: { format: null, formatter: function () {
        return this.y === null ? '' : text.share(this.y);
      } } }, false);
    }
    (series.data || []).forEach(function (point) {
      if (!point) return;
      if (point.korapCombinedParts && remainingName) {
        point.name = point.options.name = remainingName;
        point.tooltipLabel = point.options.tooltipLabel = escape(remainingName);
      }
      var shown = tooltip(point, series);
      if (shown !== null) { point.tooltipText = shown; point.options.tooltipText = shown; }
    });
  });
  var changes = {};
  if (spec.kind === 'dispersion' && chart.options.title.text === spec.title) {
    changes.title = { text: escape(text.dispersionTitle()) };
  }
  var valueAxis = chart.yAxis[0];
  if (valueAxis) {
    var axisChanges = {};
    if (spec.kind === 'dispersion' && valueAxis.options.title.text === spec.defaultY)
      axisChanges.title = { text: text.shareAxis };
    else if (String(valueAxis.options.title.text).toLowerCase() === 'ipm') axisChanges.title = { text: 'IPM' };
    var axisLabels = valueAxis.options.labels || {};
    var percentage = /^\{value\}\s*%$/.test(axisLabels.format || '');
    if (!axisLabels.formatter && (!axisLabels.format || percentage)) {
      axisChanges.labels = { format: null, formatter: function () { return percentage ? percent(this.value) : number(this.value); } };
    }
    valueAxis.update(axisChanges, false);
  }
  if ((chart.options.caption.text || '') === (spec.caption || '')) {
    var notes = text.caption();
    changes.caption = { text: notes ? escape(notes) : null };
  }
  chart.korapLanguage = language;
  chart.update(changes, false);
  chart.redraw(false);
}
