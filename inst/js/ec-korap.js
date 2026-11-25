function (el, x, spec, korapChartText) {
  var chart = get_e_charts(el.id);
  var text = korapChartText(spec.locale);
  var option = spec.option;
  var horizontal = spec.horizontal;
  var frequencies = spec.frequencySeries;
  // Panels have grids and axes of their own.
  var grids = [].concat(option.grid);

  // Texts
  if (spec.locale.kind === 'dispersion') {
    option.title[0].text = text.dispersionTitle();
    option.xAxis.name = text.shareAxis;
    var reference = option.series[0];
    reference.name = text.corpusTokens;
    if (option.series.slice(1).some(function (s) { return s.name === reference.name; }))
      reference.name += text.reference;
    option.series.forEach(function (s) {
      s.label.formatter = function (p) { return typeof p.value === 'number' ? text.share(p.value) : ''; };
    });
    var remaining = text.remainingName;
    if (remaining) {
      var categories = option.yAxis.data;
      if (categories.slice(0, -1).indexOf(remaining) !== -1) remaining += text.combined;
      categories[categories.length - 1] = remaining;
    }
  }
  var caption = text.caption(), lines = 0, width = chart.getWidth();
  if (caption) {
    lines = Math.ceil(caption.length * 6.2 / Math.max(width - 30, 100));
    option.title[1] = { text: caption, left: 10, bottom: 4,
      textStyle: { fontSize: 11, fontWeight: 'normal', lineHeight: 15, width: Math.max(width - 30, 100), overflow: 'break' } };
  }
  // Room for the caption, the legend above it and, where the value axis is below, its title.
  var below = lines ? lines * 15 + 6 : 0;
  if (option.legend.show !== false && option.legend.bottom !== undefined) {
    option.legend.bottom += below;
    below += 30;
  }
  grids.forEach(function (grid) { grid.bottom += below ? below + (horizontal ? 20 : 0) : 0; });
  [].concat(horizontal ? option.xAxis : option.yAxis).forEach(function (valueAxis) {
    if (String(valueAxis.name).toLowerCase() === 'ipm') valueAxis.name = 'IPM';
    valueAxis.axisLabel = { hideOverlap: grids.length > 1, formatter: function (v) {
      return spec.unit === '%' ? text.percent(v) : text.number(v) + (spec.unit ? ' ' + spec.unit : ''); } };
  });
  if (spec.categories && !horizontal) [].concat(option.xAxis).forEach(function (axis) {
    axis.axisLabel.width = Math.max(40, .8 * width / spec.categories);
  });
  option.toolbox.feature.saveAsImage.title = text.de ? 'Als Bild speichern' : 'Save as image';
  if (option.legend.selector) {
    option.legend.selector[0].title = text.de ? 'Alle' : 'All';
    option.legend.selector[1].title = text.de ? 'Keine' : 'None';
  }

  // Series names are the legend entries, so an interval hides with its values.
  option.tooltip = { trigger: spec.trigger, confine: true, formatter: function (params) {
    var points = [].concat(params).filter(function (p) { return p.data && p.data.korap; });
    if (!points.length) return '';
    if (spec.locale.kind === 'box') return text.boxTooltip(points[0].data.korap, points[0].name, !!points[0].data.webUIRequestUrl);
    var head = spec.trigger === 'axis' ? points[0].data.korap.year : points[0].name;
    return '<b>' + text.escape(head) + '</b><br/>' + points.map(function (p) {
      return (spec.trigger === 'axis' ? p.marker : '') + text.tooltip(p.data.korap, { name: p.seriesName });
    }).join('<br/>');
  } };

  // Whisker on its bar in both orientations, leaving out the hidden groups.
  function whisker(params, api) {
    var low = api.value(1), high = api.value(2);
    if (isNaN(low) || isNaN(high)) return;
    var visible = api.currentSeriesIndices().filter(function (i) { return i < frequencies; });
    var slot = visible.indexOf(params.seriesIndex - frequencies);
    if (slot < 0) return;
    var bar = option.series[params.seriesIndex - frequencies];
    var layout = api.barLayout({ count: visible.length, barGap: bar.barGap, barCategoryGap: bar.barCategoryGap })[slot];
    var category = api.value(0), across = horizontal ? 1 : 0;
    var from = api.coord(horizontal ? [low, category] : [category, low]);
    var to = api.coord(horizontal ? [high, category] : [category, high]);
    from[across] += layout.offsetCenter; to[across] += layout.offsetCenter;
    var half = Math.min(layout.width * .3, 8);
    function line(a, b) {
      return { type: 'line', shape: { x1: a[0], y1: a[1], x2: b[0], y2: b[1] }, style: { stroke: ink, lineWidth: 1.2 } };
    }
    function cap(p) {
      var a = p.slice(), b = p.slice();
      a[across] -= half; b[across] += half;
      return line(a, b);
    }
    return { type: 'group', children: [line(from, to), cap(from), cap(to)] };
  }

  // Box with whiskers and median, or a single value beyond, next to the visible boxes of its category.
  function box(params, api) {
    var panel = Math.floor(params.seriesIndex / frequencies);
    var visible = api.currentSeriesIndices().filter(function (i) { return Math.floor(i / frequencies) === panel; });
    var slot = visible.indexOf(params.seriesIndex);
    if (slot < 0) return;
    var layout = api.barLayout({ count: visible.length, barGap: '25%', barCategoryGap: '35%', barMaxWidth: 48 })[slot];
    var across = horizontal ? 1 : 0;
    function at(i, shift) {
      var p = api.coord(horizontal ? [api.value(i), api.value(0)] : [api.value(0), api.value(i)]);
      p[across] += layout.offsetCenter + shift;
      return p;
    }
    var color = api.visual('color'), half = layout.width / 2;
    if (api.value(6)) {
      var center = at(1, 0);
      return { type: 'circle', shape: { cx: center[0], cy: center[1], r: 2.5 }, style: { fill: color, opacity: .5 } };
    }
    function line(a, b, width) {
      return { type: 'line', shape: { x1: a[0], y1: a[1], x2: b[0], y2: b[1] }, style: { stroke: color, lineWidth: width } };
    }
    var from = at(2, -half), to = at(4, half), shape = {};
    shape.x = Math.min(from[0], to[0]); shape.width = Math.abs(to[0] - from[0]);
    shape.y = Math.min(from[1], to[1]); shape.height = Math.abs(to[1] - from[1]);
    return { type: 'group', children: [
      line(at(1, 0), at(2, 0), 1.5), line(at(4, 0), at(5, 0), 1.5),
      line(at(1, -half / 2), at(1, half / 2), 1.5), line(at(5, -half / 2), at(5, half / 2), 1.5),
      { type: 'rect', shape: shape, style: { fill: color, fillOpacity: .4, stroke: color, lineWidth: 1.5 } },
      line(at(3, -half), at(3, half), 2.5)
    ] };
  }
  option.series.forEach(function (s) { if (s.type === 'custom') s.renderItem = spec.locale.kind === 'box' ? box : whisker; });

  // A theme, whether chosen before or after this function, brings its own palette.
  if (!x.theme) option.color = spec.palette;
  var theirs = Object.assign({}, x.opts);
  if (theirs.legend && option.legend.selector) {
    // e_legend() keeps the all/none buttons unless they are set explicitly.
    theirs.legend = [].concat(theirs.legend).map(function (l) {
      return Object.assign({ selector: option.legend.selector }, l);
    });
  }
  // Room for a title that was added with e_title()
  var added = theirs.title && [].concat(theirs.title)[0];
  if (added && added.text && !option.title[0].text) grids.concat(option.title.slice(2)).forEach(function (part) { part.top += 34; });
  var ink = '#737373';
  chart.setOption(option, true);
  if (Object.keys(theirs).length) chart.setOption(theirs);
  var paper = chart.getOption().backgroundColor;
  var rgb = typeof paper === 'string' ? echarts.color.parse(paper) : null;
  if (rgb && rgb[3] > 0 && (rgb[0] + rgb[1] + rgb[2]) / 3 < 128) {
    ink = '#C8C8C8';
    chart.resize(); // redraw the whiskers
  }

  // ECharts offers "all" and "inverse" buttons only: let the second one mean none.
  chart.on('legendinverseselect', function (e) {
    var shown = Object.keys(e.selected).filter(function (name) { return e.selected[name]; });
    if (shown.length) chart.dispatchAction({ type: 'legendUnSelect', batch: shown.map(function (name) { return { name: name }; }) });
  });
}
