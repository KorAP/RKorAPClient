const fs = require('fs');
const vm = require('vm');
const assert = require('assert/strict');
const load = fs.readFileSync(process.argv[2], 'utf8');
const payload = JSON.parse(fs.readFileSync(process.argv[3], 'utf8'));
function merge(target, source) {
  for (const [key, value] of Object.entries(source || {})) {
    if (value && typeof value === 'object' && !Array.isArray(value)) {
      target[key] = merge(target[key] || {}, value);
    } else target[key] = value;
  }
  return target;
}
function chartModel(widget) {
  const options = merge(merge({title:{text:null},caption:{text:null}}, widget.theme), widget.hc_opts);
  const chart = {options, xAxis:[], yAxis:[], series:[], redraw(){}, update(change){merge(options, change);}};
  const x = {categories:options.xAxis.categories || [], setCategories(c){this.categories=c;}};
  chart.xAxis = [x];
  chart.yAxis = [{options:options.yAxis, update(change){merge(this.options, change);}}];
  chart.series = options.series.map((raw, index) => {
    const series = {index, name:raw.name, options:merge(merge({}, options.plotOptions.series), raw)};
    function points() {
      series.data = series.options.data.map(p => p === null ? null : Object.assign({}, p, {options:p}));
    }
    series.update = function(change){merge(this.options, change); this.name=this.options.name; points();};
    points(); return series;
  });
  return chart;
}
function localized(name, languages, forced) {
  const chart = chartModel(JSON.parse(JSON.stringify(payload[name])));
  if (forced) chart.options.chart.korapLocale.language=forced;
  const callback = vm.runInNewContext('(' + load + ')', {navigator:{languages}, Intl});
  callback.call(chart); return chart;
}
let de = localized('frequency', ['de-DE','en-US']);
let text = de.series[0].data[0].tooltipText;
assert.match(text, /Relative Häufigkeit/);
assert.match(text, /Absolute Häufigkeit: 3/);
assert.match(text, /Korpusgröße \(Tokens\): 50\.000\.000\.000/);
assert.match(text, /0,00006/);
assert.match(text, /IPM/);
assert.match(text, /Konfidenzintervall/);
assert.equal(de.korapLanguage, 'de');
let en = localized('frequency', ['en-US','de-DE']);
assert.match(en.series[0].data[0].tooltipText, /Corpus size \(tokens\): 50,000,000,000/);
assert.match(en.series[0].data[0].tooltipText, /0\.00006/);
assert.equal(localized('frequency',['fr-FR','de-AT','en-US']).korapLanguage,'de');
assert.equal(localized('frequency',['fr-FR']).korapLanguage,'en');
assert.equal(localized('frequency',['de-DE'],'en').korapLanguage,'en');
assert.equal(de.options.tooltip.pointFormat,'{point.tooltipText}');
let custom = chartModel(JSON.parse(JSON.stringify(payload.frequency)));
custom.options.tooltip.pointFormat='custom {point.count}';
vm.runInNewContext('(' + load + ')',{navigator:{languages:['de']},Intl}).call(custom);
assert.equal(custom.options.tooltip.pointFormat,'custom {point.count}');
de = localized('alternatives',['de']);
text = de.series[0].data[0].tooltipText;
assert.match(text,/Gesamthäufigkeit der Varianten: 100/);
assert.doesNotMatch(text,/Korpusgröße/);
assert.doesNotMatch(text,/IPM/);
de = localized('year',['de']);
assert.match(de.series[0].data[0].tooltipText,/Konfidenzintervall/);
assert.match(de.series[0].data[0].tooltipText,/Korpusgröße/);
de = localized('dispersion',['de']);
assert.equal(de.series[0].name,'Korpustokens');
assert.match(de.options.title.text,/Verteilung nach Quelle/);
assert.match(de.options.caption.text,/Teilkorpora/);
assert.match(de.series[1].data[0].tooltipText,/Anteil an der Gesamthäufigkeit/);
assert.match(de.series[1].data[0].tooltipText,/Relative Häufigkeit/);
assert.match(de.series[1].data[0].tooltipText,/IPM/);
assert.doesNotMatch(de.series[1].data[0].tooltipText,/<s>/);
assert.match(de.series[1].data[0].tooltipText,/&lt;s&gt;/);
assert.match(de.series[1].data[1].tooltipLabel,/Übriges Teilkorpus/);
assert.match(de.series[1].data[1].tooltipText,/Teilkorpus zusammengefasst/);
assert.match(de.series[2].data[0].tooltipText,/Geschätzte absolute Häufigkeit/);
assert.match(de.series[2].data[0].tooltipText,/Stichprobe/);
for (const language of ['de','en']) {
  for (const name of ['frequencyNoCI','yearNoCI']) {
    const chart = localized(name,[language]);
    assert.equal(chart.series.length,1);
    assert.doesNotMatch(chart.series[0].data[0].tooltipText,/Confidence interval|Konfidenzintervall/);
    assert.match(chart.series[0].data[0].tooltipText,/IPM/);
  }
}
const calls=[];
const window={open(url,target){calls.push([url,target]);return new Proxy({}, {get(){throw Error('cross-origin access');}});}};
const click=vm.runInNewContext('('+payload.frequency.hc_opts.plotOptions.series.point.events.click+')',{window});
click.call({webUIRequestUrl:'https://example.invalid/one'});
click.call({webUIRequestUrl:'https://example.invalid/two'});
click.call({webUIRequestUrl:'javascript:alert(1)'});
assert.deepEqual(calls,[['https://example.invalid/one','korap'],['https://example.invalid/two','korap']]);
console.log('Language selection, localized scientific tooltips, rare values, custom formats and repeated cross-origin clicks passed.');
