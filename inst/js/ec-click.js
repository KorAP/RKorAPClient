function (el) {
  var chart = get_e_charts(el.id);
  function url(p) {
    var u = p.data && p.data.webUIRequestUrl;
    return typeof u === 'string' && /^https?:\/\/[^\s<>"']+$/i.test(u) ? u : null;
  }
  chart.on('click', function (p) {
    var u = url(p);
    if (!u) return;
    var w = window.open(u, 'korap');
    if (w) { try { w.focus(); } catch (e) {} }
  });
  chart.on('mousemove', function (p) { if (!url(p)) chart.getZr().setCursorStyle('default'); });
}
