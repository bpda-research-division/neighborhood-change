window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label.label[i].toString());
    }
  });
};

// Add CSS to hide tooltips when window isn't focused
var style = document.createElement('style');
style.id = 'tooltip-visibility-control';
document.head.appendChild(style);

function clearTooltipsNow() {
  // Find and hide the entire tooltip pane
  var tooltipPanes = document.querySelectorAll('.leaflet-tooltip-pane, .leaflet-overlay-pane');
  tooltipPanes.forEach(function(pane) {
    pane.style.display = 'none';
    void pane.offsetHeight;
    pane.style.display = '';
  });
  
  // Remove individual tooltips
  document.querySelectorAll('.leaflet-tooltip').forEach(function(el) {
    el.parentNode.removeChild(el);
  });
  
  // RESET POLYGON STYLES
  document.querySelectorAll('.leaflet-interactive').forEach(function(path) {
    path.setAttribute('stroke-width', '1');
    path.setAttribute('stroke', '#818185');
    path.style.strokeWidth = '1';
    path.style.stroke = '#818185';
  });
  
  // Clear any transform/position styles that might be cached
  var mapPanes = document.querySelectorAll('.leaflet-map-pane');
  mapPanes.forEach(function(pane) {
    var transform = pane.style.transform;
    pane.style.transform = 'none';
    void pane.offsetHeight;
    pane.style.transform = transform;
  });
}

// Hide tooltips when window loses focus
window.addEventListener('blur', function() {
  style.textContent = '.leaflet-tooltip { display: none !important; }';
  clearTooltipsNow();
});

// Show tooltips when window gains focus
window.addEventListener('focus', function() {
  style.textContent = '';
});

document.addEventListener('mouseleave', clearTooltipsNow);

setInterval(function() {
  var hovering = document.querySelector('.leaflet-container:hover');
  if (!hovering && document.querySelectorAll('.leaflet-tooltip').length > 0) {
    clearTooltipsNow();
  }
}, 150);