window.addEventListener('hashchange', function(event) {
  var newAnchor = event.newURL.replace(/.*#/, '');
  Array.prototype.slice.apply(document.getElementsByClassName('selected')).forEach(function (element) {
    element.classList.remove('selected');
  });
  Array.prototype.slice.apply(document.getElementsByTagName('a')).forEach(function (element) {
    var name = element.attributes.name && element.attributes.name.textContent;
    var href = element.attributes.href && element.attributes.href.textContent;
    if (name === newAnchor || href === '#' + newAnchor) {
      element.classList.add('selected');
    }
  });
});
