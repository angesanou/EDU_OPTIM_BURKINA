// EDU-OPTIM BURKINA — interactions (navigation, raccourcis). Aucun CDN.
(function () {
  function activer(page) {
    document.querySelectorAll('.nav-lien').forEach(function (a) {
      var on = a.getAttribute('data-page') === page;
      a.classList.toggle('actif', on);
      a.setAttribute('aria-current', on ? 'page' : 'false');
    });
  }
  function aller(page) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue('nav_page', page, { priority: 'event' });
    activer(page);
    window.scrollTo({ top: 0, behavior: 'smooth' });
    if (window.innerWidth < 992) {
      var l = document.querySelector('.bslib-sidebar-layout');
      var t = document.querySelector('.bslib-sidebar-layout > .collapse-toggle');
      if (t && l && !l.classList.contains('sidebar-collapsed')) t.click();
    }
  }
  document.addEventListener('click', function (e) {
    var a = e.target.closest('[data-page]');
    if (!a) return;
    e.preventDefault();
    aller(a.getAttribute('data-page'));
  });
  document.addEventListener('keydown', function (e) {
    var a = e.target.closest && e.target.closest('[data-page]');
    if (a && (e.key === 'Enter' || e.key === ' ')) { e.preventDefault(); aller(a.getAttribute('data-page')); }
  });
  document.addEventListener('DOMContentLoaded', function () {
    var b = document.createElement('div'); b.className = 'shiny-busy-indic'; document.body.appendChild(b);
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('activer_nav', activer);
  });
  window.eduOptimAller = aller;
})();
