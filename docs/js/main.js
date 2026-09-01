(function () {
  "use strict";

  /* ---------- theme toggle (light / dark, persisted) ---------- */
  var root = document.documentElement;
  var STORAGE_KEY = "slick-theme";
  var toggleBtn = document.querySelector("[data-theme-toggle]");

  function systemPrefersDark() {
    return window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;
  }

  function applyTheme(theme) {
    root.setAttribute("data-theme", theme);
    if (toggleBtn) {
      toggleBtn.textContent = theme === "dark" ? "☀" : "☾";
      toggleBtn.setAttribute("aria-label", theme === "dark" ? "Switch to light mode" : "Switch to dark mode");
    }
  }

  var saved = null;
  try { saved = localStorage.getItem(STORAGE_KEY); } catch (e) { /* storage disabled */ }
  applyTheme(saved || (systemPrefersDark() ? "dark" : "light"));

  if (toggleBtn) {
    toggleBtn.addEventListener("click", function () {
      var next = root.getAttribute("data-theme") === "dark" ? "light" : "dark";
      applyTheme(next);
      try { localStorage.setItem(STORAGE_KEY, next); } catch (e) { /* storage disabled */ }
    });
  }

  /* ---------- reading progress bar (post pages only) ---------- */
  var progress = document.querySelector("[data-progress]");
  var article = document.querySelector("[data-post-body]");
  if (progress && article) {
    var onScroll = function () {
      var rect = article.getBoundingClientRect();
      var total = rect.height - window.innerHeight;
      var scrolled = Math.min(Math.max(-rect.top, 0), Math.max(total, 1));
      var pct = total > 0 ? (scrolled / total) * 100 : 0;
      progress.style.width = pct + "%";
    };
    window.addEventListener("scroll", onScroll, { passive: true });
    window.addEventListener("resize", onScroll);
    onScroll();
  }

  /* ---------- query-string pagination (listing pages only) ---------- */
  var entries = document.querySelectorAll(".entry");
  var paginations = document.querySelectorAll("[data-pagination]");
  var postsPerPage = paginations.length ? Number(paginations[0].getAttribute("data-posts-per-page")) : 0;
  var currentPage = 0;

  function pageUrl(page) {
    var nextUrl = new URL(window.location.href);
    nextUrl.searchParams.set("page", String(page));
    return nextUrl.pathname + nextUrl.search + nextUrl.hash;
  }

  function requestedPage() {
    var value = new URL(window.location.href).searchParams.get("page");
    if (value === null || !/^(0|[1-9][0-9]*)$/.test(value)) return null;
    return Number(value);
  }

  function renderPage(page) {
    if (!paginations.length || !postsPerPage) return;
    var pageCount = Math.max(1, Math.ceil(entries.length / postsPerPage));
    var requested = page === null ? 0 : page;
    currentPage = Math.min(Math.max(requested, 0), pageCount - 1);

    if (page === null || requested !== currentPage) {
      window.location.replace(pageUrl(currentPage));
      return;
    }

    entries.forEach(function (entry) { entry.classList.add("is-page-hidden"); });
    Array.prototype.slice.call(entries, currentPage * postsPerPage, (currentPage + 1) * postsPerPage)
      .forEach(function (entry) { entry.classList.remove("is-page-hidden"); });

    paginations.forEach(function (pagination) {
      pagination.hidden = pageCount <= 1;
      var pageStatus = pagination.querySelector("[data-page-status]");
      var previousPageBtn = pagination.querySelector("[data-page-previous]");
      var nextPageBtn = pagination.querySelector("[data-page-next]");
      if (pageStatus) pageStatus.textContent = "Page " + (currentPage + 1) + " of " + pageCount;
      if (previousPageBtn) {
        previousPageBtn.href = pageUrl(Math.max(currentPage - 1, 0));
        previousPageBtn.setAttribute("aria-disabled", String(currentPage === 0));
        previousPageBtn.classList.toggle("is-disabled", currentPage === 0);
      }
      if (nextPageBtn) {
        nextPageBtn.href = pageUrl(Math.min(currentPage + 1, pageCount - 1));
        nextPageBtn.setAttribute("aria-disabled", String(currentPage === pageCount - 1));
        nextPageBtn.classList.toggle("is-disabled", currentPage === pageCount - 1);
      }
    });
  }
  renderPage(requestedPage());

  /* ---------- copy-to-clipboard on code blocks ---------- */
  document.querySelectorAll(".post-body pre").forEach(function (pre) {
    var btn = document.createElement("button");
    btn.className = "code-copy";
    btn.type = "button";
    btn.textContent = "copy";
    btn.setAttribute("aria-label", "Copy code to clipboard");
    pre.style.position = "relative";
    btn.style.position = "absolute";
    btn.style.top = "8px";
    btn.style.right = "8px";
    btn.style.fontFamily = "var(--font-mono)";
    btn.style.fontSize = "0.72rem";
    btn.style.padding = "3px 8px";
    btn.style.border = "1px solid var(--line)";
    btn.style.borderRadius = "3px";
    btn.style.background = "var(--paper)";
    btn.style.color = "var(--ink-soft)";
    btn.style.cursor = "pointer";
    btn.addEventListener("click", function () {
      var code = pre.querySelector("code");
      var text = code ? code.textContent : pre.textContent;
      if (navigator.clipboard) {
        navigator.clipboard.writeText(text).then(function () {
          btn.textContent = "copied";
          setTimeout(function () { btn.textContent = "copy"; }, 1200);
        });
      }
    });
    pre.appendChild(btn);
  });
})();
