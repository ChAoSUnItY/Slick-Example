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

  /* ---------- tag cloud filtering (index page only — no-ops elsewhere) ---------- */
  var tagPills = document.querySelectorAll(".tag-pill");
  var entries = document.querySelectorAll(".entry");
  var statusBar = document.querySelector("[data-filter-status]");
  var statusTagLabel = document.querySelector("[data-filter-tag]");
  var clearBtn = document.querySelector("[data-clear-filter]");

  function slugify(s) { return (s || "").trim().toLowerCase(); }

  function applyFilter(tagSlug) {
    if (!tagSlug) {
      entries.forEach(function (e) { e.classList.remove("is-hidden"); });
      tagPills.forEach(function (p) { p.classList.remove("is-active"); });
      if (statusBar) statusBar.hidden = true;
      return;
    }
    entries.forEach(function (e) {
      var entryTags = (e.getAttribute("data-tags") || "")
        .split(/\s+/).filter(Boolean).map(slugify);
      e.classList.toggle("is-hidden", entryTags.indexOf(tagSlug) === -1);
    });
    tagPills.forEach(function (p) {
      p.classList.toggle("is-active", slugify(p.getAttribute("data-tag")) === tagSlug);
    });
    if (statusBar) {
      statusBar.hidden = false;
      if (statusTagLabel) statusTagLabel.textContent = tagSlug;
    }
  }

  if (tagPills.length) {
    tagPills.forEach(function (pill) {
      pill.addEventListener("click", function (evt) {
        evt.preventDefault();
        var slug = slugify(pill.getAttribute("data-tag"));
        var next = pill.classList.contains("is-active") ? "" : slug;
        history.replaceState(null, "", next ? "#tag-" + next : location.pathname);
        applyFilter(next);
      });
    });
    if (clearBtn) {
      clearBtn.addEventListener("click", function () {
        history.replaceState(null, "", location.pathname);
        applyFilter("");
      });
    }
    var initialTag = location.hash.replace(/^#tag-/, "");
    if (initialTag) applyFilter(slugify(decodeURIComponent(initialTag)));
  }

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