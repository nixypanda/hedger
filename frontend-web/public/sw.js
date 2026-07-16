const APP_SHELL_CACHE = "hedger-app-shell-v1";
const PRECACHE_URLS = ["/", "/index.html", "/manifest.webmanifest", "/icon.svg"];

self.addEventListener("install", (event) => event.waitUntil(caches.open(APP_SHELL_CACHE).then((cache) => cache.addAll(PRECACHE_URLS))));
self.addEventListener("activate", (event) => event.waitUntil(caches.keys().then((names) => Promise.all(names.filter((name) => name.startsWith("hedger-") && name !== APP_SHELL_CACHE).map((name) => caches.delete(name)))).then(() => self.clients.claim())));
self.addEventListener("message", (event) => { if (event.data?.type === "SKIP_WAITING") self.skipWaiting(); });
self.addEventListener("fetch", (event) => {
  if (event.request.method !== "GET") return;
  const url = new URL(event.request.url);
  if (url.origin !== self.location.origin || url.pathname === "/api" || url.pathname.startsWith("/api/")) return;
  if (event.request.mode === "navigate") {
    event.respondWith(fetch(event.request).then((response) => { const clone = response.clone(); caches.open(APP_SHELL_CACHE).then((cache) => cache.put("/index.html", clone)); return response; }).catch(() => caches.match("/index.html")));
    return;
  }
  event.respondWith(caches.match(event.request).then((cached) => cached || fetch(event.request).then((response) => { if (response.ok) caches.open(APP_SHELL_CACHE).then((cache) => cache.put(event.request, response.clone())); return response; })));
});
