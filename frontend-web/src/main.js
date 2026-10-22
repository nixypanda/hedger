import "./main.css";
import { Elm } from "./Main.elm";

const CONFIG_KEY = "hedger-preferences-v1";
const readSaved = () => {
  try { return JSON.parse(localStorage.getItem(CONFIG_KEY) || "null"); }
  catch (_) { return null; }
};
const normalizeTheme = (value) => value === "dark" ? "dark" : "light";
const applyTheme = (mode) => {
  const normalized = normalizeTheme(mode);
  document.documentElement.dataset.theme = normalized;
  document.documentElement.style.colorScheme = normalized;
  document.querySelector('meta[name="theme-color"]')?.setAttribute("content", normalized === "dark" ? "#0f172a" : "#703ee8");
};

const saved = readSaved();
const initialMode = normalizeTheme(saved?.themeMode);
applyTheme(initialMode);
const isIos = /iphone|ipad|ipod/i.test(navigator.userAgent) || (navigator.platform === "MacIntel" && navigator.maxTouchPoints > 1);
const isStandalone = matchMedia("(display-mode: standalone)").matches || navigator.standalone === true;
const app = Elm.Main.init({ node: document.getElementById("app"), flags: { isIos, isStandalone, viewportWidth: innerWidth, viewportHeight: innerHeight, initialMode } });

app.ports.storeConfig.subscribe((preferences) => {
  try { localStorage.setItem(CONFIG_KEY, JSON.stringify(preferences)); } catch (_) {}
  applyTheme(preferences.themeMode);
});
if (saved) app.ports.configLoaded.send(saved);

let waitingWorker = null;
if (!import.meta.env.PROD && "serviceWorker" in navigator) {
  navigator.serviceWorker.getRegistrations().then((registrations) => registrations.forEach((registration) => registration.unregister()));
  if ("caches" in window) {
    caches.keys().then((keys) => keys.filter((key) => key.startsWith("hedger-")).forEach((key) => caches.delete(key)));
  }
}
app.ports.registerServiceWorker.subscribe(() => {
  if (!import.meta.env.PROD || !("serviceWorker" in navigator)) return;
  navigator.serviceWorker.register("/sw.js").then((registration) => {
    if (registration.waiting) { waitingWorker = registration.waiting; app.ports.swStatus.send("update-ready"); }
    registration.addEventListener("updatefound", () => {
      const worker = registration.installing;
      worker?.addEventListener("statechange", () => {
        if (worker.state === "installed" && navigator.serviceWorker.controller) { waitingWorker = worker; app.ports.swStatus.send("update-ready"); }
        else app.ports.swStatus.send(worker.state);
      });
    });
  });
});
navigator.serviceWorker?.addEventListener("controllerchange", () => { app.ports.swStatus.send("controllerchange"); location.reload(); });
app.ports.skipWaiting.subscribe(() => waitingWorker?.postMessage({ type: "SKIP_WAITING" }));

let deferredPrompt;
addEventListener("beforeinstallprompt", (event) => { event.preventDefault(); deferredPrompt = event; app.ports.promptInstallReady.send(null); });
app.ports.promptInstall.subscribe(() => { if (deferredPrompt) deferredPrompt.prompt().finally(() => { deferredPrompt = null; }); });

// The backend streams a "revision" event whenever the ledger is reloaded on
// disk. EventSource reconnects on its own, so no retry logic is needed here.
const journalEvents = new EventSource("/api/v1/events");
journalEvents.addEventListener("revision", (event) => app.ports.journalChanged.send(event.data));
