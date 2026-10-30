#!/usr/bin/env node

// Capture the theme-aware screenshots used by README.md.
//
// Run `just dev` first, then `just screenshots`. This deliberately uses the
// Chrome DevTools Protocol directly instead of adding a browser automation
// dependency. Set CHROME to override Chrome/Chromium discovery, and HEDGER_URL
// to capture from a different frontend address.

import { spawn, spawnSync } from "node:child_process";
import { once } from "node:events";
import { accessSync, constants, promises as fs } from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const SCRIPT_DIR = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(SCRIPT_DIR, "../..");
const OUTPUT_ROOT = path.join(REPO_ROOT, "docs/images/readme");
const BASE_URL = normalizeBaseUrl(process.env.HEDGER_URL ?? "http://localhost:1234");
const THEMES = ["light", "dark"];
const PHONE_WIDTH = 390;
const PHONE_HEIGHT = 844;
const PHONE_GAP = 16;
const PHONE_COUNT = 4;

const args = parseArgs(process.argv.slice(2));
const selectedNames = new Set(
  (args.get("--only") ?? "")
    .split(",")
    .map((value) => value.trim())
    .filter(Boolean),
);
const selectedThemes = args.has("--theme")
  ? [args.get("--theme")]
  : THEMES;

const desktopShots = [
  {
    name: "net-worth-desktop",
    path: "/net-worth",
    width: 1600,
    height: 1000,
    waitFor: "Balance over time",
  },
  {
    name: "investments-desktop",
    path: "/investments",
    width: 1600,
    height: 1000,
    waitFor: "Portfolio value",
  },
  {
    name: "cashflow-desktop",
    path: "/cashflow",
    width: 1600,
    height: 1000,
    waitFor: "Cash flow over time",
  },
  {
    name: "accounts-desktop",
    path: "/accounts",
    width: 1600,
    height: 1000,
    waitFor: "liabilities",
    expandAccount: "assets",
  },
  {
    name: "account-register-tablet",
    path: `/accounts/${encodeURIComponent("assets:equity")}`,
    width: 820,
    height: 1024,
    waitFor: "Account register",
  },
];

const analysisShots = [
  {
    name: "net-worth-monthly-analysis-desktop",
    path: "/net-worth",
    width: 1600,
    height: 1200,
    waitFor: "Composition and activity",
    granularity: "monthly",
    scrollTo: "Composition and activity",
  },
  {
    name: "net-worth-yearly-analysis-desktop",
    path: "/net-worth",
    width: 1600,
    height: 1200,
    waitFor: "Composition and activity",
    granularity: "yearly",
    scrollTo: "Composition and activity",
  },
  {
    name: "cashflow-monthly-analysis-desktop",
    path: "/cashflow",
    width: 1600,
    height: 1200,
    waitFor: "Composition and activity",
    granularity: "monthly",
    scrollTo: "Composition and activity",
  },
  {
    name: "cashflow-yearly-analysis-desktop",
    path: "/cashflow",
    width: 1600,
    height: 1200,
    waitFor: "Composition and activity",
    granularity: "yearly",
    scrollTo: "Composition and activity",
  },
];

const phonePanels = [
  { name: "net-worth", path: "/net-worth", waitFor: "Balance over time" },
  { name: "investments", path: "/investments", waitFor: "Portfolio value" },
  { name: "cashflow", path: "/cashflow", waitFor: "Cash flow over time" },
  { name: "transactions", path: "/transactions", waitFor: "hledger query" },
];

let chrome = null;
let chromeProfile = null;
let cdp = null;

async function main() {
  try {
    await ensureServerRunning();
    chromeProfile = await fs.mkdtemp(
      path.join(os.tmpdir(), "hedger-readme-shots-"),
    );
    chrome = await launchChrome(chromeProfile);
    cdp = await connectToPage(chrome.port);
    await cdp.send("Page.enable");
    await cdp.send("Runtime.enable");
    await cdp.send("Network.enable");
    await cdp.send("Page.addScriptToEvaluateOnNewDocument", {
      source: `window.addEventListener(
        "beforeinstallprompt",
        (event) => {
          event.preventDefault();
          event.stopImmediatePropagation();
        },
        true,
      );`,
    });
    const network = trackApiRequests(cdp);

    for (const theme of selectedThemes) {
      if (!THEMES.includes(theme)) {
        throw new Error(`Unknown theme "${theme}". Use light or dark.`);
      }

      await navigate(cdp, `${BASE_URL}/net-worth`, network);
      await setTheme(cdp, theme);

      for (const shot of desktopShots) {
        if (!wanted(shot.name)) continue;
        await captureDesktopShot(cdp, network, shot, theme);
      }

      for (const shot of analysisShots) {
        if (!wanted(shot.name)) continue;
        await captureAnalysisShot(cdp, network, shot, theme);
      }

      if (wanted("phone-views")) {
        const panels = [];
        for (const panel of phonePanels) {
          panels.push(await capturePhonePanel(cdp, network, panel));
        }
        const composite = await composePhonePanels(cdp, panels);
        await writeImage(
          path.join(OUTPUT_ROOT, theme, "phone-views.png"),
          composite,
          PHONE_WIDTH * PHONE_COUNT + PHONE_GAP * (PHONE_COUNT - 1),
          PHONE_HEIGHT,
        );
      }
    }

    console.log(
      `\nScreenshots written to ${path.relative(REPO_ROOT, OUTPUT_ROOT)}/`,
    );
  } finally {
    await cleanup();
  }
}

function parseArgs(values) {
  const parsed = new Map();
  for (const value of values) {
    const [key, ...rest] = value.split("=");
    parsed.set(key, rest.join("="));
  }
  return parsed;
}

function normalizeBaseUrl(value) {
  return value.replace(/\/+$/, "");
}

function wanted(name) {
  return selectedNames.size === 0 || selectedNames.has(name);
}

function findChrome() {
  const explicit = process.env.CHROME;
  if (explicit) return explicit;

  const candidates =
    process.platform === "darwin"
      ? [
          "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
          "/Applications/Chromium.app/Contents/MacOS/Chromium",
          "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary",
        ]
      : [];

  for (const candidate of candidates) {
    try {
      accessSync(candidate, constants.X_OK);
      return candidate;
    } catch {
      // Try the next candidate.
    }
  }

  for (const command of [
    "google-chrome",
    "google-chrome-stable",
    "chromium",
    "chromium-browser",
    "chrome",
  ]) {
    const result = spawnSync("which", [command]);
    if (result.status === 0) return result.stdout.toString().trim();
  }

  throw new Error(
    "Chrome/Chromium was not found. Set CHROME=/path/to/chrome and retry.",
  );
}

async function ensureServerRunning() {
  const deadline = Date.now() + 10_000;
  let lastError = null;

  while (Date.now() < deadline) {
    try {
      const response = await fetch(`${BASE_URL}/`);
      if (response.ok) return;
      lastError = new Error(`HTTP ${response.status}`);
    } catch (error) {
      lastError = error;
    }
    await delay(200);
  }

  throw new Error(
    `Could not reach ${BASE_URL}. Start the app with \`just dev\` first.` +
      (lastError ? ` (${lastError.message})` : ""),
  );
}

async function launchChrome(profileDir) {
  const chromePath = findChrome();
  const child = spawn(
    chromePath,
    [
      "--headless=new",
      "--disable-gpu",
      "--disable-dev-shm-usage",
      "--hide-scrollbars",
      "--no-first-run",
      "--no-default-browser-check",
      "--disable-background-networking",
      "--disable-component-update",
      "--disable-default-apps",
      "--disable-sync",
      "--disable-translate",
      "--metrics-recording-only",
      "--mute-audio",
      "--safebrowsing-disable-auto-update",
      "--no-service-autorun",
      "--password-store=basic",
      "--use-mock-keychain",
      "--disable-features=BackForwardCache,Translate,MediaRouter,OptimizationHints",
      "--remote-allow-origins=*",
      "--remote-debugging-port=0",
      `--user-data-dir=${profileDir}`,
      "--window-size=1600,1200",
      "--force-device-scale-factor=1",
      "about:blank",
    ],
    { stdio: ["ignore", "ignore", "pipe"] },
  );

  let stderr = "";
  child.stderr.on("data", (chunk) => {
    stderr += chunk.toString();
  });

  const exited = once(child, "exit").then(([code, signal]) => {
    throw new Error(
      `Chrome exited before the DevTools endpoint was ready` +
        ` (code ${code ?? "null"}, signal ${signal ?? "none"}).\n${stderr}`,
    );
  });
  exited.catch(() => {});

  const portFile = path.join(profileDir, "DevToolsActivePort");
  const deadline = Date.now() + 15_000;

  while (Date.now() < deadline) {
    try {
      const contents = await fs.readFile(portFile, "utf8");
      const [port] = contents.split("\n");
      if (port) {
        return { child, port: Number(port), stderr: () => stderr };
      }
    } catch {
      // Chrome has not written the port file yet.
    }
    await delay(100);
  }

  child.kill("SIGTERM");
  throw new Error(
    `Timed out waiting for Chrome's DevTools endpoint.\n${stderr}`,
  );
}

async function connectToPage(port) {
  const targets = await (await fetch(`http://127.0.0.1:${port}/json/list`)).json();
  const page = targets.find((target) => target.type === "page");
  if (!page?.webSocketDebuggerUrl) {
    throw new Error("Chrome did not expose a page target.");
  }

  const client = new CdpClient(page.webSocketDebuggerUrl);
  await client.connect();
  return client;
}

function trackApiRequests(client) {
  const inFlight = new Set();
  let loaderId = null;
  let tracking = false;
  client.on("Network.requestWillBeSent", ({ requestId, request, loaderId: requestLoaderId }) => {
    const url = request.url;
    if (!tracking) return;
    if (loaderId && requestLoaderId && requestLoaderId !== loaderId) return;
    if (url.includes("/api/v1/") && !url.includes("/api/v1/events")) {
      inFlight.add(requestId);
    }
  });
  const finish = ({ requestId }) => {
    inFlight.delete(requestId);
  };
  client.on("Network.loadingFinished", finish);
  client.on("Network.loadingFailed", finish);

  return {
    start(nextLoaderId) {
      inFlight.clear();
      loaderId = nextLoaderId || null;
      tracking = true;
    },
    async waitForIdle(timeout = 20_000) {
      const deadline = Date.now() + timeout;
      while (Date.now() < deadline) {
        if (inFlight.size === 0) {
          await delay(350);
          if (inFlight.size === 0) return;
        }
        await delay(50);
      }
      throw new Error("Timed out waiting for API requests to settle.");
    },
  };
}

async function captureDesktopShot(client, network, shot, theme) {
  await setViewport(client, shot.width, shot.height);
  await navigate(client, `${BASE_URL}${shot.path}`, network);
  await waitForText(client, shot.waitFor);
  await network.waitForIdle();

  if (shot.expandAccount) {
    await expandAccount(client, shot.expandAccount);
  }

  await removeDevChrome(client);
  await scrollToTop(client);
  await injectStaticStyles(client);
  await delay(250);

  const image = await capture(client);
  await writeImage(
    path.join(OUTPUT_ROOT, theme, `${shot.name}.png`),
    image,
    shot.width,
    shot.height,
  );
}

async function captureAnalysisShot(client, network, shot, theme) {
  await setViewport(client, shot.width, shot.height);
  await navigate(client, `${BASE_URL}${shot.path}`, network);
  await waitForText(client, shot.waitFor);
  await network.waitForIdle();

  if (shot.granularity === "yearly") {
    await clickButton(client, "Y");
    await waitForText(client, "FY ");
    await network.waitForIdle();
  }

  await removeDevChrome(client);
  await scrollToTop(client);
  await scrollToText(client, shot.scrollTo, 84);
  await injectStaticStyles(client);
  await delay(250);

  const image = await capture(client);
  await writeImage(
    path.join(OUTPUT_ROOT, theme, `${shot.name}.png`),
    image,
    shot.width,
    shot.height,
  );
}

async function capturePhonePanel(client, network, panel) {
  await setViewport(client, PHONE_WIDTH, PHONE_HEIGHT);
  await navigate(client, `${BASE_URL}${panel.path}`, network);
  await waitForText(client, panel.waitFor);
  await network.waitForIdle();
  await removeDevChrome(client);
  await scrollToTop(client);
  await injectStaticStyles(client);
  await delay(250);
  return capture(client);
}

async function composePhonePanels(client, panels) {
  const totalWidth =
    PHONE_WIDTH * PHONE_COUNT + PHONE_GAP * (PHONE_COUNT - 1);
  const images = panels
    .map(
      (panel) =>
        `<img alt="" width="${PHONE_WIDTH}" height="${PHONE_HEIGHT}" src="data:image/png;base64,${panel.toString("base64")}">`,
    )
    .join("");
  const html = `<!doctype html>
<html>
<head>
<meta charset="utf-8">
<style>
  html, body {
    width: ${totalWidth}px;
    height: ${PHONE_HEIGHT}px;
    margin: 0;
    padding: 0;
    overflow: hidden;
    background: transparent;
  }
  body {
    display: flex;
    gap: ${PHONE_GAP}px;
    align-items: flex-start;
  }
  img { display: block; }
</style>
</head>
<body>${images}</body>
</html>`;

  await setViewport(client, totalWidth, PHONE_HEIGHT);
  await client.send("Emulation.setDefaultBackgroundColorOverride", {
    color: { r: 0, g: 0, b: 0, a: 0 },
  });
  await navigate(
    client,
    `data:text/html;base64,${Buffer.from(html).toString("base64")}`,
  );
  await waitForLoad(client);
  await delay(150);

  const result = await client.send("Page.captureScreenshot", {
    format: "png",
    fromSurface: true,
    captureBeyondViewport: false,
    omitBackground: true,
  });
  await client.send("Emulation.setDefaultBackgroundColorOverride");
  return Buffer.from(result.data, "base64");
}

async function setTheme(client, theme) {
  await evaluate(
    client,
    `localStorage.setItem("hedger-preferences-v1", JSON.stringify({ themeMode: ${JSON.stringify(theme)} }))`,
  );
}

async function setViewport(client, width, height) {
  await client.send("Emulation.setDeviceMetricsOverride", {
    width,
    height,
    deviceScaleFactor: 1,
    mobile: false,
    screenWidth: width,
    screenHeight: height,
  });
}

async function navigate(client, url, network = null) {
  network?.start(null);
  const loaded = new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      offLoad();
      reject(new Error(`Timed out navigating to ${url}.`));
    }, 20_000);
    const offLoad = client.on("Page.loadEventFired", () => {
      clearTimeout(timeout);
      offLoad();
      resolve();
    });
  });

  const result = await client.send("Page.navigate", { url });
  if (result.errorText) {
    throw new Error(`Could not navigate to ${url}: ${result.errorText}`);
  }
  network?.start(result.loaderId);
  await loaded;
  await waitForLoad(client);
}

async function waitForLoad(client, timeout = 20_000) {
  const deadline = Date.now() + timeout;
  while (Date.now() < deadline) {
    try {
      if (await evaluate(client, "document.readyState === 'complete'")) return;
    } catch {
      // The execution context is being replaced during navigation.
    }
    await delay(50);
  }
  throw new Error("Timed out waiting for the page to load.");
}

async function waitForText(client, text, timeout = 20_000) {
  const deadline = Date.now() + timeout;
  while (Date.now() < deadline) {
    try {
      const found = await evaluate(
        client,
        `Boolean(document.body && document.body.innerText.includes(${JSON.stringify(text)}))`,
      );
      if (found) return;
    } catch {
      // The execution context is being replaced during navigation.
    }
    await delay(100);
  }
  throw new Error(`Timed out waiting for page text: ${text}`);
}

async function clickButton(client, label) {
  const clicked = await evaluate(
    client,
    `(() => {
      const button = [...document.querySelectorAll('button, [role="button"]')].find(
        (candidate) => candidate.textContent.trim() === ${JSON.stringify(label)}
      );
      if (!button) return false;
      button.click();
      button.blur();
      return true;
    })()`,
  );
  if (!clicked) throw new Error(`Could not find the "${label}" button.`);
}

async function expandAccount(client, accountName) {
  const clicked = await evaluate(
    client,
    `(() => {
      const button = [...document.querySelectorAll('button, [role="button"]')].find(
        (candidate) => candidate.textContent.trim().startsWith(${JSON.stringify(accountName)})
      );
      if (!button) return false;
      button.click();
      button.blur();
      return true;
    })()`,
  );
  if (!clicked) {
    const labels = await evaluate(
      client,
      `JSON.stringify([...document.querySelectorAll('button, [role="button"]')].map((button) => button.textContent.trim()).filter(Boolean).slice(0, 40))`,
    );
    throw new Error(
      `Could not expand the "${accountName}" account. Buttons: ${labels}`,
    );
  }
  await delay(250);
}

async function removeDevChrome(client) {
  await evaluate(
    client,
    `(() => {
      document.querySelectorAll("vite-error-overlay").forEach((element) => element.remove());
      document
        .querySelectorAll('body > div[style*="bottom: 2em"]')
        .forEach((element) => element.remove());
      const dismiss = [...document.querySelectorAll('button, [role="button"]')].find(
        (candidate) => candidate.textContent.trim() === "Dismiss"
      );
      if (dismiss) dismiss.click();
      return true;
    })()`,
  );
  await delay(200);
}

async function scrollToTop(client) {
  await evaluate(
    client,
    `(() => {
      const scroller = document.querySelector(".app-content");
      if (scroller) scroller.scrollTop = 0;
      window.scrollTo(0, 0);
      return true;
    })()`,
  );
}

async function scrollToText(client, text, offset) {
  const scrolled = await evaluate(
    client,
    `(() => {
      const target = [...document.querySelectorAll("*")].find(
        (element) =>
          element.children.length === 0 &&
          element.textContent.trim() === ${JSON.stringify(text)}
      );
      const scroller = document.querySelector(".app-content");
      if (!target || !scroller) return false;
      const targetRect = target.getBoundingClientRect();
      const scrollerRect = scroller.getBoundingClientRect();
      scroller.scrollTop += targetRect.top - scrollerRect.top - ${offset};
      return true;
    })()`,
  );
  if (!scrolled) throw new Error(`Could not scroll to "${text}".`);
  await delay(200);
}

async function injectStaticStyles(client) {
  await evaluate(
    client,
    `(() => {
      let style = document.getElementById("hedger-readme-shot-styles");
      if (!style) {
        style = document.createElement("style");
        style.id = "hedger-readme-shot-styles";
        document.head.appendChild(style);
      }
      style.textContent =
        "*, *::before, *::after { transition: none !important; animation: none !important; caret-color: transparent !important; }";
      return true;
    })()`,
  );
}

async function capture(client) {
  const result = await client.send("Page.captureScreenshot", {
    format: "png",
    fromSurface: true,
    captureBeyondViewport: false,
  });
  return Buffer.from(result.data, "base64");
}

async function evaluate(client, expression, awaitPromise = false) {
  const result = await client.send("Runtime.evaluate", {
    expression,
    returnByValue: true,
    awaitPromise,
  });
  if (result.exceptionDetails) {
    throw new Error(
      result.exceptionDetails.text ??
        result.exceptionDetails.exception?.description ??
        "Page evaluation failed.",
    );
  }
  return result.result.value;
}

async function writeImage(filePath, image, expectedWidth, expectedHeight) {
  await fs.mkdir(path.dirname(filePath), { recursive: true });
  await fs.writeFile(filePath, image);
  const { width, height } = pngSize(image);
  if (width !== expectedWidth || height !== expectedHeight) {
    throw new Error(
      `${filePath} is ${width}x${height}; expected ${expectedWidth}x${expectedHeight}.`,
    );
  }
  console.log(
    `${path.relative(REPO_ROOT, filePath)} (${width}x${height}, ${Math.round(image.length / 1024)} KiB)`,
  );
}

function pngSize(buffer) {
  if (buffer.toString("ascii", 1, 4) !== "PNG") {
    throw new Error("Screenshot is not a PNG.");
  }
  return {
    width: buffer.readUInt32BE(16),
    height: buffer.readUInt32BE(20),
  };
}

function delay(milliseconds) {
  return new Promise((resolve) => setTimeout(resolve, milliseconds));
}

async function cleanup() {
  if (cdp) {
    try {
      cdp.close();
    } catch {
      // Ignore close errors.
    }
  }

  if (chrome) {
    chrome.child.kill("SIGTERM");
    await Promise.race([
      once(chrome.child, "exit").catch(() => {}),
      delay(2_000),
    ]);
  }

  if (chromeProfile) {
    await fs.rm(chromeProfile, { recursive: true, force: true }).catch(() => {});
  }
}

class CdpClient {
  constructor(url) {
    this.url = url;
    this.socket = null;
    this.nextId = 1;
    this.pending = new Map();
    this.listeners = new Map();
  }

  async connect() {
    this.socket = new WebSocket(this.url);
    await new Promise((resolve, reject) => {
      this.socket.addEventListener("open", resolve, { once: true });
      this.socket.addEventListener("error", reject, { once: true });
    });
    this.socket.addEventListener("message", (event) => {
      const message = JSON.parse(event.data);
      if (message.id) {
        const pending = this.pending.get(message.id);
        if (!pending) return;
        this.pending.delete(message.id);
        if (message.error) {
          pending.reject(
            new Error(`${message.error.message} (${message.error.code})`),
          );
        } else {
          pending.resolve(message.result);
        }
        return;
      }

      if (message.method) {
        for (const listener of this.listeners.get(message.method) ?? []) {
          listener(message.params);
        }
      }
    });
    this.socket.addEventListener("close", () => {
      for (const pending of this.pending.values()) {
        pending.reject(new Error("Chrome DevTools connection closed."));
      }
      this.pending.clear();
    });
  }

  on(method, listener) {
    const listeners = this.listeners.get(method) ?? [];
    listeners.push(listener);
    this.listeners.set(method, listeners);
    return () => {
      const index = listeners.indexOf(listener);
      if (index >= 0) listeners.splice(index, 1);
    };
  }

  send(method, params = {}) {
    const id = this.nextId++;
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.socket.send(JSON.stringify({ id, method, params }));
    });
  }

  close() {
    this.socket?.close();
  }
}

await main();
