import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
  server: {
    port: 1234,
    strictPort: true,
    proxy: {
      "/api": {
        target: process.env.API_TARGET ?? "http://127.0.0.1:8080",
        changeOrigin: true,
      },
    },
  },
  build: { outDir: "dist" },
});
