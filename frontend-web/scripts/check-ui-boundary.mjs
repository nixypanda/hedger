import { readFileSync, readdirSync } from "node:fs";
import { extname, join, relative } from "node:path";

const sourceRoot = new URL("../src/", import.meta.url).pathname;

const forbidden = [
  /Element\.(?:padding|paddingXY|paddingEach|spacing|width|height|moveDown|moveUp|moveLeft|moveRight)\s+(?:\(?Element\.(?:px|fillPortion)\s+)?\d+/,
  /Border\.(?:rounded|width|widthEach)\s+\d+/,
  /Font\.size\s+\d+/,
  /Element\.rgba?(?:255)?\s+\d+/,
  /(?:Html\.Attributes|Attr)\.(?:class|style)\b/,
];

function elmFiles(directory) {
  return readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name);
    return entry.isDirectory()
      ? elmFiles(path)
      : extname(entry.name) === ".elm"
        ? [path]
        : [];
  });
}

const violations = elmFiles(sourceRoot)
  .filter((path) => {
    const modulePath = relative(sourceRoot, path);
    return modulePath !== "Ui.elm" && !modulePath.startsWith("Ui/");
  })
  .flatMap((path) =>
    readFileSync(path, "utf8")
      .split("\n")
      .flatMap((line, index) =>
        forbidden.some((pattern) => pattern.test(line))
          ? [`${relative(sourceRoot, path)}:${index + 1}: ${line.trim()}`]
          : [],
      ),
  );

if (violations.length > 0) {
  console.error("Appearance must be expressed through Ui modules:\n");
  console.error(violations.join("\n"));
  process.exitCode = 1;
}
