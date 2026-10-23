import { readFileSync, readdirSync } from "node:fs";
import { extname, join, relative } from "node:path";

const sourceRoot = new URL("../src/", import.meta.url).pathname;
const typographyModule = "Ui/Typography.elm";

const appearanceForbidden = [
  /Element\.(?:padding|paddingXY|paddingEach|spacing|width|height|moveDown|moveUp|moveLeft|moveRight)\s+(?:\(?Element\.(?:px|fillPortion)\s+)?\d+/,
  /Border\.(?:rounded|width|widthEach)\s+\d+/,
  /Element\.rgba?(?:255)?\s+\d+/,
  /(?:Html\.Attributes|Attr)\.(?:class|style)\b/,
];

const typographyForbidden = [
  /Font\.size\b/,
  /Font\.(?:family|variant)\b/,
  /Font\.(?:bold|semiBold|medium|regular|light|extraLight|extraBold|heavy|hairline)\b/,
  /(?:Html\.Attributes|Attr)\.style\s+"font-(?:size|weight|family)"/,
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

function rulesFor(modulePath) {
  if (modulePath === typographyModule) {
    return [];
  }

  if (modulePath.startsWith("Ui/")) {
    return typographyForbidden;
  }

  return [...appearanceForbidden, ...typographyForbidden];
}

const violations = elmFiles(sourceRoot).flatMap((path) => {
  const modulePath = relative(sourceRoot, path);
  const rules = rulesFor(modulePath);

  return readFileSync(path, "utf8")
    .split("\n")
    .flatMap((line, index) =>
      rules.some((pattern) => pattern.test(line))
        ? [`${relative(sourceRoot, path)}:${index + 1}: ${line.trim()}`]
        : [],
    );
});

if (violations.length > 0) {
  console.error(
    "Appearance must come from Ui modules and typography from Ui.Typography:\n",
  );
  console.error(violations.join("\n"));
  process.exitCode = 1;
}
