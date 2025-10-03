import esbuild from "esbuild";
import { dirname, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const isProd = process.argv.includes("production");

const banner = {
  js: `/*
THIS IS A GENERATED FILE. DO NOT EDIT.
https://github.com/elamdf/bismuth
*/`
};

async function main() {
  await esbuild.build({
    entryPoints: [resolve(__dirname, "src/main.ts")],
    bundle: true,
    outfile: resolve(__dirname, "main.js"),
    sourcemap: !isProd,
    minify: isProd,
    format: "cjs",
    banner,
    target: "es2017",
    external: ["obsidian"],
    logLevel: "info"
  });
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
