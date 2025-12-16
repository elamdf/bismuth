use anyhow::{bail, Context, Result};
use clap::Parser;
use regex::Regex;
use serde_json::json;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// Scan files for Obsidian/Markdown blockquote CR/XCR threads.
#[derive(Parser, Debug)]
#[command(name = "crscan")]
#[command(about = "Scan files for > CR / > XCR threads and output JSON", long_about = None)]
struct Args {
    /// Path to a file or directory to scan
    path: PathBuf,

    /// If path is a directory, only include files with these extensions (repeatable).
    /// Example: --ext md --ext markdown
    #[arg(long = "ext")]
    exts: Vec<String>,

    /// If set, include all regular files (ignores --ext filtering)
    #[arg(long)]
    all: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Header regex:
    // ^\s*>\s*(X?CR)\s+([^ ]+)\s+for\s+([^:]+):.*$
    //
    // Groups:
    // 1 = kind (CR or XCR)
    // 2 = reviewer (no spaces)
    // 3 = author (anything up to :)
    let header_re = Regex::new(r"^\s*>\s*(X?CR)\s+([^ ]+)\s+for\s+([^:]+):.*$")
        .context("failed to compile header regex")?;

    let mut out: BTreeMap<String, BTreeMap<String, serde_json::Value>> = BTreeMap::new();

    let meta = fs::metadata(&args.path)
        .with_context(|| format!("failed to stat path: {}", args.path.display()))?;

    if meta.is_file() {
        let display_name = args.path.to_string_lossy().to_string();
        let file_map = scan_file(&args.path, &header_re)?;
        if !file_map.is_empty() {
            out.insert(display_name, file_map);
        }
    } else if meta.is_dir() {
        let root = args.path.canonicalize().unwrap_or(args.path.clone());

        for entry in WalkDir::new(&args.path)
            .follow_links(false)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            if !entry.file_type().is_file() {
                continue;
            }
            let p = entry.path();

            if !args.all && !args.exts.is_empty() && !has_allowed_ext(p, &args.exts) {
                continue;
            }

            let rel = p
                .canonicalize()
                .ok()
                .and_then(|cp| cp.strip_prefix(&root).ok().map(|x| x.to_path_buf()))
                .unwrap_or_else(|| p.to_path_buf());

            let display_name = rel.to_string_lossy().to_string();

            let file_map = scan_file(p, &header_re)
                .with_context(|| format!("while scanning {}", p.display()))?;
            if !file_map.is_empty() {
                out.insert(display_name, file_map);
            }
        }
    } else {
        bail!("path is neither file nor directory: {}", args.path.display());
    }

    let v = serde_json::Value::Object(
        out.into_iter()
            .map(|(k, v)| (k, serde_json::Value::Object(v.into_iter().collect())))
            .collect(),
    );

    println!("{}", serde_json::to_string_pretty(&v)?);
    Ok(())
}

fn has_allowed_ext(path: &Path, exts: &[String]) -> bool {
    let ext = match path.extension().and_then(|s| s.to_str()) {
        Some(e) => e.to_ascii_lowercase(),
        None => return false,
    };
    exts.iter()
        .any(|x| x.to_ascii_lowercase().trim_start_matches('.').eq(&ext))
}

fn scan_file(path: &Path, header_re: &Regex) -> Result<BTreeMap<String, serde_json::Value>> {
    let bytes = fs::read(path)?;
    let text = String::from_utf8_lossy(&bytes);

    // Keep line endings normalized for output stability.
    let lines: Vec<&str> = text.lines().collect();

    let mut file_map: BTreeMap<String, serde_json::Value> = BTreeMap::new();

    let mut i = 0usize;
    while i < lines.len() {
        let line = lines[i];

        if let Some(caps) = header_re.captures(line) {
            let kind = caps.get(1).unwrap().as_str();
            let reviewer = caps.get(2).unwrap().as_str();
            let author = caps.get(3).unwrap().as_str().trim();

            let header = line.to_string();

            // Collect subsequent quoted lines as "thread" until:
            // - a non-`>` line, OR
            // - another CR/XCR header line (so threads don't swallow each other).
            let mut thread_lines: Vec<String> = Vec::new();
            let mut j = i + 1;
            while j < lines.len() {
                let l = lines[j];

                // Stop if not a quoted line
                if !l.trim_start().starts_with('>') {
                    break;
                }

                // Stop if this quoted line is itself a new header
                if header_re.is_match(l) {
                    break;
                }

                thread_lines.push(l.to_string());
                j += 1;
            }

            let thread = if thread_lines.is_empty() {
                String::new()
            } else {
                thread_lines.join("\n")
            };

            // Line numbers: 1-based
            let line_no = (i + 1).to_string();

            // Store as: line_number : [CR/XCR, reviewer, author, header, thread]
            file_map.insert(
                line_no,
                json!([kind, reviewer, author, header, thread]),
            );

            i = j;
            continue;
        }

        i += 1;
    }

    Ok(file_map)
}
