// NOTE: chatgpt wrote most of this using the emacs implementation as a reference


import {
  App,
  Plugin,
  PluginSettingTab,
  Setting,
  Notice,
  MarkdownView,
  TFile,
  FuzzySuggestModal,
  Platform,
  ItemView,
  WorkspaceLeaf,
} from "obsidian";

import {
  EditorView,
  Decoration,
  DecorationSet,
  ViewPlugin,
  ViewUpdate,
} from "@codemirror/view";
import { RangeSetBuilder, Extension } from "@codemirror/state";

import { execFile } from "child_process";

/* ---------------- Types / Settings ---------------- */

type InlineCrSettings = {
  user: string;
  // Use external parser binary (`crscan`) when available (desktop only)
  useCrscan: boolean;
  // Path to crscan executable. If empty, uses "crscan" (must be in PATH).
  crscanPath: string;
  // Extensions to scan when running crscan (space separated, e.g. "md markdown txt")
  crscanExts: string;
};

const DEFAULT_SETTINGS: InlineCrSettings = {
  user: "",
  useCrscan: true,
  crscanPath: "crscan",
  crscanExts: "md markdown txt",
};

type ThreadInfo = {
  headerLineNo: number;
  startLineNo: number; // header line
  endLineNo: number; // inclusive
  actionable: boolean;
};

type ActionableHit = {
  file: TFile;
  lineNo: number;
  lineText: string;
  kind: "CR" | "XCR";
  reviewer: string;
  author: string;
};

type CrscanEntry = {
  filePath: string; // vault-relative (as returned by crscan)
  lineNo: number;
  kind: "CR" | "XCR";
  reviewer: string;
  author: string;
  header: string;
  thread: string;
};

const VIEW_TYPE_ACTIONABLES = "inline-cr-actionables-view";

/* ---------------- Thread parsing (in-editor) ---------------- */

function headerMatch(
  lineText: string
): null | { kind: "CR" | "XCR"; reviewer: string; author: string } {
  // > CR reviewer for author: ...
  // > XCR reviewer for author: ...
  const m = lineText.match(/^\s*>\s*(X?CR)\s+(\S+)\s+for\s+([^:]+)\s*:/);
  if (!m) return null;
  const kind = (m[1] === "XCR" ? "XCR" : "CR") as "CR" | "XCR";
  return { kind, reviewer: m[2], author: m[3].trim() };
}

function isThreadLine(lineText: string): boolean {
  return /^\s*>/.test(lineText);
}

function bodyAuthorMatch(lineText: string): string | null {
  // Matches: > name: blah
  const m = lineText.match(/^\s*>\s*(\S+)\s*:/);
  return m ? m[1] : null;
}

function computeActionable(
  h: { kind: "CR" | "XCR"; reviewer: string; author: string },
  user: string
): boolean {
  if (!user) return false;
  // CR actionable if author == user
  // XCR actionable if reviewer == user
  if (h.kind === "CR") return h.author === user;
  return h.reviewer === user;
}

function scanThreads(view: EditorView, user: string): ThreadInfo[] {
  const doc = view.state.doc;
  const threads: ThreadInfo[] = [];

  let lineNo = 1;
  while (lineNo <= doc.lines) {
    const line = doc.line(lineNo);
    const hm = headerMatch(line.text);

    if (!hm) {
      lineNo++;
      continue;
    }

    const actionable = computeActionable(hm, user);

    let end = lineNo;
    let next = lineNo + 1;
    while (next <= doc.lines) {
      const ln = doc.line(next);
      if (!isThreadLine(ln.text)) break;
      end = next;
      next++;
    }

    threads.push({
      headerLineNo: lineNo,
      startLineNo: lineNo,
      endLineNo: end,
      actionable,
    });

    lineNo = end + 1;
  }

  return threads;
}

function findThreadAtLine(
  view: EditorView,
  user: string,
  lineNo: number
): ThreadInfo | null {
  const threads = scanThreads(view, user);
  for (const t of threads) {
    if (lineNo >= t.startLineNo && lineNo <= t.endLineNo) return t;
  }
  return null;
}

function lastAuthorInThread(view: EditorView, thread: ThreadInfo): string | null {
  const doc = view.state.doc;
  let last: string | null = null;

  for (let ln = thread.startLineNo + 1; ln <= thread.endLineNo; ln++) {
    const line = doc.line(ln);
    const a = bodyAuthorMatch(line.text);
    if (a) last = a;
  }
  return last;
}

function buildDecorations(view: EditorView, user: string): DecorationSet {
  const builder = new RangeSetBuilder<Decoration>();
  const threads = scanThreads(view, user);
  const lineDeco = (cls: string) => Decoration.line({ class: cls });

  for (const t of threads) {
    const blockClass = t.actionable ? "inline-cr-block-actionable" : "inline-cr-block";
    const headerClass = t.actionable
      ? "inline-cr-header-actionable"
      : "inline-cr-header-nonactionable";

    for (let ln = t.startLineNo; ln <= t.endLineNo; ln++) {
      const line = view.state.doc.line(ln);

      builder.add(line.from, line.from, lineDeco(blockClass));

      if (ln === t.headerLineNo) {
        builder.add(line.from, line.from, lineDeco("inline-cr-header"));
        builder.add(line.from, line.from, lineDeco(headerClass));
      }
    }
  }

  return builder.finish();
}

function makeInlineCrExtension(getUser: () => string, getVersion: () => number): Extension {
  return ViewPlugin.fromClass(
    class {
      decorations: DecorationSet;
      private lastVersion: number;

      constructor(view: EditorView) {
        this.lastVersion = getVersion();
        this.decorations = buildDecorations(view, getUser());
      }

      update(update: ViewUpdate) {
        const v = getVersion();
        if (update.docChanged || update.viewportChanged || v !== this.lastVersion) {
          this.lastVersion = v;
          this.decorations = buildDecorations(update.view, getUser());
        }
      }
    },
    { decorations: (v) => v.decorations }
  );
}

function jumpToActionable(view: EditorView, user: string, direction: "next" | "prev"): boolean {
  const doc = view.state.doc;
  const threads = scanThreads(view, user).filter((t) => t.actionable);
  if (threads.length === 0) return false;

  const pos = view.state.selection.main.head;
  const curLine = doc.lineAt(pos).number;

  const headerLines = threads.map((t) => t.headerLineNo).sort((a, b) => a - b);

  const pick = (() => {
    if (direction === "next") {
      for (const ln of headerLines) if (ln > curLine) return ln;
      return headerLines[0];
    } else {
      for (let i = headerLines.length - 1; i >= 0; i--) {
        if (headerLines[i] < curLine) return headerLines[i];
      }
      return headerLines[headerLines.length - 1];
    }
  })();

  const target = doc.line(pick);
  view.dispatch({
    selection: { anchor: target.from },
    scrollIntoView: true,
  });
  return true;
}

function insertReplyAtEndOfThread(view: EditorView, user: string): boolean {
  const doc = view.state.doc;
  const pos = view.state.selection.main.head;
  const curLine = doc.lineAt(pos).number;

  const thread = findThreadAtLine(view, user, curLine);
  if (!thread) {
    view.dispatch({
      changes: { from: pos, to: pos, insert: "\n" },
      selection: { anchor: pos + 1 },
      scrollIntoView: true,
    });
    return false;
  }

  const lastAuthor = lastAuthorInThread(view, thread);
  const prefix = lastAuthor && lastAuthor === user ? "> " : `> ${user}: `;
  const insertText = "\n" + prefix;

  const endLine = doc.line(thread.endLineNo);
  const insertPos = endLine.to;

  view.dispatch({
    changes: { from: insertPos, to: insertPos, insert: insertText },
    selection: { anchor: insertPos + insertText.length },
    scrollIntoView: true,
  });

  return true;
}

/* ---------------- Vault-wide aggregation (fallback) ---------------- */

function isActionableHeaderLine(
  lineText: string,
  user: string
): null | { kind: "CR" | "XCR"; reviewer: string; author: string } {
  const hm = headerMatch(lineText);
  if (!hm) return null;
  return computeActionable(hm, user) ? hm : null;
}

async function collectActionablesInVault(app: App, user: string): Promise<ActionableHit[]> {
  if (!user) return [];

  const files = app.vault.getMarkdownFiles();
  const hits: ActionableHit[] = [];

  for (const file of files) {
    let text: string;
    try {
      text = await app.vault.read(file);
    } catch {
      continue;
    }
    if (file.name === "Actionables.md") continue;

    let start = 0;
    let lineNo = 1;

    for (let i = 0; i <= text.length; i++) {
      const isEol = i === text.length || text.charCodeAt(i) === 10; // '\n'
      if (!isEol) continue;

      const lineText = text.slice(start, i);
      const hm = isActionableHeaderLine(lineText, user);
      if (hm) {
        hits.push({
          file,
          lineNo,
          lineText,
          kind: hm.kind,
          reviewer: hm.reviewer,
          author: hm.author,
        });
      }

      start = i + 1;
      lineNo++;
    }
  }

  hits.sort((a, b) => {
    const ap = a.file.path.localeCompare(b.file.path);
    return ap !== 0 ? ap : a.lineNo - b.lineNo;
  });

  return hits;
}

async function openHit(app: App, hit: ActionableHit) {
  const leaf = app.workspace.getLeaf(false);
  await leaf.openFile(hit.file);

  const mdView = app.workspace.getActiveViewOfType(MarkdownView);
  if (!mdView) return;

  // Use setCursor if available
  try {
    (mdView.editor as any).setCursor({ line: Math.max(0, hit.lineNo - 1), ch: 0 });
    (mdView.editor as any).scrollIntoView(
      { from: { line: Math.max(0, hit.lineNo - 1), ch: 0 }, to: { line: Math.max(0, hit.lineNo - 1), ch: 0 } },
      true
    );
  } catch {
    // fallback: search for the header text
    const editorAny: any = mdView.editor;
    const needle = hit.lineText.trim();
    const cur = editorAny?.getValue?.() ?? "";
    const idx = cur.indexOf(needle);
    if (idx >= 0 && editorAny?.setCursor) {
      // best-effort
      editorAny.setCursor(editorAny.offsetToPos ? editorAny.offsetToPos(idx) : { line: 0, ch: 0 });
    }
  }
}

class ActionablePicker extends FuzzySuggestModal<ActionableHit> {
  constructor(
    app: App,
    private hits: ActionableHit[],
    private onPick: (hit: ActionableHit) => Promise<void>
  ) {
    super(app);
  }

  getItems(): ActionableHit[] {
    return this.hits;
  }

  getItemText(hit: ActionableHit): string {
    return `${hit.file.path}:${hit.lineNo}  ${hit.lineText.trim()}`;
  }

  async onChooseItem(hit: ActionableHit): Promise<void> {
    await this.onPick(hit);
  }
}

/* ---------------- Sidebar view (Actionables panel) ---------------- */

function isDesktopApp(): boolean {
  return !!(Platform as any)?.isDesktopApp;
}

function getVaultBasePath(app: App): string | null {
  return (app.vault.adapter as any).getBasePath?.() ?? null;
}

function actionableFromCrscan(e: CrscanEntry, user: string): boolean {
  if (!user) return false;
  if (e.kind === "CR") return e.author === user;
  return e.reviewer === user;
}

async function openFileAtLine(app: App, file: TFile, lineNo: number) {
  const leaf = app.workspace.getLeaf(false);
  await leaf.openFile(file);

  const mdView = app.workspace.getActiveViewOfType(MarkdownView);
  if (!mdView) return;

  try {
    (mdView.editor as any).setCursor({ line: Math.max(0, lineNo - 1), ch: 0 });
    (mdView.editor as any).scrollIntoView(
      { from: { line: Math.max(0, lineNo - 1), ch: 0 }, to: { line: Math.max(0, lineNo - 1), ch: 0 } },
      true
    );
  } catch {
    // ignore
  }
}
function isActionable(
  kind: "CR" | "XCR",
  reviewer: string,
  author: string,
  user: string
): boolean {
  if (!user) return false;
  return (
    (kind === "CR" && author === user) ||
    (kind === "XCR" && reviewer === user)
  );
}

class InlineCrActionablesView extends ItemView {
  constructor(leaf: WorkspaceLeaf, private plugin: InlineCrPlugin) {
    super(leaf);
  }

  getViewType(): string {
    return VIEW_TYPE_ACTIONABLES;
  }

  getDisplayText(): string {
    return "CR/XCR Actionables";
  }

  getIcon(): string {
    return "check-square";
  }

  async onOpen() {
    await this.render();
  }

  async render() {
    const el = this.contentEl;
    el.empty();

    const header = el.createDiv({ cls: "inline-cr-actionables-header" });
    header.createEl("h3", { text: "CR/XCR Actionables" });

    const controls = header.createDiv({ cls: "inline-cr-actionables-controls" });
    const refreshBtn = controls.createEl("button", { text: "Refresh" });

    refreshBtn.onclick = async () => {
      refreshBtn.disabled = true;
      try {
        await this.render();
      } finally {
        refreshBtn.disabled = false;
      }
    };

    const user = this.plugin.settings.user;
    if (!user) {
      el.createEl("p", { text: "Set your User in settings to show actionables." });
      return;
    }

    let entries: CrscanEntry[] = [];
    try {
      entries = await this.plugin.collectActionablesForPanel();
entries = entries.filter((e) =>
  isActionable(e.kind, e.reviewer, e.author, user)
);


    } catch (e: any) {
      el.createEl("p", { text: `Failed to collect actionables: ${e?.message ?? String(e)}` });
      return;
    }

    entries = entries.filter((e) => actionableFromCrscan(e, user));

    el.createEl("p", { text: `Found ${entries.length} actionable thread(s) for ${user}.` });

    const byFile = new Map<string, CrscanEntry[]>();
    for (const e of entries) {
      const arr = byFile.get(e.filePath) ?? [];
      arr.push(e);
      byFile.set(e.filePath, arr);
    }
    for (const arr of byFile.values()) arr.sort((a, b) => a.lineNo - b.lineNo);

    const container = el.createDiv({ cls: "inline-cr-actionables-list" });

    const filesSorted = Array.from(byFile.keys()).sort((a, b) => a.localeCompare(b));
    for (const filePath of filesSorted) {
      const fileEntries = byFile.get(filePath)!;

      const fileHeader = container.createEl("h4", { text: filePath });
      fileHeader.style.marginTop = "1em";

      const ul = container.createEl("ul");
      for (const e of fileEntries) {
        const li = ul.createEl("li");

        const label = `${e.lineNo}  ${e.kind}  ${e.reviewer} → ${e.author}`;
        const a = li.createEl("a", { text: label, href: "#" });

        a.onclick = async (evt) => {
          evt.preventDefault();
          const tf = this.plugin.app.vault.getAbstractFileByPath(filePath);
          if (!(tf instanceof TFile)) {
            new Notice(`File not found: ${filePath}`);
            return;
          }
          await openFileAtLine(this.plugin.app, tf, e.lineNo);
        };

        li.createEl("div", { text: e.header.trim() }).style.opacity = "0.8";
      }
    }
  }
}

/* ---------------- Settings UI ---------------- */

class InlineCrSettingTab extends PluginSettingTab {
  plugin: InlineCrPlugin;

  constructor(app: App, plugin: InlineCrPlugin) {
    super(app, plugin);
    this.plugin = plugin;
  }

  display(): void {
    const { containerEl } = this;
    containerEl.empty();

    containerEl.createEl("h2", { text: "Inline CR/XCR settings" });

    new Setting(containerEl)
      .setName("User")
      .setDesc("Used to determine which CR/XCR headers are actionable and how replies are prefixed.")
      .addText((text) =>
        text
          .setPlaceholder("e.g. elamdf")
          .setValue(this.plugin.settings.user)
          .onChange(async (value) => {
            this.plugin.settings.user = value.trim();
            await this.plugin.saveSettings();
            this.plugin.bumpSettingsVersion();
          })
      );

    new Setting(containerEl)
      .setName("Use crscan (desktop)")
      .setDesc("If enabled, uses the external `crscan` binary for vault-wide actionables (desktop only).")
      .addToggle((tog) =>
        tog.setValue(this.plugin.settings.useCrscan).onChange(async (v) => {
          this.plugin.settings.useCrscan = v;
          await this.plugin.saveSettings();
        })
      );

    new Setting(containerEl)
      .setName("crscan path")
      .setDesc("Path to the `crscan` executable. Leave as `crscan` if it’s on PATH.")
      .addText((text) =>
        text
          .setPlaceholder("crscan")
          .setValue(this.plugin.settings.crscanPath)
          .onChange(async (value) => {
            this.plugin.settings.crscanPath = value.trim() || "crscan";
            await this.plugin.saveSettings();
          })
      );

    new Setting(containerEl)
      .setName("crscan extensions")
      .setDesc("Space-separated extensions to scan when running crscan (e.g. `md markdown txt`).")
      .addText((text) =>
        text
          .setPlaceholder("md markdown txt")
          .setValue(this.plugin.settings.crscanExts)
          .onChange(async (value) => {
            this.plugin.settings.crscanExts = value.trim() || "md markdown txt";
            await this.plugin.saveSettings();
          })
      );
  }
}

/* ---------------- Plugin ---------------- */

export default class InlineCrPlugin extends Plugin {
  settings: InlineCrSettings = { ...DEFAULT_SETTINGS };
  private settingsVersion = 0;

  bumpSettingsVersion() {
    this.settingsVersion++;
  }

  private getUser = () => this.settings.user;
  private getVersion = () => this.settingsVersion;

  async onload() {
    await this.loadSettings();

    this.addSettingTab(new InlineCrSettingTab(this.app, this));
    this.registerEditorExtension(makeInlineCrExtension(this.getUser, this.getVersion));

    // Register sidebar view
    this.registerView(VIEW_TYPE_ACTIONABLES, (leaf) => new InlineCrActionablesView(leaf, this));

    // Ribbon icon: open/reuse panel
    this.addRibbonIcon("check-square", "CR/XCR Actionables", async () => {
      const existing = this.app.workspace.getLeavesOfType(VIEW_TYPE_ACTIONABLES)[0];
      const leaf = existing ?? this.app.workspace.getLeaf("tab");
      await leaf.setViewState({ type: VIEW_TYPE_ACTIONABLES, active: true });
      this.app.workspace.revealLeaf(leaf);
    });

    const getEditorView = (): EditorView | null => {
      const mdView = this.app.workspace.getActiveViewOfType(MarkdownView);
      if (!mdView) return null;

      const editorAny: any = mdView.editor;
      const cm = editorAny?.cm;
      if (cm && typeof cm === "object" && "state" in cm && "dispatch" in cm) return cm as EditorView;

      const cm2 = editorAny?.cm?.cm;
      if (cm2 && typeof cm2 === "object" && "state" in cm2 && "dispatch" in cm2) return cm2 as EditorView;

      return null;
    };

    // Commands: per-file navigation
    this.addCommand({
      id: "inline-cr-next-actionable",
      name: "Next actionable CR/XCR in this file",
      callback: () => {
        const ev = getEditorView();
        if (!ev) return void new Notice("No active markdown editor found.");
        const ok = jumpToActionable(ev, this.settings.user, "next");
        if (!ok) new Notice(`No actionable CR/XCR for ${this.settings.user}`);
      },
    });

    this.addCommand({
      id: "inline-cr-prev-actionable",
      name: "Previous actionable CR/XCR in this file",
      callback: () => {
        const ev = getEditorView();
        if (!ev) return void new Notice("No active markdown editor found.");
        const ok = jumpToActionable(ev, this.settings.user, "prev");
        if (!ok) new Notice(`No actionable CR/XCR for ${this.settings.user}`);
      },
    });

    this.addCommand({
      id: "inline-cr-insert-reply-at-end",
      name: "Insert reply at end of thread",
      callback: () => {
        const ev = getEditorView();
        if (!ev) return void new Notice("No active markdown editor found.");
        if (!this.settings.user) return void new Notice("Set your User in settings first.");
        insertReplyAtEndOfThread(ev, this.settings.user);
      },
    });

    // Command: fuzzy picker (vault-wide)
    this.addCommand({
      id: "inline-cr-pick-actionable",
      name: "Pick an actionable (jump)",
      callback: async () => {
        if (!this.settings.user) return void new Notice("Set your User in settings first.");

        const hits = await collectActionablesInVault(this.app, this.settings.user);
        if (hits.length === 0) return void new Notice(`No actionable CR/XCR for ${this.settings.user}`);

        new ActionablePicker(this.app, hits, async (hit) => openHit(this.app, hit)).open();
      },
    });

    // Command: open panel
    this.addCommand({
      id: "inline-cr-open-actionables-panel",
      name: "Open CR/XCR Actionables panel",
      callback: async () => {
        const existing = this.app.workspace.getLeavesOfType(VIEW_TYPE_ACTIONABLES)[0];
        const leaf = existing ?? this.app.workspace.getLeaf("tab");
        await leaf.setViewState({ type: VIEW_TYPE_ACTIONABLES, active: true });
        this.app.workspace.revealLeaf(leaf);
      },
    });
  }

  onunload() {
    // Close any open leaves of our custom view on unload
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_ACTIONABLES);
  }

  async loadSettings() {
    this.settings = Object.assign({}, DEFAULT_SETTINGS, await this.loadData());
    this.bumpSettingsVersion();
  }

  async saveSettings() {
    await this.saveData(this.settings);
  }

  /* ---------------- Desktop crscan integration ---------------- */

  private async runCrscanOnVault(): Promise<Record<string, Record<string, [string, string, string, string, string]>>> {
    if (!isDesktopApp()) {
      throw new Error("External binaries only supported on desktop.");
    }

    const base = getVaultBasePath(this.app);
    if (!base) {
      throw new Error("Could not determine vault base path (vault adapter has no getBasePath).");
    }

    const bin = (this.settings.crscanPath || "crscan").trim();
    const exts = (this.settings.crscanExts || "md markdown txt")
      .split(/\s+/)
      .map((s) => s.trim())
      .filter(Boolean);

    const args: string[] = [];
    for (const ext of exts) {
      args.push("--ext", ext);
    }
    args.push(base);

    const stdout = await new Promise<string>((resolve, reject) => {
      execFile(
        bin,
        args,
        { timeout: 60_000, maxBuffer: 50 * 1024 * 1024 },
        (err, out, stderr) => {
          if (err) reject(new Error(`${err.message}\n${stderr ?? ""}`));
          else resolve(out);
        }
      );
    });

    return JSON.parse(stdout);
  }

  // Used by the panel: prefer crscan when configured & available, else fallback
  async collectActionablesForPanel(): Promise<CrscanEntry[]> {
    const user = this.settings.user;
    if (!user) return [];

    // If allowed + desktop, try crscan
    if (this.settings.useCrscan && isDesktopApp()) {
      const obj = await this.runCrscanOnVault();
      const out: CrscanEntry[] = [];

      for (const [filePath, lines] of Object.entries(obj)) {
        for (const [lineStr, tuple] of Object.entries(lines)) {
          const lineNo = Number(lineStr);
          const kind = (tuple[0] === "XCR" ? "XCR" : "CR") as "CR" | "XCR";
          out.push({
            filePath,
            lineNo,
            kind,
            reviewer: tuple[1],
            author: tuple[2],
            header: tuple[3],
            thread: tuple[4],
          });
        }
      }

      out.sort((a, b) => a.filePath.localeCompare(b.filePath) || a.lineNo - b.lineNo);
      return out;
    }

    // Fallback: in-TS scan markdown files
    const hits = await collectActionablesInVault(this.app, user);
    return hits.map((h) => ({
      filePath: h.file.path,
      lineNo: h.lineNo,
      kind: h.kind,
      reviewer: h.reviewer,
      author: h.author,
      header: h.lineText,
      thread: "",
    }));
  }
}
