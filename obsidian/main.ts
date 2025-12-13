// NOTE: chatgpt wrote most of this using the emacs implementation as a reference
import {
  App,
  Plugin,
  PluginSettingTab,
  Setting,
  Notice,
  MarkdownView,
  TFile,
  FuzzySuggestModal
} from "obsidian";

import {
  EditorView,
  Decoration,
  DecorationSet,
  ViewPlugin,
  ViewUpdate
} from "@codemirror/view";
import { RangeSetBuilder, Extension } from "@codemirror/state";

type InlineCrSettings = {
  user: string;
};

const DEFAULT_SETTINGS: InlineCrSettings = {
  user: ""
};

type ThreadInfo = {
  headerLineNo: number;
  startLineNo: number; // header line
  endLineNo: number;   // inclusive
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

function headerMatch(lineText: string): null | {
  kind: "CR" | "XCR";
  reviewer: string;
  author: string;
} {
  // > CR reviewer for author: ...
  // > XCR reviewer for author: ...
  const m = lineText.match(/^\s*>\s*(X?CR)\s+(\S+)\s+for\s+([^:]+)\s*:/);
  if (!m) return null;
  const kind = (m[1] === "XCR" ? "XCR" : "CR") as "CR" | "XCR";
  return { kind, reviewer: m[2], author: m[3].trim() };
}

function isThreadLine(lineText: string): boolean {
  // Any quoted line
  return /^\s*>/.test(lineText);
}

function bodyAuthorMatch(lineText: string): string | null {
  // Matches: > name: blah
  // Returns "name" or null
  const m = lineText.match(/^\s*>\s*(\S+)\s*:/);
  return m ? m[1] : null;
}

function computeActionable(
  h: { kind: "CR" | "XCR"; reviewer: string; author: string },
  user: string
): boolean {
  if (!user) return false;
  // elisp:
  // CR actionable if whom (author) == user
  // XCR actionable if who (reviewer) == user
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
      actionable
    });

    lineNo = end + 1;
  }

  return threads;
}

function findThreadAtLine(view: EditorView, user: string, lineNo: number): ThreadInfo | null {
  const threads = scanThreads(view, user);
  for (const t of threads) {
    if (lineNo >= t.startLineNo && lineNo <= t.endLineNo) return t;
  }
  return null;
}

function lastAuthorInThread(view: EditorView, thread: ThreadInfo): string | null {
  const doc = view.state.doc;
  let last: string | null = null;

  // Start from the line after the header, scan through thread body
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
    const headerClass = t.actionable ? "inline-cr-header-actionable" : "inline-cr-header-nonactionable";

    for (let ln = t.startLineNo; ln <= t.endLineNo; ln++) {
      const line = view.state.doc.line(ln);

      // Whole thread block background
      builder.add(line.from, line.from, lineDeco(blockClass));

      // Header styling
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
  const threads = scanThreads(view, user).filter(t => t.actionable);
  if (threads.length === 0) return false;

  const pos = view.state.selection.main.head;
  const curLine = doc.lineAt(pos).number;

  const headerLines = threads.map(t => t.headerLineNo).sort((a, b) => a - b);

  const pick = (() => {
    if (direction === "next") {
      for (const ln of headerLines) if (ln > curLine) return ln;
      return headerLines[0]; // wrap
    } else {
      for (let i = headerLines.length - 1; i >= 0; i--) {
        if (headerLines[i] < curLine) return headerLines[i];
      }
      return headerLines[headerLines.length - 1]; // wrap
    }
  })();

  const target = doc.line(pick);
  view.dispatch({
    selection: { anchor: target.from },
    scrollIntoView: true
  });
  return true;
}

function insertReplyAtEndOfThread(view: EditorView, user: string): boolean {
  const doc = view.state.doc;
  const pos = view.state.selection.main.head;
  const curLine = doc.lineAt(pos).number;

  const thread = findThreadAtLine(view, user, curLine);
  if (!thread) {
    // Not in a thread → normal newline
    view.dispatch({
      changes: { from: pos, to: pos, insert: "\n" },
      selection: { anchor: pos + 1 },
      scrollIntoView: true
    });
    return false;
  }

  const lastAuthor = lastAuthorInThread(view, thread);
  const prefix = (lastAuthor && lastAuthor === user) ? "> " : `> ${user}: `;
  const insertText = "\n" + prefix;

  const endLine = doc.line(thread.endLineNo);
  const insertPos = endLine.to;

  view.dispatch({
    changes: { from: insertPos, to: insertPos, insert: insertText },
    selection: { anchor: insertPos + insertText.length },
    scrollIntoView: true
  });

  return true;
}

/* ---------------- Vault-wide aggregation ---------------- */

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
          author: hm.author
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

function formatActionablesNote(hits: ActionableHit[], user: string): string {
  const out: string[] = [];
  out.push(`# Inline CR actionables for \`${user}\``);
  out.push("");
  out.push(`Found **${hits.length}** actionable thread(s).`);
  out.push("");

  let curFile: string | null = null;
  for (const h of hits) {
    if (curFile !== h.file.path) {
      curFile = h.file.path;
      out.push(`## ${curFile}`);
      out.push("");
    }

    // Note: Obsidian doesn't universally support line-jump links.
    // We include the line number + a file link; the picker command can jump precisely.
    out.push(`- **Line ${h.lineNo}** (${h.kind} ${h.reviewer} for ${h.author}): [[${h.file.path}]]`);
    out.push(`  - \`${h.lineText.trim().replace(/`/g, "\\`")}\``);
  }

  out.push("");
  return out.join("\n");
}

async function openHit(app: App, hit: ActionableHit) {
  const leaf = app.workspace.getLeaf(false);
  await leaf.openFile(hit.file);

  const mdView = app.workspace.getActiveViewOfType(MarkdownView);
  if (!mdView) return;

  const editorAny: any = mdView.editor;
  const ev: any =
    (editorAny?.cm && typeof editorAny.cm === "object" && "state" in editorAny.cm && "dispatch" in editorAny.cm)
      ? editorAny.cm
      : editorAny?.cm?.cm;

  if (!ev) return;

  const needle = hit.lineText.trim();
  const docText = ev.state.doc.toString();
  const idx = docText.indexOf(needle);

  if (idx >= 0) {
    ev.dispatch({
      selection: { anchor: idx },
      scrollIntoView: true
    });
  } else {
    // fallback: go to line number if possible
    // (CM6 has line APIs, but we only have a raw idx fallback; keep it simple)
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

    containerEl.createEl("h2", { text: "Bismuth settings" });

    new Setting(containerEl)
      .setName("User")
      .setDesc("Used to determine which CR/XCR headers are actionable and how replies are prefixed.")
      .addText(text =>
        text
          .setPlaceholder("e.g. elamdf")
          .setValue(this.plugin.settings.user)
          .onChange(async (value) => {
            this.plugin.settings.user = value.trim();
            await this.plugin.saveSettings();
            this.plugin.bumpSettingsVersion();
          })
      );
  }
}

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

    this.addCommand({
      id: "inline-cr-next-actionable",
      name: "Next actionable CR/XCR in this file",
      callback: () => {
        const ev = getEditorView();
        if (!ev) {
          new Notice("no active markdown editor found.");
          return;
        }
        const ok = jumpToActionable(ev, this.settings.user, "next");
        if (!ok) new Notice(`No actionable CR/XCR for ${this.settings.user}`);
      }
    });

    this.addCommand({
      id: "inline-cr-prev-actionable",
      name: "Previous actionable CR/XCR in this file",
      callback: () => {
        const ev = getEditorView();
        if (!ev) {
          new Notice("no active markdown editor found.");
          return;
        }
        const ok = jumpToActionable(ev, this.settings.user, "prev");
        if (!ok) new Notice(`No actionable CR/XCR for ${this.settings.user}`);
      }
    });

    this.addCommand({
      id: "inline-cr-insert-reply-at-end",
      name: "Insert reply at end of thread",
      callback: () => {
        const ev = getEditorView();
        if (!ev) {
          new Notice("no active markdown editor found.");
          return;
        }
        if (!this.settings.user) {
          new Notice("set your User in settings first.");
          return;
        }
        insertReplyAtEndOfThread(ev, this.settings.user);
      }
    });

    this.addCommand({
      id: "inline-cr-list-actionables-vault",
      name: "List all actionables in vault",
      callback: async () => {
        if (!this.settings.user) {
          new Notice("set your User in settings first.");
          return;
        }

        const hits = await collectActionablesInVault(this.app, this.settings.user);

        const reportPath = "Actionables.md";
        const reportText = formatActionablesNote(hits, this.settings.user);

        const existing = this.app.vault.getAbstractFileByPath(reportPath);
        let reportFile: TFile;

        if (existing && existing instanceof TFile) {
          reportFile = existing;
          await this.app.vault.modify(reportFile, reportText);
        } else {
          reportFile = await this.app.vault.create(reportPath, reportText);
        }

        await this.app.workspace.getLeaf(false).openFile(reportFile);
        new Notice(`wrote ${hits.length} actionable(s) to ${reportPath}`);
      }
    });

    this.addCommand({
      id: "inline-cr-pick-actionable",
      name: "Pick an actionable (jump)",
      callback: async () => {
        if (!this.settings.user) {
          new Notice("set your User in settings first.");
          return;
        }
        const hits = await collectActionablesInVault(this.app, this.settings.user);
        if (hits.length === 0) {
          new Notice(`No actionable CR/XCR for ${this.settings.user}`);
          return;
        }
        new ActionablePicker(this.app, hits, async (hit) => openHit(this.app, hit)).open();
      }
    });
  }

  onunload() {}

  async loadSettings() {
    this.settings = Object.assign({}, DEFAULT_SETTINGS, await this.loadData());
    this.bumpSettingsVersion();
  }

  async saveSettings() {
    await this.saveData(this.settings);
  }
}
