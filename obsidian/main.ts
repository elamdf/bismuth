import { App, Plugin, PluginSettingTab, Setting, Notice } from "obsidian";
import { MarkdownView } from "obsidian";
import type { EditorView } from "@codemirror/view";


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
  const doc = view.state.doc;

  const threads = scanThreads(view, user);

  const lineDeco = (cls: string) => Decoration.line({ class: cls });

  for (const t of threads) {
    const blockClass = t.actionable ? "inline-cr-block-actionable" : "inline-cr-block";
    const headerClass = t.actionable ? "inline-cr-header-actionable" : "inline-cr-header-nonactionable";

    for (let ln = t.startLineNo; ln <= t.endLineNo; ln++) {
      const line = doc.line(ln);

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

class InlineCrSettingTab extends PluginSettingTab {
  plugin: InlineCrPlugin;

  constructor(app: App, plugin: InlineCrPlugin) {
    super(app, plugin);
    this.plugin = plugin;
  }

  display(): void {
    const { containerEl } = this;
    containerEl.empty();

    containerEl.createEl("h2", { text: "Inline CR settings" });

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

  // Obsidian’s CM6 EditorView is stashed on the editor object in a couple common places.
  const editorAny: any = mdView.editor;

  // Most common: editor.cm is the EditorView
  const cm = editorAny?.cm;
  if (cm && typeof cm === "object" && "state" in cm && "dispatch" in cm) return cm as EditorView;

  // Sometimes: editor.cm.cm is the EditorView
  const cm2 = editorAny?.cm?.cm;
  if (cm2 && typeof cm2 === "object" && "state" in cm2 && "dispatch" in cm2) return cm2 as EditorView;

  return null;
};


    this.addCommand({
      id: "inline-cr-next-actionable",
      name: "Next actionable CR/XCR",
      callback: () => {
        const ev = getEditorView();
        if (!ev) return;
        const ok = jumpToActionable(ev, this.settings.user, "next");
        if (!ok) new Notice(`No actionable CR/XCR for ${this.settings.user}`);
      }
    });

    this.addCommand({
      id: "inline-cr-prev-actionable",
      name: "Previous actionable CR/XCR",
      callback: () => {
        const ev = getEditorView();
        if (!ev) return;
        const ok = jumpToActionable(ev, this.settings.user, "prev");
        if (!ok) new Notice(`No actionable CR/XCR for ${this.settings.user}`);
      }
    });

this.addCommand({
  id: "inline-cr-insert-reply-at-end",
  name: "Inline CR: Insert reply at end of thread",
  callback: () => {
    const ev = getEditorView();
    if (!ev) {
      new Notice("Inline CR: no active markdown editor found.");
      return;
    }
    insertReplyAtEndOfThread(ev, this.settings.user);
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
