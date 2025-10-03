import { App, MarkdownPostProcessorContext, Plugin, PluginSettingTab, Setting } from "obsidian";

interface InlineCRSettings {
  username: string;
}

const DEFAULT_SETTINGS: InlineCRSettings = {
  username: ""
};

const HEADER_REGEX = /^\s*(X?CR)\s+(\S+)\s+for\s+([^:]+):/i;

export default class InlineCRPlugin extends Plugin {
  settings: InlineCRSettings = DEFAULT_SETTINGS;

  async onload(): Promise<void> {
    await this.loadSettings();

    this.addSettingTab(new InlineCRSettingTab(this.app, this));

    this.registerMarkdownPostProcessor((element: HTMLElement, _context: MarkdownPostProcessorContext) => {
      this.decorateContainer(element);
    });
  }

  private decorateContainer(container: HTMLElement): void {
    const blockquotes = container.querySelectorAll("blockquote");

    blockquotes.forEach((blockquote) => {
      this.decorateBlockquote(blockquote as HTMLElement);
    });
  }

  private decorateBlockquote(blockquote: HTMLElement): void {
    const blockChildren = Array.from(blockquote.children).filter((child): child is HTMLElement =>
      child instanceof HTMLElement && (child.tagName === "P" || child.tagName === "DIV")
    );

    if (blockChildren.length === 0) {
      this.decorateBlockElement(blockquote);
      return;
    }

    blockChildren.forEach((child) => {
      this.decorateBlockElement(child);
    });
  }

  private decorateBlockElement(block: HTMLElement): void {
    const nodes = Array.from(block.childNodes);
    const documentRef = block.ownerDocument;

    if (!documentRef) {
      return;
    }

    const segments: Node[][] = [];
    let current: Node[] = [];

    nodes.forEach((node) => {
      if (node.nodeName === "BR") {
        segments.push(current);
        current = [];
      } else {
        current.push(node);
      }
    });

    segments.push(current);

    block.textContent = "";

    segments.forEach((segment, index) => {
      const decorated = this.decorateSegment(segment, documentRef);
      if (decorated) {
        block.appendChild(decorated);
      }

      if (index < segments.length - 1) {
        block.appendChild(documentRef.createElement("br"));
      }
    });
  }

  private decorateSegment(segment: Node[], doc: Document): Node | null {
    if (!segment.length) {
      return doc.createTextNode("");
    }

    const textContent = segment.map((node) => node.textContent ?? "").join("").trim();
    const match = textContent.match(HEADER_REGEX);

    const fragment = doc.createDocumentFragment();
    segment.forEach((node) => fragment.appendChild(node));

    if (!match) {
      return fragment;
    }

    const kind = match[1].toUpperCase();
    const reviewer = match[2];
    const author = match[3];
    const actionable = this.isActionable(kind, reviewer, author);

    const span = doc.createElement("span");
    span.addClass("inline-cr-line");
    span.addClass(actionable ? "inline-cr-actionable-line" : "inline-cr-nonactionable-line");
    span.setAttribute("data-inline-cr-kind", kind);
    span.setAttribute("data-inline-cr-reviewer", reviewer.trim());
    span.setAttribute("data-inline-cr-author", author.trim());
    span.setAttribute("data-inline-cr-actionable", actionable ? "true" : "false");

    span.appendChild(fragment);
    return span;
  }

  private isActionable(kind: string, reviewer: string, author: string): boolean {
    const username = this.normalizeName(this.settings.username);

    if (!username) {
      return false;
    }

    if (kind === "CR") {
      return this.normalizeName(author) === username;
    }

    if (kind === "XCR") {
      return this.normalizeName(reviewer) === username;
    }

    return false;
  }

  private normalizeName(value: string): string {
    return value.trim().toLowerCase();
  }

  private async loadSettings(): Promise<void> {
    const stored = await this.loadData();
    this.settings = Object.assign({}, DEFAULT_SETTINGS, stored);
  }

  async saveSettings(): Promise<void> {
    await this.saveData(this.settings);
  }
}

class InlineCRSettingTab extends PluginSettingTab {
  plugin: InlineCRPlugin;

  constructor(app: App, plugin: InlineCRPlugin) {
    super(app, plugin);
    this.plugin = plugin;
  }

  display(): void {
    const { containerEl } = this;
    containerEl.empty();

    containerEl.createEl("h2", { text: "Inline CR Settings" });

    new Setting(containerEl)
      .setName("Username")
      .setDesc("Used to determine whether CR/XCR comments require your attention.")
      .addText((text) =>
        text
          .setPlaceholder("github-username")
          .setValue(this.plugin.settings.username)
          .onChange(async (value) => {
            this.plugin.settings.username = value;
            await this.plugin.saveSettings();
          })
      );
  }
}
