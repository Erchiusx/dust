const componentStylesheet = '<link rel="stylesheet" href="/components.css">';
const dieFaces = ["✦", "●", "▲", "■", "◆", "✦"];

class DustDie extends HTMLElement {
  connectedCallback() {
    if (this.shadowRoot) return;

    const root = this.attachShadow({ mode: "open" });
    const face = Number(this.getAttribute("face") ?? 0);
    root.innerHTML = `
      ${componentStylesheet}
      <span aria-label="骰面 ${face + 1}">${dieFaces[face % dieFaces.length]}</span>
    `;
  }
}

class DustCombatField extends HTMLElement {
  connectedCallback() {
    if (this.shadowRoot) return;

    const root = this.attachShadow({ mode: "open" });
    const opponent = this.getAttribute("side") === "opponent";
    root.innerHTML = `
      ${componentStylesheet}
      <div class="lane" aria-label="${opponent ? "对方" : "己方"}战斗区域"></div>
    `;
  }
}

class DustArtifactField extends HTMLElement {
  connectedCallback() {
    if (this.shadowRoot) return;

    const root = this.attachShadow({ mode: "open" });
    const opponent = this.getAttribute("side") === "opponent";
    const column = this.getAttribute("column") ?? "1";
    const labels = {
      1: ["锋刃原型", "主动 / 攻击"],
      2: ["回响原型", "主动 / 控制"],
      3: ["终式原型", "主动 / 终极"],
    };
    const [name, ability] = labels[column] ?? labels[1];
    const dieArea = {
      1: { kind: "attack", label: "攻击骰" },
      2: { kind: "defense", label: "防御骰" },
      3: { kind: "thought", label: "冥想骰" },
    }[column];

    root.innerHTML = `
      ${componentStylesheet}
      <article class="artifact">
        <span class="column-mark"></span>
        <div>
          <h3>${opponent ? "对方" : "己方"} · ${name}</h3>
          <p>${ability}</p>
        </div>
        ${column === "3" ? `<span class="charge" title="充能层数">${opponent ? 1 : 2}</span>` : ""}
      </article>
      <div class="dice-bay">
        <div class="die-area" data-kind="${dieArea.kind}" aria-label="${opponent ? "对方" : "己方"}${dieArea.label}区域">
          ${
            opponent
              ? `<dust-die face="${Number(column) + 1}"></dust-die>`
              : `<dust-die face="${Number(column) + 2}"></dust-die>
                 <dust-die face="${Number(column) + 3}"></dust-die>`
          }
          <span class="empty-slot">+</span>
        </div>
      </div>
    `;
  }
}

class DustBoardZone extends HTMLElement {
  connectedCallback() {
    if (this.shadowRoot) return;

    const root = this.attachShadow({ mode: "open" });
    const kind = this.getAttribute("kind") ?? "artifact";
    const column = this.getAttribute("column") ?? "1";
    const title = this.getAttribute("label") ?? "区域";
    const fieldTag = kind === "combat" ? "dust-combat-field" : "dust-artifact-field";
    const columnAttribute = kind === "combat" ? "" : `column="${column}"`;

    root.innerHTML = `
      ${componentStylesheet}
      <span class="zone-label">${title}</span>
      <${fieldTag} class="opponent" side="opponent" ${columnAttribute}></${fieldTag}>
      <${fieldTag} class="player" side="player" ${columnAttribute}></${fieldTag}>
    `;
  }
}

class DustSealMarker extends HTMLElement {
  static observedAttributes = ["owner"];

  connectedCallback() {
    if (!this.shadowRoot) {
      const root = this.attachShadow({ mode: "open" });
      root.innerHTML = `
        ${componentStylesheet}
        <span class="aura"></span>
        <span class="ring"></span>
        <img src="/assets/ui/dust-seal.webp" alt="尘印" />
      `;
    }

    this.updateLabel();
  }

  attributeChangedCallback() {
    this.updateLabel();
  }

  updateLabel() {
    const owner = this.getAttribute("owner");
    const ownerLabel =
      owner === "opponent" ? "对方持有" : owner === "player" ? "己方持有" : "无人持有";
    this.setAttribute("aria-label", `尘印，${ownerLabel}`);
  }
}

class DustPublicInfo extends HTMLElement {
  static observedAttributes = [
    "round",
    "dust-fall",
    "active-side",
    "phase",
    "opponent-life",
    "opponent-speed",
    "opponent-will",
    "player-life",
    "player-speed",
    "player-will",
  ];

  connectedCallback() {
    if (!this.shadowRoot) {
      const root = this.attachShadow({ mode: "open" });
      root.innerHTML = `
        ${componentStylesheet}
        <div class="timeline opponent" data-side="opponent" aria-label="对方回合阶段"></div>
        <section class="panel" aria-label="公开信息">
          <div class="stats" aria-label="双方公开属性">
            <div class="stat-column">
              <b class="opponent" data-value="opponent-life"></b>
              <span>生命</span>
              <b class="player" data-value="player-life"></b>
            </div>
            <div class="stat-column">
              <b class="opponent" data-value="opponent-speed"></b>
              <span>速度</span>
              <b class="player" data-value="player-speed"></b>
            </div>
            <div class="stat-column">
              <b class="opponent" data-value="opponent-will"></b>
              <span>意志</span>
              <b class="player" data-value="player-will"></b>
            </div>
          </div>
          <div class="public">
            <div class="metric"><b data-value="dust-fall"></b><span>尘落</span></div>
            <div class="metric"><b data-value="round"></b><span>回合</span></div>
          </div>
        </section>
        <div class="timeline player" data-side="player" aria-label="己方回合阶段"></div>
      `;
    }

    this.render();
  }

  attributeChangedCallback() {
    this.render();
  }

  render() {
    if (!this.shadowRoot) return;

    const phases = [
      ["supply", "补充"],
      ["reroll", "重掷"],
      ["dust-up", "尘起"],
      ["main", "主要"],
      ["end", "结束"],
    ];
    const activeSide = this.getAttribute("active-side") ?? "none";
    const activePhase = this.getAttribute("phase") ?? "none";

    this.shadowRoot.querySelector('[data-value="dust-fall"]').textContent =
      this.getAttribute("dust-fall") ?? "0";
    this.shadowRoot.querySelector('[data-value="round"]').textContent =
      this.getAttribute("round") ?? "1";

    [
      "opponent-life",
      "opponent-speed",
      "opponent-will",
      "player-life",
      "player-speed",
      "player-will",
    ].forEach((name) => {
      this.shadowRoot.querySelector(`[data-value="${name}"]`).textContent =
        this.getAttribute(name) ?? "-";
    });

    this.shadowRoot.querySelectorAll(".timeline").forEach((timeline) => {
      const side = timeline.dataset.side;
      timeline.innerHTML = phases
        .map(
          ([phase, label]) =>
            `<span class="tick ${side === activeSide && phase === activePhase ? "active" : ""}" data-label="${label}" title="${phase}"></span>`,
        )
        .join("");
    });
  }
}

class DustGameBoard extends HTMLElement {
  connectedCallback() {
    if (this.shadowRoot) return;

    const root = this.attachShadow({ mode: "open" });
    root.innerHTML = `
      ${componentStylesheet}
      <header>
        <span class="connection"></span>
        <span>布局预览 · 未连接</span>
        <span class="seal-demo" aria-label="尘印光效测试">
          <button type="button" data-seal-owner="opponent" aria-pressed="false">对方</button>
          <button type="button" data-seal-owner="none" aria-pressed="false">无人</button>
          <button type="button" data-seal-owner="player" aria-pressed="true">己方</button>
        </span>
      </header>
      <dust-board-zone kind="combat" label="战斗区域"></dust-board-zone>
      <dust-board-zone kind="artifact" column="1" label="第一列"></dust-board-zone>
      <dust-board-zone kind="artifact" column="2" label="第二列"></dust-board-zone>
      <dust-board-zone kind="artifact" column="3" label="第三列"></dust-board-zone>
      <dust-public-info
        dust-fall="3"
        round="2"
        active-side="player"
        phase="main"
        opponent-life="18"
        opponent-speed="4"
        opponent-will="7"
        player-life="20"
        player-speed="4"
        player-will="8"
      ></dust-public-info>
      <dust-seal-marker owner="player"></dust-seal-marker>
    `;

    const marker = root.querySelector("dust-seal-marker");
    root.querySelectorAll("[data-seal-owner]").forEach((button) => {
      button.addEventListener("click", () => {
        marker.setAttribute("owner", button.dataset.sealOwner);
        root.querySelectorAll("[data-seal-owner]").forEach((candidate) => {
          candidate.setAttribute("aria-pressed", String(candidate === button));
        });
      });
    });
  }
}

customElements.define("dust-die", DustDie);
customElements.define("dust-combat-field", DustCombatField);
customElements.define("dust-artifact-field", DustArtifactField);
customElements.define("dust-board-zone", DustBoardZone);
customElements.define("dust-seal-marker", DustSealMarker);
customElements.define("dust-public-info", DustPublicInfo);
customElements.define("dust-game-board", DustGameBoard);
