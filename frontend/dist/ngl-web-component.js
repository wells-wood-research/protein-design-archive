class NGLViewer extends HTMLElement {
  constructor() {
    super();
  }
  connectedCallback() {
    this.showStructure();
  }

  showStructure() {
    const pdbString = this.getAttribute("pdb-string");

    var stage = new NGL.Stage(this.id, { backgroundColor: "white" });
    var stringBlob = new Blob([pdbString], { type: "text/plain" });
    stage.loadFile("http://files.rcsb.org/download/" + pdbString + ".pdb").then(function (comp) {
      comp.addRepresentation("cartoon");
      comp.autoView();
    });
  }
}

customElements.define("ngl-viewer", NGLViewer);