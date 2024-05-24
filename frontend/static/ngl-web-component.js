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
    stage.loadFile("/pdb_files/" + pdbString.toUpperCase() + ".pdb1.gz").then(function (comp) {
      comp.addRepresentation("cartoon");
      comp.autoView();
    });
  }
}

customElements.define("ngl-viewer", NGLViewer);