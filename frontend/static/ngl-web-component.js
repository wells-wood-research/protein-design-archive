class NGLViewer extends HTMLElement {
  constructor() {
    super();
  }
  connectedCallback() {
    this.showStructure();
  }
  // attributeChangedCallback() {
  //   this.showStructure();
  // }
  // static get observedAttributes() {
  //   return ["pdb-string"];
  // }

  showStructure() {
    const pdbString = this.getAttribute("pdb-string");

    var stage = new NGL.Stage(this.id, { backgroundColor: "white" });
    var stringBlob = new Blob([pdbString], { type: "text/plain" });
    stage.loadFile("http://files.rcsb.org/download/" + pdbString + ".cif").then(function (comp) {
      comp.addRepresentation("cartoon");
      comp.autoView();
    });
    //stage.loadFile(stringBlob, { ext: "pdb" }).then(function (component) {
    //  component.addRepresentation("cartoon"); 
    //  component.autoView();
    //});
  }
}

customElements.define("ngl-viewer", NGLViewer);