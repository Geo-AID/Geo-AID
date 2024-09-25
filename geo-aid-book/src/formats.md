# Renderers

Geo-AID supports five different renderers, also called drawers.

## LaTeX

Using [`LaTeX`](https://www.latex-project.org/), [`tikz`](https://www.overleaf.com/learn/latex/TikZ_package) and [
`tikz-euclide`](https://ctan.org/pkg/tkz-euclide), one of the two recommended ways of drawing the figure.

## SVG

Outputs the figure in the [`svg`](https://developer.mozilla.org/en-US/docs/Web/SVG) format. One of the two - and the
most tested - ways of drawing the figure.

## JSON

Machine-readable [JSON](https://www.json.org/json-en.html) format according to the Schema available in Geo-AID's
repository. Can be used to integrate other tools with Geo-AID.

## Plaintext

A human-readable format, pure [text](https://en.wikipedia.org/wiki/Plain_text). Contains descriptions of the positions
of each object in the figure.

## GeoGebra

You can import Geogebra (*.ggb) output directly in GeoGebra by either choosing "load" from the menu in the app
or simply by dragging the file onto the tool. This format has not been extensively tested and you may encounter bugs.
If you do, please report them.
