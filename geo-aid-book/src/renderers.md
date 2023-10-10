# Renderers

Geo-AID supports four different renderers, also called drawers. One more is planned.

## JSON

The popularized [JSON](https://www.json.org/json-en.html) format. Currently very not standardized, mostly useless - enforcing a better standard is work in progress.

## Raw

A human-readable format, pure [text](https://en.wikipedia.org/wiki/Plain_text). Contains descriptions of the positions of each object in the figure.

## LaTeX

Using [`LaTeX`](https://www.latex-project.org/), [`tikz`](https://www.overleaf.com/learn/latex/TikZ_package) and [`tikz-euclide`](https://ctan.org/pkg/tkz-euclide), one of the two recommended ways of drawing the figure.

## SVG

Outputs the figure in the [`svg`](https://developer.mozilla.org/en-US/docs/Web/SVG) format. One of the two - and the most tested - way of drawing the figure.

## GeoGebra

Work in progress, Geo-AID will support outputting in the GeoGebra format importable in the very tool.