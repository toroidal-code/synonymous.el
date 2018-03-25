# synonymous.el
A thesaurus at your fingertips

Synonymous is a simple Emacs plugin that allows you to select a word and replace it with a synonym or antonym. It
uses `x-menu` for offering choices, and the simple functions `synonymous-synonyms` and `synonymous-antonyms` for
displaying the selection menu.

It's paired with the [synonymous server](https://github.com/toroidal-code/synonymous) if you would like to run the server locally, otherwise it accesses an instance running on OpenShift.
