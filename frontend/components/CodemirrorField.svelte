<script lang="ts">
  import CodeMirror, { basicSetup } from "codemirror-svelte"
  import { EditorView, ViewPlugin } from "@codemirror/view";

  export let doc: string;
  export let onChange: (doc: string) => Promise<void>;

  let extensions = [
    basicSetup,
    EditorView.theme({
      "&": {
        "font-size": "1rem"
      }
    }),
    ViewPlugin.fromClass(class {
      constructor(view) {}

      update(update) {
        if (update.docChanged) {
          doc = update.state.doc.text.join('\n')
          onChange(doc).then()
        }
      }
    })
  ]
</script>

<CodeMirror {doc} {extensions}/>
