<script lang="ts">
  import CodeMirror, { basicSetup } from "codemirror-svelte"
  import { EditorViewConfig, EditorView, ViewPlugin, keymap } from "@codemirror/view";
  import { EditorState } from "@codemirror/state";
  import { indentWithTab } from "@codemirror/commands"

  export let readonly: boolean = false;
  export let doc: string;
  export let onChange: (doc: string) => Promise<void> = async (arg0) => {};

  let codemirrorView: EditorView | undefined = undefined

  export const cleanField = () => {
    if (codemirrorView != undefined) {
      codemirrorView.setState(EditorState.create({doc: '', extensions: extensions}))
    }
  }

  let extensions = [
    basicSetup,
    keymap.of([indentWithTab]),
    EditorView.theme({
      "&": {
        "font-size": "1rem"
      }
    }),
    ViewPlugin.fromClass(class {
      constructor(view) {
        codemirrorView = view
      }

      update(update) {
        if (update.docChanged) {
          doc = update.state.doc.text.join('\n')
          onChange(doc).then()
        }
      }
    })
  ]

  if (readonly) {
    extensions = [
      EditorState.readOnly.of(true),
      ...extensions
    ]
  }
</script>

<CodeMirror {doc} {extensions}/>
