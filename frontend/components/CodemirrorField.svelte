<script lang="ts">
  import CodeMirror, { basicSetup } from "codemirror-svelte"
  import { EditorView } from "@codemirror/view";

  export let doc: string;
  export let onChange: (doc: string) => Promise<void>;

  $: doc && docChanged()

  const docChanged = () => {
    onChange(doc)
  }

  let extensions = [
    basicSetup,
    EditorView.theme({
      "&": {
        "font-size": "1rem"
      }
    })
  ]
</script>

<CodeMirror {doc} {extensions}/>
