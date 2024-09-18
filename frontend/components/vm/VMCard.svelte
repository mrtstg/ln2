<script lang="ts">
  import { onMount } from 'svelte'
  import type { TemplateVM, VM, VMType } from "../../api/types/vm"
  import type { VMTemplate } from "../../api/types/template"

  // TODO: diff types 
  let templateHint: string = ''

  const updateTemplateHint = () => {
    let matchedTemplate = availableTemplates.filter(v => v.name == data.template).pop()
    if (matchedTemplate != undefined) {
      templateHint = matchedTemplate.comment
    }
  }

  export let data: TemplateVM
  export let deleteCallback: () => Promise<void> = async () => {}
  export let availableNetworks: Array<string> = Array()
  export let availableTemplates: Array<VMTemplate> = Array()
</script>

<div class="card">
  <div class="card-content">
    <div class="control">
      <label class="label"> Шаблон VM </label>
      <div class="select">
        <select bind:value={data.template} on:change={updateTemplateHint}>
          {#each availableTemplates as item}
            <option value={item.name}> { item.name } </option>
          {/each}
        </select>
      </div>
      <p class="hint"> { templateHint } </p>
    </div>
  </div>
  <footer class="card-footer">
    <button class="card-footer-item" on:click={deleteCallback}> Удалить </button>
  </footer>
</div>
