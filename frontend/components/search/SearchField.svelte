<script lang="ts">
  export let query: string = ''
  export let queryWrapper: (query: string) => Promise<void> = async (_) => {}
  export let maxLength: number = 30

  const cleanQuery = () => {
    query = ''
    queryWrapper(query)
  }
</script>

<div class="is-flex is-flex-direction-row is-justify-content-space-between is-fullwidth mb-5">
  <input maxlength="{maxLength}" class="input is-flex-grow-1" type="text" placeholder="Ваш запрос" bind:value={query}>
  <button class="button ml-3" on:click={() => queryWrapper(query)}> Искать </button>
  <button class="button ml-3" on:click={() => cleanQuery()}> Очистить </button>
</div>
