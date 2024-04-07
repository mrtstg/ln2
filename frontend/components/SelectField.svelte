<script lang="ts">
  export let items: string[]
  export let title: string
  export let selectCallback: (item: string) => Promise<void>;

  if (items.length > 0) {
    selectCallback(items[0]).then()
  }
</script>

<div class="field">
  <label class="label"> { title } </label>
  <div class="control">
    <div class="select">
      <select on:change={async (event) => { selectCallback(event.target.value) }}>
        {#each items as item}
          <option value={item}> { item } </option>
        {/each}
      </select>
    </div>
  </div>
</div>
