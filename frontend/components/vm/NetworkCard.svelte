<script lang="ts">
  export let value: string;
  export let readOnly: boolean = false;
  export let note: string = '';
  export let deleteCallback: () => Promise<void> = async () => {}
</script>

<div class="card">
  <div class="card-content">
    <div class="field">
      <label class="label"> Название сети </label>
      <div class="control">
        <input bind:value={value} class="input" type="text" maxlength="15" readonly={readOnly}>
      </div>
      {#if note.length > 0}
        <p class="help"> { note } </p>
      {/if}
    </div>
  </div>
  {#if !readOnly}
    <footer class="card-footer">
      <button class="card-footer-item" on:click={deleteCallback}> Удалить </button>
    </footer>
  {/if}
</div>
