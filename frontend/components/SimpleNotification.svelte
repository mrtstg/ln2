<script lang="ts">
  export let title: string;
  export let description: string;
  export let listeningPromise: Promise<null>

  export let additionalStyle: string = '';
</script>

{#await listeningPromise}
  <article class="message {additionalStyle}">
    <div class="message-header">
      <p> { title } </p>
    </div>
    <div class="message-body">
      { description }
    </div>
  </article>
{/await}
