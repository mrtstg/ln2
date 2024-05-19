<script lang="ts">
  import type { CheckMessage } from "../api/types"

  export let messageStyles: string = ''
  export let messageData: CheckMessage

  const splitText = (text: string): Array<string> => text.split('\n')
</script>

<article class="message {messageStyles}">
  <div class="message-header">
    <p> { messageData.title } </p>
  </div>
  <div class="message-body">
    {#each messageData.blocks as block}
      {#if block.type == 'code'}
        <pre>
          { block.content }
        </pre>
      {:else}
          {#each splitText(block.content) as t}
            <p>{ t }</p>
          {/each}
      {/if}
    {/each}
  </div>
</article>
