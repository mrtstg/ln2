<script lang="ts">
  import { onMount } from "svelte";

  import RFB from "@novnc/novnc/lib/rfb"

  let parent: HTMLElement;
  let rfb: RFB | null = null

  export let url: string;


  onMount(() => {
    rfb = new RFB(parent, url)

    return () => {
      rfb?.disconnect()
    }
  })
</script>

<div bind:this={parent}></div>
