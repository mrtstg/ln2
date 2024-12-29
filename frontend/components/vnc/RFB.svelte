<script lang="ts">
  import RFB, { NoVncEvent } from "@novnc/novnc/lib/rfb"
  import { onMount } from "svelte";
  
  let rfb: RFB | null = null
  let quality: number = 7
  let parent: HTMLElement;

  export let desktopCallback: (e: CustomEvent<{ name: string }>) => void;
  export let connectCallback: (state: boolean) => void
  export let url: string;
  export let onConnectCallback: () => Promise<void> = async () => {}
  export let onDisconnectCallback: () => Promise<void> = async () => {}

  onMount(() => {
    rfb = new RFB(parent, url)
    rfb.compressionLevel = 3
    rfb.qualityLevel = quality
    rfb.viewOnly = false
    rfb.dragViewport = false
    rfb.clipViewport = false
    rfb.scaleViewport = true

    rfb.addEventListener("desktopname", desktopCallback)
    rfb.addEventListener("connect", (_) => { connectCallback(true); onConnectCallback() })
    rfb.addEventListener("disconnect", (_) => { connectCallback(false); onDisconnectCallback() })

    return () => {
      rfb.disconnect()
    }
  })
</script>

<div class="w-max-content h-max-content">
  <div class="vnc-screen-container" bind:this={parent}></div>
</div>
