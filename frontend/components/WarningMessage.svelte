<script lang="ts">
  import SimpleMessage from "./SimpleMessage.svelte";

  export let title: string;
  export let description: string;
  export let additionalStyle: string = "";
</script>

<SimpleMessage title={title} description={description} additionalStyle="is-warning {additionalStyle}"/>

