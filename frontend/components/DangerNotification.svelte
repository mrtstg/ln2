<script lang="ts">
  import SimpleNotification from "./SimpleNotification.svelte"

  export let title: string;
  export let description: string;
  export let listeningPromise: Promise<null>;

  export let additionalStyle: string = '';
</script>

<SimpleNotification title={title} description={description} listeningPromise={listeningPromise} additionalStyle="is-danger {additionalStyle}"/>
