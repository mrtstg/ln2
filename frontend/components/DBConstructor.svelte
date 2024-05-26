<script lang="ts">
  import type { TableData, DatabaseData, ColumnData, ColumnType } from "../api/db"
  import { allTypes } from "../api/db";

  export let changeCallback: (tables: Array<TableData>) => void = () => {};
  export let dbData: DatabaseData = {tables: []}
  
  let allFields: Array<String>

  const updateReferences = () => {
    let tableNames = dbData.tables.map(el => el.name)
    for (let i = 0; i < dbData.tables.length; i++) {
      for (let ci = 0; ci < dbData.tables[i].columns.length; ci++) {
        if (dbData.tables[i].columns[ci].references != null) {
          let refTable = dbData.tables[i].columns[ci].references!.split(".")[0]
          if (!tableNames.includes(refTable)) {
            dbData.tables[i].columns[ci].references = null
          }
        }
      }
    }
  }

  const correctTableName = (tableIndex: number) => {
    dbData.tables[tableIndex].name = dbData.tables[tableIndex].name.replace(".", "").replace(" ", "")
  }

  const updateAllFields = () => {
    allFields = dbData.tables.map(el => el.columns.map(col => el.name + "." + col.name)).flat()
  }
  
  const addTable = () => {
    dbData.tables = [...dbData.tables, {name: "Впишите имя таблицы", columns: []}]
    changeCallback(dbData.tables)
  }

  const addCell = (tableIndex: number) => {
    dbData.tables[tableIndex].columns = [
      ...dbData.tables[tableIndex].columns,
      {name: "", type: { type: "varchar" }, primary: false, unique: false, null: false, references: null}
    ]
    changeCallback(dbData.tables)
    updateAllFields()
  }

  const deleteTable = (tableIndex: number) => {
    dbData.tables.splice(tableIndex, 1)
    dbData.tables = [...dbData.tables]
    changeCallback(dbData.tables)
    updateAllFields()
  }

  const removeCell = (tableIndex: number, cellIndex: number) => {
    dbData.tables[tableIndex].columns.splice(cellIndex, 1)
    dbData.tables[tableIndex] = dbData.tables[tableIndex]
    changeCallback(dbData.tables)
    updateAllFields()
  }

  updateAllFields()
  updateReferences()
</script>

<button class="button is-fullwidth" on:click={addTable}> Добавить таблицу </button>
{#each dbData.tables as table, tableIndex}
<div class="py-5">
  <div class="is-flex is-flex-direction-row is-fullwidth">
    <button class="button is-danger mr-3" on:click={() => deleteTable(tableIndex)}> X </button>
    <input class="input is-fullwidth" bind:value={table.name} on:change={() => { updateReferences(); changeCallback(dbData.tables); updateAllFields()}}/>
  </div>  
  <table class="table is-fullwidth">
    <thead>
      <tr>
        <th> Название колонки </th>
        <th> Тип </th>
        <th> Первичный ключ? </th>
        <th> Уникальное? </th>
        <th> NULL? </th>
        <th> Cсылается на... </th>
      </tr>
    </thead>
    <tbody>
      {#each table.columns as column, colIndex}
        <tr>
          <th class="is-flex is-flex-direction-row">
            <button class="button is-danger mr-3" on:click={() => removeCell(tableIndex, colIndex)}> X </button>
            <input class="input" bind:value={column.name} on:change={() => { correctTableName(colIndex); changeCallback(dbData.tables); updateAllFields()}}/>
          </th>
          <th>
            <div class="select">
              <select bind:value={column.type.type} on:change={() => changeCallback(dbData.tables)}>
                {#each allTypes as opt}
                  <option value={opt}> { opt } </option>
                {/each}
              </select>
            </div>
          </th>
          <th>
            <label class="checkbox">
              <input type="checkbox" bind:checked={column.primary} on:change={() => changeCallback(dbData.tables)}>
            </label>
          </th>
          <th>
            <label class="checkbox">
              <input type="checkbox" bind:checked={column.unique} on:change={() => changeCallback(dbData.tables)}>
            </label>
          </th>
          <th>
            <label class="checkbox">
              <input type="checkbox" bind:checked={column.null} on:change={() => changeCallback(dbData.tables)}>
            </label>
          </th>
          <th>
            <div class="select">
              <select bind:value={column.references} on:change={() => changeCallback(dbData.tables)}>
                <option value={null}></option>
                {#each allFields as field}
                  <option value={field}> {field}</option>
                {/each}
              </select>
            </div>
          </th>
        </tr>
      {/each}
    </tbody>
    <tfoot>
      <tr>
        <th colspan="6"><button class="button is-fullwidth" on:click={() => addCell(tableIndex)}> Создать ряд </button></th>
      </tr>
    </tfoot>
  </table>
</div>
{/each}
