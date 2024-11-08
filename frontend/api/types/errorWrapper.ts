export type ErrorWrapper<T> = {
  type: T | null,
  error: string | null,
  value: string | null
  body: object
}

export function createErrorWrapper<T>(fn: (type: any) => T | null, body: object): ErrorWrapper<T> {
  //@ts-ignore
  const type = fn(body.type)
  return {
    type: type,
    //@ts-ignore
    error: 'error' in body ? body.error : null,
    //@ts-ignore
    value: 'value' in body ? body.value : null,
    body: body
  }
}

export function genericArrayErrorCreator<T>(allErrors: any): (type: any) => T | null {
  return (type: any) => {
    if (allErrors.includes(type)) {
      return type as T
    } else {
      return null
    }
  }
}
