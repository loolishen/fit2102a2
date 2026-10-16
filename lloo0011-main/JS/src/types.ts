export type State = Readonly<{
    model: string;
    run: boolean;
}>;

export type Output = Readonly<
    { result: string; error: "" } | { result: ""; error: string }
>;

export type Action<T> = (_: T) => T;
